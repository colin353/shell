use pty;
use regex::Regex;
use std::sync::LazyLock;

/// Result of handling CTRL+C on a pane.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CtrlCResult {
    /// Forwarded CTRL+C to a running subprocess (via PTY, letting it decide how to handle)
    KilledSubprocess,
    /// Cleared the shell input buffer
    ClearedInput,
    /// Input was already empty, caller should close this pane
    ClosePane,
}

/// Result of handling input on a pane.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PaneInputResult {
    /// No changes, no rerender needed
    None,
    /// Content changed, rerender needed
    Rerender,
    /// Request to rename the containing window/tab
    RenameWindow(String),
    /// The pane connected to a remote host; the compositor should mark the
    /// containing tab as remote-owned so new splits auto-connect.
    ConnectedRemote(String),
}

/// State for the `reconnect` session picker: choose a persistent session on a
/// host to resume (or kill). Lives in the pane because resuming/killing is a
/// compositor operation and the data comes from `ssh <host> shell sessions`.
pub struct SessionPicker {
    target: String,
    env: Vec<(String, String)>,
    /// `(name, age)` for each live session.
    sessions: Vec<(String, String)>,
    selected: usize,
}

fn is_replayable_typeahead(input: &[u8]) -> bool {
    !input.is_empty()
        && input.iter().all(
            |&byte| matches!(byte, b'\t' | b'\r' | b'\n' | 0x08 | 0x7f | 0x20..=0x7e | 0x80..=0xff),
        )
}

/// Regex pattern for matching URLs (based on Alacritty's approach)
/// Matches common URL schemes followed by valid URL characters
static URL_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    // Supported schemes: ipfs, ipns, magnet, mailto, gemini, gopher, https, http, news, file, git, ssh, ftp
    // Excluded characters: C0/C1 control chars, angle brackets, quotes, whitespace, braces, caret, backtick, backslash
    Regex::new(concat!(
        r"(?i)(ipfs:|ipns:|magnet:|mailto:|gemini://|gopher://|https://|http://|news:|file:|git://|ssh:|ftp://)",
        r#"[^\x00-\x1f\x7f-\x9f<>"\s{}\^`\\]+"#
    )).unwrap()
});

/// Post-process a URL match to handle bracket balancing and trailing delimiter trimming.
/// This follows Alacritty's approach for better URL extraction.
fn post_process_url(url: &str) -> &str {
    let mut result = url;

    // Stage 1: Bracket balancing
    // Track opening/closing brackets and truncate if a closer has no matching opener
    result = balance_brackets(result);

    // Stage 2: Trim trailing delimiters that are likely sentence punctuation
    result = trim_trailing_delimiters(result);

    result
}

/// Balance brackets in a URL, truncating if a closing bracket has no matching opener.
fn balance_brackets(url: &str) -> &str {
    let bytes = url.as_bytes();
    let mut paren_depth: i32 = 0; // ()
    let mut bracket_depth: i32 = 0; // []

    let mut last_valid_end = 0;

    for (i, &byte) in bytes.iter().enumerate() {
        match byte {
            b'(' => paren_depth += 1,
            b')' => {
                paren_depth -= 1;
                if paren_depth < 0 {
                    // Unmatched closing paren - truncate here
                    return &url[..last_valid_end];
                }
            }
            b'[' => bracket_depth += 1,
            b']' => {
                bracket_depth -= 1;
                if bracket_depth < 0 {
                    // Unmatched closing bracket - truncate here
                    return &url[..last_valid_end];
                }
            }
            _ => {}
        }
        last_valid_end = i + 1;
    }

    url
}

/// Trim trailing punctuation that's likely sentence delimiters, not part of the URL.
fn trim_trailing_delimiters(url: &str) -> &str {
    let trailing_delimiters = ['.', ',', ':', ';', '?', '!', '(', '[', '\''];

    let mut result = url;
    while !result.is_empty() {
        if let Some(last_char) = result.chars().last() {
            if trailing_delimiters.contains(&last_char) {
                result = &result[..result.len() - last_char.len_utf8()];
            } else {
                break;
            }
        } else {
            break;
        }
    }

    result
}

/// A match found during search, storing the line index and column range
#[derive(Clone, Debug, PartialEq)]
pub struct SearchMatch {
    /// Line index (negative values are scrollback lines, counting from -1 as most recent scrollback)
    /// 0 and positive values are grid lines
    pub line_index: isize,
    /// Starting column of the match
    pub start_col: usize,
    /// Ending column of the match (exclusive)
    pub end_col: usize,
}

/// A URL match found in the terminal.
///
/// A URL may span several physical rows when a long line soft-wraps. The match
/// therefore records both a start and an end line: `(line_index, start_col)` is
/// the first cell of the URL and `(end_line_index, end_col)` is one past its
/// last cell. For a URL contained on a single row, `end_line_index == line_index`.
/// Columns are display columns (cell positions), not byte offsets.
#[derive(Clone, Debug, PartialEq)]
pub struct UrlMatch {
    /// Line index of the first cell (negative values are scrollback lines,
    /// counting from -1 as most recent scrollback; 0 and positive are grid lines).
    pub line_index: isize,
    /// Starting column of the match on `line_index`.
    pub start_col: usize,
    /// Line index of the last cell of the match.
    pub end_line_index: isize,
    /// Ending column of the match on `end_line_index` (exclusive).
    pub end_col: usize,
    /// The actual URL text (reassembled across wrapped rows).
    pub url: String,
}

impl UrlMatch {
    /// Whether the cell at `(line_index, col)` falls within this URL's span.
    /// Handles URLs that span multiple wrapped rows.
    pub fn contains(&self, line_index: isize, col: usize) -> bool {
        if line_index < self.line_index || line_index > self.end_line_index {
            return false;
        }
        let after_start = line_index > self.line_index || col >= self.start_col;
        let before_end = line_index < self.end_line_index || col < self.end_col;
        after_start && before_end
    }
}

/// Direction for incremental URL search
#[derive(Clone, Copy, PartialEq)]
enum SearchDirection {
    /// Search toward older content (up/back in history)
    Up,
    /// Search toward newer content (down/forward in history)  
    Down,
}

pub struct Pane {
    pub terminal_emulator: emulator::TerminalEmulator,
    /// The embedded shell instance
    pub shell: libshell::Shell,
    /// Currently running subprocess (if any) - takes over PTY when active
    pub subprocess: Option<pty::PtyProcess>,
    /// Active remote session (if any) - takes over the pane when connected.
    /// Mutually exclusive with `subprocess`.
    pub remote: Option<crate::remote::RemoteProcess>,
    /// Active `reconnect` session picker (if any) - intercepts input while open.
    pub session_picker: Option<SessionPicker>,
    /// Whether we sent SIGINT to the subprocess (to display CTRL+C instead of exit code)
    pub sent_sigint: bool,
    /// Temp file for edit-in-editor feature (CTRL+X CTRL+E)
    pub edit_temp_file: Option<std::path::PathBuf>,
    /// Line-oriented input typed while a subprocess is running.
    subprocess_typeahead: Vec<u8>,
    pub read_buffer: [u8; 4096],
    /// Whether the pane is in scrollback mode
    pub scrollback_mode: bool,
    /// Scroll offset (number of lines scrolled up from the bottom)
    pub scroll_offset: usize,
    /// Whether the pane is in search mode (sub-mode of scrollback)
    pub search_mode: bool,
    /// Whether the search input is focused (receiving keyboard input)
    pub search_input_focused: bool,
    /// Current search query
    pub search_query: String,
    /// All matches found for the current search
    pub search_matches: Vec<SearchMatch>,
    /// Index of the currently selected match (if any)
    pub current_match_index: Option<usize>,
    /// Whether the pane is in URL mode (sub-mode of scrollback)
    pub url_mode: bool,
    /// All URLs found in the terminal content
    pub url_matches: Vec<UrlMatch>,
    /// Index of the currently selected URL (if any)
    pub current_url_index: Option<usize>,

    /// Vim cursor engine for scrollback navigation (persisted across inputs)
    pub vim_engine: libvim::VimCursorEngine<'static>,
}

impl Pane {
    /// Create a new pane with the given dimensions.
    pub fn new(width: usize, height: usize) -> Self {
        let (shell, initial_output) = libshell::Shell::new(width as u16, height as u16);
        let mut terminal_emulator = emulator::TerminalEmulator::new(width, height);

        // Process the initial prompt output
        terminal_emulator.process(&initial_output);

        Pane {
            terminal_emulator,
            shell,
            subprocess: None,
            remote: None,
            session_picker: None,
            sent_sigint: false,
            edit_temp_file: None,
            subprocess_typeahead: Vec::new(),
            read_buffer: [0u8; 4096],
            scrollback_mode: false,
            scroll_offset: 0,
            search_mode: false,
            search_input_focused: false,
            search_query: String::new(),
            search_matches: Vec::new(),
            current_match_index: None,
            url_mode: false,
            url_matches: Vec::new(),
            current_url_index: None,
            vim_engine: libvim::VimCursorEngine::new_owned(Vec::new(), height, width),
        }
    }

    /// Create a new pane with a shared ShellCore (for custom history).
    pub fn with_core(
        width: usize,
        height: usize,
        core: std::sync::Arc<libshell::ShellCore>,
    ) -> Self {
        let (shell, initial_output) = libshell::Shell::with_core(core, width as u16, height as u16);
        let mut terminal_emulator = emulator::TerminalEmulator::new(width, height);

        // Process the initial prompt output
        terminal_emulator.process(&initial_output);

        Pane {
            terminal_emulator,
            shell,
            subprocess: None,
            remote: None,
            session_picker: None,
            sent_sigint: false,
            edit_temp_file: None,
            subprocess_typeahead: Vec::new(),
            read_buffer: [0u8; 4096],
            scrollback_mode: false,
            scroll_offset: 0,
            search_mode: false,
            search_input_focused: false,
            search_query: String::new(),
            search_matches: Vec::new(),
            current_match_index: None,
            url_mode: false,
            url_matches: Vec::new(),
            current_url_index: None,
            vim_engine: libvim::VimCursorEngine::new_owned(Vec::new(), height, width),
        }
    }

    /// Handle keyboard input.
    ///
    /// If a subprocess is running, input goes to it.
    /// Otherwise, input goes to the shell.
    ///
    /// Returns a `PaneInputResult` indicating what action the compositor should take.
    pub fn handle_input(&mut self, input: &[u8]) -> PaneInputResult {
        if self.session_picker.is_some() {
            return self.handle_session_picker_input(input);
        }
        if let Some(remote) = self.remote.as_mut() {
            // Connected to a remote session: forward keystrokes verbatim.
            let _ = remote.write(input);
            return PaneInputResult::None;
        }
        if self.subprocess.is_some() {
            self.record_subprocess_typeahead(input);
            let Some(ref mut proc) = self.subprocess else {
                return PaneInputResult::None;
            };
            // Subprocess is active - send input directly to it
            let _ = proc.write(input);
            // Subprocess output will trigger rerender via poll
            PaneInputResult::None
        } else {
            self.handle_shell_input(input)
        }
    }

    fn record_subprocess_typeahead(&mut self, input: &[u8]) {
        if input.iter().any(|&byte| byte == 0x03 || byte == 0x04) {
            self.subprocess_typeahead.clear();
            return;
        }

        let should_buffer = self
            .subprocess
            .as_ref()
            .map(|proc| proc.is_canonical_echo_mode())
            .unwrap_or(false)
            && is_replayable_typeahead(input);

        if should_buffer {
            self.subprocess_typeahead.extend_from_slice(input);
        } else {
            self.subprocess_typeahead.clear();
        }
    }

    fn handle_shell_input(&mut self, input: &[u8]) -> PaneInputResult {
        match self.shell.handle_input(input) {
            libshell::ShellAction::None => PaneInputResult::None,
            libshell::ShellAction::Output(data) => {
                self.terminal_emulator.process(&data);
                PaneInputResult::Rerender
            }
            libshell::ShellAction::SpawnSubprocess {
                output,
                command,
                args,
                env,
                cwd,
                history_id: _,
            } => {
                // First, write any pending output (e.g., the newline after the command)
                if !output.is_empty() {
                    self.terminal_emulator.process(&output);
                }

                // Build the full command string
                let full_command = if args.is_empty() {
                    command
                } else {
                    format!("{} {}", command, args.join(" "))
                };

                // Spawn the subprocess
                let width = self.terminal_emulator.grid().cols as u16;
                let height = self.terminal_emulator.grid().rows as u16;

                // Change to the shell's cwd before spawning
                let _ = std::env::set_current_dir(&cwd);

                self.subprocess_typeahead.clear();
                match pty::PtyProcess::spawn_with_env(&full_command, width, height, &env) {
                    Ok(proc) => {
                        self.subprocess = Some(proc);
                    }
                    Err(e) => {
                        // Failed to spawn - show error and prompt
                        let error_msg = format!("spawn error: {}\r\n", e);
                        self.terminal_emulator.process(error_msg.as_bytes());
                        let prompt = self.shell.subprocess_exited(1);
                        self.terminal_emulator.process(&prompt);
                    }
                }
                PaneInputResult::Rerender
            }
            libshell::ShellAction::RenameWindow { output, name } => {
                // Write any pending output (e.g., newline after the command)
                if !output.is_empty() {
                    self.terminal_emulator.process(&output);
                }
                // Show the prompt
                let prompt = self.shell.get_prompt();
                self.terminal_emulator.process(prompt.as_bytes());
                PaneInputResult::RenameWindow(name)
            }
            libshell::ShellAction::EditInEditor {
                output,
                temp_file,
                editor,
            } => {
                // Write any pending output
                if !output.is_empty() {
                    self.terminal_emulator.process(&output);
                }

                // Spawn the editor as a subprocess with the temp file as argument
                let width = self.terminal_emulator.grid().cols as u16;
                let height = self.terminal_emulator.grid().rows as u16;
                let full_command = format!("{} {}", editor, temp_file.display());

                self.subprocess_typeahead.clear();
                let env = libshell::shell_env_snapshot();
                match pty::PtyProcess::spawn_with_env(&full_command, width, height, &env) {
                    Ok(proc) => {
                        self.subprocess = Some(proc);
                        // Store the temp file path so we can read it when editor exits
                        self.edit_temp_file = Some(temp_file);
                    }
                    Err(e) => {
                        // Failed to spawn editor - show error and prompt
                        let error_msg = format!("editor error: {}\r\n", e);
                        self.terminal_emulator.process(error_msg.as_bytes());
                        // Clean up temp file
                        let _ = std::fs::remove_file(&temp_file);
                        let prompt = self.shell.get_prompt();
                        self.terminal_emulator.process(prompt.as_bytes());
                    }
                }
                PaneInputResult::Rerender
            }
            libshell::ShellAction::Connect {
                output,
                target,
                session,
                env,
            } => {
                // Write pending output (e.g. the newline after the command).
                if !output.is_empty() {
                    self.terminal_emulator.process(&output);
                }

                match self.connect_remote_session(&target, session.as_deref(), &env) {
                    Ok(()) => PaneInputResult::ConnectedRemote(target),
                    Err(e) => {
                        let error_msg = format!("connect error: {}\r\n", e);
                        self.terminal_emulator.process(error_msg.as_bytes());
                        // Treat as an immediately-failed command so the prompt
                        // returns and the exit is recorded.
                        let prompt = self.shell.subprocess_exited(1);
                        self.terminal_emulator.process(&prompt);
                        PaneInputResult::Rerender
                    }
                }
            }
            libshell::ShellAction::Reconnect {
                output,
                target,
                env,
            } => {
                if !output.is_empty() {
                    self.terminal_emulator.process(&output);
                }
                match crate::remote::list_sessions(&target) {
                    Ok(sessions) if !sessions.is_empty() => {
                        self.session_picker = Some(SessionPicker {
                            target,
                            env,
                            sessions,
                            selected: 0,
                        });
                        self.render_session_picker();
                    }
                    Ok(_) => {
                        let msg = format!("no sessions on {}\r\n", target);
                        self.terminal_emulator.process(msg.as_bytes());
                        let prompt = self.shell.subprocess_exited(0);
                        self.terminal_emulator.process(&prompt);
                    }
                    Err(e) => {
                        let msg = format!("reconnect: {}\r\n", e);
                        self.terminal_emulator.process(msg.as_bytes());
                        let prompt = self.shell.subprocess_exited(1);
                        self.terminal_emulator.process(&prompt);
                    }
                }
                PaneInputResult::Rerender
            }
            libshell::ShellAction::Exit => {
                // Shell wants to exit - could close the pane
                // For now, just show a message
                self.terminal_emulator.process(b"[shell exited]\r\n");
                PaneInputResult::Rerender
            }
        }
    }

    /// Handle keystrokes while the `reconnect` session picker is open.
    /// Navigation stays entirely local; only resume/kill/cancel act on the
    /// chosen session. Cancel uses Esc (the compositor intercepts Ctrl+C).
    fn handle_session_picker_input(&mut self, input: &[u8]) -> PaneInputResult {
        enum Key {
            Up,
            Down,
            Resume,
            Kill,
            Cancel,
            Ignore,
        }
        let key = match input {
            b"\x1b[A" | b"\x1b[OA" | b"k" => Key::Up,
            b"\x1b[B" | b"\x1b[OB" | b"j" => Key::Down,
            b"\r" | b"\n" => Key::Resume,
            b"d" => Key::Kill,
            b"\x1b" | b"q" => Key::Cancel,
            _ => Key::Ignore,
        };

        let picker = match self.session_picker.as_mut() {
            Some(p) => p,
            None => return PaneInputResult::None,
        };

        match key {
            Key::Up => {
                picker.selected = picker.selected.saturating_sub(1);
                self.render_session_picker();
                PaneInputResult::Rerender
            }
            Key::Down => {
                if picker.selected + 1 < picker.sessions.len() {
                    picker.selected += 1;
                }
                self.render_session_picker();
                PaneInputResult::Rerender
            }
            Key::Resume => {
                let target = picker.target.clone();
                let env = picker.env.clone();
                let name = picker.sessions[picker.selected].0.clone();
                self.session_picker = None;
                // Wipe the picker; the remote repaints authoritatively via
                // GridResync once the link is up.
                self.terminal_emulator.process(b"\x1b[2J\x1b[H\x1b[?25h");
                match self.connect_remote_session(&target, Some(&name), &env) {
                    Ok(()) => PaneInputResult::ConnectedRemote(target),
                    Err(e) => {
                        let msg = format!("connect error: {}\r\n", e);
                        self.terminal_emulator.process(msg.as_bytes());
                        let prompt = self.shell.subprocess_exited(1);
                        self.terminal_emulator.process(&prompt);
                        PaneInputResult::Rerender
                    }
                }
            }
            Key::Kill => {
                let target = picker.target.clone();
                let name = picker.sessions[picker.selected].0.clone();
                let _ = crate::remote::kill_session(&target, &name);
                picker.sessions.remove(picker.selected);
                if picker.selected >= picker.sessions.len() {
                    picker.selected = picker.selected.saturating_sub(1);
                }
                if picker.sessions.is_empty() {
                    self.close_session_picker();
                } else {
                    self.render_session_picker();
                }
                PaneInputResult::Rerender
            }
            Key::Cancel => {
                self.close_session_picker();
                PaneInputResult::Rerender
            }
            Key::Ignore => PaneInputResult::None,
        }
    }

    /// Dismiss the picker and return the pane to its local shell prompt by
    /// completing the tracked `reconnect` command.
    fn close_session_picker(&mut self) {
        self.session_picker = None;
        self.terminal_emulator.process(b"\x1b[2J\x1b[H\x1b[?25h");
        let prompt = self.shell.subprocess_exited(0);
        self.terminal_emulator.process(&prompt);
    }

    /// Draw the session picker over the pane's screen: a title, the list of
    /// resumable sessions (selected one in inverse video), and key hints.
    fn render_session_picker(&mut self) {
        let Some(picker) = self.session_picker.as_ref() else {
            return;
        };
        let mut out = String::new();
        // Clear, home, and hide the cursor while the picker owns the screen.
        out.push_str("\x1b[2J\x1b[H\x1b[?25l");
        out.push_str(&format!("\x1b[1mSessions on {}\x1b[0m\r\n\r\n", picker.target));
        for (i, (name, age)) in picker.sessions.iter().enumerate() {
            let age = if age.is_empty() {
                String::new()
            } else {
                format!("  ({})", age)
            };
            if i == picker.selected {
                out.push_str(&format!("\x1b[7m  {}{}  \x1b[0m\r\n", name, age));
            } else {
                out.push_str(&format!("  {}{}\r\n", name, age));
            }
        }
        out.push_str(
            "\r\n\x1b[2m\u{2191}/\u{2193} or j/k: select   \u{23ce}: resume   d: kill   Esc: cancel\x1b[0m",
        );
        self.terminal_emulator.process(out.as_bytes());
    }

    /// Read available data from the subprocess and process it through the emulator.
    /// Also checks if the subprocess has exited and returns control to the shell.
    pub fn read_and_process(&mut self) {
        // Check for DEBUG_PTY env var for debugging
        let debug = std::env::var("DEBUG_PTY").is_ok();
        self.read_and_process_internal(debug)
    }

    /// Read and process with optional debug output
    #[allow(dead_code)]
    pub fn read_and_process_debug(&mut self) {
        self.read_and_process_internal(true)
    }

    fn read_and_process_internal(&mut self, debug: bool) {
        if self.remote.is_some() {
            self.read_and_process_remote();
            return;
        }
        if let Some(ref proc) = self.subprocess {
            // Read all available data from subprocess
            loop {
                match proc.read(&mut self.read_buffer) {
                    Ok(Some(0)) => break, // EOF
                    Ok(Some(n)) => {
                        if debug {
                            eprintln!(
                                "PTY read {} bytes: {:?}",
                                n,
                                String::from_utf8_lossy(&self.read_buffer[..n])
                            );
                            eprintln!("  Raw bytes: {:02x?}", &self.read_buffer[..n]);
                        }
                        // Process through terminal emulator
                        self.terminal_emulator.process(&self.read_buffer[..n]);

                        // Handle any responses from the terminal (e.g., cursor position queries)
                        let responses = self.terminal_emulator.drain_responses();
                        for response in responses {
                            if let Some(ref proc) = self.subprocess {
                                let _ = proc.write(&response);
                            }
                        }
                    }
                    Ok(None) => break, // No more data available (EAGAIN)
                    Err(_) => break,   // Error reading
                }
            }
        }

        // Check if subprocess has exited
        if let Some(ref proc) = self.subprocess {
            if let Some(exit_code) = proc.try_wait() {
                let should_replay_typeahead =
                    self.edit_temp_file.is_none() && !(self.sent_sigint && exit_code == 130);
                let typeahead = if should_replay_typeahead {
                    std::mem::take(&mut self.subprocess_typeahead)
                } else {
                    self.subprocess_typeahead.clear();
                    Vec::new()
                };

                // Check if cursor is not at the start of a line (partial line output)
                // If so, emit a partial line indicator before the prompt
                let cursor_x = self.terminal_emulator.cursor_position().0;
                if cursor_x != 0 {
                    if typeahead.is_empty() {
                        // Emit '%' with inverted colors, then reset, then newline
                        // \x1b[7m = inverse video, \x1b[0m = reset, \r\n = newline
                        self.terminal_emulator.process(b"\x1b[7m%\x1b[0m\r\n");
                    } else {
                        // Clear locally echoed typeahead so replay renders it at the shell prompt.
                        self.terminal_emulator.process(b"\r\x1b[2K");
                    }
                }

                // Subprocess exited - clean up
                drop(self.subprocess.take());

                // Reset cursor visibility - subprocess may have hidden it
                self.terminal_emulator.grid_mut().cursor_visible = true;

                // Check if this was an edit-in-editor session
                if let Some(temp_file) = self.edit_temp_file.take() {
                    // Editor exited - read the temp file and replace input
                    let output = self.shell.editor_exited(&temp_file);
                    self.terminal_emulator.process(&output);
                } else if self.sent_sigint && exit_code == 130 {
                    // Check if we sent SIGINT (CTRL+C) - if so, use subprocess_killed
                    // Exit code 130 = 128 + 2 (SIGINT)
                    self.sent_sigint = false;
                    let output = self.shell.subprocess_killed();
                    self.terminal_emulator.process(&output);
                } else {
                    self.sent_sigint = false;
                    let output = self.shell.subprocess_exited(exit_code);
                    self.terminal_emulator.process(&output);
                }

                if !typeahead.is_empty() && self.subprocess.is_none() {
                    let _ = self.handle_shell_input(&typeahead);
                }
            }
        }
    }

    /// Connect this pane to a remote host, taking over from the local shell.
    /// Reusable by the `connect` builtin and by auto-connected splits in a
    /// remote-owned tab. Returns `Err` if spawning the transport fails.
    pub fn connect_remote(&mut self, target: &str, env: &[(String, String)]) -> std::io::Result<()> {
        self.connect_remote_session(target, None, env)
    }

    /// Like [`connect_remote`](Self::connect_remote), but to a named session
    /// (reattach-or-create). `None` makes a fresh per-pane session.
    pub fn connect_remote_session(
        &mut self,
        target: &str,
        session: Option<&str>,
        env: &[(String, String)],
    ) -> std::io::Result<()> {
        let width = self.terminal_emulator.grid().cols as u16;
        let height = self.terminal_emulator.grid().rows as u16;
        self.subprocess_typeahead.clear();
        let remote = crate::remote::RemoteProcess::connect(target, session, width, height, env)?;
        self.remote = Some(remote);
        Ok(())
    }

    /// If this pane is remote, ask it to repaint from the authoritative remote
    /// screen (used by `Ctrl-b r`).
    pub fn request_remote_resync(&mut self) {
        if let Some(remote) = self.remote.as_mut() {
            let _ = remote.request_resync();
        }
    }

    /// Take a window-rename pushed up by the remote session, if any.
    pub fn take_remote_title(&mut self) -> Option<String> {
        self.remote.as_mut().and_then(|r| r.take_title())
    }

    /// Drain output from the active remote session into the emulator, and on
    /// transport exit return the pane to its local shell.
    fn read_and_process_remote(&mut self) {
        loop {
            let result = match self.remote.as_mut() {
                Some(remote) => remote.read(&mut self.read_buffer),
                None => break,
            };
            match result {
                Ok(Some(0)) => break, // EOF
                Ok(Some(n)) => {
                    self.terminal_emulator.process(&self.read_buffer[..n]);
                    let responses = self.terminal_emulator.drain_responses();
                    if !responses.is_empty() {
                        if let Some(remote) = self.remote.as_mut() {
                            for response in responses {
                                let _ = remote.write(&response);
                            }
                        }
                    }
                }
                Ok(None) => break, // nothing available right now
                Err(_) => break,
            }
        }

        // If the transport ended, tear down and return to the local shell.
        if let Some(exit_code) = self.remote.as_mut().and_then(|r| r.try_wait()) {
            self.remote = None;
            self.terminal_emulator.grid_mut().cursor_visible = true;
            let output = self.shell.subprocess_exited(exit_code);
            self.terminal_emulator.process(&output);
        }
    }

    /// Check if the pane is still active (shell hasn't exited).
    pub fn is_running(&self) -> bool {
        // Pane is running if shell hasn't exited
        !self.shell.should_exit()
    }

    /// Check if a subprocess is currently running.
    pub fn has_subprocess(&self) -> bool {
        self.subprocess.is_some()
    }

    pub fn mouse_mode(&self) -> emulator::MouseMode {
        if self.subprocess.is_some() {
            self.terminal_emulator.mouse_mode()
        } else {
            emulator::MouseMode::default()
        }
    }

    /// Handle CTRL+C with cascading logic.
    ///
    /// Returns a `CtrlCResult` indicating what action was taken:
    /// - `KilledSubprocess`: Forwarded CTRL+C to the running subprocess
    /// - `ClearedInput`: Cleared the shell input buffer (or showed ^C on empty line)
    pub fn handle_ctrl_c(&mut self) -> CtrlCResult {
        if let Some(remote) = self.remote.as_mut() {
            // Remote session owns the screen (it may be in a history picker, an
            // app, or at a prompt). Forward the interrupt and let the remote
            // decide; never run the local shell handler, which would paint a
            // local prompt over the remote content.
            let _ = remote.write(&[0x03]);
            return CtrlCResult::ClearedInput;
        }
        if let Some(ref mut proc) = self.subprocess {
            // CTRL+C - forward to subprocess via PTY
            // The TTY driver will handle SIGINT generation based on terminal settings
            // This allows programs that intercept CTRL+C to handle it themselves
            let _ = proc.write(&[0x03]);
            self.sent_sigint = true;
            CtrlCResult::KilledSubprocess
        } else {
            // Shell is active - clear input or show ^C
            if let Some(output) = self.shell.handle_ctrl_c() {
                self.terminal_emulator.process(&output);
            } else {
                // Input was already empty - just show ^C and new prompt
                let prompt = self.shell.get_prompt();
                let output = format!("^C\r\n{}", prompt);
                self.terminal_emulator.process(output.as_bytes());
            }
            CtrlCResult::ClearedInput
        }
    }

    /// Handle CTRL+D with cascading logic.
    ///
    /// Returns a `CtrlCResult` indicating what action was taken:
    /// - `KilledSubprocess`: Sent EOF to the running subprocess
    /// - `ClosePane`: Input was empty, caller should close this pane
    pub fn handle_ctrl_d(&mut self) -> CtrlCResult {
        if let Some(remote) = self.remote.as_mut() {
            // Forward EOF to the remote (e.g. exits the remote shell, which then
            // returns this pane to the local shell). Don't close the pane here.
            let _ = remote.write(&[0x04]);
            return CtrlCResult::ClearedInput;
        }
        if let Some(ref proc) = self.subprocess {
            // CTRL+D - send EOF to subprocess
            let _ = proc.write(&[0x04]);
            CtrlCResult::KilledSubprocess
        } else {
            // Shell is active - close pane if input is empty
            if self.shell.input_is_empty() {
                CtrlCResult::ClosePane
            } else {
                // Input is not empty - do nothing (or could delete char under cursor)
                CtrlCResult::ClearedInput
            }
        }
    }

    /// Get the file descriptor to poll for this pane's active backend (remote
    /// session or local subprocess), if any.
    pub fn subprocess_fd(&self) -> Option<std::os::fd::RawFd> {
        use std::os::fd::AsRawFd;
        if let Some(remote) = self.remote.as_ref() {
            return Some(remote.as_raw_fd());
        }
        self.subprocess.as_ref().map(|p| p.as_raw_fd())
    }

    /// Enter scrollback mode
    pub fn enter_scrollback_mode(&mut self) {
        self.scrollback_mode = true;
        self.scroll_offset = 0;

        // Initialize vim cursor at the terminal's current cursor position
        let grid = self.terminal_emulator.grid();
        let scrollback_len = grid.scrollback_len();

        // Convert grid cursor position to absolute line number
        // Grid cursor_y is relative to the grid, so add scrollback_len to get absolute position
        let abs_row = scrollback_len + grid.cursor_y;
        let col = grid.cursor_x;

        // Initialize vim engine with current lines and cursor position
        let lines = self.get_all_lines();
        let viewport_height = self.viewport_height();
        let viewport_width = self.viewport_width();
        self.vim_engine =
            libvim::VimCursorEngine::new_owned(lines, viewport_height, viewport_width);
        self.vim_engine.cursor = libvim::Position::new(abs_row, col);
        self.vim_engine.scroll_offset_row = scrollback_len.saturating_sub(self.scroll_offset);

        // Start with scroll at bottom (showing the grid, no scrollback)
        self.scroll_offset = 0;
    }

    /// Exit scrollback mode
    pub fn exit_scrollback_mode(&mut self) {
        self.scrollback_mode = false;
        self.scroll_offset = 0;
        // Also clear search state
        self.search_mode = false;
        self.search_input_focused = false;
        self.search_query.clear();
        self.search_matches.clear();
        self.current_match_index = None;
        // Also clear URL mode state
        self.url_mode = false;
        self.url_matches.clear();
        self.current_url_index = None;
        // Reset vim state
        self.vim_engine.mode = libvim::Mode::Normal;
        self.vim_engine.input_state = libvim::InputState::default();
    }

    /// Get the total number of lines (scrollback + grid)
    pub fn total_lines(&self) -> usize {
        let grid = self.terminal_emulator.grid();
        grid.scrollback_len() + grid.rows
    }

    /// Get all lines (scrollback + grid) as strings for vim processing
    fn get_all_lines(&self) -> Vec<String> {
        let grid = self.terminal_emulator.grid();
        let scrollback_len = grid.scrollback_len();
        let grid_rows = grid.rows;
        let mut lines = Vec::with_capacity(scrollback_len + grid_rows);

        // Add scrollback lines (oldest first), trimming trailing whitespace
        for i in 0..scrollback_len {
            if let Some(row) = grid.get_scrollback_row(i) {
                let line: String = row.iter().map(|c| c.character).collect();
                lines.push(line.trim_end().to_string());
            }
        }

        // Add grid lines, trimming trailing whitespace
        for y in 0..grid_rows {
            if let Some(row) = grid.get_row(y) {
                let line: String = row.iter().map(|c| c.character).collect();
                lines.push(line.trim_end().to_string());
            }
        }

        lines
    }

    /// Get the viewport height for vim
    fn viewport_height(&self) -> usize {
        self.terminal_emulator.grid().rows
    }

    /// Get the viewport width for vim
    fn viewport_width(&self) -> usize {
        self.terminal_emulator.grid().cols
    }

    /// Handle vim input in scrollback mode
    /// Returns true if the input was handled, false if it should be passed to search mode
    pub fn handle_vim_input(&mut self, input: &[u8]) -> bool {
        // Note: Lines are captured once when entering scrollback mode (in enter_scrollback_mode)
        // and are not updated on each keystroke since scrollback is frozen in this mode.

        // Update viewport dimensions in case of resize
        self.vim_engine.viewport_height = self.viewport_height();
        self.vim_engine.viewport_width = self.viewport_width();

        // Calculate scroll_offset_row from our scroll_offset
        // Our scroll_offset is lines from bottom, vim's is lines from top
        let scrollback_len = self.terminal_emulator.grid().scrollback_len();
        // scroll_offset=0 means showing grid (bottom), so scroll_offset_row = scrollback_len
        // scroll_offset=scrollback_len means showing from top
        self.vim_engine.scroll_offset_row = scrollback_len.saturating_sub(self.scroll_offset);

        // Process input
        self.vim_engine.handle_input(input);

        // Convert vim scroll_offset_row back to our scroll_offset
        // vim scroll_offset_row is the first visible line from top
        // we need to convert to lines from bottom
        let new_scroll_row = self.vim_engine.scroll_offset_row;
        let viewport_height = self.viewport_height();
        // The bottom of visible area is at new_scroll_row + viewport_height - 1
        // If that's >= scrollback_len, we're showing some grid
        // scroll_offset = how many scrollback lines are visible at top
        if new_scroll_row + viewport_height <= scrollback_len {
            // All visible lines are in scrollback
            self.scroll_offset = scrollback_len - new_scroll_row;
        } else if new_scroll_row >= scrollback_len {
            // All visible lines are in grid
            self.scroll_offset = 0;
        } else {
            // Mixed: some scrollback, some grid
            self.scroll_offset = scrollback_len - new_scroll_row;
        }

        // Clamp scroll_offset
        self.scroll_offset = self.scroll_offset.min(scrollback_len);

        true
    }

    /// Get vim cursor info for rendering (row, col, visible)
    /// Row is relative to the visible viewport
    pub fn get_vim_cursor_info(&self) -> Option<(usize, usize, bool)> {
        if !self.scrollback_mode {
            return None;
        }

        let scrollback_len = self.terminal_emulator.grid().scrollback_len();
        let viewport_height = self.viewport_height();

        // Calculate which line is at the top of the viewport
        // scroll_offset = number of scrollback lines shown
        // If scroll_offset > 0, first visible line is scrollback[scrollback_len - scroll_offset]
        let first_visible_line = scrollback_len.saturating_sub(self.scroll_offset);
        let last_visible_line = first_visible_line + viewport_height;

        // Check if cursor is visible
        let cursor = &self.vim_engine.cursor;
        if cursor.row >= first_visible_line && cursor.row < last_visible_line {
            let viewport_row = cursor.row - first_visible_line;
            Some((cursor.col, viewport_row, true))
        } else {
            None
        }
    }

    /// Get vim selection info for rendering
    /// Returns (start_row, start_col, end_row, end_col, mode) in absolute line coordinates
    pub fn get_vim_selection_info(
        &self,
    ) -> Option<(libvim::Position, libvim::Position, libvim::Mode)> {
        if !self.scrollback_mode || self.vim_engine.mode == libvim::Mode::Normal {
            return None;
        }
        Some((
            self.vim_engine.selection_start,
            self.vim_engine.selection_end,
            self.vim_engine.mode,
        ))
    }

    /// Get the current vim mode
    pub fn get_vim_mode(&self) -> libvim::Mode {
        self.vim_engine.mode
    }

    /// Check if in scrollback mode
    pub fn is_in_scrollback_mode(&self) -> bool {
        self.scrollback_mode
    }

    /// Scroll up by the given number of lines (increases scroll_offset)
    pub fn scroll_up(&mut self, lines: usize) {
        let max_offset = self.terminal_emulator.grid().scrollback_len();
        self.scroll_offset = (self.scroll_offset + lines).min(max_offset);
    }

    /// Scroll down by the given number of lines (decreases scroll_offset)
    pub fn scroll_down(&mut self, lines: usize) {
        self.scroll_offset = self.scroll_offset.saturating_sub(lines);
    }

    /// Get the current scroll offset
    pub fn scroll_offset(&self) -> usize {
        self.scroll_offset
    }

    /// Get the total number of scrollback lines
    pub fn scrollback_len(&self) -> usize {
        self.terminal_emulator.grid().scrollback_len()
    }

    /// Get the terminal emulator
    pub fn emulator(&self) -> &emulator::TerminalEmulator {
        &self.terminal_emulator
    }

    /// Enter search mode (must be in scrollback mode first)
    pub fn enter_search_mode(&mut self) {
        if self.scrollback_mode {
            self.search_mode = true;
            self.search_input_focused = true;
            self.search_query.clear();
            self.search_matches.clear();
            self.current_match_index = None;
        }
    }

    /// Exit search mode (back to scrollback mode)
    pub fn exit_search_mode(&mut self) {
        self.search_mode = false;
        self.search_input_focused = false;
        self.search_query.clear();
        self.search_matches.clear();
        self.current_match_index = None;
    }

    /// Check if in search mode
    pub fn is_in_search_mode(&self) -> bool {
        self.search_mode
    }

    /// Check if search input is focused
    pub fn is_search_input_focused(&self) -> bool {
        self.search_input_focused
    }

    /// Focus the search input
    pub fn focus_search_input(&mut self) {
        self.search_input_focused = true;
    }

    /// Unfocus the search input
    pub fn unfocus_search_input(&mut self) {
        self.search_input_focused = false;
    }

    /// Get the current search query
    pub fn search_query(&self) -> &str {
        &self.search_query
    }

    /// Get the current match index and total matches
    pub fn search_match_info(&self) -> (Option<usize>, usize) {
        (self.current_match_index, self.search_matches.len())
    }

    /// Handle a character input for the search query
    pub fn search_input_char(&mut self, c: char) {
        self.search_query.push(c);
        self.update_search();
    }

    /// Handle backspace for the search query
    pub fn search_input_backspace(&mut self) {
        self.search_query.pop();
        self.update_search();
    }

    /// Clear the search query
    pub fn search_clear(&mut self) {
        self.search_query.clear();
        self.update_search();
    }

    /// Update search results based on current query
    fn update_search(&mut self) {
        self.search_matches.clear();
        self.current_match_index = None;

        if self.search_query.is_empty() {
            return;
        }

        const MAX_MATCHES: usize = 100;

        let query_lower = self.search_query.to_lowercase();
        let grid = self.terminal_emulator.grid();
        let scrollback_len = grid.scrollback_len();

        // Search from bottom to top (most recent first) so we find the most relevant matches
        // when hitting the MAX_MATCHES limit

        // First, search current grid from bottom to top
        'grid: for y in (0..grid.rows).rev() {
            if let Some(row) = grid.get_row(y) {
                let line_text: String = row.iter().map(|c| c.character).collect();
                let line_lower = line_text.to_lowercase();

                // Find all matches in this line (still left to right within line)
                let mut search_start = 0;
                while let Some(pos) = line_lower[search_start..].find(&query_lower) {
                    let start_col = search_start + pos;
                    let end_col = start_col + self.search_query.len();
                    self.search_matches.push(SearchMatch {
                        line_index: y as isize,
                        start_col,
                        end_col,
                    });
                    if self.search_matches.len() >= MAX_MATCHES {
                        break 'grid;
                    }
                    search_start = start_col + 1;
                    if search_start >= line_lower.len() {
                        break;
                    }
                }
            }
        }

        // Then search scrollback from newest to oldest (only if we haven't hit the limit)
        // Scrollback index scrollback_len-1 is the most recent, 0 is the oldest
        if self.search_matches.len() < MAX_MATCHES {
            'scrollback: for i in (0..scrollback_len).rev() {
                if let Some(row) = grid.get_scrollback_row(i) {
                    let line_text: String = row.iter().map(|c| c.character).collect();
                    let line_lower = line_text.to_lowercase();

                    // Find all matches in this line
                    let mut search_start = 0;
                    while let Some(pos) = line_lower[search_start..].find(&query_lower) {
                        let start_col = search_start + pos;
                        let end_col = start_col + self.search_query.len();
                        // Convert scrollback index to our line_index format:
                        // scrollback line 0 (oldest) -> -(scrollback_len)
                        // scrollback line (scrollback_len - 1) (newest) -> -1
                        let line_index = (i as isize) - (scrollback_len as isize);
                        self.search_matches.push(SearchMatch {
                            line_index,
                            start_col,
                            end_col,
                        });
                        if self.search_matches.len() >= MAX_MATCHES {
                            break 'scrollback;
                        }
                        search_start = start_col + 1;
                        if search_start >= line_lower.len() {
                            break;
                        }
                    }
                }
            }
        }

        // Initially select the first match (which is now the most recent/bottom-most
        // since we searched from bottom to top)
        if !self.search_matches.is_empty() {
            self.current_match_index = Some(0);
            self.jump_to_current_match();
        }
    }

    /// Jump to the next match (toward bottom/more recent)
    pub fn next_match(&mut self) {
        if self.search_matches.is_empty() {
            return;
        }
        match self.current_match_index {
            Some(idx) if idx < self.search_matches.len() - 1 => {
                self.current_match_index = Some(idx + 1);
            }
            _ => {
                // Wrap to start
                self.current_match_index = Some(0);
            }
        }
        self.jump_to_current_match();
    }

    /// Jump to the previous match (toward top/older)
    pub fn prev_match(&mut self) {
        if self.search_matches.is_empty() {
            return;
        }
        match self.current_match_index {
            Some(idx) if idx > 0 => {
                self.current_match_index = Some(idx - 1);
            }
            _ => {
                // Wrap to end
                self.current_match_index = Some(self.search_matches.len() - 1);
            }
        }
        self.jump_to_current_match();
    }

    /// Adjust scroll_offset to make the current match visible and move vim cursor to it
    fn jump_to_current_match(&mut self) {
        if let Some(idx) = self.current_match_index {
            if let Some(m) = self.search_matches.get(idx) {
                let grid = self.terminal_emulator.grid();
                let scrollback_len = grid.scrollback_len();
                let visible_rows = grid.rows;

                // Convert line_index to absolute line number (for vim cursor)
                let abs_line = if m.line_index < 0 {
                    // Scrollback line: line_index = -1 means scrollback_len - 1
                    (scrollback_len as isize + m.line_index) as usize
                } else {
                    // Grid line
                    scrollback_len + m.line_index as usize
                };

                // Move vim cursor to the match
                self.vim_engine.cursor = libvim::Position::new(abs_line, m.start_col);

                // The display model (from composite_into):
                // - scroll_offset = number of scrollback lines shown at the top of the screen
                // - For screen row `row`:
                //   - If row < scroll_offset: shows scrollback[scrollback_len - scroll_offset + row]
                //   - If row >= scroll_offset: shows grid[row - scroll_offset]
                //
                // line_index encoding:
                // - Negative: scrollback line, -1 = most recent (scrollback_idx = scrollback_len - 1)
                // - Non-negative: grid row number

                if m.line_index < 0 {
                    // This is a scrollback line
                    // lines_back = how many lines back from grid start (1 = most recent scrollback)
                    let lines_back = (-m.line_index) as usize;

                    // The scrollback line at `lines_back` has scrollback_idx = scrollback_len - lines_back
                    // It appears at screen row = scroll_offset - lines_back
                    // For it to be visible (row >= 0 and row < visible_rows):
                    //   scroll_offset >= lines_back (so row >= 0)
                    //   scroll_offset - lines_back < visible_rows (so row < visible_rows)
                    //     => scroll_offset < visible_rows + lines_back

                    // Minimum scroll_offset to see this line (appears at bottom of scrollback area)
                    let min_offset = lines_back;

                    // Maximum scroll_offset to see this line (line must appear in visible area)
                    // row = scroll_offset - lines_back < visible_rows
                    // scroll_offset < visible_rows + lines_back
                    let max_offset = (visible_rows + lines_back).saturating_sub(1);

                    // Try to center the line on screen
                    // We want row ≈ visible_rows / 2
                    // scroll_offset - lines_back = visible_rows / 2
                    // scroll_offset = lines_back + visible_rows / 2
                    let ideal_offset = lines_back + visible_rows / 2;

                    // Clamp to valid range
                    self.scroll_offset = ideal_offset
                        .max(min_offset)
                        .min(max_offset)
                        .min(scrollback_len);
                } else {
                    // This is a grid line
                    let grid_line = m.line_index as usize;

                    // Grid row `grid_line` appears at screen row = scroll_offset + grid_line
                    // For it to be visible: scroll_offset + grid_line < visible_rows
                    // So: scroll_offset < visible_rows - grid_line
                    // Max valid scroll_offset = visible_rows - grid_line - 1

                    if grid_line < visible_rows {
                        let max_offset = visible_rows - grid_line - 1;
                        if self.scroll_offset > max_offset {
                            self.scroll_offset = max_offset;
                        }
                    } else {
                        // Grid line is beyond visible area - shouldn't happen normally
                        // but handle gracefully by scrolling to 0
                        self.scroll_offset = 0;
                    }
                }
            }
        }
    }

    /// Get search matches for highlighting
    pub fn get_search_matches(&self) -> &[SearchMatch] {
        &self.search_matches
    }

    /// Get the current match index
    pub fn current_match_index(&self) -> Option<usize> {
        self.current_match_index
    }

    /// Get the currently selected text in visual mode.
    /// Returns None if not in scrollback mode or not in visual mode.
    pub fn get_selected_text(&self) -> Option<String> {
        if !self.scrollback_mode {
            return None;
        }

        let mode = self.vim_engine.mode;
        if mode == libvim::Mode::Normal {
            return None;
        }

        let start = self.vim_engine.selection_start;
        let end = self.vim_engine.selection_end;

        // Get all lines for extracting text
        let lines = self.get_all_lines_for_selection();

        let mut result = String::new();

        match mode {
            libvim::Mode::Visual => {
                // Character-wise selection
                if start.row == end.row {
                    // Single line selection
                    if let Some(line) = lines.get(start.row) {
                        let start_col = start.col.min(line.len());
                        let end_col = (end.col + 1).min(line.len());
                        if start_col <= end_col {
                            result.push_str(&line[start_col..end_col]);
                        }
                    }
                } else {
                    // Multi-line selection
                    for row in start.row..=end.row {
                        if let Some(line) = lines.get(row) {
                            if row == start.row {
                                // First line: from start_col to end
                                let col = start.col.min(line.len());
                                result.push_str(&line[col..]);
                            } else if row == end.row {
                                // Last line: from start to end_col (inclusive)
                                let end_col = (end.col + 1).min(line.len());
                                result.push_str(&line[..end_col]);
                            } else {
                                // Middle lines: entire line
                                result.push_str(line);
                            }
                        }
                        if row < end.row {
                            result.push('\n');
                        }
                    }
                }
            }
            libvim::Mode::VisualLine => {
                // Line-wise selection: entire lines from start.row to end.row
                for row in start.row..=end.row {
                    if let Some(line) = lines.get(row) {
                        result.push_str(line);
                    }
                    if row < end.row {
                        result.push('\n');
                    }
                }
            }
            libvim::Mode::Normal => unreachable!(),
        }

        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    }

    /// Get all lines for selection extraction (same as get_all_lines but accessible for selection)
    fn get_all_lines_for_selection(&self) -> Vec<String> {
        let grid = self.terminal_emulator.grid();
        let scrollback_len = grid.scrollback_len();
        let grid_rows = grid.rows;
        let mut lines = Vec::with_capacity(scrollback_len + grid_rows);

        // Add scrollback lines (oldest first), trimming trailing whitespace
        for i in 0..scrollback_len {
            if let Some(row) = grid.get_scrollback_row(i) {
                let line: String = row.iter().map(|c| c.character).collect();
                lines.push(line.trim_end().to_string());
            }
        }

        // Add grid lines, trimming trailing whitespace
        for y in 0..grid_rows {
            if let Some(row) = grid.get_row(y) {
                let line: String = row.iter().map(|c| c.character).collect();
                lines.push(line.trim_end().to_string());
            }
        }

        lines
    }

    /// Enter URL mode (must be in scrollback mode first)
    /// Uses incremental search - only finds the first URL from the current cursor position
    pub fn enter_url_mode(&mut self) {
        if self.scrollback_mode {
            self.url_mode = true;
            self.url_matches.clear();
            self.current_url_index = None;

            // Find the first URL starting from the current cursor position, searching upward (older)
            // This is typically what users want - find a URL they just saw
            if let Some(url_match) = self.find_url_from_cursor(SearchDirection::Up) {
                self.url_matches.push(url_match);
                self.current_url_index = Some(0);
                self.jump_to_current_url();
            }
        }
    }

    /// Exit URL mode (back to scrollback mode)
    pub fn exit_url_mode(&mut self) {
        self.url_mode = false;
        self.url_matches.clear();
        self.current_url_index = None;
    }

    /// Check if in URL mode
    pub fn is_in_url_mode(&self) -> bool {
        self.url_mode
    }

    /// Convert absolute line number (vim cursor style) to line_index (UrlMatch style)
    /// Vim cursor: 0..scrollback_len are scrollback, scrollback_len..scrollback_len+grid_rows are grid
    /// line_index: negative for scrollback (-scrollback_len..-1), non-negative for grid (0..grid_rows)
    fn abs_line_to_line_index(&self, abs_line: usize) -> isize {
        let scrollback_len = self.terminal_emulator.grid().scrollback_len();
        if abs_line < scrollback_len {
            // Scrollback line: abs_line 0 -> -(scrollback_len), abs_line scrollback_len-1 -> -1
            (abs_line as isize) - (scrollback_len as isize)
        } else {
            // Grid line
            (abs_line - scrollback_len) as isize
        }
    }

    /// Convert line_index (UrlMatch style) to absolute line number (vim cursor style)
    fn line_index_to_abs_line(&self, line_index: isize) -> usize {
        let scrollback_len = self.terminal_emulator.grid().scrollback_len();
        if line_index < 0 {
            // Scrollback line
            (scrollback_len as isize + line_index) as usize
        } else {
            // Grid line
            scrollback_len + line_index as usize
        }
    }

    /// Total number of lines available (scrollback + visible grid).
    fn total_url_lines(&self) -> usize {
        let grid = self.terminal_emulator.grid();
        grid.scrollback_len() + grid.rows
    }

    /// Whether `abs_line` soft-wraps into `abs_line + 1` (so the two rows are
    /// part of a single logical line). Scrollback wrap data is not tracked, so
    /// scrollback rows are treated as un-wrapped.
    fn line_is_wrapped(&self, abs_line: usize) -> bool {
        let grid = self.terminal_emulator.grid();
        let scrollback_len = grid.scrollback_len();
        if abs_line < scrollback_len {
            false
        } else {
            grid.line_is_wrapped(abs_line - scrollback_len)
        }
    }

    /// First absolute line of the logical line that `abs_line` belongs to.
    fn logical_line_start(&self, mut abs_line: usize) -> usize {
        while abs_line > 0 && self.line_is_wrapped(abs_line - 1) {
            abs_line -= 1;
        }
        abs_line
    }

    /// Last absolute line of the logical line beginning at `start`.
    fn logical_line_end(&self, start: usize) -> usize {
        let total = self.total_url_lines();
        let mut end = start;
        while end + 1 < total && self.line_is_wrapped(end) {
            end += 1;
        }
        end
    }

    /// Reassemble the text of the logical line beginning at `start`, joining all
    /// rows it soft-wraps across. Returns the text together with a per-character
    /// map to the originating `(abs_line, display_col)` so regex matches can be
    /// translated back into grid coordinates. Wide-character spacer cells are
    /// skipped, so columns are true display columns rather than byte offsets.
    fn build_logical_line(&self, start: usize) -> (String, Vec<(usize, usize)>) {
        let grid = self.terminal_emulator.grid();
        let scrollback_len = grid.scrollback_len();
        let total = scrollback_len + grid.rows;

        let mut text = String::new();
        let mut map: Vec<(usize, usize)> = Vec::new();
        let mut abs_line = start;
        loop {
            let row = if abs_line < scrollback_len {
                grid.get_scrollback_row(abs_line)
            } else {
                grid.get_row(abs_line - scrollback_len)
            };
            if let Some(cells) = row {
                for (col, cell) in cells.iter().enumerate() {
                    if cell.is_wide_char_spacer {
                        continue;
                    }
                    text.push(cell.character);
                    map.push((abs_line, col));
                }
            }
            if abs_line + 1 < total && self.line_is_wrapped(abs_line) {
                abs_line += 1;
            } else {
                break;
            }
        }
        (text, map)
    }

    /// All URLs in the logical line beginning at `start`, in left-to-right
    /// order. Each entry is `(char_offset_in_logical_line, url_match)`.
    fn urls_in_logical_line(&self, start: usize) -> Vec<(usize, UrlMatch)> {
        let (text, map) = self.build_logical_line(start);
        let mut out = Vec::new();
        for mat in URL_REGEX.find_iter(&text) {
            let processed = post_process_url(mat.as_str());
            if processed.is_empty() {
                continue;
            }
            let char_start = text[..mat.start()].chars().count();
            let char_len = processed.chars().count();
            let char_end = char_start + char_len; // exclusive
            if char_start >= map.len() || char_end == 0 || char_end > map.len() {
                continue;
            }
            let (start_abs, start_col) = map[char_start];
            let (end_abs, end_col_cell) = map[char_end - 1];
            out.push((
                char_start,
                UrlMatch {
                    line_index: self.abs_line_to_line_index(start_abs),
                    start_col,
                    end_line_index: self.abs_line_to_line_index(end_abs),
                    end_col: end_col_cell + 1,
                    url: processed.to_string(),
                },
            ));
        }
        out
    }

    /// Character offset of `(abs_line, col)` within the logical line that begins
    /// at `start` (number of characters lying before that cell).
    fn cursor_offset_in_logical(&self, start: usize, abs_line: usize, col: usize) -> usize {
        let (_, map) = self.build_logical_line(start);
        map.iter()
            .take_while(|(al, c)| *al < abs_line || (*al == abs_line && *c < col))
            .count()
    }

    /// Last URL found in the nearest logical line strictly above the logical
    /// line beginning at `start`.
    fn last_url_above(&self, start: usize) -> Option<UrlMatch> {
        let mut s = start;
        while s > 0 {
            let prev_start = self.logical_line_start(s - 1);
            if let Some((_, um)) = self.urls_in_logical_line(prev_start).into_iter().last() {
                return Some(um);
            }
            if prev_start == 0 {
                break;
            }
            s = prev_start;
        }
        None
    }

    /// First URL found in the nearest logical line strictly below the logical
    /// line beginning at `start`.
    fn first_url_below(&self, start: usize) -> Option<UrlMatch> {
        let total = self.total_url_lines();
        let mut s = start;
        loop {
            let next = self.logical_line_end(s) + 1;
            if next >= total {
                return None;
            }
            if let Some((_, um)) = self.urls_in_logical_line(next).into_iter().next() {
                return Some(um);
            }
            s = next;
        }
    }

    /// Find a URL starting from the current cursor position in the given direction.
    /// Returns the first URL found, or None if no URL is found.
    fn find_url_from_cursor(&self, direction: SearchDirection) -> Option<UrlMatch> {
        let total = self.total_url_lines();
        if total == 0 {
            return None;
        }
        let cursor_abs_line = self.vim_engine.cursor.row.min(total - 1);
        let cursor_col = self.vim_engine.cursor.col;
        let cur_start = self.logical_line_start(cursor_abs_line);
        let cursor_offset = self.cursor_offset_in_logical(cur_start, cursor_abs_line, cursor_col);

        match direction {
            SearchDirection::Up => {
                // Closest URL on the current logical line that starts before the cursor.
                if let Some((_, um)) = self
                    .urls_in_logical_line(cur_start)
                    .into_iter()
                    .rev()
                    .find(|(off, _)| *off < cursor_offset)
                {
                    return Some(um);
                }
                self.last_url_above(cur_start)
            }
            SearchDirection::Down => {
                // Closest URL on the current logical line at or after the cursor.
                if let Some((_, um)) = self
                    .urls_in_logical_line(cur_start)
                    .into_iter()
                    .find(|(off, _)| *off >= cursor_offset)
                {
                    return Some(um);
                }
                self.first_url_below(cur_start)
            }
        }
    }

    /// Find a URL continuing from the current URL position in the given direction.
    /// Used for next_url/prev_url navigation.
    fn find_next_url_from_current(&self, direction: SearchDirection) -> Option<UrlMatch> {
        let current_url = self.url_matches.first()?;
        let start_abs = self.line_index_to_abs_line(current_url.line_index);
        let cur_start = self.logical_line_start(start_abs);

        match direction {
            SearchDirection::Up => {
                let cur_off =
                    self.cursor_offset_in_logical(cur_start, start_abs, current_url.start_col);
                if let Some((_, um)) = self
                    .urls_in_logical_line(cur_start)
                    .into_iter()
                    .rev()
                    .find(|(off, _)| *off < cur_off)
                {
                    return Some(um);
                }
                self.last_url_above(cur_start)
            }
            SearchDirection::Down => {
                let end_abs = self.line_index_to_abs_line(current_url.end_line_index);
                let cur_end_off =
                    self.cursor_offset_in_logical(cur_start, end_abs, current_url.end_col);
                if let Some((_, um)) = self
                    .urls_in_logical_line(cur_start)
                    .into_iter()
                    .find(|(off, _)| *off >= cur_end_off)
                {
                    return Some(um);
                }
                self.first_url_below(cur_start)
            }
        }
    }

    /// Find the first URL in the logical line containing `abs_line`.
    /// (Used by tests to inspect a single line's first match.)
    #[cfg(test)]
    fn find_first_url_in_line(&self, abs_line: usize) -> Option<UrlMatch> {
        let start = self.logical_line_start(abs_line);
        self.urls_in_logical_line(start)
            .into_iter()
            .next()
            .map(|(_, um)| um)
    }

    /// Jump to the next URL (toward bottom/more recent, j key)
    /// Uses incremental search - only searches when needed
    pub fn next_url(&mut self) {
        if let Some(url_match) = self.find_next_url_from_current(SearchDirection::Down) {
            self.url_matches.clear();
            self.url_matches.push(url_match);
            self.current_url_index = Some(0);
            self.jump_to_current_url();
        }
        // If no next URL found, stay at current position (no wrapping for incremental search)
    }

    /// Jump to the previous URL (toward top/older, k key)
    /// Uses incremental search - only searches when needed
    pub fn prev_url(&mut self) {
        if let Some(url_match) = self.find_next_url_from_current(SearchDirection::Up) {
            self.url_matches.clear();
            self.url_matches.push(url_match);
            self.current_url_index = Some(0);
            self.jump_to_current_url();
        }
        // If no previous URL found, stay at current position (no wrapping for incremental search)
    }

    /// Adjust scroll_offset to make the current URL visible
    fn jump_to_current_url(&mut self) {
        if let Some(idx) = self.current_url_index {
            if let Some(m) = self.url_matches.get(idx) {
                let grid = self.terminal_emulator.grid();
                let scrollback_len = grid.scrollback_len();
                let visible_rows = grid.rows;

                if m.line_index < 0 {
                    // This is a scrollback line
                    let lines_back = (-m.line_index) as usize;
                    let min_offset = lines_back;
                    let max_offset = (visible_rows + lines_back).saturating_sub(1);
                    let ideal_offset = lines_back + visible_rows / 2;

                    self.scroll_offset = ideal_offset
                        .max(min_offset)
                        .min(max_offset)
                        .min(scrollback_len);
                } else {
                    // This is a grid line
                    let grid_line = m.line_index as usize;

                    if grid_line < visible_rows {
                        let max_offset = visible_rows - grid_line - 1;
                        if self.scroll_offset > max_offset {
                            self.scroll_offset = max_offset;
                        }
                    } else {
                        self.scroll_offset = 0;
                    }
                }
            }
        }
    }

    /// Get the currently selected URL
    pub fn get_current_url(&self) -> Option<&str> {
        self.current_url_index
            .and_then(|idx| self.url_matches.get(idx))
            .map(|m| m.url.as_str())
    }

    /// Get URL info for status bar (current index, total count)
    pub fn url_match_info(&self) -> (Option<usize>, usize) {
        (self.current_url_index, self.url_matches.len())
    }

    /// Get URL matches for highlighting
    pub fn get_url_matches(&self) -> &[UrlMatch] {
        &self.url_matches
    }

    /// Get the current URL index
    pub fn current_url_index(&self) -> Option<usize> {
        self.current_url_index
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ------------------------------------------------------------------
    // Bug B: wrap-aware line editing (regression coverage).
    // ------------------------------------------------------------------

    /// Width (in columns) of the prompt currently shown on row 0, derived from
    /// where the terminal cursor sits immediately after the prompt is printed.
    fn prompt_cols(pane: &Pane) -> usize {
        pane.terminal_emulator.grid().cursor_x
    }

    /// The resting (row, col) the terminal cursor should occupy after `total`
    /// columns have been printed on a terminal `cols` wide (delayed-wrap rules).
    fn resting(total: usize, cols: usize) -> (usize, usize) {
        if total > 0 && total % cols == 0 {
            (total / cols - 1, cols - 1)
        } else {
            (total / cols, total % cols)
        }
    }

    fn type_str(pane: &mut Pane, s: &str) {
        for &b in s.as_bytes() {
            pane.handle_input(&[b]);
        }
    }

    fn joined_screen(pane: &Pane) -> String {
        let g = pane.terminal_emulator.grid();
        (0..g.rows)
            .map(|r| g.get_line_text(r))
            .collect::<Vec<_>>()
            .join("")
            .trim_end()
            .to_string()
    }

    #[test]
    fn test_wrapped_input_renders_once_and_cursor_at_end() {
        let cols = 20;
        let mut pane = Pane::new(cols, 10);
        let p = prompt_cols(&pane);
        let input = "abcdefghijklmnopqrstuvwxyz0123"; // 30 chars -> wraps over 3 rows
        type_str(&mut pane, input);

        let screen = joined_screen(&pane);
        assert!(
            screen.ends_with(input),
            "rendered line should end with the typed input, got {screen:?}"
        );
        assert_eq!(
            screen.matches(input).count(),
            1,
            "input must appear exactly once (no runaway re-render): {screen:?}"
        );

        let g = pane.terminal_emulator.grid();
        assert_eq!(
            (g.cursor_y, g.cursor_x),
            resting(p + input.chars().count(), cols),
            "cursor should rest just past the last typed character"
        );
    }

    #[test]
    fn test_navigation_across_wrap_boundary() {
        let cols = 20;
        let mut pane = Pane::new(cols, 10);
        let p = prompt_cols(&pane);
        let input = "abcdefghijklmnopqrstuvwxyz0123";
        type_str(&mut pane, input);

        // Home (Ctrl+A) returns to the first row, right after the prompt.
        pane.handle_input(&[0x01]);
        {
            let g = pane.terminal_emulator.grid();
            assert_eq!(
                (g.cursor_y, g.cursor_x),
                (0, p),
                "Ctrl+A should return to the prompt row"
            );
        }

        // End (Ctrl+E) returns to the end again.
        pane.handle_input(&[0x05]);
        {
            let g = pane.terminal_emulator.grid();
            assert_eq!(
                (g.cursor_y, g.cursor_x),
                resting(p + input.chars().count(), cols),
                "Ctrl+E should return to the end of the wrapped input"
            );
        }

        // Left arrow three times moves the caret back three cells (crossing a
        // wrap boundary), not just within the last physical row.
        for _ in 0..3 {
            pane.handle_input(&[0x1b, b'[', b'D']);
        }
        let g = pane.terminal_emulator.grid();
        let abs = p + input.chars().count() - 3;
        assert_eq!(
            (g.cursor_y, g.cursor_x),
            (abs / cols, abs % cols),
            "Left arrow should move back across the wrap boundary"
        );
    }

    #[test]
    fn test_backspace_unwraps_without_filling_screen() {
        let cols = 20;
        let mut pane = Pane::new(cols, 10);
        let p = prompt_cols(&pane);
        let input = "abcdefghijklmnopqrstuvwxyz0123"; // 3 rows
        type_str(&mut pane, input);

        // Delete 15 characters; the input should shrink back to fewer rows.
        for _ in 0..15 {
            pane.handle_input(&[0x7f]);
        }

        let remaining = &input[..input.len() - 15];
        let screen = joined_screen(&pane);
        assert!(
            screen.ends_with(remaining),
            "after backspaces the line should end with the remaining input, got {screen:?}"
        );

        let g = pane.terminal_emulator.grid();
        let non_blank = (0..g.rows)
            .filter(|&r| !g.get_line_text(r).trim().is_empty())
            .count();
        assert!(
            non_blank <= 2,
            "remaining input should occupy at most 2 rows, found {non_blank}"
        );
        assert_eq!(
            (g.cursor_y, g.cursor_x),
            resting(p + remaining.chars().count(), cols),
            "cursor should rest at the end of the shortened input"
        );
    }

    #[test]
    fn test_midline_insert_reflows_wrapped_line() {
        let cols = 20;
        let mut pane = Pane::new(cols, 10);
        let p = prompt_cols(&pane);
        type_str(&mut pane, "abcdefghijklmnopqrstuvwxyz0123");

        // Move to the very beginning, then insert a character. Everything must
        // reflow and the caret must end up one cell past the prompt.
        pane.handle_input(&[0x01]); // Ctrl+A
        pane.handle_input(&[b'Z']);

        let screen = joined_screen(&pane);
        assert!(
            screen.ends_with("Zabcdefghijklmnopqrstuvwxyz0123"),
            "insertion at start should reflow the whole wrapped line, got {screen:?}"
        );

        let g = pane.terminal_emulator.grid();
        let abs = p + 1; // prompt + the single inserted char 'Z'
        assert_eq!(
            (g.cursor_y, g.cursor_x),
            (abs / cols, abs % cols),
            "caret should sit just after the inserted character"
        );
    }

    // ------------------------------------------------------------------
    // Regression tests for known bugs (currently FAILING by design).
    // ------------------------------------------------------------------

    /// BUG A (reported): When a URL is long enough to wrap onto a second
    /// terminal row, `Ctrl+B u` only captures the portion on the first row.
    ///
    /// Here a 30-character URL is printed onto a 20-column terminal, so it
    /// occupies row 0 (cols 0..20) and row 1 (cols 0..10). URL mode should
    /// reconstruct the whole URL, but only the first row is returned.
    #[test]
    fn test_url_spanning_wrapped_lines_is_captured_in_full() {
        let mut pane = Pane::new(20, 10);
        pane.terminal_emulator.process(b"\x1b[2J\x1b[H");
        pane.terminal_emulator
            .process(b"https://example.com/abcdefghij");

        pane.enter_scrollback_mode();
        pane.enter_url_mode();

        assert_eq!(
            pane.get_current_url(),
            Some("https://example.com/abcdefghij"),
            "a URL that wraps across two rows should be captured in full, \
             not truncated at the wrap point"
        );
    }

    /// BUG A (highlight): the selected URL span must cover *both* rows it wraps
    /// across, so the highlight is not limited to the first row.
    #[test]
    fn test_wrapped_url_highlight_spans_both_rows() {
        let mut pane = Pane::new(20, 10);
        pane.terminal_emulator.process(b"\x1b[2J\x1b[H");
        pane.terminal_emulator
            .process(b"https://example.com/abcdefghij"); // 30 chars -> rows 0 and 1

        pane.enter_scrollback_mode();
        pane.enter_url_mode();

        let m = pane
            .get_url_matches()
            .first()
            .expect("a URL match should be present");

        assert_eq!(m.line_index, 0, "URL should start on row 0");
        assert_eq!(m.end_line_index, 1, "URL should extend onto row 1");
        // A cell on the first row (within the URL) is part of the match.
        assert!(m.contains(0, 5), "first-row cell should be highlighted");
        // A cell on the wrapped second row is also part of the match.
        assert!(m.contains(1, 5), "second-row cell should be highlighted");
        // A cell past the end of the URL on the second row is not.
        assert!(
            !m.contains(1, 15),
            "cells beyond the URL must not be highlighted"
        );
    }

    /// BUG B (reported): Typing past the wrap point re-emits the entire input
    /// line on every keystroke without accounting for line wrapping, so the
    /// screen fills up with copies of the input instead of wrapping once.
    ///
    /// On a 20-column terminal, the prompt plus 40 characters should occupy
    /// only a handful of rows. With the bug, all 10 rows fill up.
    #[test]
    fn test_typing_past_wrap_point_does_not_fill_screen() {
        let mut pane = Pane::new(20, 10);
        for _ in 0..40 {
            pane.handle_input(&[b'a']);
        }

        let grid = pane.terminal_emulator.grid();
        let non_blank = (0..grid.rows)
            .filter(|&r| !grid.get_line_text(r).trim().is_empty())
            .count();

        assert!(
            non_blank <= 4,
            "prompt + 40 chars on a 20-col screen should wrap to a few rows, \
             but {} of {} rows are filled (runaway re-rendering)",
            non_blank,
            grid.rows
        );
    }

    /// BUG D: URL match columns are computed from UTF-8 byte offsets rather
    /// than display columns. When a multibyte character precedes the URL on a
    /// line, the highlighted region is shifted, mis-aligning the highlight.
    ///
    /// "café " is 5 display columns but 6 UTF-8 bytes, so the reported
    /// `start_col` is 6 instead of 5.
    #[test]
    fn test_url_match_column_accounts_for_multibyte_prefix() {
        let mut pane = Pane::new(40, 5);
        pane.terminal_emulator.process(b"\x1b[2J\x1b[H");
        pane.terminal_emulator
            .process("café https://example.com".as_bytes());

        let m = pane
            .find_first_url_in_line(0)
            .expect("URL should be found on the line");

        assert_eq!(
            m.start_col, 5,
            "start_col should be the display column where the URL begins \
             (5), not the UTF-8 byte offset ({})",
            m.start_col
        );
    }

    #[test]
    fn test_url_regex_basic() {
        // Basic HTTP/HTTPS URLs
        assert!(URL_REGEX.is_match("https://example.com"));
        assert!(URL_REGEX.is_match("http://example.com"));
        assert!(URL_REGEX.is_match("https://example.com/path?query=value"));
    }

    #[test]
    fn test_url_regex_schemes() {
        // Various supported schemes
        assert!(URL_REGEX.is_match("mailto:user@example.com"));
        assert!(URL_REGEX.is_match("ftp://ftp.example.com/file.txt"));
        assert!(URL_REGEX.is_match("ssh://user@host"));
        assert!(URL_REGEX.is_match("git://github.com/user/repo.git"));
        assert!(URL_REGEX.is_match("file:///home/user/doc.txt"));
    }

    #[test]
    fn test_url_regex_case_insensitive() {
        // Case insensitivity
        assert!(URL_REGEX.is_match("HTTPS://EXAMPLE.COM"));
        assert!(URL_REGEX.is_match("Http://Example.Com"));
    }

    #[test]
    fn test_url_regex_invalid() {
        // Invalid URLs (missing scheme or malformed)
        assert!(!URL_REGEX.is_match("example.com")); // No scheme
        assert!(!URL_REGEX.is_match("http://")); // Empty body
        assert!(!URL_REGEX.is_match("not a url"));
    }

    #[test]
    fn test_bracket_balancing_matched_parens() {
        // Matched parentheses should be preserved
        assert_eq!(
            balance_brackets("http://example.com/page_(1)"),
            "http://example.com/page_(1)"
        );
        assert_eq!(
            balance_brackets("http://example.com/wiki/Thing_(concept)"),
            "http://example.com/wiki/Thing_(concept)"
        );
    }

    #[test]
    fn test_bracket_balancing_unmatched_parens() {
        // Unmatched closing paren should truncate
        assert_eq!(
            balance_brackets("http://example.com)"),
            "http://example.com"
        );
        assert_eq!(
            balance_brackets("http://example.com/page)more"),
            "http://example.com/page"
        );
    }

    #[test]
    fn test_bracket_balancing_unmatched_brackets() {
        // Unmatched closing bracket should truncate
        assert_eq!(
            balance_brackets("http://example.com]"),
            "http://example.com"
        );
        assert_eq!(
            balance_brackets("http://example.com/path]rest"),
            "http://example.com/path"
        );
    }

    #[test]
    fn test_bracket_balancing_nested() {
        // Nested brackets should work
        assert_eq!(
            balance_brackets("http://example.com/f(g(x))"),
            "http://example.com/f(g(x))"
        );
    }

    #[test]
    fn test_trailing_delimiter_trimming() {
        // Trailing punctuation should be trimmed
        assert_eq!(
            trim_trailing_delimiters("http://example.com."),
            "http://example.com"
        );
        assert_eq!(
            trim_trailing_delimiters("http://example.com,"),
            "http://example.com"
        );
        assert_eq!(
            trim_trailing_delimiters("http://example.com;"),
            "http://example.com"
        );
        assert_eq!(
            trim_trailing_delimiters("http://example.com:"),
            "http://example.com"
        );
        assert_eq!(
            trim_trailing_delimiters("http://example.com?"),
            "http://example.com"
        );
        assert_eq!(
            trim_trailing_delimiters("http://example.com!"),
            "http://example.com"
        );
    }

    #[test]
    fn test_trailing_delimiter_preserves_query() {
        // Query strings with ? should not be stripped if not trailing
        assert_eq!(
            trim_trailing_delimiters("http://example.com/path?query=1"),
            "http://example.com/path?query=1"
        );
    }

    #[test]
    fn test_trailing_delimiter_multiple() {
        // Multiple trailing delimiters
        assert_eq!(
            trim_trailing_delimiters("http://example.com.."),
            "http://example.com"
        );
        assert_eq!(
            trim_trailing_delimiters("http://example.com.,"),
            "http://example.com"
        );
    }

    #[test]
    fn test_post_process_url_combined() {
        // Combined bracket balancing and trailing trimming
        assert_eq!(
            post_process_url("http://example.com)."),
            "http://example.com"
        );
        assert_eq!(
            post_process_url("http://example.com/page_(1)."),
            "http://example.com/page_(1)"
        );
    }

    #[test]
    fn test_post_process_url_embedded_in_parens() {
        // URL embedded in parentheses - simulates "(see http://example.com)"
        // The regex would match "http://example.com)" and post-processing fixes it
        assert_eq!(
            post_process_url("http://example.com)"),
            "http://example.com"
        );
    }
}
