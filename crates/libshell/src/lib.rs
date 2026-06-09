//! libshell library
//!
//! This library provides an embeddable shell implementation that can be used
//! as a direct replacement for subprocess-based shells (like bash) in terminal
//! emulators and compositors.
//!
//! # Architecture
//!
//! The shell uses a "PTY handoff" model:
//! - When at the prompt, the shell processes input and produces terminal output
//! - When executing an external command, the shell tells the compositor to spawn
//!   the command on the PTY, and the subprocess gets direct PTY access
//! - When the subprocess exits, control returns to the shell
//!
//! This allows subprocesses (like vim, less, etc.) to work correctly with
//! full terminal capabilities.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex, RwLock};
use unicode_width::UnicodeWidthChar;

pub mod backend;
mod diagnostics;
mod environment;
pub mod file_finder;
pub mod history;
pub mod picker;
mod syntax;

pub use backend::{LocalBackend, ShellBackend};
pub use diagnostics::{
    clear_global_diagnostic_flag, global_diagnostic_flags, set_global_diagnostic_flag,
    GlobalDiagnosticFlag,
};
pub use environment::{reload_shell_env, shell_env_file_path, shell_env_snapshot, ShellEnvError};
pub use history::{
    BackupConfig, CommandSource, EntryId, HistoryEntry, HistoryMirror, HistorySearchResult,
    SearchResult, ShellHistory,
};
pub use picker::{
    FileItem, PickerConfig, PickerItem, PickerMode, PickerState, TabCompletionContext,
    TabCompletionItem,
};

/// Find the longest common prefix among a set of strings
fn find_common_prefix<'a>(strings: impl Iterator<Item = &'a str>) -> String {
    let strings: Vec<&str> = strings.collect();
    if strings.is_empty() {
        return String::new();
    }
    if strings.len() == 1 {
        return strings[0].to_string();
    }

    let first = strings[0];
    let mut prefix_len = first.len();

    for s in &strings[1..] {
        prefix_len = first
            .chars()
            .zip(s.chars())
            .take(prefix_len)
            .take_while(|(a, b)| a == b)
            .count();
        if prefix_len == 0 {
            break;
        }
    }

    first.chars().take(prefix_len).collect()
}

/// Check if a character is a word boundary for word navigation.
/// Word boundaries include whitespace and path separators (/).
fn is_word_boundary(c: char) -> bool {
    c.is_whitespace() || c == '/'
}

/// Expand a path string, handling:
/// - `~` and `~/...` -> $HOME expansion
/// - `$VAR` and `${VAR}` -> environment variable expansion
///
/// Returns the expanded path as a PathBuf.
pub fn expand_path(path: &str) -> PathBuf {
    let mut result = path.to_string();

    // Handle tilde expansion
    if result == "~" {
        if let Ok(home) = std::env::var("HOME") {
            return PathBuf::from(home);
        }
    } else if result.starts_with("~/") {
        if let Ok(home) = std::env::var("HOME") {
            result = format!("{}{}", home, &result[1..]);
        }
    }

    // Handle environment variable expansion
    // Match $VAR or ${VAR} patterns
    let mut expanded = String::new();
    let mut chars = result.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '$' {
            // Check for ${VAR} syntax
            if chars.peek() == Some(&'{') {
                chars.next(); // consume '{'
                let mut var_name = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch == '}' {
                        chars.next(); // consume '}'
                        break;
                    }
                    var_name.push(chars.next().unwrap());
                }
                if let Ok(value) = std::env::var(&var_name) {
                    expanded.push_str(&value);
                } else {
                    // Keep the original if var not found
                    expanded.push_str(&format!("${{{}}}", var_name));
                }
            } else {
                // $VAR syntax - collect alphanumeric and underscore chars
                let mut var_name = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        var_name.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                if var_name.is_empty() {
                    expanded.push('$');
                } else if let Ok(value) = std::env::var(&var_name) {
                    expanded.push_str(&value);
                } else {
                    // Keep the original if var not found
                    expanded.push('$');
                    expanded.push_str(&var_name);
                }
            }
        } else {
            expanded.push(c);
        }
    }

    PathBuf::from(expanded)
}

/// Error type for shell operations
#[derive(Debug)]
pub enum ShellError {
    /// I/O error during read/write operations
    Io(std::io::Error),
    /// Command execution failed
    CommandFailed(String),
}

impl std::fmt::Display for ShellError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ShellError::Io(e) => write!(f, "I/O error: {}", e),
            ShellError::CommandFailed(msg) => write!(f, "Command failed: {}", msg),
        }
    }
}

impl std::error::Error for ShellError {}

impl From<std::io::Error> for ShellError {
    fn from(e: std::io::Error) -> Self {
        ShellError::Io(e)
    }
}

/// Action that the shell wants the compositor to take.
///
/// When processing input, the shell may need the compositor to perform
/// actions on its behalf (like spawning a subprocess).
#[derive(Debug, Clone, PartialEq)]
pub enum ShellAction {
    /// No action needed - shell handled everything internally.
    None,

    /// Shell produced output that should be written to the terminal.
    /// The compositor should feed this to the terminal emulator.
    Output(Vec<u8>),

    /// Shell wants to spawn a subprocess that takes over the PTY.
    ///
    /// The compositor should:
    /// 1. Write `output` to the terminal first (e.g., the newline after the command)
    /// 2. Spawn this command on the PTY (subprocess gets PTY slave)
    /// 3. Route all input to the subprocess (not the shell)
    /// 4. When subprocess exits, call `Shell::subprocess_exited()`
    SpawnSubprocess {
        /// Output to write to terminal before spawning (e.g., newline)
        output: Vec<u8>,
        /// The command to execute (e.g., "vim", "/usr/bin/ls")
        command: String,
        /// Arguments to pass to the command
        args: Vec<String>,
        /// Environment variables to set (in addition to inherited ones)
        env: Vec<(String, String)>,
        /// Working directory for the subprocess
        cwd: std::path::PathBuf,
        /// History entry ID for tracking this command's exit status
        history_id: Option<EntryId>,
    },

    /// Shell wants to rename the current window/tab.
    RenameWindow {
        /// Output to write to terminal (e.g., newline after the command)
        output: Vec<u8>,
        /// The new name for the window
        name: String,
    },

    /// Shell wants to connect this pane to a remote host. The compositor should
    /// spawn the remote transport and route the pane's I/O to it (like
    /// `SpawnSubprocess`, but the "process" is a remote shell session). When the
    /// remote ends, call `Shell::subprocess_exited()`.
    Connect {
        /// Output to write before connecting (e.g. the newline after the command).
        output: Vec<u8>,
        /// Connection target: `user@host`, or `local`/`self` for an in-process
        /// stdio daemon (used by tests and local experimentation).
        target: String,
        /// Optional session name (reattach-or-create). `None` makes a fresh
        /// per-pane session.
        session: Option<String>,
        /// Local environment to forward to the remote (merged remote-wins).
        env: Vec<(String, String)>,
    },

    /// Shell wants to open the session picker for a host: list its persistent
    /// sessions and let the user resume or kill one. The compositor fetches the
    /// list and drives the picker.
    Reconnect {
        output: Vec<u8>,
        target: String,
        env: Vec<(String, String)>,
    },

    /// Shell wants to exit.
    Exit,

    /// Shell wants to edit the current input in an external editor.
    ///
    /// The compositor should:
    /// 1. Write `output` to the terminal first
    /// 2. Spawn the editor with the temp file
    /// 3. When editor exits, call `Shell::editor_exited()` with the temp file path
    EditInEditor {
        /// Output to write to terminal before spawning editor
        output: Vec<u8>,
        /// Path to the temporary file containing the input
        temp_file: std::path::PathBuf,
        /// The editor command to run
        editor: String,
    },
}

/// Shared shell state that can be accessed across threads
pub struct ShellCore {
    #[allow(dead_code)]
    env: RwLock<HashMap<String, String>>,
    history: ShellHistory,
    /// When set (on a daemon), every command recorded here is also captured for
    /// dual-write up to the client. `None` for an ordinary local shell.
    history_mirror: Mutex<Option<Arc<HistoryMirror>>>,
}

impl ShellCore {
    /// Create a new ShellCore with default history path (~/.myshell_history.log)
    pub fn new() -> Result<Self, std::io::Error> {
        let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
        let history_path = PathBuf::from(&home).join(".myshell_history.log");
        Self::with_history_path(history_path)
    }

    /// Create a new ShellCore with a custom history path
    pub fn with_history_path(history_path: PathBuf) -> Result<Self, std::io::Error> {
        let is_new_history = !history_path.exists();
        let history = ShellHistory::new(&history_path)?;

        // If this is a brand new history, try to import from zsh/bash
        if is_new_history {
            let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
            let home_path = PathBuf::from(&home);

            // Try to import zsh history
            let zsh_history = home_path.join(".zsh_history");
            if zsh_history.exists() {
                match history.import_zsh_history(&zsh_history) {
                    Ok(count) => {
                        if count > 0 {
                            eprintln!("Imported {} entries from .zsh_history", count);
                        }
                    }
                    Err(e) => eprintln!("Warning: failed to import .zsh_history: {}", e),
                }
            }

            // Try to import bash history
            let bash_history = home_path.join(".bash_history");
            if bash_history.exists() {
                match history.import_bash_history(&bash_history) {
                    Ok(count) => {
                        if count > 0 {
                            eprintln!("Imported {} entries from .bash_history", count);
                        }
                    }
                    Err(e) => eprintln!("Warning: failed to import .bash_history: {}", e),
                }
            }
        }

        Ok(ShellCore {
            env: RwLock::new(HashMap::new()),
            history,
            history_mirror: Mutex::new(None),
        })
    }

    /// Get a reference to the shell history
    pub fn history(&self) -> &ShellHistory {
        &self.history
    }

    /// Install a [`HistoryMirror`] so commands recorded here are also captured
    /// for dual-write to a client (used by the daemon).
    pub fn set_history_mirror(&self, mirror: Arc<HistoryMirror>) {
        *self.history_mirror.lock().unwrap() = Some(mirror);
    }

    /// Record a command execution
    pub fn record_command(
        &self,
        command: String,
        source: CommandSource,
        cwd: Option<String>,
    ) -> Result<EntryId, std::io::Error> {
        let id = self
            .history
            .record_command_with_cwd(command.clone(), source, cwd.clone())?;
        if let Some(m) = self.history_mirror.lock().unwrap().as_ref() {
            m.on_record(id.as_str(), &command, cwd);
        }
        Ok(id)
    }

    /// Record the exit status of a command
    pub fn record_exit(
        &self,
        id: &EntryId,
        exit_code: i32,
        duration_ms: u64,
    ) -> Result<(), std::io::Error> {
        self.history.record_exit(id, exit_code, duration_ms)?;
        if let Some(m) = self.history_mirror.lock().unwrap().as_ref() {
            m.on_exit(id.as_str(), Some(exit_code));
        }
        Ok(())
    }

    /// Mark a command as killed (Ctrl+C)
    pub fn mark_killed(&self, id: &EntryId) -> Result<(), std::io::Error> {
        self.history.mark_killed(id)?;
        if let Some(m) = self.history_mirror.lock().unwrap().as_ref() {
            m.on_exit(id.as_str(), None);
        }
        Ok(())
    }
}

impl Default for ShellCore {
    fn default() -> Self {
        Self::new().expect("Failed to create default ShellCore")
    }
}

/// An embeddable shell that controls PTY handoff to subprocesses.
///
/// # Architecture
///
/// The shell operates in two modes:
/// 1. **Shell mode**: Shell processes input, handles line editing, builtins
/// 2. **Subprocess mode**: A subprocess owns the PTY, shell is dormant
///
/// The compositor should:
/// 1. Call `handle_input()` when shell is active
/// 2. When `ShellAction::SpawnSubprocess` is returned, spawn the process
/// 3. Route all I/O to the subprocess while it runs
/// 4. Call `subprocess_exited()` when the process terminates
///
/// # Example
///
/// ```ignore
/// let core = Arc::new(ShellCore::new().unwrap());
/// let mut shell = Shell::with_core(Arc::clone(&core), 80, 24);
/// let mut subprocess: Option<PtyProcess> = None;
///
/// loop {
///     if let Some(ref mut proc) = subprocess {
///         // Subprocess owns the PTY
///         proc.write(input);
///         if !proc.is_running() {
///             let exit_code = proc.wait();
///             shell.subprocess_exited(exit_code);
///             subprocess = None;
///         }
///     } else {
///         // Shell is active
///         match shell.handle_input(input) {
///             ShellAction::Output(data) => terminal.write(&data),
///             ShellAction::SpawnSubprocess { command, args, .. } => {
///                 subprocess = Some(PtyProcess::spawn(&command, &args));
///             }
///             _ => {}
///         }
///     }
/// }
/// ```
pub struct Shell {
    /// The shell "brain": syntax/completion/history/exec, local or remote.
    backend: Box<dyn ShellBackend>,
    /// Terminal dimensions
    cols: u16,
    rows: u16,
    /// Current input line being edited
    input_buffer: String,
    /// Saved input buffer when navigating history
    saved_input: Option<String>,
    /// Cursor position within input_buffer
    cursor_pos: usize,
    /// Current working directory
    cwd: std::path::PathBuf,
    /// Whether the shell should exit
    should_exit: bool,
    /// Pending action from command execution (checked after input processing)
    pending_action: Option<ShellAction>,
    /// Current position in history navigation (0 = current input, 1 = last command, etc.)
    history_position: usize,
    /// Cached history entries for navigation (refreshed on each new prompt)
    history_cache: Vec<String>,
    /// Escape sequence buffer for parsing multi-byte sequences
    escape_buffer: Vec<u8>,
    /// Whether we're currently parsing an escape sequence
    in_escape_sequence: bool,
    /// Currently running command's history ID (for tracking exit status)
    current_command_id: Option<EntryId>,
    /// Start time of currently running command
    command_start_time: Option<std::time::Instant>,

    // --- Picker UI state (used for CTRL+R history search, tab completion, etc.) ---
    /// Current picker state, if a picker is active
    picker_state: Option<PickerState>,
    /// Saved input buffer when entering a picker mode that uses its own input
    saved_input_buffer: Option<String>,
    /// Saved cursor position when entering a picker mode
    saved_cursor_pos: Option<usize>,

    /// Whether we're waiting for CTRL+E after CTRL+X (for edit-in-editor)
    waiting_for_ctrl_x_combo: bool,

    /// Buffer for accumulating UTF-8 multi-byte sequences
    utf8_buffer: Vec<u8>,
}

impl Shell {
    /// Create a new shell instance with the given terminal dimensions.
    /// Creates its own ShellCore instance.
    ///
    /// Returns the shell and initial output (the prompt) that should be
    /// written to the terminal.
    pub fn new(cols: u16, rows: u16) -> (Self, Vec<u8>) {
        let core = Arc::new(ShellCore::new().expect("Failed to create ShellCore"));
        Self::with_core(core, cols, rows)
    }

    /// Create a new shell instance with a shared ShellCore.
    /// Use this to share history and environment across multiple shell panes.
    ///
    /// Returns the shell and initial output (the prompt) that should be
    /// written to the terminal.
    pub fn with_core(core: Arc<ShellCore>, cols: u16, rows: u16) -> (Self, Vec<u8>) {
        Self::with_backend(Box::new(LocalBackend::new(core)), cols, rows)
    }

    /// Create a shell over an explicit backend (local or remote).
    ///
    /// Returns the shell and initial output (the prompt) to write to the
    /// terminal.
    pub fn with_backend(
        backend: Box<dyn ShellBackend>,
        cols: u16,
        rows: u16,
    ) -> (Self, Vec<u8>) {
        let cwd = std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from("/"));

        // Build initial history cache
        let history_cache: Vec<String> = backend.recent_commands(1000);

        let shell = Shell {
            backend,
            cols,
            rows,
            input_buffer: String::new(),
            saved_input: None,
            cursor_pos: 0,
            cwd,
            should_exit: false,
            pending_action: None,
            history_position: 0,
            history_cache,
            escape_buffer: Vec::new(),
            in_escape_sequence: false,
            current_command_id: None,
            command_start_time: None,
            // Picker UI state
            picker_state: None,
            saved_input_buffer: None,
            saved_cursor_pos: None,
            // CTRL+X combo state
            waiting_for_ctrl_x_combo: false,
            // UTF-8 multi-byte sequence buffer
            utf8_buffer: Vec::new(),
        };

        let prompt = shell.get_prompt();
        (shell, prompt.into_bytes())
    }

    /// Refresh the history cache (call this after executing a command)
    fn refresh_history_cache(&mut self) {
        self.history_cache = self.backend.recent_commands(1000);
        self.history_position = 0;
        self.saved_input = None;
    }

    /// Process input from the terminal and return the action to take.
    ///
    /// This is the main entry point for keyboard input when the shell is active.
    /// The compositor should only call this when no subprocess is running.
    ///
    /// Returns a `ShellAction` indicating what the compositor should do:
    /// - `Output`: Write this data to the terminal
    /// - `SpawnSubprocess`: Start this command and hand over the PTY
    /// - `Exit`: Shell wants to terminate
    /// - `None`: Nothing to do
    pub fn handle_input(&mut self, input: &[u8]) -> ShellAction {
        let mut output = Vec::new();

        for &byte in input {
            if let Some(data) = self.process_input_byte(byte) {
                output.extend(data);
            }

            // Check if command execution produced a spawn request
            if let Some(action) = self.pending_action.take() {
                // Combine output with the action
                match action {
                    ShellAction::SpawnSubprocess {
                        output: _,
                        command,
                        args,
                        env,
                        cwd,
                        history_id,
                    } => {
                        return ShellAction::SpawnSubprocess {
                            output,
                            command,
                            args,
                            env,
                            cwd,
                            history_id,
                        };
                    }
                    ShellAction::RenameWindow { output: _, name } => {
                        return ShellAction::RenameWindow { output, name };
                    }
                    ShellAction::EditInEditor {
                        output: _,
                        temp_file,
                        editor,
                    } => {
                        return ShellAction::EditInEditor {
                            output,
                            temp_file,
                            editor,
                        };
                    }
                    ShellAction::Connect {
                        output: _,
                        target,
                        session,
                        env,
                    } => {
                        return ShellAction::Connect {
                            output,
                            target,
                            session,
                            env,
                        };
                    }
                    ShellAction::Reconnect {
                        output: _,
                        target,
                        env,
                    } => {
                        return ShellAction::Reconnect {
                            output,
                            target,
                            env,
                        };
                    }
                    other => return other,
                }
            }
        }

        if self.should_exit {
            return ShellAction::Exit;
        }

        if output.is_empty() {
            ShellAction::None
        } else {
            ShellAction::Output(output)
        }
    }

    /// Notify the shell that a subprocess has exited.
    ///
    /// Call this when a subprocess that was spawned via `ShellAction::SpawnSubprocess`
    /// has terminated. The shell will display the prompt again.
    ///
    /// Returns output to write to the terminal (typically a newline + prompt).
    pub fn subprocess_exited(&mut self, exit_code: i32) -> Vec<u8> {
        // Record the exit status in history
        if let Some(id) = self.current_command_id.take() {
            let duration_ms = self
                .command_start_time
                .take()
                .map(|t| t.elapsed().as_millis() as u64)
                .unwrap_or(0);

            self.backend.record_exit(&id, exit_code, duration_ms);
        }

        // Refresh history cache for arrow key navigation
        self.refresh_history_cache();

        let mut output = Vec::new();

        // Show exit code with white text on red background for non-zero exits
        if exit_code != 0 {
            // \x1b[41m = red background, \x1b[97m = bright white text, \x1b[0m = reset
            output.extend(format!("\x1b[41m\x1b[97m exit {} \x1b[0m\r\n", exit_code).as_bytes());
        }

        // Show prompt
        output.extend(self.get_prompt().as_bytes());
        output
    }

    /// Notify the shell that a subprocess was killed (e.g., by Ctrl+C).
    ///
    /// Similar to `subprocess_exited` but marks the command as killed.
    pub fn subprocess_killed(&mut self) -> Vec<u8> {
        // Record as killed in history
        if let Some(id) = self.current_command_id.take() {
            self.backend.mark_killed(&id);
        }
        self.command_start_time = None;

        // Refresh history cache for arrow key navigation
        self.refresh_history_cache();

        let mut output = Vec::new();
        // Show CTRL+C indicator with white text on red background
        // \x1b[41m = red background, \x1b[97m = bright white text, \x1b[0m = reset
        output.extend(b"\x1b[41m\x1b[97m CTRL+C \x1b[0m\r\n");
        output.extend(self.get_prompt().as_bytes());
        output
    }

    /// Mirror a command that ran in a remote pane into this (local) shell's
    /// history, tagged `Remote(host)`. Used for history dual-write so a remote
    /// session's commands persist locally after the pane closes.
    pub fn mirror_remote_record(&mut self, record: &protocol::HistoryRecord, host: &protocol::HostId) {
        self.backend.mirror_remote_record(record, host);
    }

    /// Notify the shell that an external editor session has exited.
    ///
    /// Call this when an editor spawned via `ShellAction::EditInEditor` has
    /// terminated. The shell reads the temp file and replaces the input buffer.
    ///
    /// Returns output to write to the terminal (the new prompt with input).
    pub fn editor_exited(&mut self, temp_file: &std::path::Path) -> Vec<u8> {
        let mut output = Vec::new();

        // Read the edited content from the temp file
        match std::fs::read_to_string(temp_file) {
            Ok(content) => {
                // Trim trailing newlines that editors typically add
                let content = content.trim_end_matches('\n').trim_end_matches('\r');
                self.input_buffer = content.to_string();
                self.cursor_pos = self.input_buffer.chars().count();
            }
            Err(e) => {
                eprintln!("Warning: failed to read temp file: {}", e);
                // Keep existing input buffer
            }
        }

        // Clean up the temp file
        let _ = std::fs::remove_file(temp_file);

        // Re-render prompt with the (possibly new) input
        // For multi-line input, we need to render each line with appropriate prompts
        output.extend(self.render_full_multiline_input().as_bytes());
        output
    }

    /// Render the full input buffer with prompts, handling multi-line correctly.
    fn render_full_multiline_input(&mut self) -> String {
        let lines: Vec<&str> = self.input_buffer.split('\n').collect();
        let mut result = String::new();

        for (i, line) in lines.iter().enumerate() {
            if i == 0 {
                // First line gets the main prompt
                result.push_str(&self.get_prompt());
            } else {
                // Subsequent lines get CRLF and continuation prompt
                result.push_str("\r\n");
                result.push_str(&self.get_continuation_prompt());
            }
            // Highlight just this line
            result.push_str(&self.backend.highlight(line, &self.cwd));
        }

        result
    }

    /// Start editing the current input in an external editor (CTRL+X CTRL+E).
    fn edit_in_external_editor(&mut self) -> Option<Vec<u8>> {
        // Get the editor from $EDITOR, $VISUAL, or default to vim
        let editor = std::env::var("EDITOR")
            .or_else(|_| std::env::var("VISUAL"))
            .unwrap_or_else(|_| "vim".to_string());

        // Create a temporary file with the current input
        let temp_dir = std::env::temp_dir();
        let temp_file = temp_dir.join(format!("shell_edit_{}.sh", std::process::id()));

        // Write current input to the temp file
        if let Err(e) = std::fs::write(&temp_file, &self.input_buffer) {
            eprintln!("Warning: failed to create temp file: {}", e);
            return None;
        }

        // Set the pending action to spawn the editor
        self.pending_action = Some(ShellAction::EditInEditor {
            output: vec![],
            temp_file,
            editor,
        });

        // Clear the current line and move to new line before spawning editor
        Some(b"\r\n".to_vec())
    }

    /// Resize the shell's terminal dimensions.
    pub fn resize(&mut self, cols: u16, rows: u16) {
        self.cols = cols;
        self.rows = rows;
    }

    /// Check if the shell wants to exit.
    pub fn should_exit(&self) -> bool {
        self.should_exit
    }

    /// Get the current working directory.
    pub fn cwd(&self) -> &std::path::Path {
        &self.cwd
    }

    /// Check if the input buffer is empty (no pending input).
    ///
    /// This is used to determine whether CTRL+C should clear input or
    /// trigger a pane close action.
    pub fn is_input_empty(&self) -> bool {
        self.input_buffer.is_empty() && self.picker_state.is_none()
    }

    /// Handle CTRL+C when the shell is active (no subprocess).
    ///
    /// If there is input, clears it and shows a new prompt.
    /// Returns the output to write to the terminal, or None if input was already empty.
    pub fn handle_ctrl_c(&mut self) -> Option<Vec<u8>> {
        // If in picker mode, exit it first
        if self.picker_state.is_some() {
            let output = self.exit_picker();
            return Some(output);
        }

        if self.input_buffer.is_empty() {
            // Input is already empty - caller should handle this case
            // (e.g., close the pane)
            None
        } else {
            // Clear input and show new prompt
            self.input_buffer.clear();
            self.cursor_pos = 0;
            let mut output = b"^C\r\n".to_vec();
            output.extend(self.get_prompt().as_bytes());
            Some(output)
        }
    }

    // --- Private implementation ---

    fn process_input_byte(&mut self, byte: u8) -> Option<Vec<u8>> {
        // If in picker mode, route to picker input handler
        if self.picker_state.is_some() {
            return self.process_picker_input_byte(byte);
        }

        // Handle CTRL+X combo: if waiting for second key after CTRL+X
        if self.waiting_for_ctrl_x_combo {
            self.waiting_for_ctrl_x_combo = false;
            if byte == 0x05 {
                // CTRL+X CTRL+E - edit in external editor
                return self.edit_in_external_editor();
            }
            // Any other key cancels the combo - continue processing normally
        }

        // Handle escape sequences
        if self.in_escape_sequence {
            self.escape_buffer.push(byte);
            return self.try_parse_escape_sequence();
        }

        if byte == 0x1b {
            // ESC - start of escape sequence
            self.in_escape_sequence = true;
            self.escape_buffer.clear();
            self.escape_buffer.push(byte);
            return None;
        }

        match byte {
            // Enter - execute command (or continue if line ends with backslash)
            b'\r' | b'\n' => {
                // Check if the input ends with a backslash (line continuation)
                if self.input_buffer.ends_with('\\') {
                    // Remove the trailing backslash and add a newline to the input buffer
                    self.input_buffer.pop();
                    self.input_buffer.push('\n');
                    self.cursor_pos = self.input_buffer.chars().count();

                    // Move to next line and show continuation prompt
                    let mut output = vec![b'\r', b'\n'];
                    output.extend(self.get_continuation_prompt().as_bytes());
                    Some(output)
                } else {
                    let mut output = vec![b'\r', b'\n'];
                    output.extend(self.execute_current_command());
                    Some(output)
                }
            }
            // Backspace (0x7f) or Ctrl+H (0x08)
            0x7f | 0x08 => {
                if self.cursor_pos > 0 {
                    // Capture the caret position before mutating the buffer.
                    let old_caret = self.caret_phys(self.cursor_pos);

                    self.cursor_pos -= 1;
                    // cursor_pos is a character index, convert to byte offset for remove
                    let byte_pos = self
                        .input_buffer
                        .char_indices()
                        .nth(self.cursor_pos)
                        .map(|(i, _)| i)
                        .unwrap_or(self.input_buffer.len());
                    self.input_buffer.remove(byte_pos);

                    // Re-render the whole line (handles wrapped lines and the
                    // case where a hard newline was deleted).
                    Some(self.render_line_from(old_caret))
                } else {
                    None
                }
            }
            // Ctrl+R - enter history search mode
            0x12 => Some(self.enter_history_search()),
            // Ctrl+P - enter file finder mode
            0x10 => Some(self.enter_file_finder()),
            // Ctrl+C - cancel current input
            0x03 => {
                self.input_buffer.clear();
                self.cursor_pos = 0;
                let mut output = b"^C\r\n".to_vec();
                output.extend(self.get_prompt().as_bytes());
                Some(output)
            }
            // Ctrl+D - EOF (exit if line is empty)
            0x04 => {
                if self.input_buffer.is_empty() {
                    self.should_exit = true;
                    Some(b"exit\r\n".to_vec())
                } else {
                    None
                }
            }
            // Ctrl+L - clear screen
            0x0c => {
                let mut output = b"\x1b[2J\x1b[H".to_vec(); // Clear screen and home
                output.extend(self.render_full_multiline_input().as_bytes());
                // Position the caret correctly (wrap-aware).
                output.extend(self.reposition_to_cursor());
                Some(output)
            }
            // Ctrl+A - move to beginning of line
            0x01 => self.cursor_home(),
            // Ctrl+E - move to end of line
            0x05 => self.cursor_end(),
            // Ctrl+X - start of CTRL+X combo (wait for next key)
            0x18 => {
                self.waiting_for_ctrl_x_combo = true;
                None
            }
            // Ctrl+W - delete word before cursor
            0x17 => self.delete_word_backward(),
            // Ctrl+K - kill to end of line
            0x0b => self.kill_to_end(),
            // Ctrl+U - kill to beginning of line
            0x15 => self.kill_to_beginning(),
            // Tab - completion
            0x09 => self.handle_tab_completion(),
            // Regular printable ASCII character
            0x20..=0x7e => {
                // Capture the terminal caret position before mutating the buffer.
                let old_caret = self.caret_phys(self.cursor_pos);
                // cursor_pos is a character index, convert to byte offset
                let byte_pos = self
                    .input_buffer
                    .char_indices()
                    .nth(self.cursor_pos)
                    .map(|(i, _)| i)
                    .unwrap_or(self.input_buffer.len());
                self.input_buffer.insert(byte_pos, byte as char);
                self.cursor_pos += 1;

                // Re-render the entire line (wrap-aware) with syntax highlighting.
                Some(self.render_line_from(old_caret))
            }
            // UTF-8 multi-byte sequence handling
            // UTF-8 leading bytes: 0xC0-0xDF (2-byte), 0xE0-0xEF (3-byte), 0xF0-0xF7 (4-byte)
            // UTF-8 continuation bytes: 0x80-0xBF
            0x80..=0xBF => {
                // Continuation byte - add to buffer if we're accumulating
                if !self.utf8_buffer.is_empty() {
                    self.utf8_buffer.push(byte);
                    self.try_complete_utf8_char()
                } else {
                    // Stray continuation byte, ignore
                    None
                }
            }
            0xC0..=0xF7 => {
                // Leading byte of a multi-byte sequence
                self.utf8_buffer.clear();
                self.utf8_buffer.push(byte);
                self.try_complete_utf8_char()
            }
            // Invalid UTF-8 bytes or other control characters
            _ => None,
        }
    }

    /// Try to complete a UTF-8 character from the buffer.
    /// Returns Some(output) if a complete character was formed, None if more bytes needed.
    fn try_complete_utf8_char(&mut self) -> Option<Vec<u8>> {
        if self.utf8_buffer.is_empty() {
            return None;
        }

        // Determine expected length from first byte
        let first = self.utf8_buffer[0];
        let expected_len = if first & 0b1111_1000 == 0b1111_0000 {
            4 // 4-byte sequence (0xF0-0xF7)
        } else if first & 0b1111_0000 == 0b1110_0000 {
            3 // 3-byte sequence (0xE0-0xEF)
        } else if first & 0b1110_0000 == 0b1100_0000 {
            2 // 2-byte sequence (0xC0-0xDF)
        } else {
            // Invalid leading byte, clear buffer
            self.utf8_buffer.clear();
            return None;
        };

        if self.utf8_buffer.len() < expected_len {
            // Need more bytes
            return None;
        }

        // We have enough bytes, try to decode
        let bytes: Vec<u8> = self.utf8_buffer.drain(..).collect();
        match std::str::from_utf8(&bytes) {
            Ok(s) => {
                // Valid UTF-8 - insert the character(s)
                let old_caret = self.caret_phys(self.cursor_pos);
                for c in s.chars() {
                    // cursor_pos is a character index, need to find byte offset
                    let byte_pos = self
                        .input_buffer
                        .char_indices()
                        .nth(self.cursor_pos)
                        .map(|(i, _)| i)
                        .unwrap_or(self.input_buffer.len());
                    self.input_buffer.insert(byte_pos, c);
                    self.cursor_pos += 1;
                }
                Some(self.render_line_from(old_caret))
            }
            Err(_) => {
                // Invalid UTF-8, discard
                None
            }
        }
    }

    /// Check if the input buffer is empty.
    pub fn input_is_empty(&self) -> bool {
        self.input_buffer.is_empty()
    }

    /// Get the current prompt string.
    pub fn get_prompt(&self) -> String {
        let home_dir = std::env::var("HOME").ok().map(PathBuf::from);
        let dir_name = if self.cwd.as_os_str() == "/" {
            "/".to_string()
        } else if home_dir.as_ref() == Some(&self.cwd) {
            "~".to_string()
        } else {
            self.cwd
                .file_name()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_else(|| "/".to_string())
        };
        // Subdued grey for dir: \x1b[38;5;245m (grey from 256-color palette)
        // Green arrow: \x1b[32m sets green color, \x1b[0m resets
        format!("\x1b[38;5;245m{}\x1b[0m \x1b[32m➜\x1b[0m ", dir_name)
    }

    /// Get the continuation prompt string (for multi-line input).
    pub fn get_continuation_prompt(&self) -> String {
        // Use a simple continuation prompt: "> "
        "> ".to_string()
    }

    /// Try to parse a complete escape sequence from the buffer.
    /// Returns Some(output) if sequence is complete, None if need more bytes.
    fn try_parse_escape_sequence(&mut self) -> Option<Vec<u8>> {
        // Escape sequences we handle:
        // ESC [ A  - Up arrow
        // ESC [ B  - Down arrow
        // ESC [ C  - Right arrow
        // ESC [ D  - Left arrow
        // ESC [ H  - Home
        // ESC [ F  - End

        if self.escape_buffer.len() < 2 {
            return None;
        }

        // Check for Alt+key sequences (ESC followed by a letter)
        if self.escape_buffer[1] != b'[' {
            // Alt+key sequence (ESC followed by a letter)
            let result = match self.escape_buffer[1] {
                b'f' | b'F' => self.forward_word(),        // Alt+F - forward word
                b'b' | b'B' => self.backward_word(),       // Alt+B - backward word
                b'd' | b'D' => self.delete_word_forward(), // Alt+D - delete word forward
                _ => None,
            };
            self.in_escape_sequence = false;
            self.escape_buffer.clear();
            return result;
        }

        if self.escape_buffer.len() < 3 {
            return None;
        }

        let result = match self.escape_buffer[2] {
            b'A' => self.history_up(),   // Up arrow
            b'B' => self.history_down(), // Down arrow
            b'C' => self.cursor_right(), // Right arrow
            b'D' => self.cursor_left(),  // Left arrow
            b'H' => self.cursor_home(),  // Home
            b'F' => self.cursor_end(),   // End
            b'0'..=b'9' => {
                // Could be an extended sequence like ESC [ 1 ~, need more bytes
                // For now, just wait for more or timeout
                if self.escape_buffer.len() < 4 {
                    return None;
                }
                // Ignore unknown extended sequences
                None
            }
            _ => None, // Unknown sequence
        };

        self.in_escape_sequence = false;
        self.escape_buffer.clear();
        result
    }

    /// Navigate to previous command in history (Up arrow)
    /// Skips duplicate commands to show only distinct entries
    fn history_up(&mut self) -> Option<Vec<u8>> {
        if self.history_cache.is_empty() {
            return None;
        }

        // Save current input if we're just starting to navigate
        if self.history_position == 0 {
            self.saved_input = Some(self.input_buffer.clone());
        }

        // Get the current command we're showing (or the input buffer if at position 0)
        let current_cmd = if self.history_position == 0 {
            self.saved_input.clone().unwrap_or_default()
        } else {
            self.history_cache[self.history_position - 1].clone()
        };

        // Find the next different command (history_cache is most-recent-first)
        let mut new_position = self.history_position + 1;
        while new_position <= self.history_cache.len() {
            let cmd = &self.history_cache[new_position - 1];
            if cmd != &current_cmd {
                let cmd = cmd.clone();
                self.history_position = new_position;
                return Some(self.replace_input_line(&cmd));
            }
            new_position += 1;
        }

        None
    }

    /// Navigate to next command in history (Down arrow)
    /// Skips duplicate commands to show only distinct entries
    fn history_down(&mut self) -> Option<Vec<u8>> {
        if self.history_position == 0 {
            return None;
        }

        // Get the current command we're showing
        let current_cmd = self.history_cache[self.history_position - 1].clone();

        // Find the next different command going toward more recent
        let mut new_position = self.history_position - 1;
        while new_position > 0 {
            let cmd = &self.history_cache[new_position - 1];
            if cmd != &current_cmd {
                let cmd = cmd.clone();
                self.history_position = new_position;
                return Some(self.replace_input_line(&cmd));
            }
            new_position -= 1;
        }

        // If we get here, restore the saved input
        self.history_position = 0;
        let saved = self.saved_input.take().unwrap_or_default();
        Some(self.replace_input_line(&saved))
    }

    /// Replace the current input line with new content
    fn replace_input_line(&mut self, new_content: &str) -> Vec<u8> {
        // Capture the caret position before changing the buffer.
        let old_caret = self.caret_phys(self.cursor_pos);

        // Update state
        self.input_buffer = new_content.to_string();
        self.cursor_pos = self.input_buffer.chars().count();

        // Re-render the whole line (wrap-aware).
        self.render_line_from(old_caret)
    }

    /// Move cursor right
    fn cursor_right(&mut self) -> Option<Vec<u8>> {
        let char_count = self.input_buffer.chars().count();
        if self.cursor_pos < char_count {
            let old_caret = self.caret_phys(self.cursor_pos);
            self.cursor_pos += 1;
            Some(self.render_line_from(old_caret))
        } else {
            None
        }
    }

    /// Move cursor left
    fn cursor_left(&mut self) -> Option<Vec<u8>> {
        if self.cursor_pos > 0 {
            let old_caret = self.caret_phys(self.cursor_pos);
            self.cursor_pos -= 1;
            Some(self.render_line_from(old_caret))
        } else {
            None
        }
    }

    /// Move cursor to start of line
    fn cursor_home(&mut self) -> Option<Vec<u8>> {
        if self.cursor_pos > 0 {
            let old_caret = self.caret_phys(self.cursor_pos);
            self.cursor_pos = 0;
            Some(self.render_line_from(old_caret))
        } else {
            None
        }
    }

    /// Move cursor to end of line
    fn cursor_end(&mut self) -> Option<Vec<u8>> {
        let char_count = self.input_buffer.chars().count();
        if self.cursor_pos < char_count {
            let old_caret = self.caret_phys(self.cursor_pos);
            self.cursor_pos = char_count;
            Some(self.render_line_from(old_caret))
        } else {
            None
        }
    }

    /// Handle tab completion.
    ///
    /// If exactly one completion matches, insert it.
    /// If multiple completions match, show the picker UI.
    fn handle_tab_completion(&mut self) -> Option<Vec<u8>> {
        // Get all completions
        let completions_data =
            self.backend
                .completions_full(&self.input_buffer, self.cursor_pos, &self.cwd);

        let Some((completions, replace_start, replace_end)) = completions_data else {
            return None;
        };

        if completions.is_empty() {
            return None;
        }

        // If exactly one completion, insert it directly
        if completions.len() == 1 {
            let (_, text, kind) = &completions[0];
            // Capture the caret position BEFORE modifying the buffer.
            let old_caret = self.caret_phys(self.cursor_pos);

            // Add trailing space unless it's a directory
            let is_directory = matches!(kind, syntax::CompletionKind::Directory);
            let completion_text = if is_directory {
                text.clone()
            } else {
                format!("{} ", text)
            };

            // Replace the text in the input buffer
            self.input_buffer
                .replace_range(replace_start..replace_end, &completion_text);

            // Update cursor position to end of inserted completion
            self.cursor_pos = replace_start + completion_text.len();

            // Re-render the whole line (wrap-aware).
            return Some(self.render_line_from(old_caret));
        }

        // Multiple completions - check if there's a common prefix we can complete first
        let common_prefix = find_common_prefix(completions.iter().map(|(_, t, _)| t.as_str()));
        let current_word = &self.input_buffer[replace_start..replace_end];

        if common_prefix.len() > current_word.len() {
            // There's a common prefix longer than what's typed - complete to that first
            // Capture the caret position BEFORE modifying the buffer.
            let old_caret = self.caret_phys(self.cursor_pos);
            self.input_buffer
                .replace_range(replace_start..replace_end, &common_prefix);
            self.cursor_pos = replace_start + common_prefix.len();
            return Some(self.render_line_from(old_caret));
        }

        // No common prefix to extend - show the picker
        self.enter_tab_completion(completions, replace_start, replace_end)
    }

    /// Enter tab completion picker mode
    fn enter_tab_completion(
        &mut self,
        completions: Vec<(String, String, syntax::CompletionKind)>,
        replace_start: usize,
        replace_end: usize,
    ) -> Option<Vec<u8>> {
        let prefix = self.input_buffer[replace_start..replace_end].to_string();

        // Convert completions to TabCompletionItems
        let items: Vec<TabCompletionItem> = completions
            .into_iter()
            .map(|(display, text, kind)| {
                // Calculate match indices for highlighting the prefix
                let match_indices: Vec<usize> = (0..prefix.len().min(display.len())).collect();
                let is_directory = matches!(kind, syntax::CompletionKind::Directory);
                TabCompletionItem {
                    display_text: display,
                    completion_text: text,
                    match_indices,
                    is_directory,
                }
            })
            .collect();

        let ctx = TabCompletionContext {
            replace_start,
            replace_end,
            prefix,
            all_completions: items,
        };

        self.picker_state = Some(PickerState::new_tab_completion(ctx));
        Some(self.render_picker_ui())
    }

    /// Visible (printed) width of a string, ignoring ANSI escape sequences.
    ///
    /// Used to measure the on-screen width of prompts, which contain SGR color
    /// codes that occupy no columns.
    fn visible_width(s: &str) -> usize {
        let mut width = 0;
        let mut chars = s.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch == '\x1b' {
                // Skip a CSI sequence: ESC '[' ... final-byte (0x40..=0x7e)
                if chars.peek() == Some(&'[') {
                    chars.next();
                    while let Some(&c) = chars.peek() {
                        chars.next();
                        if c.is_ascii_alphabetic() {
                            break;
                        }
                    }
                } else {
                    // Other escapes: skip the following byte conservatively
                    chars.next();
                }
            } else {
                width += ch.width().unwrap_or(0);
            }
        }
        width
    }

    /// Visible width of the primary prompt (first input line).
    fn prompt_width(&self) -> usize {
        Self::visible_width(&self.get_prompt())
    }

    /// Visible width of the continuation prompt (lines after a hard newline).
    fn cont_prompt_width(&self) -> usize {
        Self::visible_width(&self.get_continuation_prompt())
    }

    /// Compute the physical (row, col) pen position *before* each character of
    /// the input buffer, plus the final pen position after the last character.
    ///
    /// Rows are counted from the row containing the primary prompt (row 0).
    /// This models the terminal's layout exactly: the prompt occupies the
    /// leading columns, hard newlines (`\n`) move to a fresh row prefixed by the
    /// continuation prompt, and long lines soft-wrap at the terminal width using
    /// alacritty's delayed-wrap rules (a character that would overflow the line
    /// is pushed to the next row, matching wide-character handling at the edge).
    ///
    /// Returns a vector of length `char_count + 1`. Entry `i` is the pen state
    /// just before character `i`; the last entry is the pen after all input.
    /// A column equal to the terminal width denotes a pending wrap.
    fn line_pens(&self) -> Vec<(usize, usize)> {
        let cols = (self.cols as usize).max(1);
        let prompt_w = self.prompt_width();
        let cont_w = self.cont_prompt_width();

        let mut row = prompt_w / cols;
        let mut col = prompt_w % cols;

        let mut pens = Vec::with_capacity(self.input_buffer.len() + 1);
        for ch in self.input_buffer.chars() {
            pens.push((row, col));
            if ch == '\n' {
                // Hard newline: CR/LF to a fresh row, then the continuation prompt.
                row += 1 + cont_w / cols;
                col = cont_w % cols;
            } else {
                let w = ch.width().unwrap_or(0);
                // Delayed wrap: a glyph that does not fit is pushed to the next row.
                if col + w > cols {
                    row += 1;
                    col = 0;
                }
                col += w;
            }
        }
        // Final pen position (after the last character).
        pens.push((row, col));
        pens
    }

    /// Physical (row, col) of the visible caret when the logical cursor is at
    /// `cursor_pos` (a character index into the input buffer).
    ///
    /// Rows are relative to the primary prompt row. A caret sitting at the very
    /// end of a line that exactly fills the terminal width uses the delayed-wrap
    /// resting position (last column of the current row); a caret in the middle
    /// of the line at a wrap boundary resolves to the start of the next row.
    fn caret_phys(&self, cursor_pos: usize) -> (usize, usize) {
        let cols = (self.cols as usize).max(1);
        let pens = self.line_pens();
        let last = pens.len() - 1;
        let k = cursor_pos.min(last);
        let (row, col) = pens[k];
        if k == last {
            // Resting position at end-of-input (delayed wrap).
            if col >= cols {
                (row, cols - 1)
            } else {
                (row, col)
            }
        } else if col >= cols {
            // Mid-line wrap boundary: caret belongs at the start of the next row.
            (row + 1, 0)
        } else {
            (row, col)
        }
    }

    /// Emit the cursor movement to go from the terminal's resting position
    /// (immediately after the whole prompt + input has been printed) to the
    /// caret position for the current `cursor_pos`.
    fn reposition_to_cursor(&self) -> Vec<u8> {
        let end = self.caret_phys(usize::MAX); // resting end position
        let target = self.caret_phys(self.cursor_pos);
        let mut output = Vec::new();
        if target == end {
            // Already at the right spot; importantly this preserves a pending
            // wrap so the next typed character wraps instead of overwriting.
            return output;
        }
        if target.0 < end.0 {
            output.extend(format!("\x1b[{}A", end.0 - target.0).into_bytes());
        } else if target.0 > end.0 {
            output.extend(format!("\x1b[{}B", target.0 - end.0).into_bytes());
        }
        output.push(b'\r');
        if target.1 > 0 {
            output.extend(format!("\x1b[{}C", target.1).into_bytes());
        }
        output
    }

    /// Re-render the entire input line (prompt + buffer) in a wrap-aware way.
    ///
    /// `old_caret` is the terminal caret's physical position (row, col relative
    /// to the prompt row) *before* the buffer/cursor were mutated — capture it
    /// with [`Shell::caret_phys`] prior to editing. The line is redrawn by
    /// returning to the prompt row, clearing to the end of the screen, printing
    /// the prompt and highlighted input (letting the terminal soft-wrap), and
    /// then repositioning the caret for the new `cursor_pos`.
    fn render_line_from(&mut self, old_caret: (usize, usize)) -> Vec<u8> {
        let mut output = Vec::new();

        // 1. Return to the prompt row, column 0.
        if old_caret.0 > 0 {
            output.extend(format!("\x1b[{}A", old_caret.0).into_bytes());
        }
        output.push(b'\r');

        // 2. Clear everything from here to the end of the screen.
        output.extend_from_slice(b"\x1b[J");

        // 3. Reprint the prompt(s) and highlighted input.
        output.extend(self.render_full_multiline_input().as_bytes());

        // 4. Move the caret from the resting end position to the cursor.
        output.extend(self.reposition_to_cursor());

        output
    }


    /// Move cursor forward one word (Alt+F)
    fn forward_word(&mut self) -> Option<Vec<u8>> {
        let chars: Vec<char> = self.input_buffer.chars().collect();
        if self.cursor_pos >= chars.len() {
            return None;
        }

        let mut new_pos = self.cursor_pos;
        // Skip current word characters (non-boundaries)
        while new_pos < chars.len() && !is_word_boundary(chars[new_pos]) {
            new_pos += 1;
        }
        // Skip word boundaries (whitespace and /)
        while new_pos < chars.len() && is_word_boundary(chars[new_pos]) {
            new_pos += 1;
        }

        if new_pos > self.cursor_pos {
            let old_caret = self.caret_phys(self.cursor_pos);
            self.cursor_pos = new_pos;
            Some(self.render_line_from(old_caret))
        } else {
            None
        }
    }

    /// Move cursor backward one word (Alt+B)
    fn backward_word(&mut self) -> Option<Vec<u8>> {
        if self.cursor_pos == 0 {
            return None;
        }

        let chars: Vec<char> = self.input_buffer.chars().collect();
        let mut new_pos = self.cursor_pos;

        // Skip word boundaries before cursor
        while new_pos > 0 && is_word_boundary(chars[new_pos - 1]) {
            new_pos -= 1;
        }
        // Skip word characters (non-boundaries)
        while new_pos > 0 && !is_word_boundary(chars[new_pos - 1]) {
            new_pos -= 1;
        }

        if new_pos < self.cursor_pos {
            let old_caret = self.caret_phys(self.cursor_pos);
            self.cursor_pos = new_pos;
            Some(self.render_line_from(old_caret))
        } else {
            None
        }
    }

    /// Delete word before cursor (Ctrl+W)
    fn delete_word_backward(&mut self) -> Option<Vec<u8>> {
        if self.cursor_pos == 0 {
            return None;
        }

        let chars: Vec<char> = self.input_buffer.chars().collect();
        let mut new_pos = self.cursor_pos;

        // Capture the caret position BEFORE modifying the buffer.
        let old_caret = self.caret_phys(self.cursor_pos);

        // Skip word boundaries before cursor
        while new_pos > 0 && is_word_boundary(chars[new_pos - 1]) {
            new_pos -= 1;
        }
        // Skip word characters (non-boundaries)
        while new_pos > 0 && !is_word_boundary(chars[new_pos - 1]) {
            new_pos -= 1;
        }

        if new_pos < self.cursor_pos {
            // Convert character indices to byte indices for drain
            let byte_start = self
                .input_buffer
                .char_indices()
                .nth(new_pos)
                .map(|(i, _)| i)
                .unwrap_or(0);
            let byte_end = self
                .input_buffer
                .char_indices()
                .nth(self.cursor_pos)
                .map(|(i, _)| i)
                .unwrap_or(self.input_buffer.len());
            self.input_buffer.drain(byte_start..byte_end);
            self.cursor_pos = new_pos;

            // Re-render the whole line (wrap-aware).
            Some(self.render_line_from(old_caret))
        } else {
            None
        }
    }

    /// Delete word after cursor (Alt+D)
    fn delete_word_forward(&mut self) -> Option<Vec<u8>> {
        let chars: Vec<char> = self.input_buffer.chars().collect();
        if self.cursor_pos >= chars.len() {
            return None;
        }

        let start = self.cursor_pos;
        let mut end = self.cursor_pos;

        // Capture the caret position BEFORE modifying the buffer.
        let old_caret = self.caret_phys(self.cursor_pos);

        // Skip word characters (non-boundaries)
        while end < chars.len() && !is_word_boundary(chars[end]) {
            end += 1;
        }
        // Skip word boundaries
        while end < chars.len() && is_word_boundary(chars[end]) {
            end += 1;
        }

        if end > start {
            // Convert character indices to byte indices for drain
            let byte_start = self
                .input_buffer
                .char_indices()
                .nth(start)
                .map(|(i, _)| i)
                .unwrap_or(0);
            let byte_end = self
                .input_buffer
                .char_indices()
                .nth(end)
                .map(|(i, _)| i)
                .unwrap_or(self.input_buffer.len());
            self.input_buffer.drain(byte_start..byte_end);

            // Re-render the whole line (wrap-aware).
            Some(self.render_line_from(old_caret))
        } else {
            None
        }
    }

    /// Kill (delete) from cursor to end of line (Ctrl+K)
    fn kill_to_end(&mut self) -> Option<Vec<u8>> {
        let char_count = self.input_buffer.chars().count();
        if self.cursor_pos < char_count {
            // Capture the caret position BEFORE modifying the buffer.
            let old_caret = self.caret_phys(self.cursor_pos);
            // Find byte position from character position
            let byte_pos = self
                .input_buffer
                .char_indices()
                .nth(self.cursor_pos)
                .map(|(i, _)| i)
                .unwrap_or(self.input_buffer.len());
            self.input_buffer.truncate(byte_pos);
            // Re-render the whole line: a simple "clear to end of line" is wrong
            // once the killed text spans multiple wrapped rows.
            Some(self.render_line_from(old_caret))
        } else {
            None
        }
    }

    /// Kill (delete) from cursor to beginning of line (Ctrl+U)
    fn kill_to_beginning(&mut self) -> Option<Vec<u8>> {
        if self.cursor_pos > 0 {
            // Capture the caret position BEFORE modifying the buffer.
            let old_caret = self.caret_phys(self.cursor_pos);
            // Convert character position to byte position
            let byte_pos = self
                .input_buffer
                .char_indices()
                .nth(self.cursor_pos)
                .map(|(i, _)| i)
                .unwrap_or(self.input_buffer.len());
            self.input_buffer.drain(0..byte_pos);
            self.cursor_pos = 0;

            // Re-render the whole line (wrap-aware).
            Some(self.render_line_from(old_caret))
        } else {
            None
        }
    }

    // --- Picker UI Implementation (CTRL+R History Search, Tab Completion, etc.) ---

    /// Enter history search mode
    fn enter_history_search(&mut self) -> Vec<u8> {
        self.picker_state = Some(PickerState::new(PickerMode::HistorySearch));

        // Perform initial search with current input
        self.update_picker_items();

        // Render the picker UI
        self.render_picker_ui()
    }

    /// Enter file finder mode (CTRL+P)
    fn enter_file_finder(&mut self) -> Vec<u8> {
        use crate::file_finder::{parse_word_for_search_full, FileFinderContext};

        // Get the word under the cursor
        let (word, replace_start, replace_end) = self
            .backend
            .word_at_cursor(&self.input_buffer, self.cursor_pos)
            .unwrap_or_else(|| (String::new(), self.cursor_pos, self.cursor_pos));

        // Parse the word to determine base directory, query, and prefix info
        let parsed = parse_word_for_search_full(&word, &self.cwd);
        let initial_query = parsed.query.clone();

        // Create the file finder context with prefix preservation
        let mut ctx =
            FileFinderContext::from_parsed(parsed, replace_start, replace_end, word.clone());

        // Collect files from the directory
        ctx.collect_files();

        // Create picker state with initial search
        self.picker_state = Some(PickerState::new_file_finder(ctx, &initial_query));

        // Set the input buffer to the initial query for filtering
        // Save the original input buffer first
        self.saved_input_buffer = Some(self.input_buffer.clone());
        self.saved_cursor_pos = Some(self.cursor_pos);
        self.input_buffer = initial_query;
        self.cursor_pos = self.input_buffer.len();

        // Render the picker UI
        self.render_picker_ui()
    }

    /// Exit picker mode without selecting
    fn exit_picker(&mut self) -> Vec<u8> {
        let ui_lines = self.picker_state.as_ref().map(|s| s.ui_lines).unwrap_or(0);
        let is_file_finder = self
            .picker_state
            .as_ref()
            .map(|s| s.mode == PickerMode::FileFinder)
            .unwrap_or(false);
        self.picker_state = None;

        // Restore original input buffer if we were in file finder mode
        if is_file_finder {
            if let Some(saved) = self.saved_input_buffer.take() {
                self.input_buffer = saved;
            }
            if let Some(saved) = self.saved_cursor_pos.take() {
                self.cursor_pos = saved;
            }
        }

        // Clear picker UI and redraw prompt with current input
        self.clear_picker_ui_and_redraw(ui_lines)
    }

    /// Select current item and exit picker mode
    fn select_picker_item(&mut self) -> Vec<u8> {
        use crate::file_finder::quote_path_if_needed;

        // Extract the selection info before dropping picker_state
        // For tab completion, use current cursor position as the end of replacement
        let selection_info = self.picker_state.as_ref().and_then(|state| {
            let selected_item = state.selected_item()?;
            let selected_text = selected_item.completion_text().to_string();
            let is_directory = selected_item.is_directory();

            match &state.mode {
                PickerMode::HistorySearch => {
                    // History search replaces entire input (is_directory = false)
                    Some((selected_text, None, false, false))
                }
                PickerMode::TabCompletion => {
                    // Tab completion replaces from original start to saved end
                    let ctx = state.tab_completion_ctx.as_ref()?;
                    Some((
                        selected_text,
                        Some((ctx.replace_start, ctx.replace_end)),
                        is_directory,
                        false,
                    ))
                }
                PickerMode::FileFinder => {
                    // File finder replaces the original word with the selected path
                    let ctx = state.file_finder_ctx.as_ref()?;
                    Some((
                        selected_text,
                        Some((ctx.replace_start, ctx.replace_end)),
                        is_directory,
                        true,
                    ))
                }
            }
        });

        let ui_lines = self.picker_state.as_ref().map(|s| s.ui_lines).unwrap_or(0);
        let is_file_finder = self
            .picker_state
            .as_ref()
            .map(|s| s.mode == PickerMode::FileFinder)
            .unwrap_or(false);
        self.picker_state = None;

        // Restore original input buffer if we were in file finder mode
        if is_file_finder {
            if let Some(saved) = self.saved_input_buffer.take() {
                self.input_buffer = saved;
            }
            if let Some(saved) = self.saved_cursor_pos.take() {
                self.cursor_pos = saved;
            }
        }

        // Apply the selection
        if let Some((text, replace_range, is_directory, needs_quoting)) = selection_info {
            match replace_range {
                None => {
                    // History search: replace entire input
                    self.input_buffer = text;
                    self.cursor_pos = self.input_buffer.chars().count();
                }
                Some((start, end)) => {
                    // Tab/file completion: replace only the specified range
                    // Note: start and end are character indices, convert to byte indices
                    let byte_start = self
                        .input_buffer
                        .char_indices()
                        .nth(start)
                        .map(|(i, _)| i)
                        .unwrap_or(0);
                    let byte_end = self
                        .input_buffer
                        .char_indices()
                        .nth(end)
                        .map(|(i, _)| i)
                        .unwrap_or(self.input_buffer.len());

                    let completion_text = if needs_quoting {
                        // File finder: quote if needed, add trailing slash for dirs
                        let quoted = quote_path_if_needed(&text);
                        if is_directory {
                            format!("{}/", quoted)
                        } else {
                            format!("{} ", quoted)
                        }
                    } else if is_directory {
                        // Tab completion directory
                        text
                    } else {
                        // Tab completion file/command
                        format!("{} ", text)
                    };
                    self.input_buffer
                        .replace_range(byte_start..byte_end, &completion_text);
                    self.cursor_pos = start + completion_text.chars().count();
                }
            }
        }

        // Clear picker UI and redraw prompt with the selected text
        self.clear_picker_ui_and_redraw(ui_lines)
    }

    /// Update picker items based on current input and mode
    fn update_picker_items(&mut self) {
        if let Some(ref mut state) = self.picker_state {
            let items = match &state.mode {
                PickerMode::HistorySearch => self
                    .backend
                    .history_search(
                        &self.input_buffer,
                        picker::MAX_VISIBLE_ITEMS,
                        &protocol::HistoryScope::All,
                    )
                    .into_iter()
                    .map(PickerItem::History)
                    .collect(),
                PickerMode::TabCompletion => {
                    // Filter completions based on the current word being typed
                    if let Some(ref ctx) = state.tab_completion_ctx {
                        // Get the current filter text (what's been typed in the word position)
                        let filter = if ctx.replace_start < self.input_buffer.len() {
                            &self.input_buffer
                                [ctx.replace_start..self.cursor_pos.min(self.input_buffer.len())]
                        } else {
                            ""
                        };

                        // Filter completions that start with the current input
                        ctx.all_completions
                            .iter()
                            .filter(|item| {
                                item.completion_text
                                    .to_lowercase()
                                    .starts_with(&filter.to_lowercase())
                            })
                            .cloned()
                            .map(|mut item| {
                                // Update match indices based on current filter
                                item.match_indices =
                                    (0..filter.len().min(item.display_text.len())).collect();
                                PickerItem::TabCompletion(item)
                            })
                            .collect()
                    } else {
                        Vec::new()
                    }
                }
                PickerMode::FileFinder => {
                    // Re-search with the current input as the query
                    if let Some(ref ctx) = state.file_finder_ctx {
                        let matches = ctx.search(&self.input_buffer);
                        matches
                            .into_iter()
                            .map(|m| {
                                PickerItem::File(picker::FileItem {
                                    display_text: m.display_path,
                                    path: m.path,
                                    match_indices: m.match_indices,
                                    is_directory: m.is_directory,
                                })
                            })
                            .collect()
                    } else {
                        Vec::new()
                    }
                }
            };
            state.set_items(items);
        }
    }

    /// Process input while in picker mode
    fn process_picker_input_byte(&mut self, byte: u8) -> Option<Vec<u8>> {
        // Handle escape sequences in picker mode
        if self.in_escape_sequence {
            self.escape_buffer.push(byte);
            return self.try_parse_picker_escape_sequence();
        }

        if byte == 0x1b {
            // ESC - could be escape key or start of sequence
            self.in_escape_sequence = true;
            self.escape_buffer.clear();
            self.escape_buffer.push(byte);
            return None;
        }

        match byte {
            // Enter or Tab - select current result
            b'\r' | b'\n' | 0x09 => Some(self.select_picker_item()),
            // Ctrl+C - cancel picker
            0x03 => Some(self.exit_picker()),
            // Ctrl+R again - cycle to next result (for history search mode)
            0x12 => {
                if let Some(ref mut state) = self.picker_state {
                    state.cycle_next();
                }
                Some(self.render_picker_ui())
            }
            // Ctrl+A - move to beginning of query
            0x01 => {
                self.cursor_pos = 0;
                Some(self.render_picker_ui())
            }
            // Ctrl+E - move to end of query
            0x05 => {
                self.cursor_pos = self.input_buffer.chars().count();
                Some(self.render_picker_ui())
            }
            // Ctrl+W - delete word before cursor
            0x17 => {
                if self.cursor_pos > 0 {
                    let chars: Vec<char> = self.input_buffer.chars().collect();
                    let mut new_pos = self.cursor_pos;
                    // Skip word boundaries before cursor
                    while new_pos > 0 && is_word_boundary(chars[new_pos - 1]) {
                        new_pos -= 1;
                    }
                    // Skip word characters (non-boundaries)
                    while new_pos > 0 && !is_word_boundary(chars[new_pos - 1]) {
                        new_pos -= 1;
                    }
                    // Remove the text (convert char indices to byte indices)
                    let byte_start = self
                        .input_buffer
                        .char_indices()
                        .nth(new_pos)
                        .map(|(i, _)| i)
                        .unwrap_or(0);
                    let byte_end = self
                        .input_buffer
                        .char_indices()
                        .nth(self.cursor_pos)
                        .map(|(i, _)| i)
                        .unwrap_or(self.input_buffer.len());
                    self.input_buffer.drain(byte_start..byte_end);
                    self.cursor_pos = new_pos;
                    self.update_picker_items();
                }
                Some(self.render_picker_ui())
            }
            // Ctrl+U - kill to beginning of line
            0x15 => {
                if self.cursor_pos > 0 {
                    let byte_pos = self
                        .input_buffer
                        .char_indices()
                        .nth(self.cursor_pos)
                        .map(|(i, _)| i)
                        .unwrap_or(self.input_buffer.len());
                    self.input_buffer.drain(0..byte_pos);
                    self.cursor_pos = 0;
                    self.update_picker_items();
                }
                Some(self.render_picker_ui())
            }
            // Ctrl+K - kill to end of line
            0x0b => {
                let char_count = self.input_buffer.chars().count();
                if self.cursor_pos < char_count {
                    let byte_pos = self
                        .input_buffer
                        .char_indices()
                        .nth(self.cursor_pos)
                        .map(|(i, _)| i)
                        .unwrap_or(self.input_buffer.len());
                    self.input_buffer.truncate(byte_pos);
                    self.update_picker_items();
                }
                Some(self.render_picker_ui())
            }
            // Backspace - remove character from query
            0x7f | 0x08 => {
                if self.cursor_pos > 0 {
                    self.cursor_pos -= 1;
                    // cursor_pos is a character index, convert to byte offset for remove
                    let byte_pos = self
                        .input_buffer
                        .char_indices()
                        .nth(self.cursor_pos)
                        .map(|(i, _)| i)
                        .unwrap_or(self.input_buffer.len());
                    self.input_buffer.remove(byte_pos);
                    self.update_picker_items();
                }
                Some(self.render_picker_ui())
            }
            // Regular printable character - add to query
            0x20..=0x7e => {
                // cursor_pos is a character index, convert to byte offset
                let byte_pos = self
                    .input_buffer
                    .char_indices()
                    .nth(self.cursor_pos)
                    .map(|(i, _)| i)
                    .unwrap_or(self.input_buffer.len());
                self.input_buffer.insert(byte_pos, byte as char);
                self.cursor_pos += 1;
                self.update_picker_items();
                Some(self.render_picker_ui())
            }
            // UTF-8 multi-byte sequence handling for picker mode
            0x80..=0xBF => {
                // Continuation byte - add to buffer if we're accumulating
                if !self.utf8_buffer.is_empty() {
                    self.utf8_buffer.push(byte);
                    self.try_complete_utf8_char_picker()
                } else {
                    None
                }
            }
            0xC0..=0xF7 => {
                // Leading byte of a multi-byte sequence
                self.utf8_buffer.clear();
                self.utf8_buffer.push(byte);
                self.try_complete_utf8_char_picker()
            }
            _ => None,
        }
    }

    /// Try to complete a UTF-8 character from the buffer in picker mode.
    fn try_complete_utf8_char_picker(&mut self) -> Option<Vec<u8>> {
        if self.utf8_buffer.is_empty() {
            return None;
        }

        // Determine expected length from first byte
        let first = self.utf8_buffer[0];
        let expected_len = if first & 0b1111_1000 == 0b1111_0000 {
            4
        } else if first & 0b1111_0000 == 0b1110_0000 {
            3
        } else if first & 0b1110_0000 == 0b1100_0000 {
            2
        } else {
            self.utf8_buffer.clear();
            return None;
        };

        if self.utf8_buffer.len() < expected_len {
            return None;
        }

        let bytes: Vec<u8> = self.utf8_buffer.drain(..).collect();
        match std::str::from_utf8(&bytes) {
            Ok(s) => {
                for c in s.chars() {
                    // cursor_pos is a character index, need to find byte offset
                    let byte_pos = self
                        .input_buffer
                        .char_indices()
                        .nth(self.cursor_pos)
                        .map(|(i, _)| i)
                        .unwrap_or(self.input_buffer.len());
                    self.input_buffer.insert(byte_pos, c);
                    self.cursor_pos += 1;
                }
                self.update_picker_items();
                Some(self.render_picker_ui())
            }
            Err(_) => None,
        }
    }

    /// Try to parse escape sequence while in picker mode
    fn try_parse_picker_escape_sequence(&mut self) -> Option<Vec<u8>> {
        if self.escape_buffer.len() < 2 {
            return None;
        }

        // Just ESC key pressed (no following bytes) - exit picker
        if self.escape_buffer.len() == 1 {
            self.in_escape_sequence = false;
            self.escape_buffer.clear();
            return Some(self.exit_picker());
        }

        // Check for Alt+key sequences (ESC followed by a letter)
        if self.escape_buffer[1] != b'[' {
            let result = match self.escape_buffer[1] {
                b'f' | b'F' => {
                    // Alt+F - forward word
                    let chars: Vec<char> = self.input_buffer.chars().collect();
                    let mut new_pos = self.cursor_pos;
                    // Skip current word (non-boundaries)
                    while new_pos < chars.len() && !is_word_boundary(chars[new_pos]) {
                        new_pos += 1;
                    }
                    // Skip word boundaries
                    while new_pos < chars.len() && is_word_boundary(chars[new_pos]) {
                        new_pos += 1;
                    }
                    self.cursor_pos = new_pos;
                    Some(self.render_picker_ui())
                }
                b'b' | b'B' => {
                    // Alt+B - backward word
                    let chars: Vec<char> = self.input_buffer.chars().collect();
                    let mut new_pos = self.cursor_pos;
                    // Skip word boundaries
                    while new_pos > 0 && is_word_boundary(chars[new_pos - 1]) {
                        new_pos -= 1;
                    }
                    // Skip word characters (non-boundaries)
                    while new_pos > 0 && !is_word_boundary(chars[new_pos - 1]) {
                        new_pos -= 1;
                    }
                    self.cursor_pos = new_pos;
                    Some(self.render_picker_ui())
                }
                b'd' | b'D' => {
                    // Alt+D - delete word forward
                    let chars: Vec<char> = self.input_buffer.chars().collect();
                    let start = self.cursor_pos;
                    let mut end = self.cursor_pos;
                    // Skip current word (non-boundaries)
                    while end < chars.len() && !is_word_boundary(chars[end]) {
                        end += 1;
                    }
                    // Skip word boundaries
                    while end < chars.len() && is_word_boundary(chars[end]) {
                        end += 1;
                    }
                    if end > start {
                        self.input_buffer.drain(start..end);
                        self.update_picker_items();
                    }
                    Some(self.render_picker_ui())
                }
                _ => Some(self.exit_picker()), // Plain ESC or unknown - exit picker
            };
            self.in_escape_sequence = false;
            self.escape_buffer.clear();
            return result;
        }

        if self.escape_buffer.len() < 3 {
            return None;
        }

        let result = match self.escape_buffer[2] {
            b'A' => {
                // Up arrow - move selection up visually
                if let Some(ref mut state) = self.picker_state {
                    state.move_up();
                }
                Some(self.render_picker_ui())
            }
            b'B' => {
                // Down arrow - move selection down visually
                if let Some(ref mut state) = self.picker_state {
                    state.move_down();
                }
                Some(self.render_picker_ui())
            }
            _ => None,
        };

        self.in_escape_sequence = false;
        self.escape_buffer.clear();
        result
    }

    /// Render the picker UI above the input line
    fn render_picker_ui(&mut self) -> Vec<u8> {
        let prompt = self.get_prompt();
        if let Some(ref mut state) = self.picker_state {
            picker::render_picker_ui(
                state,
                &self.input_buffer,
                self.cursor_pos,
                &prompt,
                self.cols,
            )
        } else {
            Vec::new()
        }
    }

    /// Clear the picker UI and redraw the prompt
    fn clear_picker_ui_and_redraw(&mut self, ui_lines: usize) -> Vec<u8> {
        let prompt = self.get_prompt();
        let highlighted = self.backend.highlight(&self.input_buffer, &self.cwd);
        picker::clear_picker_ui(
            ui_lines,
            &prompt,
            &highlighted,
            self.input_buffer.len(),
            self.cursor_pos,
        )
    }

    /// Execute the current command and return output bytes.
    /// May set `pending_action` for subprocess spawning.
    fn execute_current_command(&mut self) -> Vec<u8> {
        let command = std::mem::take(&mut self.input_buffer);
        self.cursor_pos = 0;
        self.history_position = 0;
        self.saved_input = None;

        if command.is_empty() {
            return self.get_prompt().into_bytes();
        }

        // Record command in history (crash-safe, synced to disk)
        let history_id = self.backend.record_command(
            command.clone(),
            CommandSource::Human,
            Some(self.cwd.to_string_lossy().to_string()),
        );

        // Parse command
        let parts: Vec<&str> = command.split_whitespace().collect();
        if parts.is_empty() {
            return self.get_prompt().into_bytes();
        }

        let mut output = Vec::new();

        match parts[0] {
            "cd" => {
                let target = parts.get(1).copied().unwrap_or("~");

                // Expand tilde and environment variables
                let expanded = expand_path(target);

                // If the path is relative (doesn't start with /), join with cwd
                let target_path = if expanded.is_absolute() {
                    expanded
                } else {
                    self.cwd.join(expanded)
                };

                let exit_code = if target_path.is_dir() {
                    self.cwd = target_path.canonicalize().unwrap_or(target_path);
                    0
                } else {
                    output.extend(format!("cd: no such directory: {}\r\n", target).as_bytes());
                    1
                };

                // Record exit for builtin
                if let Some(id) = history_id {
                    self.backend.record_exit(&id, exit_code, 0);
                }

                // Refresh history after command
                self.refresh_history_cache();
                output.extend(self.get_prompt().as_bytes());
            }
            "pwd" => {
                output.extend(format!("{}\r\n", self.cwd.display()).as_bytes());

                // Record exit for builtin
                if let Some(id) = history_id {
                    self.backend.record_exit(&id, 0, 0);
                }

                self.refresh_history_cache();
                output.extend(self.get_prompt().as_bytes());
            }

            "history" => {
                // Show recent history
                let limit = parts
                    .get(1)
                    .and_then(|s| s.parse::<usize>().ok())
                    .unwrap_or(20);

                let entries = self.backend.recent_commands(limit);
                for (i, command) in entries.iter().rev().enumerate() {
                    output.extend(format!("{:5}  {}\r\n", i + 1, command).as_bytes());
                }

                // Record exit for builtin
                if let Some(id) = history_id {
                    self.backend.record_exit(&id, 0, 0);
                }

                self.refresh_history_cache();
                output.extend(self.get_prompt().as_bytes());
            }
            "exit" => {
                output.extend(b"Goodbye!\r\n");

                // Record exit for builtin
                if let Some(id) = history_id {
                    self.backend.record_exit(&id, 0, 0);
                }

                self.should_exit = true;
            }
            "rename-window" => {
                let name = parts[1..].join(" ");
                if name.is_empty() {
                    output.extend(b"rename-window: missing name argument\r\n");
                    // Record exit for builtin with error
                    if let Some(id) = history_id {
                        self.backend.record_exit(&id, 1, 0);
                    }
                    self.refresh_history_cache();
                    output.extend(self.get_prompt().as_bytes());
                } else {
                    // Record exit for builtin
                    if let Some(id) = history_id {
                        self.backend.record_exit(&id, 0, 0);
                    }
                    self.refresh_history_cache();
                    // Output will be combined with this action in handle_input
                    self.pending_action = Some(ShellAction::RenameWindow {
                        output: vec![],
                        name,
                    });
                }
            }
            "reload-env" => {
                let exit_code = match reload_shell_env() {
                    Ok(count) => {
                        output.extend(
                            format!(
                                "reload-env: loaded {} variables from {}\r\n",
                                count,
                                shell_env_file_path().display()
                            )
                            .as_bytes(),
                        );
                        0
                    }
                    Err(err) => {
                        output.extend(format!("reload-env: {}\r\n", err).as_bytes());
                        1
                    }
                };

                if let Some(id) = history_id {
                    self.backend.record_exit(&id, exit_code, 0);
                }

                self.refresh_history_cache();
                output.extend(self.get_prompt().as_bytes());
            }
            "connect" => {
                let target = parts.get(1).map(|s| s.to_string()).unwrap_or_default();
                // Optional session name: `connect <host> [name]`. A named session
                // is reattach-or-create, so it can be recovered after the local
                // side is lost; with no name a fresh per-pane session is made.
                let session = parts.get(2).map(|s| s.to_string());
                if target.is_empty() {
                    output.extend(b"connect: usage: connect <user@host|local> [session]\r\n");
                    if let Some(id) = history_id {
                        self.backend.record_exit(&id, 1, 0);
                    }
                    self.refresh_history_cache();
                    output.extend(self.get_prompt().as_bytes());
                } else {
                    // Track like a subprocess so subprocess_exited() records the
                    // exit when the remote session ends.
                    self.current_command_id = history_id.clone();
                    self.command_start_time = Some(std::time::Instant::now());
                    self.pending_action = Some(ShellAction::Connect {
                        output: vec![],
                        target,
                        session,
                        env: shell_env_snapshot(),
                    });
                    // Remote takes over; don't show a prompt.
                }
            }
            "reconnect" => {
                let target = parts.get(1).map(|s| s.to_string()).unwrap_or_default();
                if target.is_empty() {
                    output.extend(b"reconnect: usage: reconnect <user@host|local>\r\n");
                    if let Some(id) = history_id {
                        self.backend.record_exit(&id, 1, 0);
                    }
                    self.refresh_history_cache();
                    output.extend(self.get_prompt().as_bytes());
                } else {
                    self.current_command_id = history_id.clone();
                    self.command_start_time = Some(std::time::Instant::now());
                    self.pending_action = Some(ShellAction::Reconnect {
                        output: vec![],
                        target,
                        env: shell_env_snapshot(),
                    });
                }
            }
            _ => {
                // External command - request subprocess spawn
                // Store the history ID so we can record exit status later
                self.current_command_id = history_id.clone();
                self.command_start_time = Some(std::time::Instant::now());

                // Output will be combined with this action in handle_input
                self.pending_action = Some(ShellAction::SpawnSubprocess {
                    output: vec![], // Will be filled in by handle_input
                    command: parts[0].to_string(),
                    args: parts[1..].iter().map(|s| s.to_string()).collect(),
                    env: shell_env_snapshot(),
                    cwd: self.cwd.clone(),
                    history_id,
                });
                // Don't show prompt - subprocess will take over
            }
        }

        output
    }
}

pub fn hello() {
    println!("Hello from libshell!");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expand_path_tilde() {
        let home = std::env::var("HOME").unwrap();

        // Test ~ alone
        assert_eq!(expand_path("~"), PathBuf::from(&home));

        // Test ~/something
        assert_eq!(
            expand_path("~/Documents"),
            PathBuf::from(format!("{}/Documents", home))
        );

        // Test ~/nested/path
        assert_eq!(
            expand_path("~/a/b/c"),
            PathBuf::from(format!("{}/a/b/c", home))
        );
    }

    #[test]
    fn test_expand_path_env_var() {
        // Set a test env var
        std::env::set_var("TEST_SHELL_VAR", "/test/path");

        // Test $VAR syntax
        assert_eq!(
            expand_path("$TEST_SHELL_VAR/subdir"),
            PathBuf::from("/test/path/subdir")
        );

        // Test ${VAR} syntax
        assert_eq!(
            expand_path("${TEST_SHELL_VAR}/subdir"),
            PathBuf::from("/test/path/subdir")
        );

        // Clean up
        std::env::remove_var("TEST_SHELL_VAR");
    }

    #[test]
    fn test_expand_path_no_expansion() {
        // Absolute paths without special chars
        assert_eq!(expand_path("/usr/bin"), PathBuf::from("/usr/bin"));

        // Relative paths without special chars
        assert_eq!(expand_path("foo/bar"), PathBuf::from("foo/bar"));
    }

    /// Build a shell over a throwaway history file so tests don't touch the
    /// real `~/.myshell_history.log`. Returns the temp dir guard too, since it
    /// must outlive the shell.
    fn test_shell() -> (Shell, Arc<ShellCore>, tempfile::TempDir) {
        let dir = tempfile::tempdir().unwrap();
        let core = Arc::new(ShellCore::with_history_path(dir.path().join("history.log")).unwrap());
        let (shell, _prompt) = Shell::with_core(core.clone(), 80, 24);
        (shell, core, dir)
    }

    /// Typing a command line drives highlighting through the backend and
    /// produces (non-empty) echoed output rather than a control action.
    #[test]
    fn typing_highlights_through_backend() {
        let (mut shell, _core, _dir) = test_shell();
        match shell.handle_input(b"ls") {
            ShellAction::Output(bytes) => assert!(!bytes.is_empty()),
            _ => panic!("typing should produce Output"),
        }
    }

    /// Executing a non-builtin asks the compositor to spawn a subprocess and
    /// records the command in history via the backend (default Local origin).
    #[test]
    fn executing_command_spawns_and_records_history() {
        let (mut shell, core, _dir) = test_shell();

        let mut spawned = None;
        for &b in b"ls -la\r" {
            if let ShellAction::SpawnSubprocess { command, args, .. } = shell.handle_input(&[b]) {
                spawned = Some((command, args));
            }
        }

        let (command, args) = spawned.expect("ls should spawn a subprocess");
        assert_eq!(command, "ls");
        assert_eq!(args, vec!["-la".to_string()]);

        let recent = core.history().recent(10);
        assert_eq!(recent[0].command, "ls -la");
        assert_eq!(recent[0].origin, protocol::Origin::Local);
    }

    /// A builtin (`pwd`) is handled inline (Output, not a spawn) and is still
    /// recorded through the backend.
    #[test]
    fn builtin_records_history_through_backend() {
        let (mut shell, core, _dir) = test_shell();

        for &b in b"pwd\r" {
            let action = shell.handle_input(&[b]);
            assert!(
                !matches!(action, ShellAction::SpawnSubprocess { .. }),
                "pwd is a builtin and must not spawn"
            );
        }

        let recent = core.history().recent(10);
        assert_eq!(recent[0].command, "pwd");
    }
}
