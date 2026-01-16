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
use std::sync::{Arc, RwLock};

pub mod history;
pub mod picker;
mod syntax;

pub use history::{
    BackupConfig, CommandSource, EntryId, HistoryEntry, HistorySearchResult, SearchResult,
    ShellHistory,
};
pub use picker::{PickerConfig, PickerItem, PickerMode, PickerState, TabCompletionContext, TabCompletionItem};

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

    /// Shell wants to exit.
    Exit,
}

/// Shared shell state that can be accessed across threads
pub struct ShellCore {
    env: RwLock<HashMap<String, String>>,
    history: ShellHistory,
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
        })
    }

    /// Get a reference to the shell history
    pub fn history(&self) -> &ShellHistory {
        &self.history
    }

    /// Record a command execution
    pub fn record_command(
        &self,
        command: String,
        source: CommandSource,
        cwd: Option<String>,
    ) -> Result<EntryId, std::io::Error> {
        self.history.record_command_with_cwd(command, source, cwd)
    }

    /// Record the exit status of a command
    pub fn record_exit(
        &self,
        id: &EntryId,
        exit_code: i32,
        duration_ms: u64,
    ) -> Result<(), std::io::Error> {
        self.history.record_exit(id, exit_code, duration_ms)
    }

    /// Mark a command as killed (Ctrl+C)
    pub fn mark_killed(&self, id: &EntryId) -> Result<(), std::io::Error> {
        self.history.mark_killed(id)
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
    core: Arc<ShellCore>,
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

    // --- Syntax highlighting and completion ---
    /// Syntax highlighter and completion engine
    syntax_handler: syntax::SyntaxHandler,
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
        let cwd = std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from("/"));

        // Build initial history cache
        let history_cache: Vec<String> = core
            .history()
            .recent(1000)
            .into_iter()
            .map(|e| e.command)
            .collect();

        let shell = Shell {
            core,
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
            // Syntax highlighting
            syntax_handler: syntax::SyntaxHandler::new(),
        };

        let prompt = shell.get_prompt();
        (shell, prompt.into_bytes())
    }

    /// Get a reference to the shared ShellCore
    pub fn core(&self) -> &Arc<ShellCore> {
        &self.core
    }

    /// Refresh the history cache (call this after executing a command)
    fn refresh_history_cache(&mut self) {
        self.history_cache = self
            .core
            .history()
            .recent(1000)
            .into_iter()
            .map(|e| e.command)
            .collect();
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
                // Combine output with the spawn action
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

            if let Err(e) = self.core.record_exit(&id, exit_code, duration_ms) {
                eprintln!("Warning: failed to record exit status: {}", e);
            }
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
            if let Err(e) = self.core.mark_killed(&id) {
                eprintln!("Warning: failed to mark command as killed: {}", e);
            }
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
            // Enter - execute command
            b'\r' | b'\n' => {
                let mut output = vec![b'\r', b'\n'];
                output.extend(self.execute_current_command());
                Some(output)
            }
            // Backspace (0x7f) or Ctrl+H (0x08)
            0x7f | 0x08 => {
                if self.cursor_pos > 0 {
                    let old_pos = self.cursor_pos;
                    self.cursor_pos -= 1;
                    self.input_buffer.remove(self.cursor_pos);

                    // Re-render the entire line with syntax highlighting
                    // Terminal cursor was at old_pos before this operation
                    Some(self.render_highlighted_input_from(old_pos))
                } else {
                    None
                }
            }
            // Ctrl+R - enter history search mode
            0x12 => Some(self.enter_history_search()),
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
                output.extend(self.get_prompt().as_bytes());
                let highlighted = self.syntax_handler.highlight(&self.input_buffer, &self.cwd);
                output.extend(highlighted.as_bytes());
                // Position cursor correctly
                let move_back = self.input_buffer.len() - self.cursor_pos;
                if move_back > 0 {
                    output.extend(format!("\x1b[{}D", move_back).as_bytes());
                }
                Some(output)
            }
            // Ctrl+A - move to beginning of line
            0x01 => self.cursor_home(),
            // Ctrl+E - move to end of line
            0x05 => self.cursor_end(),
            // Ctrl+W - delete word before cursor
            0x17 => self.delete_word_backward(),
            // Ctrl+K - kill to end of line
            0x0b => self.kill_to_end(),
            // Ctrl+U - kill to beginning of line
            0x15 => self.kill_to_beginning(),
            // Tab - completion
            0x09 => self.handle_tab_completion(),
            // Regular printable character
            0x20..=0x7e => {
                let old_pos = self.cursor_pos;
                self.input_buffer.insert(self.cursor_pos, byte as char);
                self.cursor_pos += 1;

                // Re-render the entire line with syntax highlighting
                // Use old_pos since terminal cursor hasn't moved yet
                Some(self.render_highlighted_input_from(old_pos))
            }
            // TODO: Handle escape sequences for arrow keys, etc.
            _ => None,
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
        // Green arrow: \x1b[32m sets green color, \x1b[0m resets
        format!("{} \x1b[32m➜\x1b[0m ", dir_name)
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
        let old_pos = self.cursor_pos;
        
        // Update state first
        self.input_buffer = new_content.to_string();
        self.cursor_pos = self.input_buffer.len();

        // Re-render with syntax highlighting
        // Terminal cursor was at old_pos before this operation
        self.render_highlighted_input_from(old_pos)
    }

    /// Move cursor right
    fn cursor_right(&mut self) -> Option<Vec<u8>> {
        if self.cursor_pos < self.input_buffer.len() {
            self.cursor_pos += 1;
            Some(b"\x1b[C".to_vec())
        } else {
            None
        }
    }

    /// Move cursor left
    fn cursor_left(&mut self) -> Option<Vec<u8>> {
        if self.cursor_pos > 0 {
            self.cursor_pos -= 1;
            Some(b"\x1b[D".to_vec())
        } else {
            None
        }
    }

    /// Move cursor to start of line
    fn cursor_home(&mut self) -> Option<Vec<u8>> {
        if self.cursor_pos > 0 {
            let output = format!("\x1b[{}D", self.cursor_pos).into_bytes();
            self.cursor_pos = 0;
            Some(output)
        } else {
            None
        }
    }

    /// Move cursor to end of line
    fn cursor_end(&mut self) -> Option<Vec<u8>> {
        let remaining = self.input_buffer.len() - self.cursor_pos;
        if remaining > 0 {
            let output = format!("\x1b[{}C", remaining).into_bytes();
            self.cursor_pos = self.input_buffer.len();
            Some(output)
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
        let completions_data = self.syntax_handler.completions_full(
            &self.input_buffer,
            self.cursor_pos,
            &self.cwd,
        );

        let Some((completions, replace_start, replace_end)) = completions_data else {
            return None;
        };

        if completions.is_empty() {
            return None;
        }

        // If exactly one completion, insert it directly
        if completions.len() == 1 {
            let (_, text) = &completions[0];
            let old_pos = self.cursor_pos;

            // Add trailing space for complete completions
            let completion_text = format!("{} ", text);

            // Replace the text in the input buffer
            self.input_buffer.replace_range(replace_start..replace_end, &completion_text);

            // Update cursor position to end of inserted completion
            self.cursor_pos = replace_start + completion_text.len();

            // Re-render with syntax highlighting
            return Some(self.render_highlighted_input_from(old_pos));
        }

        // Multiple completions - check if there's a common prefix we can complete first
        let common_prefix = find_common_prefix(completions.iter().map(|(_, t)| t.as_str()));
        let current_word = &self.input_buffer[replace_start..replace_end];
        
        if common_prefix.len() > current_word.len() {
            // There's a common prefix longer than what's typed - complete to that first
            let old_pos = self.cursor_pos;
            self.input_buffer.replace_range(replace_start..replace_end, &common_prefix);
            self.cursor_pos = replace_start + common_prefix.len();
            return Some(self.render_highlighted_input_from(old_pos));
        }

        // No common prefix to extend - show the picker
        self.enter_tab_completion(completions, replace_start, replace_end)
    }

    /// Enter tab completion picker mode
    fn enter_tab_completion(
        &mut self,
        completions: Vec<(String, String)>,
        replace_start: usize,
        replace_end: usize,
    ) -> Option<Vec<u8>> {
        let prefix = self.input_buffer[replace_start..replace_end].to_string();
        
        // Convert completions to TabCompletionItems
        let items: Vec<TabCompletionItem> = completions
            .into_iter()
            .map(|(display, text)| {
                // Calculate match indices for highlighting the prefix
                let match_indices: Vec<usize> = (0..prefix.len().min(display.len())).collect();
                TabCompletionItem {
                    display_text: display,
                    completion_text: text,
                    match_indices,
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

    /// Render the current input line with syntax highlighting.
    ///
    /// `old_cursor_pos` is where the terminal cursor currently is.
    /// Returns ANSI escape sequences to:
    /// 1. Move cursor to start of input
    /// 2. Clear the line
    /// 3. Write highlighted text
    /// 4. Position cursor correctly
    fn render_highlighted_input_from(&mut self, old_cursor_pos: usize) -> Vec<u8> {
        let mut output = Vec::new();

        // Move cursor to start of input area (after prompt)
        if old_cursor_pos > 0 {
            output.extend(format!("\x1b[{}D", old_cursor_pos).as_bytes());
        }

        // Clear from cursor to end of line
        output.extend(b"\x1b[K");

        // Get highlighted version of input
        let highlighted = self.syntax_handler.highlight(&self.input_buffer, &self.cwd);

        // Write the highlighted input
        output.extend(highlighted.as_bytes());

        // Move cursor back to correct position
        let move_back = self.input_buffer.len() - self.cursor_pos;
        if move_back > 0 {
            output.extend(format!("\x1b[{}D", move_back).as_bytes());
        }

        output
    }

    /// Render the current input line with syntax highlighting.
    /// Assumes terminal cursor is at self.cursor_pos.
    fn render_highlighted_input(&mut self) -> Vec<u8> {
        let pos = self.cursor_pos;
        self.render_highlighted_input_from(pos)
    }

    /// Move cursor forward one word (Alt+F)
    fn forward_word(&mut self) -> Option<Vec<u8>> {
        let chars: Vec<char> = self.input_buffer.chars().collect();
        if self.cursor_pos >= chars.len() {
            return None;
        }

        let mut new_pos = self.cursor_pos;
        // Skip current word characters
        while new_pos < chars.len() && !chars[new_pos].is_whitespace() {
            new_pos += 1;
        }
        // Skip whitespace
        while new_pos < chars.len() && chars[new_pos].is_whitespace() {
            new_pos += 1;
        }

        if new_pos > self.cursor_pos {
            let move_right = new_pos - self.cursor_pos;
            self.cursor_pos = new_pos;
            Some(format!("\x1b[{}C", move_right).into_bytes())
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

        // Skip whitespace before cursor
        while new_pos > 0 && chars[new_pos - 1].is_whitespace() {
            new_pos -= 1;
        }
        // Skip word characters
        while new_pos > 0 && !chars[new_pos - 1].is_whitespace() {
            new_pos -= 1;
        }

        if new_pos < self.cursor_pos {
            let move_left = self.cursor_pos - new_pos;
            self.cursor_pos = new_pos;
            Some(format!("\x1b[{}D", move_left).into_bytes())
        } else {
            None
        }
    }

    /// Delete word before cursor (Ctrl+W)
    fn delete_word_backward(&mut self) -> Option<Vec<u8>> {
        if self.cursor_pos == 0 {
            return None;
        }

        let old_pos = self.cursor_pos;
        let chars: Vec<char> = self.input_buffer.chars().collect();
        let mut new_pos = self.cursor_pos;

        // Skip whitespace before cursor
        while new_pos > 0 && chars[new_pos - 1].is_whitespace() {
            new_pos -= 1;
        }
        // Skip word characters
        while new_pos > 0 && !chars[new_pos - 1].is_whitespace() {
            new_pos -= 1;
        }

        if new_pos < self.cursor_pos {
            self.input_buffer.drain(new_pos..self.cursor_pos);
            self.cursor_pos = new_pos;

            // Re-render with syntax highlighting
            // Terminal cursor was at old_pos before this operation
            Some(self.render_highlighted_input_from(old_pos))
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

        // Skip word characters
        while end < chars.len() && !chars[end].is_whitespace() {
            end += 1;
        }
        // Skip whitespace
        while end < chars.len() && chars[end].is_whitespace() {
            end += 1;
        }

        if end > start {
            self.input_buffer.drain(start..end);

            // Re-render with syntax highlighting
            Some(self.render_highlighted_input())
        } else {
            None
        }
    }

    /// Kill (delete) from cursor to end of line (Ctrl+K)
    fn kill_to_end(&mut self) -> Option<Vec<u8>> {
        if self.cursor_pos < self.input_buffer.len() {
            self.input_buffer.truncate(self.cursor_pos);
            Some(b"\x1b[K".to_vec()) // Clear to end of line
        } else {
            None
        }
    }

    /// Kill (delete) from cursor to beginning of line (Ctrl+U)
    fn kill_to_beginning(&mut self) -> Option<Vec<u8>> {
        if self.cursor_pos > 0 {
            let old_pos = self.cursor_pos;
            self.input_buffer.drain(0..self.cursor_pos);
            self.cursor_pos = 0;

            // Re-render with syntax highlighting (terminal cursor was at old_pos)
            Some(self.render_highlighted_input_from(old_pos))
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

    /// Exit picker mode without selecting
    fn exit_picker(&mut self) -> Vec<u8> {
        let ui_lines = self.picker_state.as_ref().map(|s| s.ui_lines).unwrap_or(0);
        self.picker_state = None;

        // Clear picker UI and redraw prompt with current input
        self.clear_picker_ui_and_redraw(ui_lines)
    }

    /// Select current item and exit picker mode
    fn select_picker_item(&mut self) -> Vec<u8> {
        // Extract the selection info before dropping picker_state
        // For tab completion, use current cursor position as the end of replacement
        let current_cursor = self.cursor_pos;
        let selection_info = self.picker_state.as_ref().and_then(|state| {
            let selected_text = state.selected_item()?.completion_text().to_string();
            
            match &state.mode {
                PickerMode::HistorySearch => {
                    // History search replaces entire input
                    Some((selected_text, None))
                }
                PickerMode::TabCompletion => {
                    // Tab completion replaces from original start to current cursor
                    let ctx = state.tab_completion_ctx.as_ref()?;
                    // Use the current cursor position as the end, since user may have typed more
                    let replace_end = current_cursor.max(ctx.replace_start);
                    Some((selected_text, Some((ctx.replace_start, replace_end))))
                }
            }
        });

        let ui_lines = self.picker_state.as_ref().map(|s| s.ui_lines).unwrap_or(0);
        self.picker_state = None;

        // Apply the selection
        if let Some((text, replace_range)) = selection_info {
            match replace_range {
                None => {
                    // History search: replace entire input
                    self.input_buffer = text;
                    self.cursor_pos = self.input_buffer.len();
                }
                Some((start, end)) => {
                    // Tab completion: replace only the specified range and add trailing space
                    let completion_text = format!("{} ", text);
                    self.input_buffer.replace_range(start..end, &completion_text);
                    self.cursor_pos = start + completion_text.len();
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
                PickerMode::HistorySearch => {
                    self.core
                        .history()
                        .search_with_indices(&self.input_buffer, picker::MAX_VISIBLE_ITEMS)
                        .into_iter()
                        .map(PickerItem::History)
                        .collect()
                }
                PickerMode::TabCompletion => {
                    // Filter completions based on the current word being typed
                    if let Some(ref ctx) = state.tab_completion_ctx {
                        // Get the current filter text (what's been typed in the word position)
                        let filter = if ctx.replace_start < self.input_buffer.len() {
                            &self.input_buffer[ctx.replace_start..self.cursor_pos.min(self.input_buffer.len())]
                        } else {
                            ""
                        };
                        
                        // Filter completions that start with the current input
                        ctx.all_completions
                            .iter()
                            .filter(|item| {
                                item.completion_text.to_lowercase().starts_with(&filter.to_lowercase())
                            })
                            .cloned()
                            .map(|mut item| {
                                // Update match indices based on current filter
                                item.match_indices = (0..filter.len().min(item.display_text.len())).collect();
                                PickerItem::TabCompletion(item)
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
                self.cursor_pos = self.input_buffer.len();
                Some(self.render_picker_ui())
            }
            // Ctrl+W - delete word before cursor
            0x17 => {
                if self.cursor_pos > 0 {
                    // Find start of previous word
                    let mut new_pos = self.cursor_pos;
                    // Skip trailing spaces
                    while new_pos > 0 && self.input_buffer.chars().nth(new_pos - 1) == Some(' ') {
                        new_pos -= 1;
                    }
                    // Skip word characters
                    while new_pos > 0 && self.input_buffer.chars().nth(new_pos - 1) != Some(' ') {
                        new_pos -= 1;
                    }
                    // Remove the text
                    self.input_buffer.drain(new_pos..self.cursor_pos);
                    self.cursor_pos = new_pos;
                    self.update_picker_items();
                }
                Some(self.render_picker_ui())
            }
            // Ctrl+U - kill to beginning of line
            0x15 => {
                if self.cursor_pos > 0 {
                    self.input_buffer.drain(0..self.cursor_pos);
                    self.cursor_pos = 0;
                    self.update_picker_items();
                }
                Some(self.render_picker_ui())
            }
            // Ctrl+K - kill to end of line
            0x0b => {
                if self.cursor_pos < self.input_buffer.len() {
                    self.input_buffer.truncate(self.cursor_pos);
                    self.update_picker_items();
                }
                Some(self.render_picker_ui())
            }
            // Backspace - remove character from query
            0x7f | 0x08 => {
                if self.cursor_pos > 0 {
                    self.cursor_pos -= 1;
                    self.input_buffer.remove(self.cursor_pos);
                    self.update_picker_items();
                }
                Some(self.render_picker_ui())
            }
            // Regular printable character - add to query
            0x20..=0x7e => {
                self.input_buffer.insert(self.cursor_pos, byte as char);
                self.cursor_pos += 1;
                self.update_picker_items();
                Some(self.render_picker_ui())
            }
            _ => None,
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
                    // Skip current word
                    while new_pos < chars.len() && chars[new_pos] != ' ' {
                        new_pos += 1;
                    }
                    // Skip spaces
                    while new_pos < chars.len() && chars[new_pos] == ' ' {
                        new_pos += 1;
                    }
                    self.cursor_pos = new_pos;
                    Some(self.render_picker_ui())
                }
                b'b' | b'B' => {
                    // Alt+B - backward word
                    let chars: Vec<char> = self.input_buffer.chars().collect();
                    let mut new_pos = self.cursor_pos;
                    // Skip spaces
                    while new_pos > 0 && chars.get(new_pos.saturating_sub(1)) == Some(&' ') {
                        new_pos -= 1;
                    }
                    // Skip word
                    while new_pos > 0 && chars.get(new_pos.saturating_sub(1)) != Some(&' ') {
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
                    // Skip current word
                    while end < chars.len() && chars[end] != ' ' {
                        end += 1;
                    }
                    // Skip spaces
                    while end < chars.len() && chars[end] == ' ' {
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
            picker::render_picker_ui(state, &self.input_buffer, self.cursor_pos, &prompt, self.cols)
        } else {
            Vec::new()
        }
    }

    /// Clear the picker UI and redraw the prompt
    fn clear_picker_ui_and_redraw(&mut self, ui_lines: usize) -> Vec<u8> {
        let prompt = self.get_prompt();
        let highlighted = self.syntax_handler.highlight(&self.input_buffer, &self.cwd);
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
        let history_id = match self.core.record_command(
            command.clone(),
            CommandSource::Human,
            Some(self.cwd.to_string_lossy().to_string()),
        ) {
            Ok(id) => Some(id),
            Err(e) => {
                eprintln!("Warning: failed to record command in history: {}", e);
                None
            }
        };

        // Parse command
        let parts: Vec<&str> = command.split_whitespace().collect();
        if parts.is_empty() {
            return self.get_prompt().into_bytes();
        }

        let mut output = Vec::new();

        match parts[0] {
            "cd" => {
                let target = parts.get(1).copied().unwrap_or("~");
                let target_path = if target == "~" {
                    std::env::var("HOME")
                        .map(std::path::PathBuf::from)
                        .unwrap_or_else(|_| self.cwd.clone())
                } else if target.starts_with('/') {
                    std::path::PathBuf::from(target)
                } else {
                    self.cwd.join(target)
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
                    let _ = self.core.record_exit(&id, exit_code, 0);
                }

                // Refresh history after command
                self.refresh_history_cache();
                output.extend(self.get_prompt().as_bytes());
            }
            "pwd" => {
                output.extend(format!("{}\r\n", self.cwd.display()).as_bytes());

                // Record exit for builtin
                if let Some(id) = history_id {
                    let _ = self.core.record_exit(&id, 0, 0);
                }

                self.refresh_history_cache();
                output.extend(self.get_prompt().as_bytes());
            }
            "echo" => {
                output.extend(format!("{}\r\n", parts[1..].join(" ")).as_bytes());

                // Record exit for builtin
                if let Some(id) = history_id {
                    let _ = self.core.record_exit(&id, 0, 0);
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

                let entries = self.core.history().recent(limit);
                for (i, entry) in entries.iter().rev().enumerate() {
                    output.extend(format!("{:5}  {}\r\n", i + 1, entry.command).as_bytes());
                }

                // Record exit for builtin
                if let Some(id) = history_id {
                    let _ = self.core.record_exit(&id, 0, 0);
                }

                self.refresh_history_cache();
                output.extend(self.get_prompt().as_bytes());
            }
            "exit" => {
                output.extend(b"Goodbye!\r\n");

                // Record exit for builtin
                if let Some(id) = history_id {
                    let _ = self.core.record_exit(&id, 0, 0);
                }

                self.should_exit = true;
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
                    env: vec![],
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
