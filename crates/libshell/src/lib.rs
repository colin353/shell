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

pub use history::{
    BackupConfig, CommandSource, EntryId, HistoryEntry, HistorySearchResult, SearchResult,
    ShellHistory,
};

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

    // --- CTRL+R history search state ---
    /// Whether we're in history search mode (CTRL+R)
    history_search_mode: bool,
    /// Search results from fuzzy matching
    history_search_results: Vec<HistorySearchResult>,
    /// Currently selected result index (0 = first/best match)
    history_search_selected: usize,
    /// Number of UI lines drawn in the last render (for cleanup)
    history_search_ui_lines: usize,
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
            // CTRL+R search state
            history_search_mode: false,
            history_search_results: Vec::new(),
            history_search_selected: 0,
            history_search_ui_lines: 0,
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
        self.input_buffer.is_empty() && !self.history_search_mode
    }

    /// Handle CTRL+C when the shell is active (no subprocess).
    ///
    /// If there is input, clears it and shows a new prompt.
    /// Returns the output to write to the terminal, or None if input was already empty.
    pub fn handle_ctrl_c(&mut self) -> Option<Vec<u8>> {
        // If in history search mode, exit it first
        if self.history_search_mode {
            let output = self.exit_history_search();
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
        // If in history search mode, route to search input handler
        if self.history_search_mode {
            return self.process_search_input_byte(byte);
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
                    self.cursor_pos -= 1;
                    self.input_buffer.remove(self.cursor_pos);

                    // Move cursor left one position
                    let mut output = b"\x1b[D".to_vec();
                    // Print the rest of the line after the deleted character
                    let rest = self.input_buffer[self.cursor_pos..].to_string();
                    output.extend(rest.as_bytes());
                    // Print a space to overwrite the last character, then clear to end of line
                    output.extend(b" \x1b[K");

                    // Move cursor back to the correct position
                    // We need to go back (rest.len() + 1) positions (rest + the space we printed)
                    let move_back = rest.len() + 1;
                    if move_back > 0 {
                        output.extend(format!("\x1b[{}D", move_back).as_bytes());
                    }
                    Some(output)
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
                output.extend(self.input_buffer.as_bytes());
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
            // Regular printable character
            0x20..=0x7e => {
                self.input_buffer.insert(self.cursor_pos, byte as char);
                self.cursor_pos += 1;

                // Echo the character and rest of line
                let rest = self.input_buffer[self.cursor_pos - 1..].to_string();
                let mut output = rest.into_bytes();

                // Move cursor back if we inserted in the middle
                let move_back = self.input_buffer.len() - self.cursor_pos;
                if move_back > 0 {
                    output.extend(format!("\x1b[{}D", move_back).as_bytes());
                }
                Some(output)
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
    fn history_up(&mut self) -> Option<Vec<u8>> {
        if self.history_cache.is_empty() {
            return None;
        }

        // Save current input if we're just starting to navigate
        if self.history_position == 0 {
            self.saved_input = Some(self.input_buffer.clone());
        }

        // Move to previous command (history_cache is most-recent-first)
        if self.history_position < self.history_cache.len() {
            self.history_position += 1;
            let cmd = self.history_cache[self.history_position - 1].clone();
            return Some(self.replace_input_line(&cmd));
        }

        None
    }

    /// Navigate to next command in history (Down arrow)
    fn history_down(&mut self) -> Option<Vec<u8>> {
        if self.history_position == 0 {
            return None;
        }

        self.history_position -= 1;

        if self.history_position == 0 {
            // Restore saved input
            let saved = self.saved_input.take().unwrap_or_default();
            return Some(self.replace_input_line(&saved));
        }

        let cmd = self.history_cache[self.history_position - 1].clone();
        Some(self.replace_input_line(&cmd))
    }

    /// Replace the current input line with new content
    fn replace_input_line(&mut self, new_content: &str) -> Vec<u8> {
        let mut output = Vec::new();

        // Move cursor to start of input
        if self.cursor_pos > 0 {
            output.extend(format!("\x1b[{}D", self.cursor_pos).as_bytes());
        }

        // Clear from cursor to end of line
        output.extend(b"\x1b[K");

        // Write new content
        output.extend(new_content.as_bytes());

        // Update state
        self.input_buffer = new_content.to_string();
        self.cursor_pos = self.input_buffer.len();

        output
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
            let deleted_len = self.cursor_pos - new_pos;
            self.input_buffer.drain(new_pos..self.cursor_pos);
            self.cursor_pos = new_pos;

            // Move cursor left, then reprint rest of line and clear
            let mut output = format!("\x1b[{}D", deleted_len).into_bytes();
            let rest = &self.input_buffer[self.cursor_pos..];
            output.extend(rest.as_bytes());
            output.extend(b"\x1b[K"); // Clear to end of line
                                      // Move cursor back to position
            if !rest.is_empty() {
                output.extend(format!("\x1b[{}D", rest.len()).as_bytes());
            }
            Some(output)
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

            // Reprint rest of line from cursor and clear
            let rest = &self.input_buffer[self.cursor_pos..];
            let mut output = Vec::new();
            output.extend(rest.as_bytes());
            output.extend(b"\x1b[K"); // Clear to end of line
                                      // Move cursor back to position
            if !rest.is_empty() {
                output.extend(format!("\x1b[{}D", rest.len()).as_bytes());
            }
            Some(output)
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
            let deleted_len = self.cursor_pos;
            self.input_buffer.drain(0..self.cursor_pos);
            self.cursor_pos = 0;

            // Move to start, reprint line, clear rest
            let mut output = format!("\x1b[{}D", deleted_len).into_bytes();
            output.extend(self.input_buffer.as_bytes());
            output.extend(b"\x1b[K"); // Clear to end of line
                                      // Move cursor back to start
            if !self.input_buffer.is_empty() {
                output.extend(format!("\x1b[{}D", self.input_buffer.len()).as_bytes());
            }
            Some(output)
        } else {
            None
        }
    }

    // --- CTRL+R History Search Implementation ---

    /// Number of search results to display
    const SEARCH_RESULT_COUNT: usize = 20;

    /// Enter history search mode
    fn enter_history_search(&mut self) -> Vec<u8> {
        self.history_search_mode = true;
        self.history_search_selected = 0;

        // Perform initial search with current input
        self.update_history_search();

        // Render the search UI
        self.render_search_ui()
    }

    /// Exit history search mode without selecting
    fn exit_history_search(&mut self) -> Vec<u8> {
        self.history_search_mode = false;
        self.history_search_results.clear();
        self.history_search_selected = 0;

        // Clear search UI and redraw prompt with current input
        self.clear_search_ui_and_redraw()
    }

    /// Select current result and exit search mode
    fn select_history_search(&mut self) -> Vec<u8> {
        let selected_command = if !self.history_search_results.is_empty() {
            Some(
                self.history_search_results[self.history_search_selected]
                    .entry
                    .command
                    .clone(),
            )
        } else {
            None
        };

        self.history_search_mode = false;
        self.history_search_results.clear();
        self.history_search_selected = 0;

        // If we have a selection, replace the input buffer
        if let Some(cmd) = selected_command {
            self.input_buffer = cmd;
            self.cursor_pos = self.input_buffer.len();
        }

        // Clear search UI and redraw prompt with the selected command
        self.clear_search_ui_and_redraw()
    }

    /// Update search results based on current input
    fn update_history_search(&mut self) {
        self.history_search_results = self
            .core
            .history()
            .search_with_indices(&self.input_buffer, Self::SEARCH_RESULT_COUNT);

        // Reset selection to first result
        self.history_search_selected = 0;
    }

    /// Process input while in history search mode
    fn process_search_input_byte(&mut self, byte: u8) -> Option<Vec<u8>> {
        // Handle escape sequences in search mode
        if self.in_escape_sequence {
            self.escape_buffer.push(byte);
            return self.try_parse_search_escape_sequence();
        }

        if byte == 0x1b {
            // ESC - could be escape key or start of sequence
            self.in_escape_sequence = true;
            self.escape_buffer.clear();
            self.escape_buffer.push(byte);
            return None;
        }

        match byte {
            // Enter - select current result
            b'\r' | b'\n' => Some(self.select_history_search()),
            // Ctrl+C or Escape (handled in escape sequence) - cancel search
            0x03 => Some(self.exit_history_search()),
            // Ctrl+R again - move to next result (cycle down)
            0x12 => {
                if !self.history_search_results.is_empty() {
                    self.history_search_selected =
                        (self.history_search_selected + 1) % self.history_search_results.len();
                }
                Some(self.render_search_ui())
            }
            // Ctrl+A - move to beginning of search query
            0x01 => {
                self.cursor_pos = 0;
                Some(self.render_search_ui())
            }
            // Ctrl+E - move to end of search query
            0x05 => {
                self.cursor_pos = self.input_buffer.len();
                Some(self.render_search_ui())
            }
            // Ctrl+W - delete word before cursor in search query
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
                    self.update_history_search();
                }
                Some(self.render_search_ui())
            }
            // Ctrl+U - kill to beginning of line
            0x15 => {
                if self.cursor_pos > 0 {
                    self.input_buffer.drain(0..self.cursor_pos);
                    self.cursor_pos = 0;
                    self.update_history_search();
                }
                Some(self.render_search_ui())
            }
            // Ctrl+K - kill to end of line
            0x0b => {
                if self.cursor_pos < self.input_buffer.len() {
                    self.input_buffer.truncate(self.cursor_pos);
                    self.update_history_search();
                }
                Some(self.render_search_ui())
            }
            // Backspace - remove character from search query
            0x7f | 0x08 => {
                if self.cursor_pos > 0 {
                    self.cursor_pos -= 1;
                    self.input_buffer.remove(self.cursor_pos);
                    self.update_history_search();
                }
                Some(self.render_search_ui())
            }
            // Regular printable character - add to search query
            0x20..=0x7e => {
                self.input_buffer.insert(self.cursor_pos, byte as char);
                self.cursor_pos += 1;
                self.update_history_search();
                Some(self.render_search_ui())
            }
            _ => None,
        }
    }

    /// Try to parse escape sequence while in search mode
    fn try_parse_search_escape_sequence(&mut self) -> Option<Vec<u8>> {
        if self.escape_buffer.len() < 2 {
            return None;
        }

        // Just ESC key pressed (no following bytes) - exit search
        if self.escape_buffer.len() == 1 {
            self.in_escape_sequence = false;
            self.escape_buffer.clear();
            return Some(self.exit_history_search());
        }

        // Check for Alt+key sequences in search mode (ESC followed by a letter)
        if self.escape_buffer[1] != b'[' {
            let result = match self.escape_buffer[1] {
                b'f' | b'F' => {
                    // Alt+F - forward word in search query
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
                    Some(self.render_search_ui())
                }
                b'b' | b'B' => {
                    // Alt+B - backward word in search query
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
                    Some(self.render_search_ui())
                }
                b'd' | b'D' => {
                    // Alt+D - delete word forward in search query
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
                        self.update_history_search();
                    }
                    Some(self.render_search_ui())
                }
                _ => Some(self.exit_history_search()), // Plain ESC or unknown - exit search
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
                // Up arrow - move selection up visually (higher index, since results are displayed bottom-to-top)
                if !self.history_search_results.is_empty() {
                    self.history_search_selected = (self.history_search_selected + 1)
                        .min(self.history_search_results.len() - 1);
                }
                Some(self.render_search_ui())
            }
            b'B' => {
                // Down arrow - move selection down visually (lower index, towards best match near prompt)
                if !self.history_search_results.is_empty() && self.history_search_selected > 0 {
                    self.history_search_selected -= 1;
                }
                Some(self.render_search_ui())
            }
            _ => None,
        };

        self.in_escape_sequence = false;
        self.escape_buffer.clear();
        result
    }

    /// Render the search UI above the input line
    fn render_search_ui(&mut self) -> Vec<u8> {
        let mut output = Vec::new();

        let num_results = self.history_search_results.len();
        let display_count = num_results.min(Self::SEARCH_RESULT_COUNT);

        // Calculate how many lines we need for the UI
        // Results + 1 for the search indicator line
        let new_ui_lines = display_count + 1;
        let prev_ui_lines = self.history_search_ui_lines;

        // Update the UI lines count immediately so subsequent renders in the same
        // batch will have the correct value (important when processing multiple chars)
        self.history_search_ui_lines = new_ui_lines;

        let prompt = self.get_prompt();
        let prompt_len = prompt.len();

        // Move cursor to column 0
        output.extend(b"\r");

        // If we previously rendered UI lines, move up past them first to get to the top
        // This ensures we're starting from a known position
        if prev_ui_lines > 0 {
            for _ in 0..prev_ui_lines {
                output.extend(b"\x1b[A"); // Move up
            }
        }

        // Now we're at the top of the previous UI (or at the prompt if first render)
        // If this is the first render, we need to create space by printing newlines
        if prev_ui_lines == 0 && new_ui_lines > 0 {
            // First time: create space by printing newlines (scrolls terminal if needed)
            for _ in 0..new_ui_lines {
                output.extend(b"\n");
            }
            // Move back up to where UI should start
            for _ in 0..new_ui_lines {
                output.extend(b"\x1b[A");
            }
        }

        // Clear from cursor to end of screen
        output.extend(b"\x1b[J");

        // Now draw each result line (from top to bottom)
        // Best matches should be at the bottom (closest to prompt), so display in reverse
        for i in (0..display_count).rev() {
            let result = &self.history_search_results[i];
            let is_selected = i == self.history_search_selected;

            // Selection indicator
            if is_selected {
                // Cyan background for selected line
                output.extend(b"\x1b[46m\x1b[30m"); // Cyan bg, black fg
                output.extend(b"> ");
            } else {
                output.extend(b"  ");
            }

            // Add exit status indicator if available (before the command)
            if let Some(exit_code) = result.entry.exit_code {
                if exit_code == 0 {
                    // Green checkmark for success
                    if is_selected {
                        output.extend("\x1b[32m✓\x1b[30m ".as_bytes()); // Green checkmark, back to black fg
                    } else {
                        output.extend("\x1b[32m✓\x1b[0m ".as_bytes()); // Green checkmark, reset
                    }
                } else {
                    // Red X with exit code for failure
                    if is_selected {
                        output.extend(format!("\x1b[31m✗\x1b[30m ").as_bytes());
                    } else {
                        output.extend(format!("\x1b[31m✗\x1b[0m ").as_bytes());
                    }
                }
            } else {
                output.extend(b"  "); // Padding when no exit code
            }

            // Render the command with match highlighting
            output.extend(self.render_highlighted_command(
                &result.entry.command,
                &result.match_indices,
                is_selected,
            ));

            // Reset colors and clear to end of line
            output.extend(b"\x1b[0m\x1b[K\r\n");
        }

        // Draw the search indicator line
        output.extend(b"\x1b[36m"); // Cyan text
        output.extend(format!("(reverse-i-search)`{}'", self.input_buffer).as_bytes());
        if num_results == 0 && !self.input_buffer.is_empty() {
            output.extend(b" [no matches]");
        } else if num_results > 0 {
            output.extend(
                format!(" [{}/{}]", self.history_search_selected + 1, num_results).as_bytes(),
            );
        }
        output.extend(b"\x1b[0m\x1b[K\r\n");

        // Now redraw the prompt line
        output.extend(prompt.as_bytes());
        output.extend(self.input_buffer.as_bytes());
        output.extend(b"\x1b[K"); // Clear to end of line

        // Position cursor correctly
        let target_col = prompt_len + self.cursor_pos;
        let current_pos = prompt_len + self.input_buffer.len();
        if current_pos > target_col {
            output.extend(format!("\x1b[{}D", current_pos - target_col).as_bytes());
        }

        output
    }

    /// Render a command with matched characters highlighted
    fn render_highlighted_command(
        &self,
        command: &str,
        match_indices: &[usize],
        is_selected: bool,
    ) -> Vec<u8> {
        let mut output = Vec::new();

        // Replace newlines and other control characters with visible representations
        // We need to track original indices for match highlighting
        let chars: Vec<char> = command.chars().collect();

        // Account for: 2 chars for selection indicator "  " or "> ", 2 chars for status "✓ " or "✗ ", 2 chars for ".."
        let max_display_width = (self.cols as usize).saturating_sub(6); // 2 + 2 + 2
        let mut display_width = 0;
        let needs_truncation = chars.len() > max_display_width;

        for (i, &ch) in chars.iter().enumerate() {
            // Check if we need to truncate - leave room for ".."
            if needs_truncation && display_width >= max_display_width {
                output.extend(b"..");
                break;
            }

            // Determine what to display for this character
            let display_char = match ch {
                '\n' | '\r' => '\\',
                '\t' => ' ',
                c if c.is_control() => ' ',
                c => c,
            };

            if match_indices.contains(&i) {
                // Highlighted match - bold yellow
                if is_selected {
                    output.extend(b"\x1b[1;33m"); // Bold yellow on cyan bg
                } else {
                    output.extend(b"\x1b[1;33m"); // Bold yellow
                }
                output.extend(display_char.to_string().as_bytes());
                if is_selected {
                    output.extend(b"\x1b[22;30m"); // Reset bold, back to black fg
                } else {
                    output.extend(b"\x1b[22;39m"); // Reset bold and color
                }
            } else {
                output.extend(display_char.to_string().as_bytes());
            }

            display_width += 1;
        }

        output
    }

    /// Clear the search UI and redraw the prompt
    fn clear_search_ui_and_redraw(&mut self) -> Vec<u8> {
        let mut output = Vec::new();

        let ui_lines = self.history_search_ui_lines;

        // Move cursor to column 0
        output.extend(b"\r");

        // Move up to the first UI line
        for _ in 0..ui_lines {
            output.extend(b"\x1b[A");
        }

        // Clear from cursor to end of screen
        output.extend(b"\x1b[J");

        // Redraw prompt with current input
        let prompt = self.get_prompt();
        output.extend(prompt.as_bytes());
        output.extend(self.input_buffer.as_bytes());

        // Position cursor correctly
        let prompt_len = prompt.len();
        let target_col = prompt_len + self.cursor_pos;
        let current_pos = prompt_len + self.input_buffer.len();
        if current_pos > target_col {
            output.extend(format!("\x1b[{}D", current_pos - target_col).as_bytes());
        }

        // Reset the UI lines tracking
        self.history_search_ui_lines = 0;

        output
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
