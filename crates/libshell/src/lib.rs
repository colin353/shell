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
use std::sync::{Arc, RwLock};

mod history;

use history::ShellHistory;

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
    },

    /// Shell wants to exit.
    Exit,
}

/// Shared shell state that can be accessed across threads
struct ShellCore {
    env: RwLock<HashMap<String, String>>,
    history: ShellHistory,
}

impl ShellCore {
    fn new() -> Self {
        ShellCore {
            env: RwLock::new(HashMap::new()),
            history: ShellHistory::default(),
        }
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
/// let mut shell = Shell::new(80, 24);
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
    /// Cursor position within input_buffer
    cursor_pos: usize,
    /// Current working directory
    cwd: std::path::PathBuf,
    /// Whether the shell should exit
    should_exit: bool,
    /// Pending action from command execution (checked after input processing)
    pending_action: Option<ShellAction>,
}

impl Shell {
    /// Create a new shell instance with the given terminal dimensions.
    ///
    /// Returns the shell and initial output (the prompt) that should be
    /// written to the terminal.
    pub fn new(cols: u16, rows: u16) -> (Self, Vec<u8>) {
        let cwd = std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from("/"));

        let shell = Shell {
            core: Arc::new(ShellCore::new()),
            cols,
            rows,
            input_buffer: String::new(),
            cursor_pos: 0,
            cwd,
            should_exit: false,
            pending_action: None,
        };

        let prompt = shell.get_prompt();
        (shell, prompt.into_bytes())
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
                    } => {
                        return ShellAction::SpawnSubprocess {
                            output,
                            command,
                            args,
                            env,
                            cwd,
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
        let mut output = Vec::new();

        // Optionally show exit code for non-zero exits
        if exit_code != 0 {
            output.extend(format!("[exit: {}]\r\n", exit_code).as_bytes());
        }

        // Show prompt
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

    // --- Private implementation ---

    fn process_input_byte(&mut self, byte: u8) -> Option<Vec<u8>> {
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

    fn get_prompt(&self) -> String {
        let dir_name = self
            .cwd
            .file_name()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_else(|| "/".to_string());
        format!("{} $ ", dir_name)
    }

    /// Execute the current command and return output bytes.
    /// May set `pending_action` for subprocess spawning.
    fn execute_current_command(&mut self) -> Vec<u8> {
        let command = std::mem::take(&mut self.input_buffer);
        self.cursor_pos = 0;

        if command.is_empty() {
            return self.get_prompt().into_bytes();
        }

        // Add to history
        // self.core.history.entries.push(command.clone());

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

                if target_path.is_dir() {
                    self.cwd = target_path.canonicalize().unwrap_or(target_path);
                } else {
                    output.extend(format!("cd: no such directory: {}\r\n", target).as_bytes());
                }
                output.extend(self.get_prompt().as_bytes());
            }
            "pwd" => {
                output.extend(format!("{}\r\n", self.cwd.display()).as_bytes());
                output.extend(self.get_prompt().as_bytes());
            }
            "echo" => {
                output.extend(format!("{}\r\n", parts[1..].join(" ")).as_bytes());
                output.extend(self.get_prompt().as_bytes());
            }
            "exit" => {
                output.extend(b"Goodbye!\r\n");
                self.should_exit = true;
            }
            _ => {
                // External command - request subprocess spawn
                // Output will be combined with this action in handle_input
                self.pending_action = Some(ShellAction::SpawnSubprocess {
                    output: vec![], // Will be filled in by handle_input
                    command: parts[0].to_string(),
                    args: parts[1..].iter().map(|s| s.to_string()).collect(),
                    env: vec![],
                    cwd: self.cwd.clone(),
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
