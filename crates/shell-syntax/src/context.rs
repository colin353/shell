//! Completion context passed from the shell.

use std::collections::HashMap;
use std::path::PathBuf;

/// Context information needed for syntax highlighting and completion.
///
/// This struct is populated by the shell and passed to the syntax highlighter
/// and completion engine. It provides access to environment variables,
/// available executables, and the current working directory.
#[derive(Debug, Clone, Default)]
pub struct CompletionContext {
    /// Available environment variables (name -> value).
    pub env_vars: HashMap<String, String>,

    /// Executables available in $PATH.
    pub path_executables: Vec<String>,

    /// Current working directory.
    pub cwd: PathBuf,
}

impl CompletionContext {
    /// Create a new empty context.
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if an environment variable exists.
    pub fn has_env_var(&self, name: &str) -> bool {
        self.env_vars.contains_key(name)
    }

    /// Check if a command is available in PATH.
    pub fn has_executable(&self, name: &str) -> bool {
        self.path_executables.iter().any(|e| e == name)
    }
}
