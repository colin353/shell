//! Shell syntax highlighting and tab completion using tree-sitter.
//!
//! This crate provides:
//! - Real-time syntax highlighting for Bash shell input
//! - Context-aware tab completion (commands, env vars, flags, files)
//! - Incremental parsing for efficiency
//!
//! # Example
//! ```
//! use shell_syntax::{ShellSyntax, CompletionContext};
//! use std::path::PathBuf;
//!
//! let mut syntax = ShellSyntax::new();
//! syntax.update("echo $HOME");
//!
//! let context = CompletionContext {
//!     env_vars: [("HOME".into(), "/home/user".into())].into(),
//!     path_executables: vec!["echo".into()],
//!     cwd: PathBuf::from("/home/user"),
//! };
//!
//! let highlights = syntax.highlight(&context);
//! ```

mod builtins;
mod complete;
mod context;
mod filesystem;
mod flags;
mod highlight;
mod parser;

pub use builtins::{is_builtin, BUILTINS};
pub use complete::{Completion, CompletionKind};
pub use context::CompletionContext;
pub use filesystem::{DirEntry, FakeFileSystem, FileSystem, RealFileSystem};
pub use highlight::{HighlightKind, HighlightedSpan};
pub use parser::ShellSyntax;

use std::collections::HashMap;
use std::path::PathBuf;

/// Expand a path string, handling:
/// - `~` and `~/...` -> $HOME expansion
/// - `$VAR` and `${VAR}` -> environment variable expansion
///
/// Returns the expanded path as a PathBuf.
pub fn expand_path(path: &str) -> PathBuf {
    expand_path_with_env(path, None)
}

/// Expand a path string using a custom environment variable map.
/// If env_vars is None, uses actual environment variables.
pub fn expand_path_with_env(path: &str, env_vars: Option<&HashMap<String, String>>) -> PathBuf {
    let get_var = |name: &str| -> Option<String> {
        if let Some(vars) = env_vars {
            vars.get(name).cloned()
        } else {
            std::env::var(name).ok()
        }
    };
    
    let mut result = path.to_string();
    
    // Handle tilde expansion
    if result == "~" {
        if let Some(home) = get_var("HOME") {
            return PathBuf::from(home);
        }
    } else if result.starts_with("~/") {
        if let Some(home) = get_var("HOME") {
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
                if let Some(value) = get_var(&var_name) {
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
                } else if let Some(value) = get_var(&var_name) {
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

/// Result of expanding a path, including prefix information for display.
#[derive(Debug, Clone)]
pub struct ExpandedPath {
    /// The expanded path
    pub expanded: PathBuf,
    /// The original prefix (e.g., "~" or "$HOME") if any
    pub original_prefix: Option<String>,
    /// The expanded prefix value (e.g., "/Users/name")
    pub expanded_prefix: Option<String>,
}

/// Expand a path and return both the expanded path and prefix information.
pub fn expand_path_with_info(path: &str) -> ExpandedPath {
    expand_path_with_info_env(path, None)
}

/// Expand a path and return both the expanded path and prefix information,
/// using a custom environment variable map.
pub fn expand_path_with_info_env(path: &str, env_vars: Option<&HashMap<String, String>>) -> ExpandedPath {
    let get_var = |name: &str| -> Option<String> {
        if let Some(vars) = env_vars {
            vars.get(name).cloned()
        } else {
            std::env::var(name).ok()
        }
    };
    
    let expanded = expand_path_with_env(path, env_vars);
    
    if path.starts_with('~') {
        let home = get_var("HOME").unwrap_or_default();
        ExpandedPath {
            expanded,
            original_prefix: Some("~".to_string()),
            expanded_prefix: Some(home),
        }
    } else if path.starts_with('$') {
        // Extract the variable name
        let var_end = path[1..].chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '{' || *c == '}')
            .count() + 1;
        let var_part = &path[..var_end];
        let var_expanded = expand_path_with_env(var_part, env_vars);
        ExpandedPath {
            expanded,
            original_prefix: Some(var_part.to_string()),
            expanded_prefix: Some(var_expanded.to_string_lossy().to_string()),
        }
    } else {
        ExpandedPath {
            expanded,
            original_prefix: None,
            expanded_prefix: None,
        }
    }
}
