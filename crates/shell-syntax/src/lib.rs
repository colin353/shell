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
