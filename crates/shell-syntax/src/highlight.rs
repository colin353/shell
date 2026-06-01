//! Syntax highlighting types and logic.

use crate::builtins::is_builtin;
use crate::context::CompletionContext;
use crate::filesystem::FileSystem;
use std::path::PathBuf;
use tree_sitter::{Node, Tree};

/// The kind of syntax element for highlighting purposes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HighlightKind {
    /// A command (executable) that exists.
    Command,
    /// A command that was not found in PATH, builtins, or as executable path.
    CommandNotFound,
    /// A shell builtin (cd, echo, export, etc.).
    Builtin,
    /// A regular argument.
    Argument,
    /// A flag (-f, --flag).
    Flag,
    /// A quoted string.
    String,
    /// An environment variable that exists.
    EnvVar,
    /// An environment variable that was not found.
    EnvVarNotFound,
    /// An operator (|, &&, ||, ;).
    Operator,
    /// A redirection (>, >>, <, 2>&1).
    Redirect,
    /// A comment.
    Comment,
    /// A syntax error.
    Error,
}

/// A highlighted span of text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HighlightedSpan {
    /// Start byte offset (inclusive).
    pub start: usize,
    /// End byte offset (exclusive).
    pub end: usize,
    /// The kind of highlight to apply.
    pub kind: HighlightKind,
}

impl HighlightedSpan {
    /// Create a new highlighted span.
    pub fn new(start: usize, end: usize, kind: HighlightKind) -> Self {
        Self { start, end, kind }
    }
}

/// Generate highlighted spans from a parsed tree.
pub fn highlight_tree<F: FileSystem>(
    tree: &Tree,
    source: &str,
    context: &CompletionContext,
    fs: &F,
) -> Vec<HighlightedSpan> {
    let mut spans = Vec::new();
    let root = tree.root_node();

    // Collect all highlights
    highlight_node(root, source, context, fs, &mut spans, None);

    // Sort by start position
    spans.sort_by_key(|s| s.start);

    // Merge overlapping spans (keep the more specific one)
    deduplicate_spans(spans)
}

fn highlight_node<F: FileSystem>(
    node: Node,
    source: &str,
    context: &CompletionContext,
    fs: &F,
    spans: &mut Vec<HighlightedSpan>,
    parent_command: Option<&str>,
) {
    let kind = node.kind();
    let start = node.start_byte();
    let end = node.end_byte();
    let text = &source[start..end.min(source.len())];

    // Handle errors
    if node.is_error() || kind == "ERROR" {
        spans.push(HighlightedSpan::new(start, end, HighlightKind::Error));
        return;
    }

    // Track command name for context in child nodes
    let mut current_command = parent_command;

    match kind {
        // Command name (first word of a command)
        "command_name" => {
            let cmd_text = text.trim();
            let highlight_kind = classify_command(cmd_text, context, fs);
            spans.push(HighlightedSpan::new(start, end, highlight_kind));
            current_command = Some(text);
        }

        // Simple variable expansion: $VAR
        "simple_expansion" | "expansion" => {
            // Extract variable name (skip the $)
            let var_name = text
                .trim_start_matches('$')
                .trim_start_matches('{')
                .trim_end_matches('}');
            let highlight_kind = if context.has_env_var(var_name) {
                HighlightKind::EnvVar
            } else {
                HighlightKind::EnvVarNotFound
            };
            spans.push(HighlightedSpan::new(start, end, highlight_kind));
            return; // Don't recurse into expansion children
        }

        // Variable name inside expansion
        "variable_name" => {
            // Already handled by parent expansion node
            return;
        }

        // Strings
        "string" | "raw_string" | "string_content" => {
            // Only add span for the whole string, not fragments
            if kind == "string" || kind == "raw_string" {
                spans.push(HighlightedSpan::new(start, end, HighlightKind::String));
            }
        }

        // Comments
        "comment" => {
            spans.push(HighlightedSpan::new(start, end, HighlightKind::Comment));
            return;
        }

        // Operators
        "|" | "&&" | "||" | ";" | "&" => {
            spans.push(HighlightedSpan::new(start, end, HighlightKind::Operator));
            return;
        }

        // Redirections
        "file_redirect" | "heredoc_redirect" | "herestring_redirect" => {
            spans.push(HighlightedSpan::new(start, end, HighlightKind::Redirect));
            return;
        }

        // Redirection operators
        ">" | ">>" | "<" | "<<" | "<<<" | ">&" | "<&" | ">|" => {
            spans.push(HighlightedSpan::new(start, end, HighlightKind::Redirect));
            return;
        }

        // Words (arguments, flags, etc.)
        "word" => {
            let highlight_kind = if text.starts_with('-') {
                HighlightKind::Flag
            } else {
                HighlightKind::Argument
            };
            spans.push(HighlightedSpan::new(start, end, highlight_kind));
        }

        // Concatenation - handle each part
        "concatenation" => {
            // Let children handle themselves
        }

        _ => {}
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        highlight_node(child, source, context, fs, spans, current_command);
    }
}

/// Classify a command as builtin, found, or not found.
fn classify_command<F: FileSystem>(
    cmd: &str,
    context: &CompletionContext,
    fs: &F,
) -> HighlightKind {
    // Check if it's a builtin
    if is_builtin(cmd) {
        return HighlightKind::Builtin;
    }

    // Check if it's in PATH executables
    if context.has_executable(cmd) {
        return HighlightKind::Command;
    }

    // Check if it's a path to an executable
    if cmd.contains('/') {
        let path = if cmd.starts_with('/') {
            PathBuf::from(cmd)
        } else {
            context.cwd.join(cmd)
        };
        if fs.is_executable(&path) {
            return HighlightKind::Command;
        }
    }

    HighlightKind::CommandNotFound
}

/// Remove duplicate/overlapping spans, preferring more specific highlights.
fn deduplicate_spans(mut spans: Vec<HighlightedSpan>) -> Vec<HighlightedSpan> {
    if spans.is_empty() {
        return spans;
    }

    spans.sort_by_key(|s| (s.start, std::cmp::Reverse(s.end)));

    let mut result = Vec::with_capacity(spans.len());
    let mut last_end = 0;

    for span in spans {
        // Skip if this span is completely contained within the last one
        if span.start >= last_end {
            last_end = span.end;
            result.push(span);
        } else if span.end > last_end {
            // Partial overlap - truncate
            result.push(HighlightedSpan::new(last_end, span.end, span.kind));
            last_end = span.end;
        }
        // else: completely contained, skip
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::filesystem::FakeFileSystem;

    fn make_context() -> CompletionContext {
        CompletionContext {
            env_vars: [("HOME".into(), "/home/user".into())].into_iter().collect(),
            path_executables: vec!["ls".into(), "grep".into(), "cat".into()],
            cwd: PathBuf::from("/home/user"),
        }
    }

    fn make_fs() -> FakeFileSystem {
        let mut fs = FakeFileSystem::new();
        fs.add_executable("/home/user/script.sh");
        fs
    }

    #[test]
    fn test_classify_builtin() {
        let context = make_context();
        let fs = make_fs();
        assert_eq!(
            classify_command("cd", &context, &fs),
            HighlightKind::Builtin
        );
        assert_eq!(
            classify_command("echo", &context, &fs),
            HighlightKind::Builtin
        );
    }

    #[test]
    fn test_classify_path_executable() {
        let context = make_context();
        let fs = make_fs();
        assert_eq!(
            classify_command("ls", &context, &fs),
            HighlightKind::Command
        );
    }

    #[test]
    fn test_classify_not_found() {
        let context = make_context();
        let fs = make_fs();
        assert_eq!(
            classify_command("nonexistent", &context, &fs),
            HighlightKind::CommandNotFound
        );
    }

    #[test]
    fn test_classify_executable_path() {
        let context = make_context();
        let fs = make_fs();
        assert_eq!(
            classify_command("./script.sh", &context, &fs),
            HighlightKind::Command
        );
    }
}
