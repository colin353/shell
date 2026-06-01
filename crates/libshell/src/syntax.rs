//! Syntax highlighting and tab completion integration.
//!
//! This module wraps the `shell-syntax` crate and provides:
//! - ANSI color rendering for highlighted spans
//! - CompletionContext construction from shell state
//! - Tab completion handling

use shell_syntax::{CompletionContext, HighlightKind, HighlightedSpan, ShellSyntax};
use std::path::PathBuf;

// Re-export CompletionKind for use in lib.rs
pub use shell_syntax::CompletionKind;

/// Syntax highlighter and completion engine for the shell.
pub struct SyntaxHandler {
    syntax: ShellSyntax,
    /// Cached PATH executables (refreshed periodically).
    path_executables: Vec<String>,
    /// Last time PATH was scanned.
    path_last_refresh: std::time::Instant,
}

impl Default for SyntaxHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl SyntaxHandler {
    /// Create a new syntax handler.
    pub fn new() -> Self {
        let path_executables = scan_path_executables();
        Self {
            syntax: ShellSyntax::new(),
            path_executables,
            path_last_refresh: std::time::Instant::now(),
        }
    }

    /// Update the input and get highlighted output.
    ///
    /// Returns the input string with ANSI color codes applied.
    pub fn highlight(&mut self, input: &str, cwd: &PathBuf) -> String {
        self.maybe_refresh_path();
        self.syntax.update(input);

        let context = self.build_context(cwd);
        let spans = self.syntax.highlight(&context);

        render_highlighted(input, &spans)
    }

    /// Get a single completion if exactly one match exists, or a common prefix.
    ///
    /// Returns (completion_text, replace_start, replace_end, is_partial).
    /// If is_partial is false, the caller should add a trailing space.
    #[allow(dead_code)]
    pub fn complete(
        &mut self,
        input: &str,
        cursor_pos: usize,
        cwd: &PathBuf,
    ) -> Option<(String, usize, usize, bool)> {
        self.maybe_refresh_path();
        self.syntax.update(input);

        let context = self.build_context(cwd);
        self.syntax
            .complete(cursor_pos, &context)
            .map(|c| (c.text, c.replace_start, c.replace_end, c.is_partial))
    }

    /// Get all completions at cursor position.
    #[allow(dead_code)]
    pub fn completions(&mut self, input: &str, cursor_pos: usize, cwd: &PathBuf) -> Vec<String> {
        self.maybe_refresh_path();
        self.syntax.update(input);

        let context = self.build_context(cwd);
        self.syntax
            .completions(cursor_pos, &context)
            .into_iter()
            .map(|c| c.text)
            .collect()
    }

    /// Get all completions with full metadata at cursor position.
    /// Returns (completions, replace_start, replace_end) or None if no completions.
    /// Each completion is (display_text, completion_text, kind).
    pub fn completions_full(
        &mut self,
        input: &str,
        cursor_pos: usize,
        cwd: &PathBuf,
    ) -> Option<(Vec<(String, String, CompletionKind)>, usize, usize)> {
        self.maybe_refresh_path();
        self.syntax.update(input);

        let context = self.build_context(cwd);
        let completions = self.syntax.completions(cursor_pos, &context);

        if completions.is_empty() {
            return None;
        }

        // All completions share the same replace range
        let replace_start = completions[0].replace_start;
        let replace_end = completions[0].replace_end;

        let items: Vec<(String, String, CompletionKind)> = completions
            .into_iter()
            .map(|c| (c.display, c.text, c.kind))
            .collect();

        Some((items, replace_start, replace_end))
    }

    /// Get the word at the cursor position with its boundaries.
    /// Returns (word, start, end) or None if cursor is not on a word.
    pub fn word_at_cursor(
        &mut self,
        input: &str,
        cursor_pos: usize,
    ) -> Option<(String, usize, usize)> {
        self.syntax.update(input);

        // Use completions to get the word boundaries (they include word info)
        // This is a bit indirect, but it uses tree-sitter properly
        let context = CompletionContext {
            env_vars: std::collections::HashMap::new(),
            path_executables: Vec::new(),
            cwd: PathBuf::from("."),
        };

        let completions = self.syntax.completions(cursor_pos, &context);

        if !completions.is_empty() {
            let first = &completions[0];
            let word = input[first.replace_start..first.replace_end].to_string();
            return Some((word, first.replace_start, first.replace_end));
        }

        // Fallback: simple word extraction
        if input.is_empty() || cursor_pos == 0 {
            return Some((String::new(), cursor_pos, cursor_pos));
        }

        let pos = cursor_pos.min(input.len());
        let bytes = input.as_bytes();

        // Find word start (scan backwards)
        let mut start = pos;
        while start > 0 {
            let prev = start - 1;
            let c = bytes[prev] as char;
            if c.is_whitespace() {
                break;
            }
            start = prev;
        }

        // Find word end (scan forwards)
        let mut end = pos;
        while end < bytes.len() {
            let c = bytes[end] as char;
            if c.is_whitespace() {
                break;
            }
            end += 1;
        }

        let word = input[start..end].to_string();
        Some((word, start, end))
    }

    /// Check if current input has syntax errors.
    #[allow(dead_code)]
    pub fn has_errors(&self) -> bool {
        self.syntax.has_errors()
    }

    /// Build completion context from current shell state.
    fn build_context(&self, cwd: &PathBuf) -> CompletionContext {
        CompletionContext {
            env_vars: std::env::vars().collect(),
            path_executables: self.path_executables.clone(),
            cwd: cwd.clone(),
        }
    }

    /// Refresh PATH executables if enough time has passed.
    fn maybe_refresh_path(&mut self) {
        // Refresh every 30 seconds
        if self.path_last_refresh.elapsed().as_secs() > 30 {
            self.path_executables = scan_path_executables();
            self.path_last_refresh = std::time::Instant::now();
        }
    }
}

/// Scan PATH for available executables.
fn scan_path_executables() -> Vec<String> {
    let path_var = std::env::var("PATH").unwrap_or_default();
    let mut executables = Vec::new();
    let mut seen = std::collections::HashSet::new();

    for dir in path_var.split(':') {
        if let Ok(entries) = std::fs::read_dir(dir) {
            for entry in entries.flatten() {
                // Use std::fs::metadata to follow symlinks (entry.metadata() doesn't)
                if let Ok(metadata) = std::fs::metadata(entry.path()) {
                    if metadata.is_file() {
                        #[cfg(unix)]
                        {
                            use std::os::unix::fs::PermissionsExt;
                            if metadata.permissions().mode() & 0o111 != 0 {
                                if let Some(name) = entry.file_name().to_str() {
                                    if seen.insert(name.to_string()) {
                                        executables.push(name.to_string());
                                    }
                                }
                            }
                        }
                        #[cfg(not(unix))]
                        {
                            if let Some(name) = entry.file_name().to_str() {
                                if seen.insert(name.to_string()) {
                                    executables.push(name.to_string());
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    executables.sort();
    executables
}

/// Render input with ANSI color codes based on highlight spans.
fn render_highlighted(input: &str, spans: &[HighlightedSpan]) -> String {
    if spans.is_empty() {
        return input.to_string();
    }

    let mut result = String::with_capacity(input.len() * 2);
    let mut pos = 0;

    for span in spans {
        // Add unhighlighted text before this span
        if span.start > pos {
            result.push_str(&input[pos..span.start]);
        }

        // Add highlighted span
        let text = &input[span.start..span.end.min(input.len())];
        let (start_code, end_code) = highlight_to_ansi(span.kind);
        result.push_str(start_code);
        result.push_str(text);
        result.push_str(end_code);

        pos = span.end;
    }

    // Add remaining text
    if pos < input.len() {
        result.push_str(&input[pos..]);
    }

    result
}

/// Convert a HighlightKind to ANSI escape codes.
fn highlight_to_ansi(kind: HighlightKind) -> (&'static str, &'static str) {
    const RESET: &str = "\x1b[0m";

    match kind {
        // Commands - bold blue
        HighlightKind::Command => ("\x1b[1;34m", RESET),
        // Command not found - red with underline
        HighlightKind::CommandNotFound => ("\x1b[31;4m", RESET),
        // Builtins - bold cyan
        HighlightKind::Builtin => ("\x1b[1;36m", RESET),
        // Arguments - default (no color)
        HighlightKind::Argument => ("", ""),
        // Flags - yellow
        HighlightKind::Flag => ("\x1b[33m", RESET),
        // Strings - green
        HighlightKind::String => ("\x1b[32m", RESET),
        // Environment variables - magenta
        HighlightKind::EnvVar => ("\x1b[35m", RESET),
        // Env var not found - red
        HighlightKind::EnvVarNotFound => ("\x1b[31m", RESET),
        // Operators - bold white
        HighlightKind::Operator => ("\x1b[1;37m", RESET),
        // Redirects - cyan
        HighlightKind::Redirect => ("\x1b[36m", RESET),
        // Comments - dim/gray
        HighlightKind::Comment => ("\x1b[90m", RESET),
        // Errors - red background
        HighlightKind::Error => ("\x1b[41;97m", RESET),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_highlight_to_ansi() {
        let (start, end) = highlight_to_ansi(HighlightKind::Command);
        assert!(start.contains("34")); // Blue
        assert_eq!(end, "\x1b[0m");
    }

    #[test]
    fn test_render_highlighted_empty() {
        let result = render_highlighted("echo hello", &[]);
        assert_eq!(result, "echo hello");
    }

    #[test]
    fn test_render_highlighted() {
        let spans = vec![
            HighlightedSpan::new(0, 4, HighlightKind::Builtin),
            HighlightedSpan::new(5, 10, HighlightKind::Argument),
        ];
        let result = render_highlighted("echo hello", &spans);
        assert!(result.contains("\x1b[1;36m")); // Cyan for builtin
        assert!(result.contains("echo"));
        assert!(result.contains("hello"));
    }
}

#[cfg(test)]
mod path_tests {
    use super::*;

    #[test]
    fn test_path_has_cargo() {
        let executables = scan_path_executables();
        assert!(
            executables.contains(&"cargo".to_string()),
            "cargo should be in PATH executables"
        );
    }

    #[test]
    fn test_completions_include_cargo() {
        let mut handler = SyntaxHandler::new();
        let cwd = std::path::PathBuf::from("/tmp");

        // There should be multiple completions for "carg" (cargo, cargo-clippy, etc.)
        let completions = handler.completions("carg", 4, &cwd);
        assert!(
            completions.contains(&"cargo".to_string()),
            "Completions should include 'cargo'. Got: {:?}",
            completions
        );

        // With multiple matches, complete() now returns the common prefix
        // "carg" -> "cargo" (common prefix of cargo, cargo-clippy, cargo-deb, etc.)
        let completion = handler.complete("carg", 4, &cwd);
        assert!(
            completion.is_some(),
            "Should return common prefix completion"
        );
        let (text, _, _, is_partial) = completion.unwrap();
        assert_eq!(text, "cargo", "Should complete to common prefix 'cargo'");
        assert!(is_partial, "Should be marked as partial completion");
    }

    #[test]
    fn test_highlight_cargo_is_command() {
        let mut handler = SyntaxHandler::new();
        let cwd = std::path::PathBuf::from("/tmp");

        // cargo should be highlighted as a valid command, not red
        let highlighted = handler.highlight("cargo build", &cwd);
        // Should contain bold blue for Command, not red for CommandNotFound
        assert!(
            highlighted.contains("\x1b[1;34m"),
            "cargo should be highlighted as Command (bold blue). Got: {:?}",
            highlighted
        );
        assert!(
            !highlighted.contains("\x1b[31;4m"),
            "cargo should NOT be highlighted as CommandNotFound (red underline)"
        );
    }
}
