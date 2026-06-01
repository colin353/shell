//! Tree-sitter parser wrapper with incremental parsing support.

use crate::complete::{get_completions, Completion};
use crate::context::CompletionContext;
use crate::filesystem::{FileSystem, RealFileSystem};
use crate::highlight::{highlight_tree, HighlightedSpan};
use tree_sitter::{Parser, Tree};

/// Shell syntax analyzer with incremental parsing support.
///
/// Maintains the parse tree across updates for efficient re-parsing.
pub struct ShellSyntax<F: FileSystem = RealFileSystem> {
    parser: Parser,
    tree: Option<Tree>,
    source: String,
    fs: F,
}

impl Default for ShellSyntax<RealFileSystem> {
    fn default() -> Self {
        Self::new()
    }
}

impl ShellSyntax<RealFileSystem> {
    /// Create a new shell syntax analyzer with the real filesystem.
    pub fn new() -> Self {
        Self::with_filesystem(RealFileSystem)
    }
}

impl<F: FileSystem> ShellSyntax<F> {
    /// Create a new shell syntax analyzer with a custom filesystem.
    pub fn with_filesystem(fs: F) -> Self {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_bash::LANGUAGE.into())
            .expect("Failed to load Bash grammar");

        Self {
            parser,
            tree: None,
            source: String::new(),
            fs,
        }
    }

    /// Update the source text and reparse.
    ///
    /// For now, we do a full reparse each time. Incremental parsing with
    /// tree-sitter requires calling `tree.edit()` before reparsing to describe
    /// the edit operation, which adds complexity. For shell input which is
    /// typically short (a few hundred characters at most), full reparsing is
    /// fast enough.
    pub fn update(&mut self, new_source: &str) {
        // Do a full reparse (pass None instead of old tree)
        self.tree = self.parser.parse(new_source, None);
        self.source = new_source.to_string();
    }

    /// Get the current source text.
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Get highlighted spans for the current source.
    ///
    /// Returns a list of spans with their highlight kinds.
    /// The caller is responsible for converting these to ANSI codes.
    pub fn highlight(&self, context: &CompletionContext) -> Vec<HighlightedSpan> {
        let Some(tree) = &self.tree else {
            return Vec::new();
        };

        highlight_tree(tree, &self.source, context, &self.fs)
    }

    /// Get a single completion if exactly one match exists, or a common prefix completion.
    ///
    /// Returns:
    /// - Full completion (with `is_partial = false`) if exactly one match
    /// - Prefix completion (with `is_partial = true`) if multiple matches share a common prefix
    /// - `None` if no matches or no common prefix beyond current word
    pub fn complete(&self, cursor_pos: usize, context: &CompletionContext) -> Option<Completion> {
        let completions = self.completions(cursor_pos, context);

        if completions.is_empty() {
            return None;
        }

        if completions.len() == 1 {
            // Single match - return full completion
            let mut completion = completions.into_iter().next().unwrap();
            completion.is_partial = false;
            return Some(completion);
        }

        // Multiple matches - find common prefix
        let first = &completions[0];
        let mut common_prefix = first.text.clone();

        for completion in &completions[1..] {
            // Find common prefix between current common_prefix and this completion
            let new_len = common_prefix
                .chars()
                .zip(completion.text.chars())
                .take_while(|(a, b)| a == b)
                .count();

            // Get the prefix in terms of bytes (chars may be multi-byte)
            let char_indices: Vec<_> = common_prefix.char_indices().collect();
            if new_len < char_indices.len() {
                common_prefix = common_prefix[..char_indices[new_len].0].to_string();
            }
        }

        // Check if the common prefix is longer than what's already typed
        // The current word is from replace_start to replace_end in the source
        let current_word_len = first.replace_end - first.replace_start;

        if common_prefix.len() > current_word_len {
            // Return prefix completion
            Some(Completion {
                text: common_prefix.clone(),
                display: common_prefix,
                kind: first.kind,
                replace_start: first.replace_start,
                replace_end: first.replace_end,
                is_partial: true,
            })
        } else {
            // Common prefix is not longer than current word - no completion
            None
        }
    }

    /// Get all possible completions at the cursor position.
    pub fn completions(&self, cursor_pos: usize, context: &CompletionContext) -> Vec<Completion> {
        let Some(tree) = &self.tree else {
            return Vec::new();
        };

        get_completions(tree, &self.source, cursor_pos, context, &self.fs)
    }

    /// Check if the current source has any syntax errors.
    pub fn has_errors(&self) -> bool {
        let Some(tree) = &self.tree else {
            return false;
        };

        has_errors_recursive(tree.root_node())
    }

    /// Get a reference to the parse tree (for advanced use).
    pub fn tree(&self) -> Option<&Tree> {
        self.tree.as_ref()
    }
}

/// Recursively check for ERROR nodes in the tree.
fn has_errors_recursive(node: tree_sitter::Node) -> bool {
    if node.is_error() || node.kind() == "ERROR" {
        return true;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if has_errors_recursive(child) {
            return true;
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::filesystem::FakeFileSystem;
    use std::path::PathBuf;

    fn make_context() -> CompletionContext {
        CompletionContext {
            env_vars: [("HOME".into(), "/home/user".into())].into_iter().collect(),
            path_executables: vec!["ls".into(), "grep".into(), "cargo".into()],
            cwd: PathBuf::from("/home/user"),
        }
    }

    fn make_fs() -> FakeFileSystem {
        FakeFileSystem::new()
    }

    #[test]
    fn test_new() {
        let syntax = ShellSyntax::new();
        assert!(syntax.tree.is_none());
        assert!(syntax.source.is_empty());
    }

    #[test]
    fn test_update() {
        let mut syntax = ShellSyntax::with_filesystem(make_fs());
        syntax.update("echo hello");
        assert!(syntax.tree.is_some());
        assert_eq!(syntax.source(), "echo hello");
    }

    #[test]
    fn test_incremental_update() {
        let mut syntax = ShellSyntax::with_filesystem(make_fs());
        syntax.update("echo hello");
        let first_tree = syntax.tree.is_some();

        syntax.update("echo hello world");
        let second_tree = syntax.tree.is_some();

        assert!(first_tree);
        assert!(second_tree);
    }

    #[test]
    fn test_has_errors_valid() {
        let mut syntax = ShellSyntax::with_filesystem(make_fs());
        syntax.update("echo hello");
        assert!(!syntax.has_errors());
    }

    #[test]
    fn test_has_errors_unclosed_quote() {
        let mut syntax = ShellSyntax::with_filesystem(make_fs());
        syntax.update("echo \"hello");
        assert!(syntax.has_errors());
    }

    #[test]
    fn test_highlight() {
        let mut syntax = ShellSyntax::with_filesystem(make_fs());
        syntax.update("echo $HOME");
        let context = make_context();
        let highlights = syntax.highlight(&context);
        assert!(!highlights.is_empty());
    }

    #[test]
    fn test_complete_single_match() {
        let mut syntax = ShellSyntax::with_filesystem(make_fs());
        syntax.update("carg");
        let context = make_context();
        let completion = syntax.complete(4, &context);
        assert!(completion.is_some());
        assert_eq!(completion.unwrap().text, "cargo");
    }

    #[test]
    fn test_complete_multiple_matches() {
        let mut syntax = ShellSyntax::with_filesystem(make_fs());
        syntax.update("ca"); // matches cargo, case, etc
        let mut context = make_context();
        context.path_executables.push("cat".into());
        let completion = syntax.complete(2, &context);
        // Multiple matches, should return None
        assert!(completion.is_none());
    }

    #[test]
    fn test_completions() {
        let mut syntax = ShellSyntax::with_filesystem(make_fs());
        syntax.update("ls");
        let context = make_context();
        let completions = syntax.completions(2, &context);
        assert!(completions.iter().any(|c| c.text == "ls"));
    }
}

#[cfg(test)]
mod prefix_tests {
    use super::*;
    use crate::filesystem::FakeFileSystem;
    use std::path::PathBuf;

    fn make_context_with_commands(commands: Vec<&str>) -> CompletionContext {
        CompletionContext {
            env_vars: [("HOME".into(), "/home/user".into())].into_iter().collect(),
            path_executables: commands.into_iter().map(|s| s.to_string()).collect(),
            cwd: PathBuf::from("/home/user"),
        }
    }

    #[test]
    fn test_prefix_completion_single_match() {
        let mut syntax = ShellSyntax::with_filesystem(FakeFileSystem::new());
        syntax.update("ech");

        // Only "echo" matches
        let context = make_context_with_commands(vec!["echo", "ls", "cat"]);
        let completion = syntax.complete(3, &context);

        assert!(completion.is_some());
        let c = completion.unwrap();
        assert_eq!(c.text, "echo");
        assert!(!c.is_partial, "Single match should be full completion");
    }

    #[test]
    fn test_prefix_completion_multiple_matches_common_prefix() {
        let mut syntax = ShellSyntax::with_filesystem(FakeFileSystem::new());
        syntax.update("car");

        // "cargo", "cargo-clippy", "cargo-deb" all match
        let context = make_context_with_commands(vec!["cargo", "cargo-clippy", "cargo-deb"]);
        let completion = syntax.complete(3, &context);

        assert!(completion.is_some());
        let c = completion.unwrap();
        assert_eq!(c.text, "cargo", "Should complete to common prefix");
        assert!(
            c.is_partial,
            "Multiple matches should be partial completion"
        );
    }

    #[test]
    fn test_prefix_completion_no_longer_prefix() {
        let mut syntax = ShellSyntax::with_filesystem(FakeFileSystem::new());
        syntax.update("cargo");

        // Already typed "cargo" - common prefix is "cargo" but not longer
        let context = make_context_with_commands(vec!["cargo", "cargo-clippy", "cargo-deb"]);
        let completion = syntax.complete(5, &context);

        // Should return None since common prefix is not longer than typed
        assert!(
            completion.is_none(),
            "Should return None when prefix is not longer"
        );
    }

    #[test]
    fn test_prefix_completion_different_starts() {
        let mut syntax = ShellSyntax::with_filesystem(FakeFileSystem::new());
        syntax.update("c");

        // "cat", "cargo", "curl" - common prefix is just "c"
        let context = make_context_with_commands(vec!["cat", "cargo", "curl"]);
        let completion = syntax.complete(1, &context);

        // Should return None since common prefix "c" is not longer than typed "c"
        assert!(
            completion.is_none(),
            "Should return None when no longer common prefix"
        );
    }
}
