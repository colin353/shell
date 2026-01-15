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

    /// Get a single completion if exactly one match exists.
    ///
    /// Returns `None` if there are zero or multiple matches.
    pub fn complete(&self, cursor_pos: usize, context: &CompletionContext) -> Option<Completion> {
        let completions = self.completions(cursor_pos, context);
        if completions.len() == 1 {
            Some(completions.into_iter().next().unwrap())
        } else {
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
            env_vars: [("HOME".into(), "/home/user".into())]
                .into_iter()
                .collect(),
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
