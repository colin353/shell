//! Tab completion engine.

use crate::builtins::{is_builtin, BUILTINS};
use crate::context::CompletionContext;
use crate::filesystem::FileSystem;
use crate::flags::get_command_info;
use std::path::PathBuf;
use tree_sitter::{Node, Tree};

/// The kind of completion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompletionKind {
    /// A command (executable or builtin).
    Command,
    /// A shell builtin.
    Builtin,
    /// An environment variable.
    EnvVar,
    /// A command flag.
    Flag,
    /// A subcommand.
    Subcommand,
    /// A file.
    File,
    /// A directory.
    Directory,
}

/// A completion suggestion.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Completion {
    /// The text to insert.
    pub text: String,
    /// The display text (may differ from insertion text).
    pub display: String,
    /// The kind of completion.
    pub kind: CompletionKind,
    /// Start position for replacement (byte offset).
    pub replace_start: usize,
    /// End position for replacement (byte offset).
    pub replace_end: usize,
    /// Whether this is a partial (prefix) completion.
    /// If true, don't add trailing space. If false, add trailing space.
    pub is_partial: bool,
}

/// Context about where the cursor is in the AST.
#[derive(Debug, Clone)]
pub struct CursorContext<'a> {
    /// The word being completed (if any).
    pub word: &'a str,
    /// Start position of the word being completed.
    pub word_start: usize,
    /// End position of the word being completed.
    pub word_end: usize,
    /// Whether we're in command position (first word).
    pub is_command_position: bool,
    /// Whether we're completing an environment variable.
    pub is_env_var: bool,
    /// The current command (for subcommand/flag completion).
    pub current_command: Option<&'a str>,
    /// Whether we're inside quotes.
    pub in_quotes: bool,
    /// The quote character if in quotes.
    pub quote_char: Option<char>,
}

/// Get completions at a cursor position.
pub fn get_completions<F: FileSystem>(
    tree: &Tree,
    source: &str,
    cursor_pos: usize,
    context: &CompletionContext,
    fs: &F,
) -> Vec<Completion> {
    let cursor_ctx = analyze_cursor_position(tree, source, cursor_pos);

    let mut completions = Vec::new();

    if cursor_ctx.is_env_var {
        // Complete environment variables
        completions.extend(complete_env_vars(&cursor_ctx, context));
    } else if cursor_ctx.is_command_position {
        // Complete commands (PATH + builtins)
        completions.extend(complete_commands(&cursor_ctx, context));
    } else if cursor_ctx.word.starts_with('-') {
        // Complete flags
        completions.extend(complete_flags(&cursor_ctx));
    } else if let Some(cmd) = cursor_ctx.current_command {
        // Check for subcommand completion
        if let Some(info) = get_command_info(cmd) {
            if !info.subcommands.is_empty() {
                completions.extend(complete_subcommands(&cursor_ctx, cmd));
            }
        }
        // Also try file completion for arguments
        completions.extend(complete_files(&cursor_ctx, context, fs));
    } else {
        // Default to file completion
        completions.extend(complete_files(&cursor_ctx, context, fs));
    }

    completions
}

/// Analyze the cursor position to determine completion context.
fn analyze_cursor_position<'a>(tree: &Tree, source: &'a str, cursor_pos: usize) -> CursorContext<'a> {
    let root = tree.root_node();

    // Find the node at cursor position
    let node = find_node_at_position(root, cursor_pos);

    // Default context
    let mut ctx = CursorContext {
        word: "",
        word_start: cursor_pos,
        word_end: cursor_pos,
        is_command_position: false,
        is_env_var: false,
        current_command: None,
        in_quotes: false,
        quote_char: None,
    };

    if let Some(node) = node {
        let start = node.start_byte();
        let end = node.end_byte();

        // Extract the word at cursor
        ctx.word_start = start;
        ctx.word_end = end.min(source.len());
        ctx.word = &source[ctx.word_start..ctx.word_end];

        // Check if we're in a variable expansion
        if node.kind() == "simple_expansion"
            || node.kind() == "expansion"
            || node.kind() == "variable_name"
        {
            ctx.is_env_var = true;
            // Extract just the variable name part
            let var_text = ctx.word.trim_start_matches('$').trim_start_matches('{');
            if let Some(end_idx) = var_text.find('}') {
                ctx.word = &var_text[..end_idx];
            } else {
                ctx.word = var_text;
            }
        }

        // Check if we're in command position
        ctx.is_command_position = is_in_command_position(node);

        // Find the current command for subcommand/flag completion
        ctx.current_command = find_current_command(node, source);

        // Check if we're inside quotes
        let (in_quotes, quote_char) = check_quote_context(node, source);
        ctx.in_quotes = in_quotes;
        ctx.quote_char = quote_char;
    } else {
        // No node found - might be at end of input or empty
        // Try to find word boundaries manually
        let (word, word_start, word_end) = extract_word_at_cursor(source, cursor_pos);
        ctx.word = word;
        ctx.word_start = word_start;
        ctx.word_end = word_end;

        // Check if this looks like a command position
        let before_cursor = &source[..cursor_pos];
        let trimmed = before_cursor.trim();
        ctx.is_command_position = trimmed.is_empty()
            || trimmed.ends_with('|')
            || trimmed.ends_with("&&")
            || trimmed.ends_with("||")
            || trimmed.ends_with(';');

        // Check for env var
        if ctx.word.starts_with('$') {
            ctx.is_env_var = true;
            ctx.word = ctx.word.trim_start_matches('$');
        }
    }

    ctx
}

/// Find the deepest node containing the cursor position.
fn find_node_at_position(node: Node, cursor_pos: usize) -> Option<Node> {
    let start = node.start_byte();
    let end = node.end_byte();

    if cursor_pos < start || cursor_pos > end {
        return None;
    }

    // Check children first (depth-first)
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if let Some(found) = find_node_at_position(child, cursor_pos) {
            return Some(found);
        }
    }

    // If no child contains the cursor, return this node
    Some(node)
}

/// Check if a node is in command position.
fn is_in_command_position(node: Node) -> bool {
    // Walk up the tree to find context
    let mut current = Some(node);
    while let Some(n) = current {
        if n.kind() == "command_name" {
            return true;
        }
        if n.kind() == "command" {
            // Check if we're the first child
            if let Some(first_child) = n.child(0) {
                if first_child.kind() == "command_name" {
                    // Check if cursor is within command_name
                    return node.start_byte() >= first_child.start_byte()
                        && node.end_byte() <= first_child.end_byte();
                }
            }
            return false;
        }
        current = n.parent();
    }
    false
}

/// Find the current command for context.
fn find_current_command<'a>(node: Node, source: &'a str) -> Option<&'a str> {
    let mut current = Some(node);
    while let Some(n) = current {
        if n.kind() == "command" {
            // Find the command_name child
            let mut cursor = n.walk();
            for child in n.children(&mut cursor) {
                if child.kind() == "command_name" {
                    let start = child.start_byte();
                    let end = child.end_byte();
                    return Some(&source[start..end.min(source.len())]);
                }
            }
        }
        current = n.parent();
    }
    None
}

/// Check if we're inside quotes.
fn check_quote_context(node: Node, source: &str) -> (bool, Option<char>) {
    let mut current = Some(node);
    while let Some(n) = current {
        if n.kind() == "string" {
            let text = &source[n.start_byte()..n.end_byte().min(source.len())];
            if text.starts_with('"') {
                return (true, Some('"'));
            } else if text.starts_with('\'') {
                return (true, Some('\''));
            }
        }
        if n.kind() == "raw_string" {
            return (true, Some('\''));
        }
        current = n.parent();
    }
    (false, None)
}

/// Extract word at cursor position from source string.
fn extract_word_at_cursor(source: &str, cursor_pos: usize) -> (&str, usize, usize) {
    if source.is_empty() || cursor_pos == 0 {
        return ("", cursor_pos, cursor_pos);
    }

    let bytes = source.as_bytes();
    let pos = cursor_pos.min(source.len());

    // Find word start
    let mut start = pos;
    while start > 0 {
        let ch = bytes[start - 1];
        if ch.is_ascii_whitespace() || ch == b'|' || ch == b';' || ch == b'&' {
            break;
        }
        start -= 1;
    }

    // Find word end
    let mut end = pos;
    while end < bytes.len() {
        let ch = bytes[end];
        if ch.is_ascii_whitespace() || ch == b'|' || ch == b';' || ch == b'&' {
            break;
        }
        end += 1;
    }

    (&source[start..end], start, end)
}

/// Complete environment variables.
fn complete_env_vars(ctx: &CursorContext, context: &CompletionContext) -> Vec<Completion> {
    let prefix = ctx.word;

    let mut completions: Vec<_> = context
        .env_vars
        .keys()
        .filter(|name| name.starts_with(prefix))
        .map(|name| Completion {
            text: format!("${}", name),
            display: name.clone(),
            kind: CompletionKind::EnvVar,
            replace_start: ctx.word_start,
            replace_end: ctx.word_end,
            is_partial: false,
        })
        .collect();

    // Sort for deterministic ordering
    completions.sort_by(|a, b| a.display.cmp(&b.display));
    completions
}

/// Complete commands (PATH executables + builtins).
fn complete_commands(ctx: &CursorContext, context: &CompletionContext) -> Vec<Completion> {
    let prefix = ctx.word;
    let mut completions = Vec::new();

    // Add matching builtins
    for &builtin in BUILTINS {
        if builtin.starts_with(prefix) {
            completions.push(Completion {
                text: builtin.to_string(),
                display: builtin.to_string(),
                kind: CompletionKind::Builtin,
                replace_start: ctx.word_start,
                replace_end: ctx.word_end,
                is_partial: false,
            });
        }
    }

    // Add matching PATH executables
    for exe in &context.path_executables {
        if exe.starts_with(prefix) && !is_builtin(exe) {
            completions.push(Completion {
                text: exe.clone(),
                display: exe.clone(),
                kind: CompletionKind::Command,
                replace_start: ctx.word_start,
                replace_end: ctx.word_end,
                is_partial: false,
            });
        }
    }

    completions
}

/// Complete command flags.
fn complete_flags(ctx: &CursorContext) -> Vec<Completion> {
    let prefix = ctx.word;
    let cmd = ctx.current_command.unwrap_or("");

    let Some(info) = get_command_info(cmd) else {
        return Vec::new();
    };

    info.flags
        .iter()
        .filter(|flag| flag.starts_with(prefix))
        .map(|flag| Completion {
            text: flag.to_string(),
            display: flag.to_string(),
            kind: CompletionKind::Flag,
            replace_start: ctx.word_start,
            replace_end: ctx.word_end,
            is_partial: false,
        })
        .collect()
}

/// Complete subcommands.
fn complete_subcommands(ctx: &CursorContext, cmd: &str) -> Vec<Completion> {
    let prefix = ctx.word;

    let Some(info) = get_command_info(cmd) else {
        return Vec::new();
    };

    info.subcommands
        .iter()
        .filter(|sub| sub.starts_with(prefix))
        .map(|sub| Completion {
            text: sub.to_string(),
            display: sub.to_string(),
            kind: CompletionKind::Subcommand,
            replace_start: ctx.word_start,
            replace_end: ctx.word_end,
            is_partial: false,
        })
        .collect()
}

/// Complete file paths.
fn complete_files<F: FileSystem>(
    ctx: &CursorContext,
    context: &CompletionContext,
    fs: &F,
) -> Vec<Completion> {
    let word = ctx.word;

    // Determine the directory to search and the prefix to match
    let (search_dir, file_prefix): (PathBuf, String) = if word.contains('/') {
        let path = if word.starts_with('/') {
            PathBuf::from(word)
        } else {
            context.cwd.join(word)
        };

        if word.ends_with('/') {
            // Directory path, list its contents
            (path, String::new())
        } else {
            // Partial path, get parent directory and filename prefix
            let parent = path.parent().unwrap_or(&context.cwd).to_path_buf();
            let prefix = path
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("")
                .to_string();
            (parent, prefix)
        }
    } else {
        // No path separator, search in cwd
        (context.cwd.clone(), word.to_string())
    };

    let Ok(entries) = fs.read_dir(&search_dir) else {
        return Vec::new();
    };

    entries
        .into_iter()
        .filter(|entry| entry.name.starts_with(&file_prefix))
        .filter(|entry| !entry.name.starts_with('.') || file_prefix.starts_with("."))
        .map(|entry| {
            let is_dir = entry.is_dir;
            let completion_text = build_file_completion_text(
                word,
                &entry.name,
                is_dir,
                ctx.in_quotes,
                ctx.quote_char,
            );

            Completion {
                text: completion_text,
                display: if is_dir {
                    format!("{}/", entry.name)
                } else {
                    entry.name.clone()
                },
                kind: if is_dir {
                    CompletionKind::Directory
                } else {
                    CompletionKind::File
                },
                replace_start: ctx.word_start,
                replace_end: ctx.word_end,
                is_partial: false,
            }
        })
        .collect()
}

/// Build the completion text for a file, handling quotes and escaping.
fn build_file_completion_text(
    original_word: &str,
    filename: &str,
    is_dir: bool,
    in_quotes: bool,
    _quote_char: Option<char>,
) -> String {
    let mut result = String::new();

    // Preserve the path prefix from the original word
    if let Some(last_slash) = original_word.rfind('/') {
        result.push_str(&original_word[..=last_slash]);
    }

    // Add the filename, with escaping if needed
    if in_quotes {
        // Inside quotes, just add the filename
        result.push_str(filename);
    } else if filename.contains(' ') || filename.contains('\'') || filename.contains('"') {
        // Needs quoting
        result.push('"');
        result.push_str(&filename.replace('"', "\\\""));
        result.push('"');
    } else {
        result.push_str(filename);
    }

    // Add trailing slash for directories
    if is_dir {
        result.push('/');
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::filesystem::{DirEntry, FakeFileSystem};

    fn make_context() -> CompletionContext {
        CompletionContext {
            env_vars: [
                ("HOME".into(), "/home/user".into()),
                ("HOSTNAME".into(), "localhost".into()),
            ]
            .into_iter()
            .collect(),
            path_executables: vec!["ls".into(), "grep".into(), "git".into(), "cargo".into()],
            cwd: PathBuf::from("/home/user"),
        }
    }

    fn make_fs() -> FakeFileSystem {
        let mut fs = FakeFileSystem::new();
        fs.add_dir(
            "/home/user",
            vec![
                DirEntry {
                    name: "file.txt".into(),
                    is_dir: false,
                },
                DirEntry {
                    name: "src".into(),
                    is_dir: true,
                },
                DirEntry {
                    name: "my file.txt".into(),
                    is_dir: false,
                },
            ],
        );
        fs
    }

    #[test]
    fn test_complete_env_vars() {
        let ctx = CursorContext {
            word: "HO",
            word_start: 6,
            word_end: 8,
            is_command_position: false,
            is_env_var: true,
            current_command: None,
            in_quotes: false,
            quote_char: None,
        };
        let context = make_context();

        let completions = complete_env_vars(&ctx, &context);
        assert_eq!(completions.len(), 2); // HOME and HOSTNAME
    }

    #[test]
    fn test_complete_commands() {
        let ctx = CursorContext {
            word: "ca",
            word_start: 0,
            word_end: 2,
            is_command_position: true,
            is_env_var: false,
            current_command: None,
            in_quotes: false,
            quote_char: None,
        };
        let context = make_context();

        let completions = complete_commands(&ctx, &context);
        assert!(completions.iter().any(|c| c.text == "cargo"));
        assert!(completions.iter().any(|c| c.text == "case")); // builtin
    }

    #[test]
    fn test_complete_subcommands() {
        let ctx = CursorContext {
            word: "comm",
            word_start: 4,
            word_end: 8,
            is_command_position: false,
            is_env_var: false,
            current_command: Some("git"),
            in_quotes: false,
            quote_char: None,
        };

        let completions = complete_subcommands(&ctx, "git");
        assert!(completions.iter().any(|c| c.text == "commit"));
    }

    #[test]
    fn test_complete_files() {
        let ctx = CursorContext {
            word: "fi",
            word_start: 4,
            word_end: 6,
            is_command_position: false,
            is_env_var: false,
            current_command: Some("cat"),
            in_quotes: false,
            quote_char: None,
        };
        let context = make_context();
        let fs = make_fs();

        let completions = complete_files(&ctx, &context, &fs);
        assert!(completions.iter().any(|c| c.display == "file.txt"));
    }

    #[test]
    fn test_file_completion_with_spaces() {
        let ctx = CursorContext {
            word: "my",
            word_start: 4,
            word_end: 6,
            is_command_position: false,
            is_env_var: false,
            current_command: Some("cat"),
            in_quotes: false,
            quote_char: None,
        };
        let context = make_context();
        let fs = make_fs();

        let completions = complete_files(&ctx, &context, &fs);
        let my_file = completions.iter().find(|c| c.display == "my file.txt");
        assert!(my_file.is_some());
        // Should be quoted
        assert!(my_file.unwrap().text.contains('"'));
    }
}
