//! Snapshot tests for syntax highlighting.

use shell_syntax::{
    CompletionContext, DirEntry, FakeFileSystem, HighlightKind, ShellSyntax,
};
use std::path::PathBuf;

fn test_context() -> CompletionContext {
    CompletionContext {
        env_vars: [
            ("HOME".into(), "/home/user".into()),
            ("PATH".into(), "/usr/bin".into()),
            ("USER".into(), "testuser".into()),
        ]
        .into_iter()
        .collect(),
        path_executables: vec![
            "ls".into(),
            "cat".into(),
            "grep".into(),
            "git".into(),
            "cargo".into(),
            "wc".into(),
        ],
        cwd: PathBuf::from("/home/user"),
    }
}

fn fake_fs() -> FakeFileSystem {
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
                name: "script.sh".into(),
                is_dir: false,
            },
        ],
    );
    fs.add_executable("/home/user/script.sh");
    fs
}

#[test]
fn test_highlight_simple_command() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("ls -la");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_builtin() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("echo hello world");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_command_not_found() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("nonexistent --flag");
    let highlights = syntax.highlight(&test_context());

    // Verify first span is CommandNotFound
    assert!(!highlights.is_empty());
    assert_eq!(highlights[0].kind, HighlightKind::CommandNotFound);
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_executable_path() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("./script.sh");
    let highlights = syntax.highlight(&test_context());

    // Should be Command (not CommandNotFound) since it's executable
    assert!(!highlights.is_empty());
    assert_eq!(highlights[0].kind, HighlightKind::Command);
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_pipeline() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("cat file.txt | grep pattern | wc -l");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_env_var() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("echo $HOME");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_env_var_braces() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("echo ${HOME}");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_env_var_not_found() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("echo $NONEXISTENT");
    let highlights = syntax.highlight(&test_context());

    // Find the env var span and verify it's EnvVarNotFound
    let env_var_span = highlights
        .iter()
        .find(|s| s.kind == HighlightKind::EnvVarNotFound);
    assert!(env_var_span.is_some());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_string_double_quotes() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("echo \"hello world\"");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_string_single_quotes() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("echo 'hello world'");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_unclosed_quote() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("echo \"hello");
    let highlights = syntax.highlight(&test_context());

    assert!(syntax.has_errors());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_redirect() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("echo hello > output.txt");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_redirect_append() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("echo hello >> output.txt");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_and_operator() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("ls && echo done");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_or_operator() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("ls || echo failed");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_comment() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("ls # this is a comment");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_complex_command() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("git commit -m \"Initial commit\" && git push origin main");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}

#[test]
fn test_highlight_multiple_env_vars() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("echo $HOME $USER $NONEXISTENT");
    let highlights = syntax.highlight(&test_context());
    insta::assert_debug_snapshot!(highlights);
}
