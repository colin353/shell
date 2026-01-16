//! Snapshot tests for tab completion.

use shell_syntax::{
    CompletionContext, CompletionKind, DirEntry, FakeFileSystem, ShellSyntax,
};
use std::path::PathBuf;

fn test_context() -> CompletionContext {
    CompletionContext {
        env_vars: [
            ("HOME".into(), "/home/user".into()),
            ("HOSTNAME".into(), "localhost".into()),
            ("PATH".into(), "/usr/bin".into()),
        ]
        .into_iter()
        .collect(),
        path_executables: vec![
            "ls".into(),
            "cat".into(),
            "grep".into(),
            "git".into(),
            "cargo".into(),
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
                name: "folder".into(),
                is_dir: true,
            },
            DirEntry {
                name: "my file.txt".into(),
                is_dir: false,
            },
            DirEntry {
                name: ".hidden".into(),
                is_dir: false,
            },
        ],
    );
    fs.add_dir(
        "/home/user/folder",
        vec![DirEntry {
            name: "nested.txt".into(),
            is_dir: false,
        }],
    );
    fs
}

// === Command Completion ===

#[test]
fn test_complete_command_single_match() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("carg");
    let context = test_context();

    let completion = syntax.complete(4, &context);
    assert!(completion.is_some());
    assert_eq!(completion.as_ref().unwrap().text, "cargo");
    insta::assert_debug_snapshot!(completion);
}

#[test]
fn test_complete_command_multiple_matches() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("ca");
    let mut context = test_context();
    context.path_executables.push("cat".into());

    // Multiple matches (cargo, cat, case builtin) - should return None for single complete
    let completion = syntax.complete(2, &context);
    assert!(completion.is_none());

    // But completions() should return all matches
    let completions = syntax.completions(2, &context);
    insta::assert_debug_snapshot!(completions);
}

#[test]
fn test_complete_builtin() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("ech");
    let context = test_context();

    let completion = syntax.complete(3, &context);
    assert!(completion.is_some());
    assert_eq!(completion.as_ref().unwrap().text, "echo");
    assert_eq!(completion.as_ref().unwrap().kind, CompletionKind::Builtin);
    insta::assert_debug_snapshot!(completion);
}

// === Environment Variable Completion ===

#[test]
fn test_complete_env_var_single_match() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("echo $PA");
    let context = test_context();

    let completions = syntax.completions(8, &context);
    insta::assert_debug_snapshot!(completions);
}

#[test]
fn test_complete_env_var_multiple_matches() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("echo $HO");
    let context = test_context();

    // Should match HOME and HOSTNAME
    let completions = syntax.completions(8, &context);
    assert_eq!(completions.len(), 2);
    insta::assert_debug_snapshot!(completions);
}

// === Flag Completion ===

#[test]
fn test_complete_flag() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("git --he");
    let context = test_context();

    let completions = syntax.completions(8, &context);
    assert!(completions.iter().any(|c| c.text == "--help"));
    insta::assert_debug_snapshot!(completions);
}

#[test]
fn test_complete_flag_short() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("ls -");
    let context = test_context();

    let completions = syntax.completions(4, &context);
    // Should include short flags like -a, -l, -h
    insta::assert_debug_snapshot!(completions);
}

// === Subcommand Completion ===

#[test]
fn test_complete_subcommand() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("git comm");
    let context = test_context();

    let completions = syntax.completions(8, &context);
    assert!(completions.iter().any(|c| c.text == "commit"));
    insta::assert_debug_snapshot!(completions);
}

#[test]
fn test_complete_subcommand_cargo() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("cargo te");
    let context = test_context();

    let completions = syntax.completions(8, &context);
    assert!(completions.iter().any(|c| c.text == "test"));
    insta::assert_debug_snapshot!(completions);
}

// === File Completion ===

#[test]
fn test_complete_file() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("cat fi");
    let context = test_context();

    let completions = syntax.completions(6, &context);
    assert!(completions.iter().any(|c| c.display == "file.txt"));
    insta::assert_debug_snapshot!(completions);
}

#[test]
fn test_complete_directory() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("cd fo");
    let context = test_context();

    let completions = syntax.completions(5, &context);
    assert!(completions.iter().any(|c| c.display == "folder/"));
    assert!(completions.iter().any(|c| c.kind == CompletionKind::Directory));
    insta::assert_debug_snapshot!(completions);
}

#[test]
fn test_complete_file_with_spaces() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("cat my");
    let context = test_context();

    let completions = syntax.completions(6, &context);
    let my_file = completions.iter().find(|c| c.display == "my file.txt");
    assert!(my_file.is_some());
    // Should be quoted since filename has spaces
    assert!(my_file.unwrap().text.contains('"'));
    insta::assert_debug_snapshot!(completions);
}

#[test]
fn test_complete_hidden_file() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("cat .hi");
    let context = test_context();

    let completions = syntax.completions(7, &context);
    assert!(completions.iter().any(|c| c.display == ".hidden"));
    insta::assert_debug_snapshot!(completions);
}

#[test]
fn test_complete_no_hidden_without_dot() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("cat ");
    let context = test_context();

    let completions = syntax.completions(4, &context);
    // Should not include .hidden since we're not completing with a dot prefix
    assert!(!completions.iter().any(|c| c.display == ".hidden"));
}

// === Path Completion ===

#[test]
fn test_complete_relative_path() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("cat ./fi");
    let context = test_context();

    let completions = syntax.completions(8, &context);
    insta::assert_debug_snapshot!(completions);
}

#[test]
fn test_complete_nested_path() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("cat folder/ne");
    let context = test_context();

    let completions = syntax.completions(13, &context);
    assert!(completions.iter().any(|c| c.display == "nested.txt"));
    insta::assert_debug_snapshot!(completions);
}

// === Edge Cases ===

#[test]
fn test_complete_empty_input() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("");
    let context = test_context();

    let completions = syntax.completions(0, &context);
    // Should offer command completions
    insta::assert_debug_snapshot!("empty_completions_count", completions.len());
}

#[test]
fn test_complete_after_pipe() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("ls | gre");
    let context = test_context();

    let completions = syntax.completions(8, &context);
    assert!(completions.iter().any(|c| c.text == "grep"));
    insta::assert_debug_snapshot!(completions);
}

#[test]
fn test_complete_cursor_mid_word() {
    let mut syntax = ShellSyntax::with_filesystem(fake_fs());
    syntax.update("cargo");
    let context = test_context();

    // Cursor in middle of word "cargo" at position 3 ("car|go")
    let completions = syntax.completions(3, &context);
    insta::assert_debug_snapshot!(completions);
}

// === Tilde and Environment Variable Path Completion ===

#[test]
fn test_complete_tilde_path() {
    let mut fs = FakeFileSystem::new();
    // Add entries for $HOME (/home/user)
    fs.add_dir(
        "/home/user",
        vec![
            DirEntry {
                name: "Documents".into(),
                is_dir: true,
            },
            DirEntry {
                name: "Downloads".into(),
                is_dir: true,
            },
        ],
    );
    
    let mut syntax = ShellSyntax::with_filesystem(fs);
    syntax.update("cat ~/Do");
    let context = test_context();

    let completions = syntax.completions(8, &context);
    // Should complete with ~/Documents and ~/Downloads (preserving ~)
    assert!(!completions.is_empty(), "Should have completions for ~/Do");
    assert!(
        completions.iter().any(|c| c.text.starts_with("~/")),
        "Completions should preserve ~ prefix: {:?}",
        completions
    );
    insta::assert_debug_snapshot!(completions);
}

#[test]
fn test_complete_tilde_alone() {
    let mut fs = FakeFileSystem::new();
    fs.add_dir(
        "/home/user",
        vec![
            DirEntry {
                name: "file.txt".into(),
                is_dir: false,
            },
        ],
    );
    
    let mut syntax = ShellSyntax::with_filesystem(fs);
    syntax.update("cat ~/");
    let context = test_context();

    let completions = syntax.completions(6, &context);
    // Should list contents of home directory with ~/ prefix
    assert!(!completions.is_empty(), "Should have completions for ~/");
    insta::assert_debug_snapshot!(completions);
}

#[test]
fn test_complete_env_var_path() {
    let mut fs = FakeFileSystem::new();
    fs.add_dir(
        "/home/user",
        vec![
            DirEntry {
                name: "Documents".into(),
                is_dir: true,
            },
        ],
    );
    
    let mut syntax = ShellSyntax::with_filesystem(fs);
    syntax.update("cat $HOME/Do");
    let context = test_context();

    let completions = syntax.completions(12, &context);
    // Should complete with $HOME/Documents (preserving $HOME)
    assert!(!completions.is_empty(), "Should have completions for $HOME/Do");
    assert!(
        completions.iter().any(|c| c.text.starts_with("$HOME/")),
        "Completions should preserve $HOME prefix: {:?}",
        completions
    );
    insta::assert_debug_snapshot!(completions);
}
