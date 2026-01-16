//! Standard shell builtins list.

/// List of standard Bash shell builtins.
pub const BUILTINS: &[&str] = &[
    // Navigation & directory
    "cd",
    "pwd",
    "pushd",
    "popd",
    "dirs",
    // Variables & environment
    "export",
    "unset",
    "set",
    "local",
    "declare",
    "readonly",
    "env",
    "printenv",
    // I/O
    "echo",
    "printf",
    "read",
    // Control flow keywords (also treated as builtins for highlighting)
    "if",
    "then",
    "else",
    "elif",
    "fi",
    "for",
    "while",
    "until",
    "do",
    "done",
    "case",
    "esac",
    "select",
    "break",
    "continue",
    "return",
    // Job control
    "jobs",
    "fg",
    "bg",
    "wait",
    "kill",
    "disown",
    // Shell control
    "exit",
    "exec",
    "source",
    ".",
    "eval",
    "trap",
    "times",
    "ulimit",
    "umask",
    // History
    "history",
    "fc",
    // Misc
    "alias",
    "unalias",
    "type",
    "which",
    "command",
    "builtin",
    "enable",
    "help",
    "hash",
    "true",
    "false",
    ":",
    "test",
    "[",
    "getopts",
    "shift",
    "shopt",
    // Custom shell builtins
    "rename-window",
];

/// Check if a command name is a shell builtin.
pub fn is_builtin(cmd: &str) -> bool {
    BUILTINS.contains(&cmd)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_common_builtins() {
        assert!(is_builtin("cd"));
        assert!(is_builtin("echo"));
        assert!(is_builtin("export"));
        assert!(is_builtin("exit"));
    }

    #[test]
    fn test_non_builtins() {
        assert!(!is_builtin("ls"));
        assert!(!is_builtin("grep"));
        assert!(!is_builtin("cargo"));
    }
}
