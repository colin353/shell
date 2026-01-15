//! Static database of command flags and subcommands.

/// Information about a command's flags and subcommands.
#[derive(Debug, Clone)]
pub struct CommandInfo {
    /// Available flags (e.g., "--help", "-v").
    pub flags: &'static [&'static str],
    /// Available subcommands (e.g., "commit", "push" for git).
    pub subcommands: &'static [&'static str],
}

/// Get command information for known commands.
pub fn get_command_info(cmd: &str) -> Option<CommandInfo> {
    match cmd {
        "git" => Some(CommandInfo {
            flags: &["--help", "--version", "-C", "-c", "--git-dir", "--work-tree"],
            subcommands: &[
                "add",
                "bisect",
                "branch",
                "checkout",
                "cherry-pick",
                "clone",
                "commit",
                "diff",
                "fetch",
                "grep",
                "init",
                "log",
                "merge",
                "mv",
                "pull",
                "push",
                "rebase",
                "remote",
                "reset",
                "restore",
                "revert",
                "rm",
                "show",
                "stash",
                "status",
                "switch",
                "tag",
            ],
        }),
        "cargo" => Some(CommandInfo {
            flags: &[
                "--help",
                "--version",
                "-V",
                "-q",
                "--quiet",
                "--color",
                "--frozen",
                "--locked",
            ],
            subcommands: &[
                "add",
                "bench",
                "build",
                "check",
                "clean",
                "clippy",
                "doc",
                "fetch",
                "fix",
                "fmt",
                "init",
                "install",
                "new",
                "publish",
                "remove",
                "run",
                "search",
                "test",
                "tree",
                "uninstall",
                "update",
            ],
        }),
        "ls" => Some(CommandInfo {
            flags: &[
                "-a",
                "--all",
                "-l",
                "--long",
                "-h",
                "--human-readable",
                "-R",
                "--recursive",
                "-S",
                "-t",
                "-r",
                "--reverse",
                "--color",
                "-1",
                "-d",
                "--directory",
            ],
            subcommands: &[],
        }),
        "cd" => Some(CommandInfo {
            flags: &["-", "-P", "-L"],
            subcommands: &[],
        }),
        "grep" => Some(CommandInfo {
            flags: &[
                "-i",
                "--ignore-case",
                "-v",
                "--invert-match",
                "-r",
                "-R",
                "--recursive",
                "-n",
                "--line-number",
                "-l",
                "--files-with-matches",
                "-c",
                "--count",
                "-E",
                "--extended-regexp",
                "-F",
                "--fixed-strings",
                "-w",
                "--word-regexp",
                "-A",
                "-B",
                "-C",
                "--context",
                "--color",
            ],
            subcommands: &[],
        }),
        "cat" => Some(CommandInfo {
            flags: &["-n", "--number", "-b", "--number-nonblank", "-s", "--squeeze-blank", "-A"],
            subcommands: &[],
        }),
        "rm" => Some(CommandInfo {
            flags: &["-r", "-R", "--recursive", "-f", "--force", "-i", "-d", "--dir", "-v"],
            subcommands: &[],
        }),
        "cp" => Some(CommandInfo {
            flags: &[
                "-r",
                "-R",
                "--recursive",
                "-f",
                "--force",
                "-i",
                "--interactive",
                "-v",
                "--verbose",
                "-a",
                "--archive",
                "-n",
                "--no-clobber",
            ],
            subcommands: &[],
        }),
        "mv" => Some(CommandInfo {
            flags: &["-f", "--force", "-i", "--interactive", "-v", "--verbose", "-n", "--no-clobber"],
            subcommands: &[],
        }),
        "mkdir" => Some(CommandInfo {
            flags: &["-p", "--parents", "-v", "--verbose", "-m", "--mode"],
            subcommands: &[],
        }),
        "chmod" => Some(CommandInfo {
            flags: &["-R", "--recursive", "-v", "--verbose", "-c", "--changes"],
            subcommands: &[],
        }),
        "chown" => Some(CommandInfo {
            flags: &["-R", "--recursive", "-v", "--verbose", "-c", "--changes", "-h"],
            subcommands: &[],
        }),
        "docker" => Some(CommandInfo {
            flags: &["--help", "--version", "-v", "-D", "--debug"],
            subcommands: &[
                "build",
                "compose",
                "container",
                "exec",
                "image",
                "images",
                "logs",
                "network",
                "ps",
                "pull",
                "push",
                "rm",
                "run",
                "start",
                "stop",
                "volume",
            ],
        }),
        "npm" => Some(CommandInfo {
            flags: &["--help", "--version", "-v", "-g", "--global"],
            subcommands: &[
                "init",
                "install",
                "uninstall",
                "update",
                "run",
                "test",
                "start",
                "build",
                "publish",
                "link",
                "pack",
                "audit",
                "outdated",
                "ls",
                "ci",
            ],
        }),
        "python" | "python3" => Some(CommandInfo {
            flags: &[
                "-c",
                "-m",
                "-u",
                "-v",
                "-V",
                "--version",
                "-h",
                "--help",
                "-i",
                "-q",
                "-O",
                "-B",
            ],
            subcommands: &[],
        }),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_git_info() {
        let info = get_command_info("git").unwrap();
        assert!(info.flags.contains(&"--help"));
        assert!(info.subcommands.contains(&"commit"));
    }

    #[test]
    fn test_unknown_command() {
        assert!(get_command_info("unknown_command_xyz").is_none());
    }
}
