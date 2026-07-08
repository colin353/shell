//! Entry point. Dispatches to one of three modes:
//!
//! - `shell`            — standalone single-process shell (default, unchanged)
//! - `shell daemon`     — headless daemon serving a Unix socket
//! - `shell attach`     — dumb client attaching to a daemon
//! - `shell run`        — start a named background session running a command
//!
//! Both daemon and attach accept `--socket <path>` (default: a per-user socket
//! under `$XDG_RUNTIME_DIR`).

use shell::{client, common, server, standalone};
use std::path::PathBuf;
use std::process::ExitCode;

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mode = args.first().map(String::as_str);

    match mode {
        None | Some("standalone") => {
            standalone::run();
            ExitCode::SUCCESS
        }
        Some("daemon") => {
            let rest = &args[1..];
            let bare = rest.iter().any(|a| a == "--bare");
            let result = if rest.iter().any(|a| a == "--stdio") {
                server::run_stdio(bare)
            } else {
                server::run(&socket_path(rest), bare, flag_value(rest, "--exec").as_deref())
            };
            match result {
                Ok(()) => ExitCode::SUCCESS,
                Err(e) => {
                    eprintln!("shell daemon: {e}");
                    ExitCode::FAILURE
                }
            }
        }
        // Start a detached, named session running a command, reattachable later
        // via the `reconnect` picker: `shell run --name <name> -- <cmd...>`.
        Some("run") => match run_command(&args[1..]) {
            Ok(()) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("shell run: {e}");
                ExitCode::FAILURE
            }
        },
        Some("attach") => match client::run(&socket_path(&args[1..])) {
            Ok(()) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("shell attach: {e}");
                ExitCode::FAILURE
            }
        },
        // Stdio<->socket bridge to a persistent (bare) daemon. Run remotely as
        // `ssh <host> shell bridge --socket <S>`; ensures the daemon exists,
        // then pumps. When the ssh link dies the bridge dies but the daemon
        // (detached) lives, so the session survives a disconnect.
        Some("bridge") => match server::run_bridge(&socket_path(&args[1..])) {
            Ok(()) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("shell bridge: {e}");
                ExitCode::FAILURE
            }
        },
        // List/kill persistent session daemons (run remotely via
        // `ssh <host> shell sessions ...`).
        Some("sessions") => match server::run_sessions(&args[1..]) {
            Ok(()) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("shell sessions: {e}");
                ExitCode::FAILURE
            }
        },
        Some("-h") | Some("--help") | Some("help") => {
            print_usage();
            ExitCode::SUCCESS
        }
        Some(other) => {
            eprintln!("shell: unknown mode `{other}`\n");
            print_usage();
            ExitCode::FAILURE
        }
    }
}

/// Resolve the socket path from `--socket <path>` or `--session <id>` (resolved
/// to a per-session path in the runtime dir), else the per-user default.
fn socket_path(args: &[String]) -> PathBuf {
    let mut it = args.iter();
    while let Some(arg) = it.next() {
        match arg.as_str() {
            "--socket" => {
                if let Some(path) = it.next() {
                    return PathBuf::from(path);
                }
            }
            "--session" => {
                if let Some(id) = it.next() {
                    return common::session_socket_path(id);
                }
            }
            _ => {}
        }
    }
    common::default_socket_path()
}

/// Return the value following `flag` in `args`, if present.
fn flag_value(args: &[String], flag: &str) -> Option<String> {
    let mut it = args.iter();
    while let Some(arg) = it.next() {
        if arg == flag {
            return it.next().cloned();
        }
    }
    None
}

/// Parse `run --name <name> -- <cmd...>` and start the named session. The
/// command is everything after `--`, joined into a single line that the
/// daemon's pane re-tokenizes through the normal shell input path.
fn run_command(args: &[String]) -> std::io::Result<()> {
    let name = flag_value(args, "--name").ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "missing required --name <name>\nusage: shell run --name <name> -- <command...>",
        )
    })?;
    let command = match args.iter().position(|a| a == "--") {
        Some(i) => args[i + 1..].join(" "),
        None => String::new(),
    };
    if command.trim().is_empty() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "missing command after `--`\nusage: shell run --name <name> -- <command...>",
        ));
    }
    server::run_command_session(&name, &command)
}

fn print_usage() {
    eprintln!(
        "usage:\n  \
         shell                          run the standalone shell\n  \
         shell daemon [--socket P]      run a headless daemon\n  \
         shell attach [--socket P]      attach to a daemon\n  \
         shell run --name <n> -- <cmd>  start a named background session running <cmd>\n  \
         shell bridge --session <id>    bridge stdio to a persistent session\n  \
         shell sessions [list|kill <n>] manage persistent sessions\n\
         \nIn the shell: `connect <host> [session]` to open/reattach a remote pane."
    );
}
