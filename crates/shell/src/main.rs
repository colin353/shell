//! Entry point. Dispatches to one of three modes:
//!
//! - `shell`            — standalone single-process shell (default, unchanged)
//! - `shell daemon`     — headless daemon serving a Unix socket
//! - `shell attach`     — dumb client attaching to a daemon
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
            let result = if rest.iter().any(|a| a == "--stdio") {
                server::run_stdio()
            } else {
                server::run(&socket_path(rest))
            };
            match result {
                Ok(()) => ExitCode::SUCCESS,
                Err(e) => {
                    eprintln!("shell daemon: {e}");
                    ExitCode::FAILURE
                }
            }
        }
        Some("attach") => match client::run(&socket_path(&args[1..])) {
            Ok(()) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("shell attach: {e}");
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

/// Resolve the socket path from `--socket <path>`, else the per-user default.
fn socket_path(args: &[String]) -> PathBuf {
    let mut it = args.iter();
    while let Some(arg) = it.next() {
        if arg == "--socket" {
            if let Some(path) = it.next() {
                return PathBuf::from(path);
            }
        }
    }
    common::default_socket_path()
}

fn print_usage() {
    eprintln!(
        "usage:\n  \
         shell                       run the standalone shell\n  \
         shell daemon [--socket P]   run a headless daemon\n  \
         shell attach [--socket P]   attach to a daemon"
    );
}
