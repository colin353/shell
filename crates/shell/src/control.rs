//! CLI support for emitting authenticated terminal-control requests.

use protocol::terminal_control::{self, TerminalControl, CONTROL_TOKEN_ENV};
use std::ffi::OsString;
use std::fs::OpenOptions;
use std::io::{self, Write};
use std::path::PathBuf;

pub fn run(args: impl IntoIterator<Item = OsString>) -> io::Result<()> {
    let args: Vec<OsString> = args.into_iter().collect();
    let operation = args
        .first()
        .and_then(|arg| arg.to_str())
        .ok_or_else(usage)?;
    let control = match operation {
        "set-cwd" => {
            if args.len() > 2 {
                return Err(usage());
            }
            let path = args
                .get(1)
                .map(PathBuf::from)
                .unwrap_or(std::env::current_dir()?);
            let path = if path.is_absolute() {
                path
            } else {
                std::env::current_dir()?.join(path)
            };
            let path = path.canonicalize().map_err(|err| {
                io::Error::new(
                    err.kind(),
                    format!("cannot set cwd to {}: {err}", path.display()),
                )
            })?;
            if !path.is_dir() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("not a directory: {}", path.display()),
                ));
            }
            TerminalControl::SetCwd(path)
        }
        "rename-window" => {
            if args.len() < 2 {
                return Err(usage());
            }
            let mut words = Vec::with_capacity(args.len() - 1);
            for word in &args[1..] {
                let word = word.to_str().ok_or_else(|| {
                    io::Error::new(io::ErrorKind::InvalidInput, "window title must be UTF-8")
                })?;
                words.push(word);
            }
            let title = words.join(" ");
            if title.is_empty() || title.chars().any(char::is_control) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "window title must be non-empty and contain no control characters",
                ));
            }
            TerminalControl::RenameWindow(title)
        }
        _ => return Err(usage()),
    };

    let token = std::env::var(CONTROL_TOKEN_ENV).map_err(|_| {
        io::Error::new(
            io::ErrorKind::NotConnected,
            "shell control is unavailable outside a shell-managed foreground process",
        )
    })?;
    if token.is_empty() || !token.bytes().all(|byte| byte.is_ascii_hexdigit()) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "invalid shell control capability",
        ));
    }

    let bytes = terminal_control::encode(&token, &control);
    let mut tty = OpenOptions::new().write(true).open("/dev/tty")?;
    tty.write_all(&bytes)?;
    tty.flush()
}

fn usage() -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidInput,
        "usage: shellctl set-cwd [PATH]\n       shellctl rename-window NAME",
    )
}
