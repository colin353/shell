//! A remote shell session backing a pane, reached over a child-process stdio
//! transport that bridges to a *persistent* remote daemon.
//!
//! `RemoteProcess` spawns `ssh <target> shell bridge --session <id>` (or, for
//! `local`/`self`, this binary's `bridge` directly). The bridge ensures a
//! detached `shell daemon --socket <S> --bare` is running on the far side and
//! pumps the protocol to it. Because the daemon outlives the ssh link, a dropped
//! connection is recoverable: on transport EOF the `RemoteProcess` re-dials the
//! same session (with backoff) and the daemon repaints via `GridResync`. Only a
//! deliberate `SessionEnded` (the remote shell exited) tears the pane down.

use std::io::{self, Read};
use std::os::fd::{AsRawFd, RawFd};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};

use protocol::codec::{self, FrameReader};
use protocol::{ClientMode, ClientMsg, Hello, HistoryRecord, ServerMsg, PROTOCOL_VERSION};

const INITIAL_BACKOFF: Duration = Duration::from_millis(250);
const MAX_BACKOFF: Duration = Duration::from_secs(5);

pub struct RemoteProcess {
    child: Child,
    stdin: ChildStdin,
    stdout: ChildStdout,
    stdout_fd: RawFd,
    frames: FrameReader,
    /// Decoded ANSI awaiting the pane's emulator.
    pending: Vec<u8>,
    /// A window-rename pushed by the remote, awaiting the local compositor.
    pending_title: Option<String>,
    /// Commands the remote executed, awaiting dual-write into local history.
    pending_history: Vec<HistoryRecord>,
    /// Latest authoritative cwd reported by the remote daemon.
    cwd: Option<PathBuf>,
    /// Whether the remote focused pane expects printable input to echo.
    input_echo: bool,

    // --- Reconnect parameters (the session is identified by `session_id`). ---
    target: String,
    session_id: String,
    cols: u16,
    rows: u16,
    env: Vec<(String, String)>,
    initial_cwd: Option<PathBuf>,

    // --- State ---
    /// The session ended deliberately (SessionEnded) — stop reconnecting.
    ended: bool,
    backoff: Duration,
    /// Earliest time we may attempt the next reconnect.
    next_attempt: Option<Instant>,
}

impl RemoteProcess {
    /// Connect to `target`, starting (or reattaching to) a persistent session.
    /// A named `session` is reattach-or-create; `None` makes a fresh per-pane
    /// session.
    pub fn connect(
        target: &str,
        session: Option<&str>,
        cols: u16,
        rows: u16,
        env: &[(String, String)],
        initial_cwd: Option<&Path>,
    ) -> io::Result<Self> {
        let session_id = session
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string())
            .unwrap_or_else(new_session_id);
        let initial_cwd = initial_cwd.map(PathBuf::from);
        let (child, stdin, stdout, stdout_fd) =
            spawn_transport(target, &session_id, cols, rows, env, initial_cwd.as_deref())?;
        Ok(RemoteProcess {
            child,
            stdin,
            stdout,
            stdout_fd,
            frames: FrameReader::new(),
            pending: Vec::new(),
            pending_title: None,
            pending_history: Vec::new(),
            cwd: None,
            input_echo: false,
            target: target.to_string(),
            session_id,
            cols,
            rows,
            env: env.to_vec(),
            initial_cwd,
            ended: false,
            backoff: INITIAL_BACKOFF,
            next_attempt: None,
        })
    }

    /// fd the compositor polls for readability (changes across reconnects).
    pub fn as_raw_fd(&self) -> RawFd {
        self.stdout_fd
    }

    /// PID of the current transport (bridge/ssh) process. Exposed mainly so a
    /// test can kill it to simulate a dropped link.
    pub fn transport_pid(&self) -> u32 {
        self.child.id()
    }

    /// Read decoded ANSI for the pane's emulator. Mirrors `PtyProcess::read`:
    /// `Ok(Some(0))` = the session ended, `Ok(None)` = nothing available right
    /// now (including while reconnecting).
    pub fn read(&mut self, buf: &mut [u8]) -> io::Result<Option<usize>> {
        if !self.pending.is_empty() {
            return Ok(Some(self.drain_pending(buf)));
        }
        if self.ended {
            return Ok(Some(0));
        }

        let mut scratch = [0u8; 8192];
        match self.stdout.read(&mut scratch) {
            Ok(0) => {
                // Transport closed without a SessionEnded: a dropped link.
                self.try_reconnect();
                Ok(None)
            }
            Ok(n) => {
                // Healthy data flow resets the backoff.
                self.backoff = INITIAL_BACKOFF;
                self.next_attempt = None;
                self.frames.push(&scratch[..n]);
                while let Some(msg) = self.frames.next_frame::<ServerMsg>()? {
                    match msg {
                        ServerMsg::Render { bytes } => self.pending.extend_from_slice(&bytes),
                        ServerMsg::GridResync { grid, .. } => self
                            .pending
                            .extend_from_slice(&emulator::render_snapshot_to_ansi(&grid)),
                        ServerMsg::Context { ctx, .. } => self.cwd = Some(ctx.cwd),
                        ServerMsg::InputMode { echo, .. } => self.input_echo = echo,
                        ServerMsg::RenameWindow { name } => self.pending_title = Some(name),
                        ServerMsg::HistoryRecorded { entry } => self.pending_history.push(entry),
                        ServerMsg::SessionEnded => self.ended = true,
                        _ => {}
                    }
                }
                if self.pending.is_empty() && self.ended {
                    Ok(Some(0))
                } else if self.pending.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(self.drain_pending(buf)))
                }
            }
            Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => Ok(None),
            Err(_) => {
                self.try_reconnect();
                Ok(None)
            }
        }
    }

    /// Re-dial the same session after a dropped link, subject to backoff. A
    /// successful re-dial just swaps in the new transport; the daemon (which
    /// persisted) repaints via GridResync once the link is up.
    fn try_reconnect(&mut self) {
        if self.ended {
            return;
        }
        let now = Instant::now();
        if let Some(t) = self.next_attempt {
            if now < t {
                return;
            }
        }
        self.next_attempt = Some(now + self.backoff);
        self.backoff = (self.backoff * 2).min(MAX_BACKOFF);

        let _ = self.child.kill();
        let _ = self.child.wait();

        if let Ok((child, stdin, stdout, fd)) = spawn_transport(
            &self.target,
            &self.session_id,
            self.cols,
            self.rows,
            &self.env,
            self.initial_cwd.as_deref(),
        ) {
            self.child = child;
            self.stdin = stdin;
            self.stdout = stdout;
            self.stdout_fd = fd;
            self.frames = FrameReader::new();
            self.pending.clear();
        }
        // On failure we keep the (dead) handles; the next poll's EOF re-enters
        // here after the backoff.
    }

    fn drain_pending(&mut self, buf: &mut [u8]) -> usize {
        let n = self.pending.len().min(buf.len());
        buf[..n].copy_from_slice(&self.pending[..n]);
        self.pending.drain(..n);
        n
    }

    /// Forward keystrokes to the remote. Best-effort: a write during a dropped
    /// link is lost (we don't buffer/predict), and the screen resyncs on
    /// reconnect.
    pub fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        let _ = codec::write_frame(
            &mut self.stdin,
            &ClientMsg::Input {
                bytes: data.to_vec(),
            },
        );
        Ok(data.len())
    }

    /// Tell the remote its size changed (remembered for reconnects).
    pub fn resize(&mut self, cols: u16, rows: u16) -> io::Result<()> {
        self.cols = cols;
        self.rows = rows;
        let _ = codec::write_frame(&mut self.stdin, &ClientMsg::Resize { cols, rows });
        Ok(())
    }

    /// Tell the remote daemon to update its authoritative window title.
    pub fn set_title(&mut self, name: &str) -> io::Result<()> {
        let _ = codec::write_frame(
            &mut self.stdin,
            &ClientMsg::SetTitle {
                name: name.to_string(),
            },
        );
        Ok(())
    }

    /// Ask the remote to repaint its authoritative screen.
    pub fn request_resync(&mut self) -> io::Result<()> {
        let _ = codec::write_frame(&mut self.stdin, &ClientMsg::RequestResync);
        Ok(())
    }

    /// Take a pending window-rename pushed by the remote, if any.
    pub fn take_title(&mut self) -> Option<String> {
        self.pending_title.take()
    }

    /// Take the commands the remote has executed since the last drain, for
    /// dual-write into local history.
    pub fn take_history_records(&mut self) -> Vec<HistoryRecord> {
        std::mem::take(&mut self.pending_history)
    }

    /// The host this session is connected to (the history-origin label).
    pub fn target(&self) -> &str {
        &self.target
    }

    /// Latest authoritative cwd reported by the remote daemon.
    pub fn cwd(&self) -> Option<&Path> {
        self.cwd.as_deref()
    }

    pub fn input_echo_mode(&self) -> bool {
        self.input_echo
    }

    /// Exit code if the session ended deliberately. Returns `None` while a
    /// dropped link is being retried, so the pane stays remote and reconnects.
    pub fn try_wait(&mut self) -> Option<i32> {
        if self.ended {
            Some(0)
        } else {
            None
        }
    }
}

impl Drop for RemoteProcess {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// Spawn the transport, send the handshake (non-blocking — we don't wait for
/// Welcome), and return the process handles.
fn spawn_transport(
    target: &str,
    session_id: &str,
    cols: u16,
    rows: u16,
    env: &[(String, String)],
    initial_cwd: Option<&Path>,
) -> io::Result<(Child, ChildStdin, ChildStdout, RawFd)> {
    let mut command =
        build_remote_command(target, &["bridge", "--session", session_id], initial_cwd)?;
    command
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit());
    let mut child = command.spawn()?;
    let mut stdin = child.stdin.take().expect("piped stdin");
    let stdout = child.stdout.take().expect("piped stdout");

    codec::write_frame(
        &mut stdin,
        &ClientMsg::Hello(Hello {
            version: PROTOCOL_VERSION,
            mode: ClientMode::Dumb,
            size: (cols, rows),
        }),
    )?;
    if !env.is_empty() {
        let _ = codec::write_frame(
            &mut stdin,
            &ClientMsg::UpdateLocalEnv { vars: env.to_vec() },
        );
    }
    if let Some(cwd) = initial_cwd {
        let _ = codec::write_frame(
            &mut stdin,
            &ClientMsg::SetCwd {
                pane: protocol::PaneId(0),
                cwd: cwd.to_path_buf(),
            },
        );
    }

    let stdout_fd = stdout.as_raw_fd();
    set_nonblocking(stdout_fd)?;
    Ok((child, stdin, stdout, stdout_fd))
}

/// Build a command that runs the `shell` binary on `target` with `args`.
///
/// `local`/`self` run this binary directly (override with
/// `SHELL_DAEMON_STDIO_CMD`, which tests point at the built `shell`). Anything
/// else is `ssh -T <target> <bootstrap>`, with `SHELL_REMOTE_BIN` overriding the
/// remote binary name/path.
///
/// The bootstrap sources `~/.shell_env` before exec'ing the binary. That file's
/// `export NAME=value` lines are POSIX-sourceable, so this puts the binary's dir
/// on PATH the same way an in-session command sees it — without it, a
/// non-interactive ssh runs with a bare PATH and can't find `shell` if it lives
/// somewhere only an rc file (or `~/.shell_env`) adds.
///
/// Critically, the whole bootstrap is passed as a *single* ssh argument: ssh
/// joins multiple command args with spaces and re-parses the result through the
/// remote login shell, so a multi-arg `sh -c '<script>' …` form would lose its
/// quoting on the far side. We build one string and shell-quote each `arg`.
fn build_remote_command(
    target: &str,
    args: &[&str],
    initial_cwd: Option<&Path>,
) -> io::Result<Command> {
    if target == "local" || target == "self" {
        let exe = match std::env::var_os("SHELL_DAEMON_STDIO_CMD") {
            Some(path) => std::path::PathBuf::from(path),
            None => std::env::current_exe()?,
        };
        let mut c = Command::new(exe);
        c.args(args);
        if let Some(cwd) = initial_cwd {
            c.current_dir(cwd);
        }
        Ok(c)
    } else {
        let remote_bin = std::env::var("SHELL_REMOTE_BIN").unwrap_or_else(|_| "shell".to_string());
        // One string for the remote login shell: source ~/.shell_env (for PATH),
        // then exec the binary with the (quoted) args. `remote_bin` is left
        // unquoted so a `~`/`$VAR` in SHELL_REMOTE_BIN still expands remotely.
        let mut remote_cmd = String::new();
        if let Some(cwd) = initial_cwd {
            remote_cmd.push_str("cd ");
            remote_cmd.push_str(&shell_single_quote(&cwd.to_string_lossy()));
            remote_cmd.push_str(" && ");
        }
        remote_cmd.push_str(r#"[ -f "$HOME/.shell_env" ] && . "$HOME/.shell_env"; exec "#);
        remote_cmd.push_str(&remote_bin);
        for a in args {
            remote_cmd.push(' ');
            remote_cmd.push_str(&shell_single_quote(a));
        }
        let mut c = Command::new("ssh");
        c.arg("-T").arg(target).arg(remote_cmd);
        Ok(c)
    }
}

/// Quote `s` for a POSIX shell by wrapping it in single quotes (closing,
/// escaping each embedded `'`, reopening). Safe for arbitrary content — session
/// names, in particular, are caller-supplied.
fn shell_single_quote(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('\'');
    for ch in s.chars() {
        if ch == '\'' {
            out.push_str(r"'\''");
        } else {
            out.push(ch);
        }
    }
    out.push('\'');
    out
}

/// List persistent sessions on `target` as `(name, age)`. Blocks on the ssh
/// round-trip.
pub fn list_sessions(target: &str) -> io::Result<Vec<(String, String)>> {
    let mut c = build_remote_command(target, &["sessions", "list"], None)?;
    c.stdin(Stdio::null()).stderr(Stdio::null());
    let out = c.output()?;
    let text = String::from_utf8_lossy(&out.stdout);
    let mut sessions = Vec::new();
    for line in text.lines() {
        let line = line.trim_end();
        if line.is_empty() {
            continue;
        }
        let (name, age) = line.split_once('\t').unwrap_or((line, ""));
        sessions.push((name.to_string(), age.to_string()));
    }
    Ok(sessions)
}

/// Kill the named session on `target`. Blocks on the ssh round-trip.
pub fn kill_session(target: &str, name: &str) -> io::Result<()> {
    let mut c = build_remote_command(target, &["sessions", "kill", name], None)?;
    c.stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    c.status()?;
    Ok(())
}

/// A process-unique session id (host pid + time + counter). Not a secret; the
/// session socket is owner-only on the remote.
fn new_session_id() -> String {
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    format!("{:x}-{:x}-{:x}", std::process::id(), nanos, n)
}

fn set_nonblocking(fd: RawFd) -> io::Result<()> {
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFL);
        if flags < 0 {
            return Err(io::Error::last_os_error());
        }
        if libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) < 0 {
            return Err(io::Error::last_os_error());
        }
    }
    Ok(())
}
