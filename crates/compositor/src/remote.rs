//! A remote shell session backing a pane, reached over a child-process stdio
//! transport.
//!
//! `RemoteProcess` spawns a transport command whose stdin/stdout speak the
//! protocol: for a real host that's `ssh <target> shell daemon --stdio`; the
//! special target `local`/`self` runs this binary in stdio-daemon mode (used by
//! tests and local experimentation). It presents a PTY-like surface to the pane
//! — `read()` yields decoded ANSI for the emulator, `write()` frames keystrokes
//! as input — so the existing poll/render machinery drives it unchanged.
//!
//! No cross-disconnect persistence yet: if the transport dies, the session ends
//! and the pane returns to its local shell. A persistent remote session (a
//! bridge to a detached remote daemon) is a later step.

use std::io::{self, Read};
use std::os::fd::{AsRawFd, RawFd};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};

use protocol::codec::{self, FrameReader};
use protocol::{ClientMode, ClientMsg, Hello, ServerMsg, PROTOCOL_VERSION};

pub struct RemoteProcess {
    child: Child,
    stdin: ChildStdin,
    stdout: ChildStdout,
    stdout_fd: RawFd,
    frames: FrameReader,
    /// Decoded ANSI awaiting the pane's emulator.
    pending: Vec<u8>,
    eof: bool,
    exit_code: Option<i32>,
    #[allow(dead_code)]
    target: String,
}

impl RemoteProcess {
    /// Connect to `target`, handshake, and forward `env` for the remote merge.
    pub fn connect(
        target: &str,
        cols: u16,
        rows: u16,
        env: &[(String, String)],
    ) -> io::Result<Self> {
        let mut command = build_command(target)?;
        command
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());
        let mut child = command.spawn()?;
        let mut stdin = child.stdin.take().expect("piped stdin");
        let mut stdout = child.stdout.take().expect("piped stdout");

        // Handshake: Hello -> Welcome.
        codec::write_frame(
            &mut stdin,
            &ClientMsg::Hello(Hello {
                version: PROTOCOL_VERSION,
                mode: ClientMode::Dumb,
                size: (cols, rows),
            }),
        )?;
        match codec::read_frame::<_, ServerMsg>(&mut stdout)? {
            Some(ServerMsg::Welcome(w)) if w.version == PROTOCOL_VERSION => {}
            Some(ServerMsg::Welcome(w)) => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("remote protocol {} != local {}", w.version, PROTOCOL_VERSION),
                ));
            }
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "expected Welcome from remote",
                ))
            }
        }

        // Forward local env (applied remote-wins as defaults on the other side).
        if !env.is_empty() {
            let _ = codec::write_frame(
                &mut stdin,
                &ClientMsg::UpdateLocalEnv { vars: env.to_vec() },
            );
        }

        // From here the pane polls stdout; make reads non-blocking.
        let stdout_fd = stdout.as_raw_fd();
        set_nonblocking(stdout_fd)?;

        Ok(RemoteProcess {
            child,
            stdin,
            stdout,
            stdout_fd,
            frames: FrameReader::new(),
            pending: Vec::new(),
            eof: false,
            exit_code: None,
            target: target.to_string(),
        })
    }

    /// fd the compositor polls for readability.
    pub fn as_raw_fd(&self) -> RawFd {
        self.stdout_fd
    }

    /// Read decoded ANSI for the pane's emulator. Mirrors `PtyProcess::read`:
    /// `Ok(Some(0))` = EOF, `Ok(None)` = nothing available right now.
    pub fn read(&mut self, buf: &mut [u8]) -> io::Result<Option<usize>> {
        if !self.pending.is_empty() {
            return Ok(Some(self.drain_pending(buf)));
        }
        if self.eof {
            return Ok(Some(0));
        }

        let mut scratch = [0u8; 8192];
        match self.stdout.read(&mut scratch) {
            Ok(0) => {
                self.eof = true;
                Ok(Some(0))
            }
            Ok(n) => {
                self.frames.push(&scratch[..n]);
                while let Some(msg) = self.frames.next_frame::<ServerMsg>()? {
                    match msg {
                        ServerMsg::Render { bytes } => self.pending.extend_from_slice(&bytes),
                        ServerMsg::GridResync { grid, .. } => self
                            .pending
                            .extend_from_slice(&emulator::render_snapshot_to_ansi(&grid)),
                        _ => {}
                    }
                }
                if self.pending.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(self.drain_pending(buf)))
                }
            }
            Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn drain_pending(&mut self, buf: &mut [u8]) -> usize {
        let n = self.pending.len().min(buf.len());
        buf[..n].copy_from_slice(&self.pending[..n]);
        self.pending.drain(..n);
        n
    }

    /// Forward keystrokes to the remote as a framed input message.
    pub fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        codec::write_frame(
            &mut self.stdin,
            &ClientMsg::Input {
                bytes: data.to_vec(),
            },
        )?;
        Ok(data.len())
    }

    /// Tell the remote its size changed.
    pub fn resize(&mut self, cols: u16, rows: u16) -> io::Result<()> {
        codec::write_frame(&mut self.stdin, &ClientMsg::Resize { cols, rows })
    }

    /// Exit code if the transport (and thus the remote session) has ended.
    pub fn try_wait(&mut self) -> Option<i32> {
        if let Some(code) = self.exit_code {
            return Some(code);
        }
        match self.child.try_wait() {
            Ok(Some(status)) => {
                let code = status.code().unwrap_or(128);
                self.exit_code = Some(code);
                Some(code)
            }
            _ => None,
        }
    }
}

impl Drop for RemoteProcess {
    fn drop(&mut self) {
        // Closing stdin signals the transport to exit; also kill it outright so
        // an `ssh` process can't linger.
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// Build the transport command for a target.
///
/// `local`/`self` run an in-process stdio daemon: normally this binary, but
/// `SHELL_DAEMON_STDIO_CMD` can point at a specific `shell` binary (tests set it,
/// since under `cargo test` `current_exe()` is the test harness, not `shell`).
fn build_command(target: &str) -> io::Result<Command> {
    if target == "local" || target == "self" {
        let exe = match std::env::var_os("SHELL_DAEMON_STDIO_CMD") {
            Some(path) => std::path::PathBuf::from(path),
            None => std::env::current_exe()?,
        };
        let mut c = Command::new(exe);
        c.args(["daemon", "--stdio"]);
        Ok(c)
    } else {
        // `shell` is expected on the remote PATH. ssh wires our stdio to it.
        let mut c = Command::new("ssh");
        c.args([target, "shell", "daemon", "--stdio"]);
        Ok(c)
    }
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
