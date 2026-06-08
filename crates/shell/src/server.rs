//! Daemon mode (`shell daemon`).
//!
//! Runs a headless [`compositor::Compositor`] that outlives client connections.
//! A client attaches over a Unix socket, ships keystrokes up, and renders the
//! `ServerMsg::Render` frames the daemon sends back (the "dumb client" model:
//! the daemon's existing delta-render output is forwarded verbatim).
//!
//! Threading:
//! - an **accept thread** blocks on `accept()`, performs the handshake, spawns a
//!   per-connection **reader thread**, and hands a ready connection to the main
//!   loop;
//! - per-connection **reader threads** do blocking framed reads and forward
//!   `ClientMsg`s over a channel;
//! - the **main loop** owns the compositor, drains the current client's channel,
//!   and *always* calls `poll_once` — so PTYs keep flowing even while detached,
//!   which is what lets a long-running process survive a disconnect.

use std::fs::File;
use std::io::{self, Write};
use std::os::fd::{FromRawFd, RawFd};
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::Path;
use std::sync::mpsc::{self, Receiver, TryRecvError};
use std::sync::{Arc, Mutex};

use protocol::codec::{self};
use protocol::{
    ClientMsg, Hello, HostId, PaneId, ServerMsg, SessionId, Welcome, PROTOCOL_VERSION,
};

/// A `Write` sink that frames everything written into `ServerMsg::Render` frames
/// and forwards them to the currently-attached client (a Unix socket for
/// `daemon`, stdout for `daemon --stdio`). With no client attached, output is
/// dropped — a reattaching client is fully repainted via `GridResync`, so
/// nothing is lost.
struct ClientSink {
    writer: Option<Box<dyn Write + Send>>,
    buf: Vec<u8>,
}

impl ClientSink {
    fn new() -> Self {
        ClientSink {
            writer: None,
            buf: Vec::new(),
        }
    }

    fn attach(&mut self, writer: Box<dyn Write + Send>) {
        self.writer = Some(writer);
        self.buf.clear();
    }

    fn detach(&mut self) {
        self.writer = None;
        self.buf.clear();
    }
}

impl Write for ClientSink {
    fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        // The compositor writes a render in several pieces, then flushes; buffer
        // until flush so each render becomes a single frame.
        self.buf.extend_from_slice(data);
        Ok(data.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        if self.buf.is_empty() {
            return Ok(());
        }
        let bytes = std::mem::take(&mut self.buf);
        if let Some(writer) = self.writer.as_mut() {
            let frame = codec::to_frame(&ServerMsg::Render { bytes });
            // A write error means the client vanished mid-render; drop it and
            // wait for the reader thread to report the disconnect.
            if writer.write_all(&frame).and_then(|_| writer.flush()).is_err() {
                self.writer = None;
            }
        }
        Ok(())
    }
}

/// A fully handshaked connection handed from the accept thread to the main loop.
struct IncomingClient {
    /// Client messages, forwarded by the per-connection reader thread.
    input: Receiver<ClientMsg>,
    /// Write half for the render sink.
    sink_stream: UnixStream,
    size: (u16, u16),
}

pub fn run(socket_path: &Path) -> io::Result<()> {
    // Replace any stale socket from a previous (dead) daemon.
    let _ = std::fs::remove_file(socket_path);
    let listener = UnixListener::bind(socket_path)?;
    restrict_permissions(socket_path);

    let host = HostId(hostname());

    let sink = Arc::new(Mutex::new(ClientSink::new()));
    let mut compositor = compositor::Compositor::with_output(80, 24, sink.clone()).map_err(|e| {
        io::Error::new(
            io::ErrorKind::Other,
            format!("failed to create compositor: {e:?}"),
        )
    })?;
    compositor.set_synchronized_output(true);
    compositor.render();

    // Accept connections on a background thread so the main loop never blocks on
    // a slow or stalled client handshake.
    let (conn_tx, conn_rx) = mpsc::channel::<IncomingClient>();
    {
        let host = host.clone();
        std::thread::spawn(move || accept_loop(listener, conn_tx, host));
    }

    let mut current: Option<Receiver<ClientMsg>> = None;

    let exit = loop {
        // Attach a newly-arrived client (taking over from any existing one).
        if let Ok(incoming) = conn_rx.try_recv() {
            // Drop any previous client first, so the refresh render below is
            // discarded rather than sent as a stray partial frame.
            sink.lock().unwrap().detach();

            let (cols, rows) = incoming.size;
            compositor.resize((cols.max(1)) as usize, (rows.max(1)) as usize);
            // Refresh the composite at the new size (output dropped: no client
            // attached yet), then snapshot the authoritative screen.
            compositor.render();
            let snapshot = compositor.grid_snapshot();

            // Paint the (re)attaching client from server state via GridResync,
            // then attach the sink so subsequent live deltas stream as Render.
            let mut stream = incoming.sink_stream;
            let resync = ServerMsg::GridResync {
                pane: PaneId(0),
                grid: snapshot,
            };
            if codec::write_frame(&mut stream, &resync).is_ok() {
                if let Ok(clone) = stream.try_clone() {
                    sink.lock().unwrap().attach(Box::new(clone));
                    current = Some(incoming.input);
                }
            }
        }

        // Drain the attached client's input.
        let mut should_exit = false;
        if let Some(rx) = &current {
            loop {
                match rx.try_recv() {
                    Ok(ClientMsg::Input { bytes }) => {
                        if compositor.handle_input(&bytes) {
                            should_exit = true;
                            break;
                        }
                    }
                    Ok(ClientMsg::Resize { cols, rows }) => {
                        compositor.resize((cols.max(1)) as usize, (rows.max(1)) as usize);
                        compositor.force_full_redraw();
                    }
                    Ok(ClientMsg::Detach) => {
                        sink.lock().unwrap().detach();
                        current = None;
                        break;
                    }
                    Ok(_) => {} // Hello / smart-mode messages: ignored by the dumb daemon
                    Err(TryRecvError::Empty) => break,
                    Err(TryRecvError::Disconnected) => {
                        // Reader thread ended: the client is gone.
                        sink.lock().unwrap().detach();
                        current = None;
                        break;
                    }
                }
            }
        }
        if should_exit {
            break true;
        }

        // Always drive PTYs/shell output, attached or not.
        if let Err(e) = compositor.poll_once(10) {
            eprintln!("shell-daemon: compositor error: {e:?}");
            break false;
        }
    };

    let _ = std::fs::remove_file(socket_path);
    let _ = exit;
    Ok(())
}

/// Serve a single client over stdin/stdout — the transport used when a remote
/// pane runs `ssh <host> shell daemon --stdio`. There is exactly one connection;
/// when stdin closes (the ssh pipe died), this process exits.
///
/// Unlike the socket daemon this does not persist across disconnects — a
/// persistent remote session (bridge + detached daemon) is a later step.
pub fn run_stdio(bare: bool) -> io::Result<()> {
    // Own dup'd copies of stdin/stdout so we get unbuffered, independently
    // closeable handles (and never touch the global buffered Stdin/Stdout).
    let mut handshake_in = dup_file(0)?;
    let mut handshake_out = dup_file(1)?;

    let hello = match codec::read_frame::<_, ClientMsg>(&mut handshake_in)? {
        Some(ClientMsg::Hello(h)) => h,
        _ => return Ok(()), // no/!Hello: nothing to serve
    };
    let (cols, rows) = hello.size;

    codec::write_frame(
        &mut handshake_out,
        &ServerMsg::Welcome(Welcome {
            version: PROTOCOL_VERSION,
            session: SessionId(1),
            host: HostId(hostname()),
        }),
    )?;

    let sink = Arc::new(Mutex::new(ClientSink::new()));
    let mut compositor = compositor::Compositor::with_output(
        (cols.max(1)) as usize,
        (rows.max(1)) as usize,
        sink.clone(),
    )
    .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("compositor: {e:?}")))?;
    compositor.set_synchronized_output(true);
    if bare {
        // No chrome: this daemon is embedded inside a remote pane.
        compositor.set_status_bar_visible(false);
    }

    // Refresh + snapshot while the sink is detached (output dropped), then paint
    // the client with a GridResync; afterwards live deltas stream as Render.
    compositor.render();
    let snapshot = compositor.grid_snapshot();
    codec::write_frame(
        &mut handshake_out,
        &ServerMsg::GridResync {
            pane: PaneId(0),
            grid: snapshot,
        },
    )?;
    sink.lock().unwrap().attach(Box::new(dup_file(1)?));

    // Reader thread: stdin -> ClientMsg.
    let (tx, rx) = mpsc::channel::<ClientMsg>();
    let mut reader = dup_file(0)?;
    std::thread::spawn(move || loop {
        match codec::read_frame::<_, ClientMsg>(&mut reader) {
            Ok(Some(m)) => {
                if tx.send(m).is_err() {
                    break;
                }
            }
            Ok(None) | Err(_) => break,
        }
    });

    loop {
        let mut exit = false;
        loop {
            match rx.try_recv() {
                Ok(ClientMsg::Input { bytes }) => {
                    if compositor.handle_input(&bytes) {
                        exit = true;
                        break;
                    }
                }
                Ok(ClientMsg::Resize { cols, rows }) => {
                    compositor.resize((cols.max(1)) as usize, (rows.max(1)) as usize);
                    compositor.force_full_redraw();
                }
                Ok(ClientMsg::UpdateLocalEnv { vars }) => apply_env_defaults(&vars),
                Ok(_) => {}
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    exit = true; // stdin closed: transport died
                    break;
                }
            }
        }
        if exit {
            break;
        }
        compositor
            .poll_once(10)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("compositor: {e:?}")))?;
    }
    Ok(())
}

/// Apply forwarded local env vars as *defaults*: a var is set only if the
/// (remote) process doesn't already have it, so the remote layer wins — the
/// merge semantics we want for a remote pane.
fn apply_env_defaults(vars: &[(String, String)]) {
    for (key, value) in vars {
        if std::env::var_os(key).is_none() {
            std::env::set_var(key, value);
        }
    }
}

/// Duplicate a raw fd into an owned, unbuffered `File`.
fn dup_file(fd: RawFd) -> io::Result<File> {
    let dup = unsafe { libc::dup(fd) };
    if dup < 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(unsafe { File::from_raw_fd(dup) })
}

/// Accept connections, handshake each, and forward ready ones to the main loop.
fn accept_loop(listener: UnixListener, conn_tx: mpsc::Sender<IncomingClient>, host: HostId) {
    for stream in listener.incoming() {
        let stream = match stream {
            Ok(s) => s,
            Err(_) => continue,
        };
        match handshake(stream, &host) {
            Ok(incoming) => {
                if conn_tx.send(incoming).is_err() {
                    break; // main loop is gone
                }
            }
            Err(_) => continue, // bad handshake; drop the connection
        }
    }
}

/// Read `Hello`, reply `Welcome`, and spawn the reader thread.
fn handshake(stream: UnixStream, host: &HostId) -> io::Result<IncomingClient> {
    let mut hs = stream.try_clone()?;
    let size = match codec::read_frame::<_, ClientMsg>(&mut hs)? {
        Some(ClientMsg::Hello(Hello { version, size, .. })) => {
            let welcome = ServerMsg::Welcome(Welcome {
                version: PROTOCOL_VERSION,
                session: SessionId(1),
                host: host.clone(),
            });
            codec::write_frame(&mut hs, &welcome)?;
            if version != PROTOCOL_VERSION {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "protocol version mismatch",
                ));
            }
            size
        }
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "expected Hello",
            ))
        }
    };

    let (tx, rx) = mpsc::channel::<ClientMsg>();
    let mut reader = stream.try_clone()?;
    std::thread::spawn(move || loop {
        match codec::read_frame::<_, ClientMsg>(&mut reader) {
            Ok(Some(msg)) => {
                if tx.send(msg).is_err() {
                    break;
                }
            }
            Ok(None) | Err(_) => break, // EOF or error: connection done
        }
    });

    Ok(IncomingClient {
        input: rx,
        sink_stream: stream,
        size,
    })
}

/// Best-effort restrict the socket to the owning user (0600).
fn restrict_permissions(path: &Path) {
    use std::os::unix::fs::PermissionsExt;
    if let Ok(meta) = std::fs::metadata(path) {
        let mut perms = meta.permissions();
        perms.set_mode(0o600);
        let _ = std::fs::set_permissions(path, perms);
    }
}

fn hostname() -> String {
    std::env::var("HOSTNAME")
        .ok()
        .or_else(|| {
            std::fs::read_to_string("/proc/sys/kernel/hostname")
                .ok()
                .map(|s| s.trim().to_string())
        })
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "localhost".to_string())
}
