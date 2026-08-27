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
use std::io::{self, Read, Write};
use std::net::Shutdown;
use std::os::fd::{FromRawFd, RawFd};
use std::os::unix::net::{UnixListener, UnixStream};
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::mpsc::{self, Receiver, TryRecvError};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use protocol::codec::{self};
use protocol::{
    ClientMsg, ContextSnapshot, Hello, HostId, Origin, PaneId, ServerMsg, SessionId,
    ValidatedDirectory, Welcome, PROTOCOL_VERSION,
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
            if writer
                .write_all(&frame)
                .and_then(|_| writer.flush())
                .is_err()
            {
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

/// Build a `ShellCore` wired to a fresh [`HistoryMirror`], so every command the
/// daemon's shell records is also captured for dual-write back to the client.
/// Panics on core init failure, matching `Compositor::with_output`'s behavior.
fn mirrored_core() -> (Arc<libshell::ShellCore>, Arc<libshell::HistoryMirror>) {
    let core = Arc::new(libshell::ShellCore::new().expect("Failed to create ShellCore"));
    let mirror = Arc::new(libshell::HistoryMirror::new());
    core.set_history_mirror(mirror.clone());
    (core, mirror)
}

/// Build and configure the daemon's compositor and its history mirror. Shared by
/// the socket (`run`) and stdio (`run_stdio`) daemons, which differ only in the
/// initial size and whether synchronized-output sequences are emitted.
fn build_compositor(
    cols: usize,
    rows: usize,
    sink: Arc<Mutex<ClientSink>>,
    synchronized_output: bool,
    bare: bool,
) -> io::Result<(compositor::Compositor, Arc<libshell::HistoryMirror>)> {
    let (core, history_mirror) = mirrored_core();
    let mut compositor =
        compositor::Compositor::with_core(cols, rows, sink, core).map_err(|e| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("failed to create compositor: {e:?}"),
            )
        })?;
    compositor.set_synchronized_output(synchronized_output);
    if bare {
        // No chrome: a persistent/embedded daemon living inside a remote pane.
        compositor.set_status_bar_visible(false);
    }
    Ok((compositor, history_mirror))
}

fn socket_synchronized_output(bare: bool) -> bool {
    !bare
}

/// What a client message asks the daemon loop to do after it's applied.
enum MsgOutcome {
    /// Keep serving.
    Continue,
    /// The shell exited at top level (e.g. Ctrl-D): tear the session down.
    Exit,
    /// The client asked to detach (socket transport); stdio treats this as a
    /// no-op since its connection is the process's lifetime.
    Detach,
    /// Send a control response to the attached client and keep serving.
    Reply(ServerMsg),
}

fn validate_directories(
    compositor: &compositor::Compositor,
    candidates: Vec<String>,
) -> Vec<ValidatedDirectory> {
    const MAX_CANDIDATES: usize = 512;
    const MAX_CANDIDATE_BYTES: usize = 4096;

    if compositor.focused_shell_waiting_for_input() != Some(true) {
        return Vec::new();
    }
    let Some(cwd) = compositor.focused_cwd() else {
        return Vec::new();
    };

    candidates
        .into_iter()
        .take(MAX_CANDIDATES)
        .filter(|candidate| {
            !candidate.is_empty()
                && candidate.len() <= MAX_CANDIDATE_BYTES
                && !candidate.contains('\0')
        })
        .filter_map(|candidate| {
            let candidate_path = Path::new(&candidate);
            let path = if candidate_path.is_absolute() {
                candidate_path.to_path_buf()
            } else {
                cwd.join(candidate_path)
            };
            if !path.is_dir() {
                return None;
            }
            Some(ValidatedDirectory {
                candidate,
                path: path.canonicalize().unwrap_or(path),
            })
        })
        .collect()
}

/// Apply a client message that both transports handle identically (input,
/// resize, resync, env merge). Transport-specific concerns — detaching the sink,
/// reacting to a dropped connection — are left to the caller via [`MsgOutcome`].
fn apply_client_msg(compositor: &mut compositor::Compositor, msg: ClientMsg) -> MsgOutcome {
    match msg {
        ClientMsg::Input { bytes } => {
            if compositor.handle_input(&bytes) {
                return MsgOutcome::Exit;
            }
        }
        ClientMsg::Resize { cols, rows } => {
            compositor.resize((cols.max(1)) as usize, (rows.max(1)) as usize);
            compositor.force_full_redraw();
        }
        ClientMsg::SetCwd { cwd, .. } => {
            if compositor.focused_shell_waiting_for_input() == Some(true)
                && cwd.is_absolute()
                && cwd.is_dir()
                && compositor.set_focused_cwd(cwd)
            {
                compositor.force_full_redraw();
            }
        }
        ClientMsg::ValidateDirectories {
            request,
            candidates,
        } => {
            return MsgOutcome::Reply(ServerMsg::ValidatedDirectories {
                request,
                directories: validate_directories(compositor, candidates),
            });
        }
        ClientMsg::SetTitle { name } => {
            compositor.active_tab_mut().rename(name);
            compositor.force_full_redraw();
        }
        ClientMsg::RequestResync => compositor.force_full_redraw(),
        ClientMsg::UpdateLocalEnv { vars } => apply_env_defaults(&vars),
        ClientMsg::Detach => return MsgOutcome::Detach,
        // Hello / smart-mode messages: ignored by the dumb daemon.
        _ => {}
    }
    MsgOutcome::Continue
}

/// Forward per-iteration control-channel traffic to the attached client:
/// mirrored history entries (for local dual-write) and a window rename.
///
/// Explicit titles are also sent as part of every attach handshake, so a
/// rename that happens while detached is restored together with the screen.
/// With no client (`out` is `None`) history mirror entries accumulate (capped)
/// until one reattaches.
fn forward_pending(
    out: Option<&mut dyn Write>,
    compositor: &compositor::Compositor,
    history_mirror: &libshell::HistoryMirror,
    last_name: &mut String,
    last_cwd: &mut Option<PathBuf>,
    last_input_echo: &mut Option<bool>,
    last_shell_waiting: &mut Option<bool>,
) {
    let name = compositor.active_tab().name.clone();
    let rename = if compositor.active_tab().name_is_explicit && name != *last_name {
        *last_name = name.clone();
        Some(name)
    } else {
        None
    };

    let cwd = compositor.focused_cwd();
    let context = if cwd != *last_cwd {
        *last_cwd = cwd.clone();
        cwd.map(context_msg)
    } else {
        None
    };

    let input_echo = compositor.focused_input_echo_mode();
    let input_mode = if input_echo != *last_input_echo {
        *last_input_echo = input_echo;
        input_echo.map(input_mode_msg)
    } else {
        None
    };

    let shell_waiting = compositor.focused_shell_waiting_for_input();
    let shell_state = if shell_waiting != *last_shell_waiting {
        *last_shell_waiting = shell_waiting;
        shell_waiting.map(shell_state_msg)
    } else {
        None
    };

    let Some(out) = out else { return };
    for entry in history_mirror.drain() {
        let _ = codec::write_frame(&mut *out, &ServerMsg::HistoryRecorded { entry });
    }
    if let Some(name) = rename {
        let _ = codec::write_frame(&mut *out, &ServerMsg::RenameWindow { name });
    }
    if let Some(context) = context {
        let _ = codec::write_frame(&mut *out, &context);
    }
    if let Some(input_mode) = input_mode {
        let _ = codec::write_frame(&mut *out, &input_mode);
    }
    if let Some(shell_state) = shell_state {
        let _ = codec::write_frame(out, &shell_state);
    }
}

fn context_msg(cwd: PathBuf) -> ServerMsg {
    ServerMsg::Context {
        pane: PaneId(0),
        ctx: ContextSnapshot {
            cwd,
            env: libshell::shell_env_snapshot(),
            path_executables: Vec::new(),
            origin: Origin::Local,
        },
    }
}

fn input_mode_msg(echo: bool) -> ServerMsg {
    ServerMsg::InputMode {
        pane: PaneId(0),
        echo,
    }
}

fn shell_state_msg(waiting_for_input: bool) -> ServerMsg {
    ServerMsg::ShellState {
        pane: PaneId(0),
        waiting_for_input,
    }
}

/// Spawn a reader thread that decodes `ClientMsg` frames off `reader` and
/// forwards them over a channel until EOF/error. Shared by the socket handshake
/// and the stdio daemon.
fn spawn_reader<R: Read + Send + 'static>(mut reader: R) -> Receiver<ClientMsg> {
    let (tx, rx) = mpsc::channel::<ClientMsg>();
    std::thread::spawn(move || loop {
        match codec::read_frame::<_, ClientMsg>(&mut reader) {
            Ok(Some(msg)) => {
                if tx.send(msg).is_err() {
                    break;
                }
            }
            Ok(None) | Err(_) => break,
        }
    });
    rx
}

pub fn run(socket_path: &Path, bare: bool, initial_command: Option<&str>) -> io::Result<()> {
    // Replace any stale socket from a previous (dead) daemon.
    let _ = std::fs::remove_file(socket_path);
    let listener = UnixListener::bind(socket_path)?;
    restrict_permissions(socket_path);

    let host = HostId(hostname());

    let sink = Arc::new(Mutex::new(ClientSink::new()));
    // Non-bare socket daemons render to a real client terminal, so synchronized output
    // (BSU/ESU) is wanted there. Bare daemons are embedded inside a remote pane,
    // so their render bytes must be plain VT for the local pane emulator.
    let (mut compositor, history_mirror) =
        build_compositor(80, 24, sink.clone(), socket_synchronized_output(bare), bare)?;

    // `shell run`: launch the requested command in the first pane immediately,
    // exactly as if it had been typed and submitted. No client is attached yet,
    // so the render is dropped; a later `attach`/`reconnect` repaints via
    // GridResync. When the command exits the pane falls back to its prompt and
    // the session stays alive (idle-exit handles teardown).
    if let Some(cmd) = initial_command {
        compositor.handle_input(format!("{cmd}\n").as_bytes());
    }
    compositor.render();

    // Accept connections on a background thread so the main loop never blocks on
    // a slow or stalled client handshake.
    let (conn_tx, conn_rx) = mpsc::channel::<Incoming>();
    {
        let host = host.clone();
        std::thread::spawn(move || accept_loop(listener, conn_tx, host));
    }

    let mut current: Option<Receiver<ClientMsg>> = None;
    // A clone of the attached client's stream for sending control frames
    // (RenameWindow, SessionEnded) that don't go through the render sink.
    let mut control: Option<UnixStream> = None;
    let mut last_name = compositor.active_tab().name.clone();
    let mut last_cwd = compositor.focused_cwd();
    let mut last_input_echo = compositor.focused_input_echo_mode();
    let mut last_shell_waiting = compositor.focused_shell_waiting_for_input();

    // Persist indefinitely while detached by default (the point of a remote
    // session). `SHELL_SESSION_IDLE_EXIT_SECS` bounds that — tests set it so the
    // detached daemon they spawn cleans itself up.
    let idle_exit: Option<Duration> = std::env::var("SHELL_SESSION_IDLE_EXIT_SECS")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .map(Duration::from_secs);
    let mut had_client = false;
    let mut detached_since: Option<Instant> = None;

    let exit = loop {
        // Honor a shutdown request (`sessions kill`): tell any attached pane the
        // session is over (so it returns to local instead of reconnecting), then
        // exit.
        match conn_rx.try_recv() {
            Ok(Incoming::Shutdown) => {
                if let Some(c) = control.as_mut() {
                    let _ = codec::write_frame(c, &ServerMsg::SessionEnded);
                }
                break false;
            }
            Ok(Incoming::Client(incoming)) => {
                // Attach a newly-arrived client (taking over from any existing
                // one). Drop the previous client first, so the refresh render
                // below is discarded rather than sent as a stray partial frame.
                sink.lock().unwrap().detach();

                let (cols, rows) = incoming.size;
                compositor.resize((cols.max(1)) as usize, (rows.max(1)) as usize);
                // Refresh the composite at the new size (output dropped: no
                // client yet), then snapshot the authoritative screen.
                compositor.render();
                let snapshot = compositor.grid_snapshot();

                // Paint the (re)attaching client via GridResync, then attach the
                // sink so subsequent live deltas stream as Render.
                let mut stream = incoming.sink_stream;
                let resync = ServerMsg::GridResync {
                    pane: PaneId(0),
                    grid: snapshot,
                };
                if codec::write_frame(&mut stream, &resync).is_ok() {
                    if compositor.active_tab().name_is_explicit {
                        let _ = codec::write_frame(
                            &mut stream,
                            &ServerMsg::RenameWindow {
                                name: compositor.active_tab().name.clone(),
                            },
                        );
                    }
                    if let Some(cwd) = compositor.focused_cwd() {
                        let _ = codec::write_frame(&mut stream, &context_msg(cwd.clone()));
                        last_cwd = Some(cwd);
                    }
                    if let Some(echo) = compositor.focused_input_echo_mode() {
                        let _ = codec::write_frame(&mut stream, &input_mode_msg(echo));
                        last_input_echo = Some(echo);
                    }
                    if let Some(waiting) = compositor.focused_shell_waiting_for_input() {
                        let _ = codec::write_frame(&mut stream, &shell_state_msg(waiting));
                        last_shell_waiting = Some(waiting);
                    }
                    if let Ok(clone) = stream.try_clone() {
                        control = stream.try_clone().ok();
                        sink.lock().unwrap().attach(Box::new(clone));
                        current = Some(incoming.input);
                        last_name = compositor.active_tab().name.clone();
                        had_client = true;
                        detached_since = None;
                    }
                }
            }
            Err(_) => {}
        }

        // Drain the attached client's input. A dropped connection or an explicit
        // Detach returns us to the detached state (the daemon keeps running); a
        // top-level shell exit tears the session down.
        let mut should_exit = false;
        if let Some(rx) = &current {
            loop {
                match rx.try_recv() {
                    Ok(msg) => match apply_client_msg(&mut compositor, msg) {
                        MsgOutcome::Continue => {}
                        MsgOutcome::Exit => {
                            should_exit = true;
                            break;
                        }
                        MsgOutcome::Detach => {
                            sink.lock().unwrap().detach();
                            current = None;
                            control = None;
                            break;
                        }
                        MsgOutcome::Reply(msg) => {
                            if let Some(c) = control.as_mut() {
                                let _ = codec::write_frame(c, &msg);
                            }
                        }
                    },
                    Err(TryRecvError::Empty) => break,
                    Err(TryRecvError::Disconnected) => {
                        // Reader thread ended: the client is gone.
                        sink.lock().unwrap().detach();
                        current = None;
                        control = None;
                        break;
                    }
                }
            }
        }

        // Ship mirrored history + any window rename up the control channel (only
        // while attached; otherwise the mirror buffers, capped, until reattach).
        forward_pending(
            control.as_mut().map(|c| c as &mut dyn Write),
            &compositor,
            &history_mirror,
            &mut last_name,
            &mut last_cwd,
            &mut last_input_echo,
            &mut last_shell_waiting,
        );

        if should_exit {
            // Tell the client this was a deliberate exit, so it returns to the
            // local shell rather than auto-reconnecting.
            if let Some(c) = control.as_mut() {
                let _ = codec::write_frame(c, &ServerMsg::SessionEnded);
            }
            break true;
        }

        // Idle-exit: clean up if we've been client-less too long (only after a
        // client has ever attached, so we don't race the first connect).
        if current.is_some() {
            detached_since = None;
        } else if had_client {
            let since = *detached_since.get_or_insert_with(Instant::now);
            if let Some(limit) = idle_exit {
                if since.elapsed() >= limit {
                    break false;
                }
            }
        }

        // Always drive PTYs/shell output, attached or not.
        if let Err(e) = compositor.poll_once(10) {
            eprintln!("shell-daemon: compositor error: {e:?}");
            break false;
        }
        if compositor.should_exit() {
            if let Some(c) = control.as_mut() {
                let _ = codec::write_frame(c, &ServerMsg::SessionEnded);
            }
            break true;
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
    // Synchronized output is OFF here: this daemon's render bytes are fed into
    // the *local* pane emulator, not a real terminal, so BSU/ESU sequences would
    // pollute that emulator's state (the local compositor wraps the real terminal
    // output itself).
    let (mut compositor, history_mirror) = build_compositor(
        (cols.max(1)) as usize,
        (rows.max(1)) as usize,
        sink.clone(),
        false,
        bare,
    )?;

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
    if compositor.active_tab().name_is_explicit {
        codec::write_frame(
            &mut handshake_out,
            &ServerMsg::RenameWindow {
                name: compositor.active_tab().name.clone(),
            },
        )?;
    }
    if let Some(cwd) = compositor.focused_cwd() {
        codec::write_frame(&mut handshake_out, &context_msg(cwd))?;
    }
    if let Some(echo) = compositor.focused_input_echo_mode() {
        codec::write_frame(&mut handshake_out, &input_mode_msg(echo))?;
    }
    if let Some(waiting) = compositor.focused_shell_waiting_for_input() {
        codec::write_frame(&mut handshake_out, &shell_state_msg(waiting))?;
    }
    sink.lock().unwrap().attach(Box::new(dup_file(1)?));

    // Reader thread: stdin -> ClientMsg.
    let rx = spawn_reader(dup_file(0)?);

    let mut last_name = compositor.active_tab().name.clone();
    let mut last_cwd = compositor.focused_cwd();
    let mut last_input_echo = compositor.focused_input_echo_mode();
    let mut last_shell_waiting = compositor.focused_shell_waiting_for_input();

    loop {
        let mut exit = false;
        let mut deliberate_exit = false;
        loop {
            match rx.try_recv() {
                // Detach is meaningless over stdio (the connection is the
                // process's lifetime), so it's treated like any ignored message.
                Ok(msg) => match apply_client_msg(&mut compositor, msg) {
                    MsgOutcome::Continue | MsgOutcome::Detach => {}
                    MsgOutcome::Reply(msg) => {
                        let _ = codec::write_frame(&mut handshake_out, &msg);
                    }
                    MsgOutcome::Exit => {
                        exit = true;
                        deliberate_exit = true;
                        break;
                    }
                },
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    exit = true; // stdin closed: transport died
                    break;
                }
            }
        }

        // Ship mirrored history + any window rename up to the client. The stdio
        // transport is always attached, so `out` is never `None` here.
        forward_pending(
            Some(&mut handshake_out),
            &compositor,
            &history_mirror,
            &mut last_name,
            &mut last_cwd,
            &mut last_input_echo,
            &mut last_shell_waiting,
        );

        if exit {
            if deliberate_exit {
                let _ = codec::write_frame(&mut handshake_out, &ServerMsg::SessionEnded);
            }
            break;
        }
        compositor
            .poll_once(10)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("compositor: {e:?}")))?;
        if compositor.should_exit() {
            let _ = codec::write_frame(&mut handshake_out, &ServerMsg::SessionEnded);
            break;
        }
    }
    Ok(())
}

/// `shell sessions [list|kill <name>]`: manage persistent session daemons on
/// this machine (run remotely via `ssh <host> shell sessions ...`).
pub fn run_sessions(args: &[String]) -> io::Result<()> {
    match args.first().map(String::as_str).unwrap_or("list") {
        "list" | "ls" => sessions_list(),
        "kill" => {
            let name = args.get(1).ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput, "usage: sessions kill <name>")
            })?;
            sessions_kill(name)
        }
        other => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("unknown sessions command `{other}` (try: list, kill <name>)"),
        )),
    }
}

fn sessions_list() -> io::Result<()> {
    let mut sessions = crate::common::session_sockets();
    sessions.sort_by(|a, b| a.0.cmp(&b.0));
    let mut any = false;
    for (name, path) in sessions {
        if UnixStream::connect(&path).is_err() {
            // Stale socket from a killed/crashed daemon: clean it up.
            let _ = std::fs::remove_file(&path);
            continue;
        }
        any = true;
        let age = path
            .metadata()
            .ok()
            .and_then(|m| m.modified().ok())
            .and_then(|t| t.elapsed().ok())
            .map(|d| format!("{}s", d.as_secs()))
            .unwrap_or_else(|| "?".into());
        // name<TAB>age — human-readable and easy to parse for a picker.
        println!("{name}\t{age}");
    }
    if !any {
        eprintln!("(no sessions)");
    }
    Ok(())
}

fn sessions_kill(name: &str) -> io::Result<()> {
    let path = crate::common::session_socket_path(name);
    match UnixStream::connect(&path) {
        Ok(mut s) => {
            let _ = codec::write_frame(&mut s, &ClientMsg::Shutdown);
            // Give the daemon a moment to exit and remove its own socket.
            std::thread::sleep(Duration::from_millis(200));
            let _ = std::fs::remove_file(&path);
            println!("killed session {name}");
        }
        Err(_) => {
            let _ = std::fs::remove_file(&path);
            eprintln!("no live session `{name}`");
        }
    }
    Ok(())
}

/// Bridge stdin/stdout to a persistent daemon's Unix socket, spawning the
/// daemon (detached) if it isn't already running.
///
/// This is what a remote pane runs over ssh (`ssh <host> shell bridge ...`).
/// The daemon outlives this process and the ssh link, so the session survives a
/// disconnect; reconnecting just re-runs the bridge and reattaches.
pub fn run_bridge(socket_path: &Path) -> io::Result<()> {
    let stream = connect_or_spawn(socket_path)?;

    // Pump bytes both directions, unbuffered. stdin<-ssh -> daemon socket;
    // daemon socket -> stdout -> ssh.
    let to_daemon = stream.try_clone()?;
    let from_daemon = stream;

    let pump_in = std::thread::spawn(move || {
        let mut stdin = dup_file(0).expect("dup stdin");
        let mut sock = to_daemon;
        pump(&mut stdin, &mut sock);
        // ssh closed our stdin: signal the daemon's reader so it detaches this
        // client (but keeps running).
        let _ = sock.shutdown(Shutdown::Write);
    });

    // Daemon -> stdout. Returns when the daemon closes the socket.
    let mut stdout = dup_file(1)?;
    let mut sock = from_daemon;
    pump(&mut sock, &mut stdout);

    let _ = pump_in.join();
    Ok(())
}

/// Connect to the daemon socket, spawning a detached daemon first if needed.
fn connect_or_spawn(socket_path: &Path) -> io::Result<UnixStream> {
    if let Ok(s) = UnixStream::connect(socket_path) {
        return Ok(s);
    }
    spawn_detached_daemon(socket_path, None)?;
    let deadline = Instant::now() + Duration::from_secs(10);
    loop {
        if let Ok(s) = UnixStream::connect(socket_path) {
            return Ok(s);
        }
        if Instant::now() > deadline {
            return Err(io::Error::new(
                io::ErrorKind::TimedOut,
                "remote session daemon did not start",
            ));
        }
        std::thread::sleep(Duration::from_millis(50));
    }
}

/// Spawn `shell daemon --socket <S> --bare [--exec <cmd>]` detached from this
/// process (and the ssh session), so it persists across disconnects. With
/// `command`, the daemon launches it in its first pane on startup (`shell run`).
fn spawn_detached_daemon(socket_path: &Path, command: Option<&str>) -> io::Result<()> {
    if let Some(parent) = socket_path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    let exe = std::env::current_exe()?;
    let mut cmd = Command::new(exe);
    cmd.arg("daemon")
        .arg("--socket")
        .arg(socket_path)
        .arg("--bare");
    if let Some(command) = command {
        cmd.arg("--exec").arg(command);
    }
    cmd.stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    unsafe {
        // New session: detach from ssh's controlling terminal and process group
        // so a SIGHUP on disconnect doesn't kill the daemon.
        cmd.pre_exec(|| {
            libc::setsid();
            Ok(())
        });
    }
    cmd.spawn()?; // detached child; we don't reap it
    Ok(())
}

/// `shell run --name <name> -- <cmd...>`: start a detached, persistent session
/// named `name` whose first pane runs `command`. The session is `--bare` (no
/// chrome) since it's reattached from inside another shell via the `reconnect`
/// picker (or `shell attach --session <name>`). Refuses to clobber a live
/// session of the same name.
pub fn run_command_session(name: &str, command: &str) -> io::Result<()> {
    let socket_path = crate::common::session_socket_path(name);
    if UnixStream::connect(&socket_path).is_ok() {
        return Err(io::Error::new(
            io::ErrorKind::AlreadyExists,
            format!("session `{name}` already exists"),
        ));
    }
    // Stale socket from a dead daemon, if any: clear it so the bind succeeds.
    let _ = std::fs::remove_file(&socket_path);
    spawn_detached_daemon(&socket_path, Some(command))?;
    println!("started session `{name}` running: {command}");
    Ok(())
}

/// Copy bytes from `from` to `to`, flushing each chunk, until EOF/error.
fn pump<R: Read, W: Write>(from: &mut R, to: &mut W) {
    let mut buf = [0u8; 65536];
    loop {
        match from.read(&mut buf) {
            Ok(0) => break,
            Ok(n) => {
                if to.write_all(&buf[..n]).and_then(|_| to.flush()).is_err() {
                    break;
                }
            }
            Err(ref e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(_) => break,
        }
    }
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
/// A handshaked connection: a client attaching, or a request to terminate.
enum Incoming {
    Client(IncomingClient),
    Shutdown,
}

fn accept_loop(listener: UnixListener, conn_tx: mpsc::Sender<Incoming>, host: HostId) {
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

/// First frame is either `Shutdown` (terminate the session) or `Hello` (attach,
/// reply `Welcome`, spawn the reader thread).
fn handshake(stream: UnixStream, host: &HostId) -> io::Result<Incoming> {
    let mut hs = stream.try_clone()?;
    let size = match codec::read_frame::<_, ClientMsg>(&mut hs)? {
        Some(ClientMsg::Shutdown) => return Ok(Incoming::Shutdown),
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
        _ => return Err(io::Error::new(io::ErrorKind::InvalidData, "expected Hello")),
    };

    let rx = spawn_reader(stream.try_clone()?);

    Ok(Incoming::Client(IncomingClient {
        input: rx,
        sink_stream: stream,
        size,
    }))
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

#[cfg(test)]
mod tests {
    use super::*;
    use compositor::{BSU, ESU};
    use protocol::{codec::FrameReader, ServerMsg};
    use std::sync::{Arc, Mutex};

    #[derive(Clone)]
    struct CaptureWriter(Arc<Mutex<Vec<u8>>>);

    impl Write for CaptureWriter {
        fn write(&mut self, data: &[u8]) -> io::Result<usize> {
            self.0.lock().unwrap().extend_from_slice(data);
            Ok(data.len())
        }

        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    fn render_bytes_for_socket_daemon(bare: bool, synchronized_output: bool) -> Vec<u8> {
        let sink = Arc::new(Mutex::new(ClientSink::new()));
        let captured = Arc::new(Mutex::new(Vec::new()));
        sink.lock()
            .unwrap()
            .attach(Box::new(CaptureWriter(captured.clone())));

        let (mut compositor, _history_mirror) =
            build_compositor(80, 24, sink, synchronized_output, bare).unwrap();
        compositor.render();

        let framed = captured.lock().unwrap().clone();
        let mut frames = FrameReader::new();
        frames.push(&framed);
        while let Some(msg) = frames.next_frame::<ServerMsg>().unwrap() {
            if let ServerMsg::Render { bytes } = msg {
                return bytes;
            }
        }
        Vec::new()
    }

    #[test]
    fn bare_socket_daemon_render_stream_is_plain_vt_for_embedded_pane() {
        let old_config_bytes = render_bytes_for_socket_daemon(true, true);
        assert!(
            old_config_bytes.windows(BSU.len()).any(|w| w == BSU)
                && old_config_bytes.windows(ESU.len()).any(|w| w == ESU),
            "the old bare socket configuration reproduced host synchronized-output leakage"
        );

        let fixed_bytes = render_bytes_for_socket_daemon(true, socket_synchronized_output(true));
        assert!(
            !fixed_bytes.windows(BSU.len()).any(|w| w == BSU)
                && !fixed_bytes.windows(ESU.len()).any(|w| w == ESU),
            "bare daemon render stream must not contain host synchronized-output sequences"
        );
    }

    #[test]
    fn non_bare_socket_daemon_keeps_synchronized_output_for_real_clients() {
        let bytes = render_bytes_for_socket_daemon(false, socket_synchronized_output(false));
        assert!(
            bytes.windows(BSU.len()).any(|w| w == BSU)
                && bytes.windows(ESU.len()).any(|w| w == ESU),
            "non-bare daemon render stream should keep synchronized-output wrapping"
        );
    }
}
