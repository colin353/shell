//! End-to-end test for `shell run`: a daemon started with an initial command
//! launches it in its first pane on startup (no client attached), and the
//! output is still present when a client later attaches. After the command
//! exits the session stays alive (the pane falls back to its prompt).

use std::io::Read;
use std::os::unix::net::UnixStream;
use std::path::Path;
use std::time::{Duration, Instant};

use protocol::codec::{self, FrameReader};
use protocol::{ClientMode, ClientMsg, Hello, ServerMsg, PROTOCOL_VERSION};

fn hello(size: (u16, u16)) -> ClientMsg {
    ClientMsg::Hello(Hello {
        version: PROTOCOL_VERSION,
        mode: ClientMode::Dumb,
        size,
    })
}

fn wait_for_socket(path: &Path) {
    let deadline = Instant::now() + Duration::from_secs(5);
    while Instant::now() < deadline {
        if path.exists() && UnixStream::connect(path).is_ok() {
            return;
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    panic!("daemon socket never appeared at {}", path.display());
}

fn expect_welcome(stream: &mut UnixStream) {
    stream
        .set_read_timeout(Some(Duration::from_secs(5)))
        .unwrap();
    match codec::read_frame::<_, ServerMsg>(stream).unwrap() {
        Some(ServerMsg::Welcome(w)) => assert_eq!(w.version, PROTOCOL_VERSION),
        other => panic!("expected Welcome, got {other:?}"),
    }
}

fn expect_grid_resync(stream: &mut UnixStream, timeout: Duration) -> protocol::GridSnapshot {
    stream
        .set_read_timeout(Some(Duration::from_millis(100)))
        .unwrap();
    let deadline = Instant::now() + timeout;
    let mut frames = FrameReader::new();
    let mut buf = [0u8; 65536];
    while Instant::now() < deadline {
        match stream.read(&mut buf) {
            Ok(0) => break,
            Ok(n) => {
                frames.push(&buf[..n]);
                while let Some(msg) = frames.next_frame::<ServerMsg>().unwrap() {
                    if let ServerMsg::GridResync { grid, .. } = msg {
                        return grid;
                    }
                }
            }
            Err(ref e)
                if e.kind() == std::io::ErrorKind::WouldBlock
                    || e.kind() == std::io::ErrorKind::TimedOut => {}
            Err(e) => panic!("socket read error: {e}"),
        }
    }
    panic!("expected a GridResync within {timeout:?}");
}

fn snapshot_text(snap: &protocol::GridSnapshot) -> String {
    let grid = emulator::TerminalGrid::from_snapshot(snap).expect("snapshot must decode");
    (0..grid.rows)
        .map(|r| grid.get_line_text(r))
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn run_session_executes_initial_command_visible_on_attach() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    let sock = dir.path().join("run.sock");

    // Start a daemon with an initial command, exactly as `shell run` does.
    let sock_for_daemon = sock.clone();
    std::thread::spawn(move || {
        let _ = shell::server::run(&sock_for_daemon, true, Some("echo runsessionmarker"));
    });
    wait_for_socket(&sock);

    // Attach only after the command has already run (no client was present when
    // it executed) — its output must still be on screen.
    let mut stream = UnixStream::connect(&sock).unwrap();
    codec::write_frame(&mut stream, &hello((80, 24))).unwrap();
    expect_welcome(&mut stream);

    let snap = expect_grid_resync(&mut stream, Duration::from_secs(5));
    let screen = snapshot_text(&snap);
    assert!(
        screen.contains("runsessionmarker"),
        "initial command output should be visible on attach; got:\n{screen}"
    );

    // The command (`echo`) has exited, but the session stays alive: a second
    // fresh attach still succeeds and still shows the output.
    drop(stream);
    std::thread::sleep(Duration::from_millis(100));

    let mut stream2 = UnixStream::connect(&sock).expect("session should still be alive");
    codec::write_frame(&mut stream2, &hello((80, 24))).unwrap();
    expect_welcome(&mut stream2);
    let snap2 = expect_grid_resync(&mut stream2, Duration::from_secs(5));
    assert!(
        snapshot_text(&snap2).contains("runsessionmarker"),
        "session should stay alive after the command exits"
    );
}
