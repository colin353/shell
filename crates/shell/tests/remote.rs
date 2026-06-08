//! End-to-end test of a remote pane over the stdio transport.
//!
//! Two hops, all in this process tree: the test acts as a dumb client of an
//! outer `server::run` daemon; inside that daemon a pane runs `connect local`,
//! which spawns the `shell` binary as `daemon --stdio` (standing in for
//! `ssh <host> shell daemon --stdio`). A command typed into the pane must
//! execute on the inner daemon and stream its output all the way back.

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
    panic!("daemon socket never appeared");
}

fn windows_contains(haystack: &[u8], needle: &[u8]) -> bool {
    haystack.windows(needle.len()).any(|w| w == needle)
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

/// Accumulate Render bytes (and GridResync repaints, rendered to ANSI) until
/// `needle` appears or the deadline passes.
fn read_until(stream: &mut UnixStream, needle: &[u8], timeout: Duration) -> Vec<u8> {
    stream
        .set_read_timeout(Some(Duration::from_millis(100)))
        .unwrap();
    let deadline = Instant::now() + timeout;
    let mut frames = FrameReader::new();
    let mut seen = Vec::new();
    let mut buf = [0u8; 65536];
    while Instant::now() < deadline {
        match stream.read(&mut buf) {
            Ok(0) => break,
            Ok(n) => {
                frames.push(&buf[..n]);
                while let Some(msg) = frames.next_frame::<ServerMsg>().unwrap() {
                    match msg {
                        ServerMsg::Render { bytes } => seen.extend_from_slice(&bytes),
                        ServerMsg::GridResync { grid, .. } => {
                            seen.extend_from_slice(&emulator::render_snapshot_to_ansi(&grid))
                        }
                        _ => {}
                    }
                }
                if windows_contains(&seen, needle) {
                    return seen;
                }
            }
            Err(ref e)
                if e.kind() == std::io::ErrorKind::WouldBlock
                    || e.kind() == std::io::ErrorKind::TimedOut => {}
            Err(e) => panic!("read error: {e}"),
        }
    }
    seen
}

fn send(stream: &mut UnixStream, bytes: &[u8]) {
    codec::write_frame(
        stream,
        &ClientMsg::Input {
            bytes: bytes.to_vec(),
        },
    )
    .unwrap();
}

#[test]
fn remote_pane_runs_a_shell_over_stdio_transport() {
    let dir = tempfile::tempdir().unwrap();
    // Keep history/env writes in the temp dir; point the `local` transport at
    // the real built `shell` binary.
    std::env::set_var("HOME", dir.path());
    std::env::set_var("SHELL_DAEMON_STDIO_CMD", env!("CARGO_BIN_EXE_shell"));
    // Forward a marker var so we can confirm env merge on the remote.
    std::fs::write(
        dir.path().join(".shell_env"),
        "export PHASE3_FORWARDED=traveled\n",
    )
    .unwrap();

    let sock = dir.path().join("outer.sock");
    let sock_for_daemon = sock.clone();
    std::thread::spawn(move || {
        let _ = shell::server::run(&sock_for_daemon);
    });
    wait_for_socket(&sock);

    let mut stream = UnixStream::connect(&sock).unwrap();
    codec::write_frame(&mut stream, &hello((100, 30))).unwrap();
    expect_welcome(&mut stream);

    // Connect the pane to a stdio daemon (stands in for `ssh host`).
    send(&mut stream, b"connect local\r");
    // Give the inner daemon time to boot and paint its prompt.
    std::thread::sleep(Duration::from_millis(1200));

    // A command typed now runs on the *remote* and streams output back.
    send(&mut stream, b"echo REMOTE_OK_MARKER\r");
    let out = read_until(&mut stream, b"REMOTE_OK_MARKER", Duration::from_secs(12));
    assert!(
        windows_contains(&out, b"REMOTE_OK_MARKER"),
        "remote command output should reach the client ({} bytes seen)",
        out.len()
    );

    // Env merge: the forwarded ~/.shell_env var is visible on the remote.
    send(&mut stream, b"echo $PHASE3_FORWARDED\r");
    let out = read_until(&mut stream, b"traveled", Duration::from_secs(8));
    assert!(
        windows_contains(&out, b"traveled"),
        "forwarded env var should be set on the remote"
    );
}
