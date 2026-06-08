//! End-to-end tests for the daemon, driven as a raw protocol client (no TTY).
//! Validates the handshake, that the prompt renders, that input is executed and
//! streamed back, and that a session survives detach/reattach.

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

/// Accumulate `Render` bytes until `needle` is seen or the deadline passes.
/// Returns the concatenated render output observed.
fn read_renders_until(stream: &mut UnixStream, needle: &[u8], timeout: Duration) -> Vec<u8> {
    stream
        .set_read_timeout(Some(Duration::from_millis(100)))
        .unwrap();
    let deadline = Instant::now() + timeout;
    let mut frames = FrameReader::new();
    let mut seen = Vec::new();
    let mut buf = [0u8; 65536];

    while Instant::now() < deadline {
        match stream.read(&mut buf) {
            Ok(0) => break, // peer closed
            Ok(n) => {
                frames.push(&buf[..n]);
                while let Some(msg) = frames.next_frame::<ServerMsg>().unwrap() {
                    if let ServerMsg::Render { bytes } = msg {
                        seen.extend_from_slice(&bytes);
                    }
                }
                if windows_contains(&seen, needle) {
                    return seen;
                }
            }
            Err(ref e)
                if e.kind() == std::io::ErrorKind::WouldBlock
                    || e.kind() == std::io::ErrorKind::TimedOut => {}
            Err(e) => panic!("socket read error: {e}"),
        }
    }
    seen
}

fn windows_contains(haystack: &[u8], needle: &[u8]) -> bool {
    needle.is_empty() || haystack.windows(needle.len()).any(|w| w == needle)
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

#[test]
fn daemon_handshakes_renders_executes_and_reattaches() {
    let dir = tempfile::tempdir().unwrap();
    // Keep history/env writes inside the temp dir, not the real home.
    std::env::set_var("HOME", dir.path());
    let sock = dir.path().join("daemon.sock");

    let sock_for_daemon = sock.clone();
    std::thread::spawn(move || {
        let _ = shell::server::run(&sock_for_daemon);
    });
    wait_for_socket(&sock);

    // --- Attach #1: handshake + prompt renders. ---
    let mut stream = UnixStream::connect(&sock).unwrap();
    codec::write_frame(&mut stream, &hello((80, 24))).unwrap();
    expect_welcome(&mut stream);

    // The force_full_redraw on attach paints the prompt; expect *some* render.
    let prompt = read_renders_until(&mut stream, b"\x1b[", Duration::from_secs(3));
    assert!(!prompt.is_empty(), "expected an initial render of the prompt");

    // --- Execute a command and see its output stream back. ---
    codec::write_frame(
        &mut stream,
        &ClientMsg::Input {
            bytes: b"echo phase1onerocks\r".to_vec(),
        },
    )
    .unwrap();
    let out = read_renders_until(&mut stream, b"phase1onerocks", Duration::from_secs(5));
    assert!(
        windows_contains(&out, b"phase1onerocks"),
        "command output should stream back to the client"
    );

    // --- Detach (drop the connection); the daemon keeps the session. ---
    drop(stream);
    std::thread::sleep(Duration::from_millis(100));

    // --- Attach #2: a fresh client gets a full repaint of the live session. ---
    let mut stream2 = UnixStream::connect(&sock).unwrap();
    codec::write_frame(&mut stream2, &hello((80, 24))).unwrap();
    expect_welcome(&mut stream2);
    let repaint = read_renders_until(&mut stream2, b"\x1b[", Duration::from_secs(3));
    assert!(
        !repaint.is_empty(),
        "reattaching client should receive a full repaint"
    );
}
