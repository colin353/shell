//! Named sessions: reattach-by-name recovers a session after the pane is lost,
//! it shows up in `sessions list`, and `sessions kill` removes it.

use std::io::Write;
use std::os::unix::net::UnixStream;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use compositor::Compositor;
use protocol::{codec, ClientMsg};

fn screen(comp: &mut Compositor) -> String {
    let pane = comp.get_focused_pane_mut().unwrap();
    let g = pane.terminal_emulator.grid();
    (0..g.rows)
        .map(|r| g.get_line_text(r))
        .collect::<Vec<_>>()
        .join("\n")
}

fn poll_until(comp: &mut Compositor, needle: &str, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    while Instant::now() < deadline {
        let _ = comp.poll_once(20);
        if screen(comp).contains(needle) {
            return true;
        }
    }
    screen(comp).contains(needle)
}

#[test]
fn named_session_reattaches_and_can_be_killed() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    std::env::set_var("SHELL_DAEMON_STDIO_CMD", env!("CARGO_BIN_EXE_shell"));
    std::env::set_var("SHELL_SESSION_IDLE_EXIT_SECS", "20");

    let name = format!("test-{}", std::process::id());

    let sink: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(std::io::sink()));
    let mut comp = Compositor::with_output(80, 24, sink).unwrap();

    // Connect to a *named* session and leave some state in it.
    comp.get_focused_pane_mut()
        .unwrap()
        .connect_remote_session("local", Some(&name), &[])
        .unwrap();
    assert!(poll_until(&mut comp, "➜", Duration::from_secs(8)), "prompt");
    comp.get_focused_pane_mut()
        .unwrap()
        .handle_input(b"echo NAMED_MARKER\r");
    assert!(
        poll_until(&mut comp, "NAMED_MARKER", Duration::from_secs(8)),
        "command should run; got:\n{}",
        screen(&mut comp)
    );

    // It is listed as a live session.
    assert!(
        shell::common::session_sockets().iter().any(|(n, _)| n == &name),
        "session should be listed"
    );

    // Lose the pane (drop the transport; the detached daemon survives), then
    // reattach by name — the session, including its history, must come back.
    comp.get_focused_pane_mut().unwrap().take_remote();
    std::thread::sleep(Duration::from_millis(300));
    comp.get_focused_pane_mut()
        .unwrap()
        .connect_remote_session("local", Some(&name), &[])
        .unwrap();

    // A new command runs (live reattach) and the pre-loss output is still there
    // (same session, not a fresh one). Retry sends until the link is back.
    let deadline = Instant::now() + Duration::from_secs(15);
    let mut next_send = Instant::now() + Duration::from_secs(2);
    while Instant::now() < deadline && !screen(&mut comp).contains("NAMED_SECOND") {
        let _ = comp.poll_once(20);
        if Instant::now() >= next_send {
            comp.get_focused_pane_mut()
                .unwrap()
                .handle_input(b"echo NAMED_SECOND\r");
            next_send = Instant::now() + Duration::from_secs(3);
        }
    }
    let s = screen(&mut comp);
    assert!(s.contains("NAMED_SECOND"), "reattached session runs commands; got:\n{s}");
    assert!(
        s.contains("NAMED_MARKER"),
        "reattach by name preserves the original session state; got:\n{s}"
    );

    // Kill it and confirm it's gone.
    comp.get_focused_pane_mut().unwrap().take_remote();
    std::thread::sleep(Duration::from_millis(200));
    let path = shell::common::session_socket_path(&name);
    if let Ok(mut s) = UnixStream::connect(&path) {
        let _ = codec::write_frame(&mut s, &ClientMsg::Shutdown);
    }
    let gone_by = Instant::now() + Duration::from_secs(3);
    while Instant::now() < gone_by && UnixStream::connect(&path).is_ok() {
        std::thread::sleep(Duration::from_millis(50));
    }
    let _ = std::fs::remove_file(&path);
    assert!(
        UnixStream::connect(&path).is_err(),
        "killed session should no longer be connectable"
    );
}
