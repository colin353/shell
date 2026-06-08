//! Persistence + auto-reconnect: a remote pane survives a dropped transport.
//!
//! Connects a pane to a persistent session, runs a command (state lives in the
//! detached daemon), kills the transport (simulating ssh dying), and verifies
//! the pane auto-reconnects to the SAME session and the earlier output is still
//! on screen.

use std::io::Write;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use compositor::Compositor;

fn screen(comp: &mut Compositor) -> String {
    let pane = comp.get_focused_pane_mut().unwrap();
    let g = pane.terminal_emulator.grid();
    (0..g.rows)
        .map(|r| g.get_line_text(r))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Poll the compositor until `needle` appears on screen or the deadline passes.
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
fn remote_pane_auto_reconnects_with_preserved_state() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    std::env::set_var("SHELL_DAEMON_STDIO_CMD", env!("CARGO_BIN_EXE_shell"));
    // Daemon persists across the simulated drop, but cleans up after the test.
    std::env::set_var("SHELL_SESSION_IDLE_EXIT_SECS", "20");

    let sink: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(std::io::sink()));
    let mut comp = Compositor::with_output(80, 24, sink).unwrap();

    let target = std::env::var("REPRO_TARGET").unwrap_or_else(|_| "local".into());
    comp.get_focused_pane_mut()
        .unwrap()
        .connect_remote(&target, &[])
        .unwrap();

    // Wait for the remote prompt to paint.
    assert!(
        poll_until(&mut comp, "➜", Duration::from_secs(8)),
        "remote prompt should appear; got:\n{}",
        screen(&mut comp)
    );

    // Run a command; its output is the state we expect to survive a reconnect.
    comp.get_focused_pane_mut()
        .unwrap()
        .handle_input(b"echo RECONNECT_MARKER\r");
    assert!(
        poll_until(&mut comp, "RECONNECT_MARKER", Duration::from_secs(8)),
        "command output should appear; got:\n{}",
        screen(&mut comp)
    );

    // Simulate the ssh link dying: kill the transport (the detached daemon lives).
    let pid = comp
        .get_focused_pane_mut()
        .unwrap()
        .remote
        .as_ref()
        .unwrap()
        .transport_pid();
    unsafe {
        libc::kill(pid as libc::pid_t, libc::SIGKILL);
    }

    // Prove the pane actually reconnected (not just that the stale screen still
    // shows the old marker): a NEW command must execute on the same session.
    // Input sent before the link is back is lost, so retry until it lands.
    let deadline = Instant::now() + Duration::from_secs(15);
    let mut next_send = Instant::now() + Duration::from_secs(2);
    loop {
        if Instant::now() > deadline || screen(&mut comp).contains("SECOND_MARKER") {
            break;
        }
        let _ = comp.poll_once(20);
        if Instant::now() >= next_send && !screen(&mut comp).contains("SECOND_MARKER") {
            comp.get_focused_pane_mut()
                .unwrap()
                .handle_input(b"echo SECOND_MARKER\r");
            next_send = Instant::now() + Duration::from_secs(3);
        }
    }

    let final_screen = screen(&mut comp);
    assert!(
        final_screen.contains("SECOND_MARKER"),
        "a command run after the drop should execute on the reconnected session; \
         got:\n{final_screen}"
    );
    // The session state survived: the pre-drop output is still there.
    assert!(
        final_screen.contains("RECONNECT_MARKER"),
        "session state should survive the reconnect; got:\n{final_screen}"
    );
    assert!(comp.get_focused_pane_mut().unwrap().remote.is_some());
}
