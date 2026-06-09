//! The `reconnect` builtin opens a picker over `sessions list`; selecting a
//! session resumes it with its pre-loss state intact.

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
fn reconnect_picker_lists_and_resumes() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    // Isolate the session socket directory from other test binaries running in
    // parallel: the picker resumes the *selected* session, so a leaked session
    // from another test sorting ahead of ours would otherwise be resumed.
    std::env::set_var("XDG_RUNTIME_DIR", dir.path());
    std::env::set_var("SHELL_DAEMON_STDIO_CMD", env!("CARGO_BIN_EXE_shell"));
    std::env::set_var("SHELL_SESSION_IDLE_EXIT_SECS", "20");

    let name = format!("pick-{}", std::process::id());

    let sink: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(std::io::sink()));
    let mut comp = Compositor::with_output(80, 24, sink).unwrap();

    // Stand up a named session and leave a marker in it.
    comp.get_focused_pane_mut()
        .unwrap()
        .connect_remote_session("local", Some(&name), &[])
        .unwrap();
    assert!(poll_until(&mut comp, "➜", Duration::from_secs(8)), "prompt");
    comp.get_focused_pane_mut()
        .unwrap()
        .handle_input(b"echo PICK_MARKER\r");
    assert!(
        poll_until(&mut comp, "PICK_MARKER", Duration::from_secs(8)),
        "command should run; got:\n{}",
        screen(&mut comp)
    );

    // Lose the pane, then drive the `reconnect` builtin to open the picker.
    comp.get_focused_pane_mut().unwrap().take_remote();
    std::thread::sleep(Duration::from_millis(300));
    comp.get_focused_pane_mut()
        .unwrap()
        .handle_input(b"reconnect local\r");
    assert!(
        poll_until(&mut comp, &name, Duration::from_secs(8)),
        "picker should list the session `{name}`; got:\n{}",
        screen(&mut comp)
    );
    assert!(
        screen(&mut comp).contains("Sessions on local"),
        "picker title; got:\n{}",
        screen(&mut comp)
    );

    // Resume the (only, selected) session and confirm the prior state returns.
    comp.get_focused_pane_mut().unwrap().handle_input(b"\r");
    let deadline = Instant::now() + Duration::from_secs(15);
    let mut next_send = Instant::now() + Duration::from_secs(2);
    while Instant::now() < deadline && !screen(&mut comp).contains("PICK_SECOND") {
        let _ = comp.poll_once(20);
        if Instant::now() >= next_send {
            comp.get_focused_pane_mut()
                .unwrap()
                .handle_input(b"echo PICK_SECOND\r");
            next_send = Instant::now() + Duration::from_secs(3);
        }
    }
    let s = screen(&mut comp);
    assert!(s.contains("PICK_SECOND"), "resumed session runs commands; got:\n{s}");
    assert!(
        s.contains("PICK_MARKER"),
        "resume preserves original session state; got:\n{s}"
    );

    // Clean up the detached daemon.
    comp.get_focused_pane_mut().unwrap().take_remote();
    std::thread::sleep(Duration::from_millis(200));
    let path = shell::common::session_socket_path(&name);
    if let Ok(mut s) = UnixStream::connect(&path) {
        let _ = codec::write_frame(&mut s, &ClientMsg::Shutdown);
    }
    std::thread::sleep(Duration::from_millis(300));
    let _ = std::fs::remove_file(&path);
}
