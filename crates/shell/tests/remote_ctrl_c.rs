//! Ctrl+C / Ctrl+D in a remote pane must be forwarded to the remote, not
//! handled by the local shell (which would paint a "^C" + local prompt over the
//! remote screen — the cause of the history-picker garbling).

use std::io::Write;
use std::sync::{Arc, Mutex};

use compositor::Compositor;

fn screen_text(comp: &mut Compositor) -> String {
    let pane = comp.get_focused_pane_mut().unwrap();
    let g = pane.terminal_emulator.grid();
    (0..g.rows)
        .map(|r| g.get_line_text(r))
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn ctrl_c_in_remote_pane_is_not_handled_by_local_shell() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    std::env::set_var("SHELL_DAEMON_STDIO_CMD", env!("CARGO_BIN_EXE_shell"));

    let sink: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(std::io::sink()));
    let mut comp = Compositor::with_output(80, 24, sink).unwrap();

    // Connect the focused pane to a remote (non-blocking; no remote frames are
    // pumped, so the pane emulator still shows only the initial local prompt).
    comp.get_focused_pane_mut()
        .unwrap()
        .connect_remote("local", &[])
        .unwrap();

    assert!(
        !screen_text(&mut comp).contains("^C"),
        "precondition: no ^C yet"
    );

    // The local shell's ctrl-c handler would paint "^C" + a fresh prompt here.
    // With the fix, the byte is forwarded to the remote and the local emulator
    // is left untouched.
    comp.handle_ctrl_c();

    assert!(
        !screen_text(&mut comp).contains("^C"),
        "Ctrl+C in a remote pane must be forwarded, not painted by the local shell"
    );
    // And the pane is still remote-connected (Ctrl+C didn't tear it down locally).
    assert!(comp.get_focused_pane_mut().unwrap().remote.is_some());
}
