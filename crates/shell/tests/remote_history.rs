//! History dual-write: a command run in a remote pane is mirrored into the
//! local history, tagged `Remote(host)`, so it persists locally after the pane
//! closes.
//!
//! The compositor is built over a `ShellCore` the test also holds, so it can
//! inspect the local history directly. The pane connects to the `local`
//! transport (the built `shell` binary as a stand-in for `ssh host`); the inner
//! daemon ships each executed command back as `HistoryRecorded`, which the pane
//! mirrors into this core.

use std::io::Write;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use compositor::Compositor;
use protocol::{HostId, Origin};

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
fn remote_command_is_mirrored_into_local_history() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    std::env::set_var("SHELL_DAEMON_STDIO_CMD", env!("CARGO_BIN_EXE_shell"));
    std::env::set_var("SHELL_SESSION_IDLE_EXIT_SECS", "20");

    // Build the compositor over a core we keep a handle to, so we can read the
    // local history the dual-write lands in.
    let core = Arc::new(libshell::ShellCore::new().unwrap());
    let sink: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(std::io::sink()));
    let mut comp = Compositor::with_core(80, 24, sink, core.clone()).unwrap();

    comp.get_focused_pane_mut()
        .unwrap()
        .connect_remote_session("local", None, &[])
        .unwrap();
    assert!(poll_until(&mut comp, "➜", Duration::from_secs(8)), "prompt");

    comp.get_focused_pane_mut()
        .unwrap()
        .handle_input(b"echo DUALWRITE_MARKER\r");
    assert!(
        poll_until(&mut comp, "DUALWRITE_MARKER", Duration::from_secs(8)),
        "command should run on the remote; got:\n{}",
        screen(&mut comp)
    );

    // The HistoryRecorded frame arrives shortly after the command's exit; poll a
    // little longer to let the pane drain and mirror it.
    let host = HostId("local".to_string());
    let deadline = Instant::now() + Duration::from_secs(8);
    let mut mirrored = false;
    while Instant::now() < deadline && !mirrored {
        let _ = comp.poll_once(20);
        mirrored = core.history().recent(50).iter().any(|e| {
            e.command.contains("echo DUALWRITE_MARKER") && e.origin == Origin::Remote(host.clone())
        });
    }

    let recent = core.history().recent(50);
    assert!(
        mirrored,
        "remote command should be mirrored into local history tagged Remote(\"local\"); \
         entries: {:?}",
        recent
            .iter()
            .map(|e| (&e.command, &e.origin))
            .collect::<Vec<_>>()
    );
}
