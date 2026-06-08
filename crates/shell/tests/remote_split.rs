//! A split in a remote-owned tab auto-connects the new pane to the same host.
//!
//! Drives the compositor API directly (the shell crate depends on it) so we can
//! inspect the new pane's backend. The `local` transport runs the real `shell`
//! binary in stdio mode (via `SHELL_DAEMON_STDIO_CMD`, since `cargo test`'s
//! `current_exe()` is the test harness).

use std::io::Write;
use std::sync::{Arc, Mutex};

use compositor::{Compositor, SplitDirection};

#[test]
fn split_in_remote_tab_auto_connects() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    std::env::set_var("SHELL_DAEMON_STDIO_CMD", env!("CARGO_BIN_EXE_shell"));
    std::env::set_var("SHELL_SESSION_IDLE_EXIT_SECS", "8");

    let sink: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(std::io::sink()));
    let mut comp = Compositor::with_output(80, 24, sink).unwrap();

    // A fresh tab is local: a split stays local.
    comp.split_focused_pane(SplitDirection::Vertical).unwrap();
    assert!(
        comp.get_focused_pane_mut().unwrap().remote.is_none(),
        "split in a local tab must not connect anywhere"
    );

    // Mark the tab remote-owned; the next split should auto-connect.
    comp.active_tab_mut().remote_host = Some("local".to_string());
    comp.split_focused_pane(SplitDirection::Horizontal).unwrap();
    assert!(
        comp.get_focused_pane_mut().unwrap().remote.is_some(),
        "split in a remote-owned tab should auto-connect the new pane"
    );
}
