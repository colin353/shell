//! A split in a remote-owned tab auto-connects the new pane to the same host.
//!
//! Drives the compositor API directly (the shell crate depends on it) so we can
//! inspect the new pane's backend. The `local` transport runs the real `shell`
//! binary in stdio mode (via `SHELL_DAEMON_STDIO_CMD`, since `cargo test`'s
//! `current_exe()` is the test harness).

use std::io::Write;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use compositor::{Compositor, SplitDirection};

fn poll_until_remote_cwd(comp: &mut Compositor, cwd: &Path, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    while Instant::now() < deadline {
        let _ = comp.poll_once(20);
        let matches = comp
            .get_focused_pane_mut()
            .and_then(|pane| pane.remote())
            .and_then(|remote| remote.cwd())
            .is_some_and(|remote_cwd| remote_cwd == cwd);
        if matches {
            return true;
        }
    }
    false
}

fn poll_until_remote_reports_cwd(comp: &mut Compositor, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    while Instant::now() < deadline {
        let _ = comp.poll_once(20);
        let has_cwd = comp
            .get_focused_pane_mut()
            .and_then(|pane| pane.remote())
            .and_then(|remote| remote.cwd())
            .is_some();
        if has_cwd {
            return true;
        }
    }
    false
}

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
        comp.get_focused_pane_mut().unwrap().remote().is_none(),
        "split in a local tab must not connect anywhere"
    );

    // Mark the tab remote-owned; the next split should auto-connect.
    comp.active_tab_mut().remote_host = Some("local".to_string());
    comp.split_focused_pane(SplitDirection::Horizontal).unwrap();
    assert!(
        comp.get_focused_pane_mut().unwrap().remote().is_some(),
        "split in a remote-owned tab should auto-connect the new pane"
    );
}

#[test]
fn split_in_remote_tab_inherits_remote_cwd() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    std::env::set_var("SHELL_DAEMON_STDIO_CMD", env!("CARGO_BIN_EXE_shell"));
    std::env::set_var("SHELL_SESSION_IDLE_EXIT_SECS", "8");

    let remote_cwd = dir.path().join("remote-cwd");
    std::fs::create_dir(&remote_cwd).unwrap();
    let remote_cwd = remote_cwd.canonicalize().unwrap();

    let sink: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(std::io::sink()));
    let mut comp = Compositor::with_output(80, 24, sink).unwrap();

    comp.get_focused_pane_mut()
        .unwrap()
        .connect_remote("local", &[])
        .unwrap();
    assert!(
        poll_until_remote_reports_cwd(&mut comp, Duration::from_secs(8)),
        "source remote pane should report its initial cwd"
    );

    let cd = format!("cd {}\r", remote_cwd.display());
    comp.get_focused_pane_mut()
        .unwrap()
        .handle_input(cd.as_bytes());
    assert!(
        poll_until_remote_cwd(&mut comp, &remote_cwd, Duration::from_secs(8)),
        "source remote pane should report cwd after cd"
    );

    comp.active_tab_mut().remote_host = Some("local".to_string());
    comp.split_focused_pane(SplitDirection::Vertical).unwrap();
    assert!(
        poll_until_remote_cwd(&mut comp, &remote_cwd, Duration::from_secs(8)),
        "split remote pane should inherit cwd from the remote source pane"
    );
}
