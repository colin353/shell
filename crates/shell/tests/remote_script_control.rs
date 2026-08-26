use std::io::Write;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use compositor::Compositor;

fn poll_until(
    comp: &mut Compositor,
    timeout: Duration,
    mut predicate: impl FnMut(&mut Compositor) -> bool,
) -> bool {
    let deadline = Instant::now() + timeout;
    while Instant::now() < deadline {
        let _ = comp.poll_once(20);
        if predicate(comp) {
            return true;
        }
    }
    predicate(comp)
}

fn remote_cwd_is(comp: &mut Compositor, expected: &Path) -> bool {
    comp.get_focused_pane_mut()
        .and_then(|pane| pane.remote())
        .and_then(|remote| remote.cwd())
        .is_some_and(|cwd| cwd == expected)
}

#[test]
fn remote_script_controls_propagate_and_survive_reconnect() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    std::env::set_var("XDG_RUNTIME_DIR", dir.path());
    std::env::set_var("SHELL_DAEMON_STDIO_CMD", env!("CARGO_BIN_EXE_shell"));
    std::env::set_var("SHELL_SESSION_IDLE_EXIT_SECS", "8");

    let initial = dir.path().join("initial");
    let target = dir.path().join("target");
    std::fs::create_dir(&initial).unwrap();
    std::fs::create_dir(&target).unwrap();
    let initial = initial.canonicalize().unwrap();
    let target = target.canonicalize().unwrap();

    let sink: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(std::io::sink()));
    let mut comp = Compositor::with_output(80, 24, sink).unwrap();
    comp.get_focused_pane_mut()
        .unwrap()
        .connect_remote_with_cwd("local", &[], Some(&initial))
        .unwrap();
    assert!(poll_until(&mut comp, Duration::from_secs(8), |comp| {
        remote_cwd_is(comp, &initial)
    }));

    let shell = env!("CARGO_BIN_EXE_shell");
    comp.get_focused_pane_mut()
        .unwrap()
        .handle_input(format!("{shell} control set-cwd {}\r", target.display()).as_bytes());
    assert!(
        poll_until(&mut comp, Duration::from_secs(8), |comp| {
            remote_cwd_is(comp, &target)
        }),
        "remote daemon should publish the script-selected cwd"
    );

    comp.get_focused_pane_mut()
        .unwrap()
        .handle_input(format!("{shell} control rename-window remote-control-title\r").as_bytes());
    assert!(
        poll_until(&mut comp, Duration::from_secs(8), |comp| {
            comp.active_tab().name == "remote-control-title"
        }),
        "remote daemon should publish the script-selected title"
    );

    // Drop only the transport. The remote daemon and its authoritative cwd and
    // title remain alive. Clobber the local title so replay is observable.
    let pid = comp
        .get_focused_pane_mut()
        .unwrap()
        .remote()
        .unwrap()
        .transport_pid();
    unsafe {
        libc::kill(pid as libc::pid_t, libc::SIGKILL);
    }
    comp.active_tab_mut().name = "temporary-local-title".to_string();

    assert!(
        poll_until(&mut comp, Duration::from_secs(15), |comp| {
            comp.active_tab().name == "remote-control-title" && remote_cwd_is(comp, &target)
        }),
        "reattach should replay both remote cwd and explicit title"
    );

    // Prove reconnect did not reapply the pane's original cwd: run a command
    // after the transport returns and inspect which directory received it.
    let marker = "after-reconnect-marker";
    let deadline = Instant::now() + Duration::from_secs(15);
    let mut next_send = Instant::now();
    while Instant::now() < deadline && !target.join(marker).exists() {
        let _ = comp.poll_once(20);
        if Instant::now() >= next_send {
            comp.get_focused_pane_mut()
                .unwrap()
                .handle_input(format!("touch {marker}\r").as_bytes());
            next_send = Instant::now() + Duration::from_secs(2);
        }
    }
    assert!(
        target.join(marker).exists(),
        "command should use restored cwd"
    );
    assert!(
        !initial.join(marker).exists(),
        "reconnect must not overwrite cwd with its initial value"
    );
}
