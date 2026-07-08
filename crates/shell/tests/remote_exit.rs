//! Exiting a remote session closes the pane instead of returning to the local
//! shell prompt that launched `connect`.

use std::io::Write;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use compositor::{Compositor, SplitDirection};

static ENV_LOCK: Mutex<()> = Mutex::new(());

fn lock_env() -> std::sync::MutexGuard<'static, ()> {
    ENV_LOCK.lock().unwrap()
}

fn screen(comp: &mut Compositor) -> String {
    let pane = comp.get_focused_pane_mut().unwrap();
    let g = pane.terminal_emulator.grid();
    (0..g.rows)
        .map(|r| g.get_line_text(r))
        .collect::<Vec<_>>()
        .join("\n")
}

fn run_remote_marker(comp: &mut Compositor, marker: &str, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    let mut next_send = Instant::now();
    let command = format!("echo {marker}\r");
    while Instant::now() < deadline {
        let _ = comp.poll_once(20);
        if screen(comp).contains(marker) {
            return true;
        }
        if Instant::now() >= next_send {
            comp.get_focused_pane_mut()
                .unwrap()
                .handle_input(command.as_bytes());
            next_send = Instant::now() + Duration::from_millis(500);
        }
    }
    screen(comp).contains(marker)
}

fn send_until_pane_count(
    comp: &mut Compositor,
    input: &[u8],
    count: usize,
    timeout: Duration,
) -> bool {
    let deadline = Instant::now() + timeout;
    let mut next_send = Instant::now();
    while Instant::now() < deadline {
        let _ = comp.poll_once(20);
        if comp.active_tab().root.pane_count() == count {
            return true;
        }
        if Instant::now() >= next_send {
            comp.get_focused_pane_mut().unwrap().handle_input(input);
            next_send = Instant::now() + Duration::from_millis(500);
        }
    }
    comp.active_tab().root.pane_count() == count
}

fn send_until_exit(comp: &mut Compositor, input: &[u8], timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    let mut next_send = Instant::now();
    while Instant::now() < deadline {
        let _ = comp.poll_once(20);
        if comp.should_exit() {
            return true;
        }
        if Instant::now() >= next_send {
            comp.get_focused_pane_mut().unwrap().handle_input(input);
            next_send = Instant::now() + Duration::from_millis(500);
        }
    }
    comp.should_exit()
}

fn new_test_compositor() -> (Compositor, tempfile::TempDir) {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    std::env::set_var("XDG_RUNTIME_DIR", dir.path());
    std::env::set_var("SHELL_DAEMON_STDIO_CMD", env!("CARGO_BIN_EXE_shell"));
    std::env::set_var("SHELL_SESSION_IDLE_EXIT_SECS", "8");

    let sink: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(std::io::sink()));
    (Compositor::with_output(80, 24, sink).unwrap(), dir)
}

#[test]
fn local_exit_requests_compositor_exit() {
    let _guard = lock_env();
    let (mut comp, _dir) = new_test_compositor();

    assert!(comp.handle_input(b"exit\r"));
    assert!(comp.should_exit());
}

#[test]
fn exiting_remote_split_closes_that_pane() {
    let _guard = lock_env();
    let (mut comp, _dir) = new_test_compositor();

    comp.split_focused_pane(SplitDirection::Vertical).unwrap();
    assert_eq!(comp.active_tab().root.pane_count(), 2);

    comp.get_focused_pane_mut()
        .unwrap()
        .connect_remote("local", &[])
        .unwrap();
    assert!(
        run_remote_marker(&mut comp, "REMOTE_SPLIT_READY", Duration::from_secs(8)),
        "remote command should run before exit; got:\n{}",
        screen(&mut comp)
    );

    assert!(
        send_until_pane_count(&mut comp, b"\x04", 1, Duration::from_secs(8)),
        "remote pane should close after remote shell exits; panes={}, screen:\n{}",
        comp.active_tab().root.pane_count(),
        screen(&mut comp)
    );
    assert!(
        comp.get_focused_pane_mut().unwrap().remote().is_none(),
        "remaining focused pane should be local"
    );
}

#[test]
fn exiting_last_remote_pane_requests_compositor_exit() {
    let _guard = lock_env();
    let (mut comp, _dir) = new_test_compositor();

    comp.get_focused_pane_mut()
        .unwrap()
        .connect_remote("local", &[])
        .unwrap();
    assert!(
        run_remote_marker(&mut comp, "REMOTE_LAST_READY", Duration::from_secs(8)),
        "remote command should run before exit; got:\n{}",
        screen(&mut comp)
    );

    assert!(
        send_until_exit(&mut comp, b"\x04", Duration::from_secs(8)),
        "exiting the last remote pane should request compositor exit; screen:\n{}",
        screen(&mut comp)
    );
}
