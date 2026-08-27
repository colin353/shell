use std::io::Write;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use compositor::{Compositor, UrlModeAction};

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

fn pane_has_line(comp: &mut Compositor, needle: &str) -> bool {
    let Some(pane) = comp.get_focused_pane_mut() else {
        return false;
    };
    let grid = pane.terminal_emulator.grid();
    (0..grid.rows).any(|row| grid.get_line_text(row).trim_end() == needle)
}

#[test]
fn remote_directory_picker_validates_and_changes_cwd_on_remote_host() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    std::env::set_var("XDG_RUNTIME_DIR", dir.path());
    std::env::set_var("SHELL_DAEMON_STDIO_CMD", env!("CARGO_BIN_EXE_shell"));
    std::env::set_var("SHELL_SESSION_IDLE_EXIT_SECS", "8");

    let initial = dir.path().join("initial");
    let target = initial.join("target-dir");
    std::fs::create_dir_all(&target).unwrap();
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

    comp.handle_input(b"printf 'target-dir\\n'\r");
    assert!(poll_until(&mut comp, Duration::from_secs(8), |comp| {
        pane_has_line(comp, "target-dir") && comp.focused_shell_waiting_for_input() == Some(true)
    }));

    comp.handle_input(&[0x02]); // Ctrl+b
    comp.handle_input(b"u");
    let selected = poll_until(&mut comp, Duration::from_secs(8), |comp| {
        matches!(
            comp.root().get_current_url_action(),
            Some(UrlModeAction::ChangeDirectory(ref path)) if path == &target
        )
    });
    if !selected {
        let action = comp.root().get_current_url_action();
        let pane = comp.get_focused_pane_mut().unwrap();
        let grid = pane.terminal_emulator.grid();
        let screen = (0..grid.rows)
            .map(|row| grid.get_line_text(row))
            .collect::<Vec<_>>()
            .join("\n");
        panic!(
            "remote directory should become selectable only after remote validation; action={action:?}; screen={screen:?}"
        );
    }

    comp.handle_input(b"\r");
    assert!(poll_until(&mut comp, Duration::from_secs(8), |comp| {
        remote_cwd_is(comp, &target) && pane_has_line(comp, "target-dir ➜")
    }));

    comp.handle_input(b"touch selected-remotely\r");
    assert!(poll_until(&mut comp, Duration::from_secs(8), |_| {
        target.join("selected-remotely").exists()
    }));
    assert!(poll_until(&mut comp, Duration::from_secs(8), |comp| {
        comp.focused_shell_waiting_for_input() == Some(true)
    }));

    let nested = target.join("nested-dir");
    std::fs::create_dir(&nested).unwrap();
    let nested = nested.canonicalize().unwrap();
    comp.handle_input(b"printf 'nested-dir\\n'; sleep 5\r");
    assert!(poll_until(&mut comp, Duration::from_secs(8), |comp| {
        pane_has_line(comp, "nested-dir") && comp.focused_shell_waiting_for_input() == Some(false)
    }));

    comp.handle_input(&[0x02]); // Ctrl+b
    comp.handle_input(b"u");
    let deadline = Instant::now() + Duration::from_millis(500);
    while Instant::now() < deadline {
        let _ = comp.poll_once(20);
        assert!(
            !matches!(
                comp.root().get_current_url_action(),
                Some(UrlModeAction::ChangeDirectory(ref path)) if path == &nested
            ),
            "directories must not be selectable while a remote child is running"
        );
    }
}
