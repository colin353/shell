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

fn focused_cwd(comp: &mut Compositor) -> &Path {
    comp.get_focused_pane_mut().unwrap().shell.cwd()
}

#[test]
fn foreground_script_can_set_cwd_and_rename_its_window() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    let target = dir.path().join("project");
    std::fs::create_dir(&target).unwrap();
    let target = target.canonicalize().unwrap();

    let sink: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(std::io::sink()));
    let mut comp = Compositor::with_output(80, 24, sink).unwrap();
    let shell = env!("CARGO_BIN_EXE_shell");

    comp.handle_input(format!("{shell} control set-cwd {}\r", target.display()).as_bytes());
    assert!(
        poll_until(&mut comp, Duration::from_secs(5), |comp| focused_cwd(comp)
            == target),
        "control request should update the parent shell cwd"
    );

    comp.handle_input(format!("{shell} control rename-window script-control-title\r").as_bytes());
    assert!(
        poll_until(&mut comp, Duration::from_secs(5), |comp| {
            comp.active_tab().name == "script-control-title"
        }),
        "control request should rename the containing tab"
    );

    // A copied or forged control sequence has no access to the current
    // foreground process's capability and must not mutate state.
    comp.handle_input(
        b"printf '\\033]777;shell-control;1;wrong;rename-window;Zm9yZ2Vk\\033\\\\'\r",
    );
    let _ = poll_until(&mut comp, Duration::from_millis(500), |comp| {
        comp.get_focused_pane_mut()
            .is_some_and(|pane| !pane.has_subprocess())
    });
    assert_eq!(comp.active_tab().name, "script-control-title");

    comp.handle_input(
        format!("sleep 0.2; {shell} control rename-window background-title\r").as_bytes(),
    );
    comp.create_tab().unwrap();
    assert_eq!(comp.active_tab_index(), 1);

    assert!(poll_until(&mut comp, Duration::from_secs(5), |comp| {
        comp.tabs[0].name == "background-title"
    }));
    assert_eq!(comp.tabs[1].name, "bash");
}
