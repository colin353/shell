//! Splitting a pane should open the new pane in the same working directory as
//! the pane the split was initiated from (not the process's startup cwd).

use std::io::Write;
use std::sync::{Arc, Mutex};

use compositor::{Compositor, SplitDirection};

fn cwd_of_focused(comp: &mut Compositor) -> std::path::PathBuf {
    comp.get_focused_pane_mut().unwrap().shell.cwd().to_path_buf()
}

#[test]
fn split_inherits_originating_pane_cwd() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    let sub = dir.path().join("subdir");
    std::fs::create_dir(&sub).unwrap();
    let sub_canon = sub.canonicalize().unwrap();

    // Isolated history file so the test never touches the real one.
    let core = Arc::new(libshell::ShellCore::with_history_path(dir.path().join("history.log")).unwrap());
    let sink: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(std::io::sink()));
    let mut comp = Compositor::with_core(80, 24, sink, core).unwrap();

    // cd the focused pane into the subdir (a synchronous builtin).
    let cd = format!("cd {}\r", sub_canon.display());
    comp.get_focused_pane_mut()
        .unwrap()
        .handle_input(cd.as_bytes());
    assert_eq!(
        cwd_of_focused(&mut comp),
        sub_canon,
        "sanity: cd should move the focused pane"
    );

    // Split — the new (now focused) pane must start in the same directory.
    comp.split_focused_pane(SplitDirection::Vertical).unwrap();
    assert_eq!(
        cwd_of_focused(&mut comp),
        sub_canon,
        "the split pane should inherit the originating pane's cwd"
    );
}
