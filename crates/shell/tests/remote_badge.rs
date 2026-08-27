//! Codex's title-derived badge is computed by the bare remote daemon and
//! propagated to the outer compositor, including when a session is reattached.

use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use compositor::{Badge, Compositor};

fn poll_for_badge(comp: &mut Compositor, expected: Badge, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    while Instant::now() < deadline {
        let _ = comp.poll_once(20);
        if comp.active_tab().badge() == expected {
            return true;
        }
    }
    comp.active_tab().badge() == expected
}

fn compositor() -> Compositor {
    let sink: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(std::io::sink()));
    Compositor::with_output(80, 24, sink).unwrap()
}

#[test]
fn remote_codex_badge_updates_and_survives_reattach() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    std::env::set_var("XDG_RUNTIME_DIR", dir.path());
    std::env::set_var("SHELL_DAEMON_STDIO_CMD", env!("CARGO_BIN_EXE_shell"));
    std::env::set_var("SHELL_SESSION_IDLE_EXIT_SECS", "8");

    let codex = dir.path().join("codex");
    let release = dir.path().join("release-agent");
    std::fs::write(
        &codex,
        "#!/bin/sh\n\
         printf '\\033]0;⠋ | project\\007'\n\
         while [ ! -e \"$1\" ]; do sleep 0.05; done\n\
         printf '\\033]0;[ . ] Action Required | project\\007'\n\
         while :; do sleep 1; done\n",
    )
    .unwrap();
    let mut permissions = std::fs::metadata(&codex).unwrap().permissions();
    permissions.set_mode(0o755);
    std::fs::set_permissions(&codex, permissions).unwrap();

    let session = format!("badge-test-{}", std::process::id());
    let mut first = compositor();
    first
        .get_focused_pane_mut()
        .unwrap()
        .connect_remote_session("local", Some(&session), &[])
        .unwrap();
    first
        .get_focused_pane_mut()
        .unwrap()
        .handle_input(format!("{} {}\r", codex.display(), release.display()).as_bytes());

    assert!(
        poll_for_badge(&mut first, Badge::AgentWorking, Duration::from_secs(8)),
        "outer tab should show the remote Codex working badge"
    );

    // Dropping the transport leaves the named remote daemon and Codex process
    // alive. A fresh outer pane must receive the badge during attach, even
    // though the bare daemon's rendered grid has no status bar.
    drop(first);
    let mut second = compositor();
    second
        .get_focused_pane_mut()
        .unwrap()
        .connect_remote_session("local", Some(&session), &[])
        .unwrap();
    assert!(
        poll_for_badge(&mut second, Badge::AgentWorking, Duration::from_secs(8)),
        "reattach should replay the remote Codex badge"
    );

    std::fs::write(&release, b"ready").unwrap();
    assert!(
        poll_for_badge(
            &mut second,
            Badge::AgentNeedsInput,
            Duration::from_secs(8)
        ),
        "live remote title changes should update the outer badge"
    );

    // Stop the helper so the detached daemon does not retain a sleeping child.
    second
        .get_focused_pane_mut()
        .unwrap()
        .handle_input(b"\x03");
    assert!(
        poll_for_badge(&mut second, Badge::ShellPrompt, Duration::from_secs(8)),
        "stopping the remote helper should restore the shell badge"
    );
}
