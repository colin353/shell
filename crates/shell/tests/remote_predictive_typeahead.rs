//! Predictive typeahead for remote panes: printable input can be drawn locally
//! while waiting for the remote echo, without mutating the authoritative pane
//! emulator.

use std::io::Write;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use compositor::Compositor;

fn screen(comp: &mut Compositor) -> String {
    let pane = comp.get_focused_pane_mut().unwrap();
    let g = pane.terminal_emulator.grid();
    (0..g.rows)
        .map(|r| g.get_line_text(r))
        .collect::<Vec<_>>()
        .join("\n")
}

fn poll_until_remote_echo_mode(comp: &mut Compositor, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    while Instant::now() < deadline {
        let _ = comp.poll_once(20);
        let echo = comp
            .get_focused_pane_mut()
            .and_then(|pane| pane.remote())
            .is_some_and(|remote| remote.input_echo_mode());
        if echo {
            return true;
        }
    }
    false
}

fn poll_until_prediction_confirmed(
    comp: &mut Compositor,
    x: usize,
    y: usize,
    timeout: Duration,
) -> bool {
    let deadline = Instant::now() + timeout;
    while Instant::now() < deadline {
        let _ = comp.poll_once(20);
        let pane = comp.get_focused_pane_mut().unwrap();
        let confirmed = pane.terminal_emulator.grid().get_cell(x, y).character == 'q'
            && pane.predicted_remote_input().is_empty();
        if confirmed {
            return true;
        }
    }
    false
}

#[test]
fn remote_printable_input_is_rendered_speculatively() {
    let dir = tempfile::tempdir().unwrap();
    std::env::set_var("HOME", dir.path());
    std::env::set_var("XDG_RUNTIME_DIR", dir.path());
    std::env::set_var("SHELL_DAEMON_STDIO_CMD", env!("CARGO_BIN_EXE_shell"));
    std::env::set_var("SHELL_SESSION_IDLE_EXIT_SECS", "8");

    let sink: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(std::io::sink()));
    let mut comp = Compositor::with_output(80, 24, sink).unwrap();
    comp.get_focused_pane_mut()
        .unwrap()
        .connect_remote("local", &[])
        .unwrap();

    assert!(
        poll_until_remote_echo_mode(&mut comp, Duration::from_secs(8)),
        "remote should report echo mode at the shell prompt; got:\n{}",
        screen(&mut comp)
    );

    let (cursor_x, cursor_y) = comp
        .get_focused_pane_mut()
        .unwrap()
        .terminal_emulator
        .cursor_position();

    comp.get_focused_pane_mut().unwrap().handle_input(b"q");

    let authoritative_cell = comp
        .get_focused_pane_mut()
        .unwrap()
        .terminal_emulator
        .grid()
        .get_cell(cursor_x, cursor_y)
        .clone();
    assert_ne!(
        authoritative_cell.character, 'q',
        "prediction should not mutate the authoritative pane emulator"
    );

    comp.render_to_vec();
    let predicted = comp.global_emulator().grid().get_cell(cursor_x, cursor_y);
    assert_eq!(predicted.character, 'q');
    assert!(predicted.attrs.dim);
    assert!(predicted.attrs.underline);

    assert!(
        poll_until_prediction_confirmed(&mut comp, cursor_x, cursor_y, Duration::from_secs(8)),
        "remote echo should replace the speculative overlay"
    );
}
