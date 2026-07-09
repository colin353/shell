//! Predictive typeahead for remote panes is temporarily disabled. Printable input
//! should be forwarded to the remote without drawing a speculative local overlay.

use std::io::Write;
use std::sync::{Arc, Mutex};

use compositor::Compositor;

#[test]
fn remote_printable_input_is_not_rendered_speculatively_while_disabled() {
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
        comp.get_focused_pane_mut().unwrap().remote().is_some(),
        "pane should be connected to a remote process"
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
    let rendered = comp.global_emulator().grid().get_cell(cursor_x, cursor_y);
    assert_ne!(
        rendered.character, 'q',
        "predictive overlay should not render while remote typeahead is disabled"
    );
    assert!(
        comp.get_focused_pane_mut()
            .unwrap()
            .predicted_remote_input()
            .is_empty(),
        "disabled predictive typeahead should not retain speculative input"
    );
}
