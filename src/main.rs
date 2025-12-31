use std::time::Duration;

mod emulator;
mod input;
mod pane;
mod pty;
mod session;
mod terminal;
mod tui;

use raw_tty::GuardMode;
use tui::{AppController, Transition};

fn main() {
    let mut pane = pane::Pane::new();
    let term = tui::Terminal::new();

    // We need to handle events differently now - we'll poll both keyboard and PTY
    let tty = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/tty")
        .unwrap();
    let mut tty_input = tty.try_clone().unwrap().guard_mode().unwrap();
    tty_input.set_raw_mode().unwrap();

    // Create the app with initial state
    let mut state = pane.initial_state();
    let mut terminal = term;

    // Initial render
    tui::AppController::render(&mut pane, &mut terminal, &state, None);
    update_cursor(&mut terminal);

    // KeyboardEventStream spawns its own thread for reading
    let mut keyboard_stream = tui::KeyboardEventStream::new(tty_input);

    loop {
        // Determine timeout based on whether we have a running session
        let timeout = if pane.has_session() {
            Duration::from_millis(10)
        } else {
            Duration::from_millis(100)
        };

        // Check for keyboard input with timeout
        if let Some(event) = keyboard_stream.try_next(timeout) {
            let transition = tui::AppController::transition(&mut pane, &state, event);
            match handle_transition(&mut pane, &mut terminal, &mut state, transition) {
                Some(true) => return, // Terminate
                _ => {}
            }
        }

        // Poll session for output
        if pane.has_session() {
            if let Some(new_state) = pane.poll_session(&state) {
                let prev_state = state.clone();
                state = new_state;
                tui::AppController::render(&mut pane, &mut terminal, &state, Some(&prev_state));
                update_cursor(&mut terminal);
            }
        }
    }
}

fn update_cursor(terminal: &mut tui::Terminal) {
    if let Some((x, y)) = terminal.get_focus() {
        terminal.move_cursor_to(x, y);
        terminal.show_cursor();
    } else {
        terminal.hide_cursor();
    }
}

fn handle_transition(
    pane: &mut pane::Pane,
    terminal: &mut tui::Terminal,
    state: &mut pane::PaneState,
    transition: Transition<pane::PaneState>,
) -> Option<bool> {
    match transition {
        Transition::Updated(new_state) => {
            terminal.hide_cursor();
            tui::AppController::render(pane, terminal, &new_state, Some(state));
            *state = new_state;
            update_cursor(terminal);
            None
        }
        Transition::Terminate(_) => {
            tui::AppController::clean_up(pane, terminal);
            Some(true)
        }
        Transition::Finished(_) => {
            tui::AppController::clean_up(pane, terminal);
            Some(true)
        }
        Transition::Nothing => None,
    }
}
