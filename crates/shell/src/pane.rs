use crate::{input, session::Session, terminal};
use emulator::TerminalGrid;
use terminal::{Line, TermEmulator, TermEmulatorState};
use tui::{AppController, Component, KeyboardEvent, Transition};

pub struct Pane {
    input_component: input::Input,
    term_emulator: terminal::TermEmulator,
    /// Currently running session (if any) - owned by Pane, not PaneState
    session: Option<Session>,
}

impl Pane {
    pub fn new() -> Self {
        Self {
            input_component: input::Input::new(" $".to_string(), "Type a command...".to_string()),
            term_emulator: TermEmulator::new(),
            session: None,
        }
    }

    /// Forward a keyboard event to the running session.
    /// Returns true if the event was forwarded, false if no session is running.
    fn forward_to_session(&self, event: &tui::KeyboardEvent) -> bool {
        if let Some(ref session) = self.session {
            let bytes = event.to_bytes();
            if !bytes.is_empty() {
                let _ = session.write(&bytes);
            }
            true
        } else {
            false
        }
    }

    /// Poll for output from running session and update state if needed.
    /// Returns Some(new_state) if there's new output, None otherwise.
    pub fn poll_session(&mut self, state: &PaneState) -> Option<PaneState> {
        if let Some(ref mut session) = self.session {
            match session.poll() {
                Ok(had_output) => {
                    let still_running = session.is_running();

                    // If process exited, capture final output to history and clean up session
                    if !still_running {
                        // Get final lines from the emulator
                        let session_lines = session.get_lines();

                        // Filter to non-empty lines for history
                        let non_empty_lines: Vec<_> = session_lines
                            .into_iter()
                            .filter(|line| match line {
                                Line::Text(ref text) => !text.is_empty(),
                                Line::Cells(ref cells) => !cells.is_empty(),
                                Line::Command(_) => true,
                            })
                            .collect();

                        let mut new_term_state = state.term_state.clone();
                        for line in non_empty_lines {
                            new_term_state.lines.push(line);
                        }

                        // Clear the session
                        self.session = None;

                        return Some(PaneState {
                            input_state: state.input_state.clone(),
                            term_state: new_term_state,
                            session_grid: None,
                            has_session: false,
                            cols: state.cols,
                            rows: state.rows,
                            shown_line_count: 0,
                        });
                    }

                    // Session still running - update session_grid if there was output
                    if had_output {
                        return Some(PaneState {
                            input_state: state.input_state.clone(),
                            term_state: state.term_state.clone(),
                            session_grid: Some(session.grid().clone()),
                            has_session: true,
                            cols: state.cols,
                            rows: state.rows,
                            shown_line_count: state.shown_line_count,
                        });
                    }

                    None
                }
                Err(_) => {
                    // Error reading, clean up session
                    self.session = None;
                    Some(PaneState {
                        input_state: state.input_state.clone(),
                        term_state: state.term_state.clone(),
                        session_grid: None,
                        has_session: false,
                        cols: state.cols,
                        rows: state.rows,
                        shown_line_count: 0,
                    })
                }
            }
        } else {
            None
        }
    }

    /// Check if there's a running session
    pub fn has_session(&self) -> bool {
        self.session.is_some()
    }

    /// Handle terminal resize - updates dimensions and propagates to running session
    pub fn resize(&mut self, state: &PaneState, cols: u16, rows: u16) -> PaneState {
        // Calculate the available rows for the pane (subtract 2 for input area when no session)
        let pane_rows = if self.session.is_some() {
            rows
        } else {
            rows.saturating_sub(2)
        };

        // If there's a running session, propagate the resize
        if let Some(ref mut session) = self.session {
            let _ = session.resize(cols, pane_rows);
        }

        PaneState {
            input_state: state.input_state.clone(),
            term_state: state.term_state.clone(),
            session_grid: state.session_grid.clone(),
            has_session: state.has_session,
            cols,
            rows,
            shown_line_count: state.shown_line_count,
        }
    }

    /// Create initial state with specified terminal dimensions
    pub fn initial_state_with_size(&self, cols: u16, rows: u16) -> PaneState {
        PaneState {
            input_state: input::InputState::new(),
            term_state: TermEmulatorState::new(),
            session_grid: None,
            has_session: false,
            cols,
            rows,
            shown_line_count: 0,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct PaneState {
    input_state: input::InputState,
    /// Terminal state for history view (when no session is active)
    term_state: TermEmulatorState,
    /// Terminal grid for active session (used for delta rendering)
    session_grid: Option<TerminalGrid>,
    /// Whether a session is currently running (the session itself is owned by Pane)
    has_session: bool,
    /// Terminal dimensions for spawning processes
    cols: u16,
    rows: u16,
    /// Number of lines already shown from the current session
    shown_line_count: usize,
}

impl tui::AppController<PaneState> for Pane {
    fn initial_state(&self) -> PaneState {
        PaneState {
            input_state: input::InputState::new(),
            term_state: TermEmulatorState::new(),
            session_grid: None,
            has_session: false,
            cols: 80,
            rows: 24,
            shown_line_count: 0,
        }
    }

    fn clean_up(&self, term: &mut tui::Terminal) {
        term.clean_up();
    }

    fn render(&mut self, t: &mut tui::Terminal, state: &PaneState, prev_state: Option<&PaneState>) {
        let has_session = state.has_session;
        let had_session = prev_state.map(|ps| ps.has_session).unwrap_or(false);

        let mut tt = t.derive("term_emulator".to_string());
        if has_session {
            tt.height = t.height;

            // Use delta rendering with session_grid from state
            if let Some(ref current_grid) = state.session_grid {
                // Get previous grid from prev_state if available
                let prev_grid = prev_state.and_then(|ps| ps.session_grid.as_ref());

                if let Some(prev_grid) = prev_grid {
                    // Compute and apply delta
                    let delta = emulator::compute_delta(prev_grid, current_grid);
                    t.write_raw(&delta);
                } else {
                    // First render - do a full clear and render
                    for y in 0..t.height {
                        t.move_cursor_to(0, y);
                        t.clear_line();
                    }
                    // Use an empty grid as the previous state to generate full output
                    let empty_grid = TerminalGrid::new(current_grid.cols, current_grid.rows);
                    let delta = emulator::compute_delta(&empty_grid, current_grid);
                    t.write_raw(&delta);
                }

                // Set focus based on cursor visibility and position
                if current_grid.cursor_visible {
                    t.set_focus(current_grid.cursor_x, current_grid.cursor_y);
                } else {
                    t.unset_focus();
                }
            }
        } else {
            tt.height = t.height - 2;

            // Force full redraw if we just transitioned from having a session
            // (need to clear the session output from the screen)
            let term_prev_state = if had_session {
                None
            } else {
                prev_state.map(|ps| &ps.term_state)
            };

            self.term_emulator
                .render(&mut tt, &state.term_state, term_prev_state);
        }

        // Only show input when no session is running
        if !has_session {
            let mut it = t.derive("input".to_string());
            it.height = 2;
            it.offset_y = t.height - 2;

            let mut prev_input_state = prev_state.map(|ps| &ps.input_state);
            if prev_state.map(|ps| ps.has_session) != Some(false) {
                // We previously had a session, so the input needs to be redrawn.
                prev_input_state = None;
            }

            AppController::render(
                &mut self.input_component,
                &mut it,
                &state.input_state,
                prev_input_state,
            );
        }
    }

    fn transition(&mut self, state: &PaneState, event: KeyboardEvent) -> Transition<PaneState> {
        // If a session is running, forward keyboard events to it
        if self.session.is_some() {
            self.forward_to_session(&event);
            // Return Nothing - the main loop's poll_session will handle any resulting output
            return Transition::Nothing;
        }

        if state.input_state.focused {
            return match input::transition(&state.input_state, event) {
                Transition::Updated(new_input_state) => Transition::Updated(PaneState {
                    input_state: new_input_state,
                    term_state: state.term_state.clone(),
                    session_grid: state.session_grid.clone(),
                    has_session: state.has_session,
                    cols: state.cols,
                    rows: state.rows,
                    shown_line_count: state.shown_line_count,
                }),
                Transition::Terminate(_) => Transition::Terminate(state.clone()),
                Transition::Nothing => Transition::Nothing,
                Transition::Finished(_) => {
                    let command = state.input_state.value.clone();
                    let mut new_term_state = state.term_state.clone();
                    new_term_state.lines.push(Line::Command(command.clone()));

                    let mut has_session = false;
                    let mut session_grid = None;

                    // Skip empty commands
                    if !command.trim().is_empty() {
                        // Spawn the command in a PTY session
                        // Session gets full terminal height since input is hidden during session
                        match Session::spawn(&command, state.cols, state.rows) {
                            Ok(session) => {
                                // Capture initial grid state
                                session_grid = Some(session.grid().clone());
                                self.session = Some(session);
                                has_session = true;
                            }
                            Err(e) => {
                                // Show error in terminal
                                new_term_state
                                    .lines
                                    .push(Line::Text(format!("Error: {}", e)));
                            }
                        }
                    }

                    let new_input_state = input::InputState::new();
                    Transition::Updated(PaneState {
                        input_state: new_input_state,
                        term_state: new_term_state,
                        session_grid,
                        has_session,
                        cols: state.cols,
                        rows: state.rows,
                        shown_line_count: 0,
                    })
                }
            };
        }

        return match event {
            KeyboardEvent::Character('/') => {
                let mut new_input_state = state.input_state.clone();
                new_input_state.focused = true;
                Transition::Updated(PaneState {
                    input_state: new_input_state,
                    term_state: state.term_state.clone(),
                    session_grid: state.session_grid.clone(),
                    has_session: state.has_session,
                    cols: state.cols,
                    rows: state.rows,
                    shown_line_count: state.shown_line_count,
                })
            }
            // App termination
            KeyboardEvent::CtrlC | KeyboardEvent::CtrlD => Transition::Terminate(state.clone()),

            // Return result
            KeyboardEvent::Enter => Transition::Finished(state.clone()),

            // Select result
            KeyboardEvent::UpArrow | KeyboardEvent::Character('k') => {
                Transition::Updated(PaneState {
                    input_state: state.input_state.clone(),
                    term_state: state.term_state.clone(),
                    session_grid: state.session_grid.clone(),
                    has_session: state.has_session,
                    cols: state.cols,
                    rows: state.rows,
                    shown_line_count: state.shown_line_count,
                })
            }

            _ => Transition::Nothing,
        };
    }
}
