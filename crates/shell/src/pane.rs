use crate::{input, session::Session, terminal};
use std::sync::{Arc, Mutex};
use terminal::{Line, TermEmulator, TermEmulatorState};
use tui::{AppController, Component, KeyboardEvent, Transition};

pub struct Pane {
    input_component: input::Input,
    term_emulator: terminal::TermEmulator,
}

impl Pane {
    pub fn new() -> Self {
        Self {
            input_component: input::Input::new(" $".to_string(), "Type a command...".to_string()),
            term_emulator: TermEmulator::new(),
        }
    }

    /// Forward a keyboard event to the running session.
    /// Returns true if the event was forwarded, false if no session is running.
    fn forward_to_session(session: &Arc<Mutex<Session>>, event: &tui::KeyboardEvent) {
        let bytes = event.to_bytes();
        if !bytes.is_empty() {
            let session = session.lock().unwrap();
            let _ = session.write(&bytes);
        }
    }

    /// Poll for output from running session and update state if needed.
    /// Returns Some(new_state) if there's new output, None otherwise.
    pub fn poll_session(&mut self, state: &PaneState) -> Option<PaneState> {
        if let Some(ref session_arc) = state.session {
            let mut session = session_arc.lock().unwrap();
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

                        return Some(PaneState {
                            input_state: state.input_state.clone(),
                            term_state: new_term_state,
                            session: None,
                            cols: state.cols,
                            rows: state.rows,
                            shown_line_count: 0,
                        });
                    }

                    // Session still running - signal update if there was output
                    // (so the UI can re-render the live session view)
                    if had_output {
                        return Some(PaneState {
                            input_state: state.input_state.clone(),
                            term_state: state.term_state.clone(),
                            session: Some(Arc::clone(session_arc)),
                            cols: state.cols,
                            rows: state.rows,
                            shown_line_count: state.shown_line_count,
                        });
                    }

                    None
                }
                Err(_) => {
                    // Error reading, clean up session
                    Some(PaneState {
                        input_state: state.input_state.clone(),
                        term_state: state.term_state.clone(),
                        session: None,
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
    pub fn has_session(&self, state: &PaneState) -> bool {
        state.session.is_some()
    }

    /// Get the current lines from the active session (for direct rendering)
    pub fn get_session_lines(&self, state: &PaneState) -> Option<Vec<Line>> {
        state.session.as_ref().map(|session_arc| {
            let session = session_arc.lock().unwrap();
            session.get_lines()
        })
    }

    /// Handle terminal resize - updates dimensions and propagates to running session
    pub fn resize(&mut self, state: &PaneState, cols: u16, rows: u16) -> PaneState {
        // Calculate the available rows for the pane (subtract 2 for input area when no session)
        let pane_rows = if state.session.is_some() {
            rows
        } else {
            rows.saturating_sub(2)
        };

        // If there's a running session, propagate the resize
        if let Some(ref session_arc) = state.session {
            let mut session = session_arc.lock().unwrap();
            let _ = session.resize(cols, pane_rows);
        }

        PaneState {
            input_state: state.input_state.clone(),
            term_state: state.term_state.clone(),
            session: state.session.clone(),
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
            session: None,
            cols,
            rows,
            shown_line_count: 0,
        }
    }
}

#[derive(Clone)]
pub struct PaneState {
    input_state: input::InputState,
    term_state: TermEmulatorState,
    /// Currently running session (if any)
    session: Option<Arc<Mutex<Session>>>,
    /// Terminal dimensions for spawning processes
    cols: u16,
    rows: u16,
    /// Number of lines already shown from the current session
    shown_line_count: usize,
}

impl PartialEq for PaneState {
    fn eq(&self, other: &Self) -> bool {
        self.input_state == other.input_state
            && self.term_state == other.term_state
            && self.cols == other.cols
            && self.rows == other.rows
            && self.shown_line_count == other.shown_line_count
            // Compare session by pointer equality (both None, or both point to same Arc)
            && match (&self.session, &other.session) {
                (None, None) => true,
                (Some(a), Some(b)) => Arc::ptr_eq(a, b),
                _ => false,
            }
    }
}

impl tui::AppController<PaneState> for Pane {
    fn initial_state(&self) -> PaneState {
        PaneState {
            input_state: input::InputState::new(),
            term_state: TermEmulatorState::new(),
            session: None,
            cols: 80,
            rows: 24,
            shown_line_count: 0,
        }
    }

    fn clean_up(&self, term: &mut tui::Terminal) {
        term.clean_up();
    }

    fn render(&mut self, t: &mut tui::Terminal, state: &PaneState, prev_state: Option<&PaneState>) {
        // Use state.session for rendering decisions
        let has_session = state.session.is_some();
        let had_session = prev_state.map(|ps| ps.session.is_some()).unwrap_or(false);

        let mut tt = t.derive("term_emulator".to_string());
        if has_session {
            tt.height = t.height;

            // When session is active, render directly from session's grid
            if let Some(ref session_arc) = state.session {
                let session = session_arc.lock().unwrap();
                let session_lines = session.get_lines();
                let live_term_state = TermEmulatorState {
                    lines: session_lines,
                };

                // Force full redraw for live session content (no prev_state comparison)
                self.term_emulator.render(&mut tt, &live_term_state, None);
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
            if prev_state.map(|ps| ps.session.is_some()) != Some(false) {
                // We previously had a session, so the input needs to be redrawn.
                prev_input_state = None;
            }

            AppController::render(
                &mut self.input_component,
                &mut it,
                &state.input_state,
                prev_input_state,
            );
        } else {
            // Session is running - just unset focus (no input shown)
            t.unset_focus();
        }
    }

    fn transition(&mut self, state: &PaneState, event: KeyboardEvent) -> Transition<PaneState> {
        // If a session is running, forward keyboard events to it
        if let Some(ref session) = state.session {
            Self::forward_to_session(session, &event);
            // Return Nothing - the main loop's poll_session will handle any resulting output
            return Transition::Nothing;
        }

        if state.input_state.focused {
            return match input::transition(&state.input_state, event) {
                Transition::Updated(new_input_state) => Transition::Updated(PaneState {
                    input_state: new_input_state,
                    term_state: state.term_state.clone(),
                    session: state.session.clone(),
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

                    let mut new_session = None;

                    // Skip empty commands
                    if !command.trim().is_empty() {
                        // Spawn the command in a PTY session
                        // Session gets full terminal height since input is hidden during session
                        match Session::spawn(&command, state.cols, state.rows) {
                            Ok(session) => {
                                new_session = Some(Arc::new(Mutex::new(session)));
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
                        session: new_session,
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
                    session: state.session.clone(),
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
                    session: state.session.clone(),
                    cols: state.cols,
                    rows: state.rows,
                    shown_line_count: state.shown_line_count,
                })
            }

            _ => Transition::Nothing,
        };
    }
}
