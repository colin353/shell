use crate::{input, session::Session, terminal, tui};
use std::os::fd::RawFd;
use terminal::{Line, TermEmulator, TermEmulatorState};
use tui::{AppController, Component, KeyboardEvent, Transition};

pub struct Pane {
    input_component: input::Input,
    term_emulator: terminal::TermEmulator,
    /// Currently running session (if any)
    session: Option<Session>,
    /// Terminal dimensions for spawning processes
    cols: u16,
    rows: u16,
    /// Number of lines already shown from the current session
    shown_line_count: usize,
}

impl Pane {
    pub fn new() -> Self {
        Self {
            input_component: input::Input::new(" $".to_string(), "Type a command...".to_string()),
            term_emulator: TermEmulator::new(),
            session: None,
            cols: 80,
            rows: 24,
            shown_line_count: 0,
        }
    }

    /// Poll for output from running session and update state if needed.
    /// Returns Some(new_state) if there's new output, None otherwise.
    pub fn poll_session(&mut self, state: &PaneState) -> Option<PaneState> {
        if let Some(ref mut session) = self.session {
            match session.poll() {
                Ok(had_output) => {
                    let still_running = session.is_running();

                    // Get lines from the emulator
                    let session_lines = session.get_lines();

                    // Filter to non-empty lines
                    let non_empty_lines: Vec<_> = session_lines
                        .into_iter()
                        .filter(|line| {
                            if let Line::Text(ref text) = line {
                                !text.is_empty()
                            } else {
                                true
                            }
                        })
                        .collect();

                    // Check if we have new lines to show
                    let new_line_count = non_empty_lines.len();

                    if new_line_count > self.shown_line_count || !still_running {
                        let mut new_term_state = state.term_state.clone();

                        // Append only the new lines
                        for line in non_empty_lines.into_iter().skip(self.shown_line_count) {
                            new_term_state.lines.push(line);
                        }

                        self.shown_line_count = new_line_count;

                        // If process exited, clean up
                        if !still_running {
                            self.session = None;
                            self.shown_line_count = 0;
                        }

                        return Some(PaneState {
                            input_state: state.input_state.clone(),
                            term_state: new_term_state,
                        });
                    }

                    // No new lines, but check if process exited
                    if !still_running {
                        self.session = None;
                        self.shown_line_count = 0;
                    }

                    None
                }
                Err(_) => {
                    // Error reading, clean up session
                    self.session = None;
                    self.shown_line_count = 0;
                    None
                }
            }
        } else {
            None
        }
    }

    /// Get the raw file descriptor of the running session for polling
    pub fn session_fd(&self) -> Option<RawFd> {
        self.session.as_ref().map(|s| s.raw_fd())
    }

    /// Check if there's a running session
    pub fn has_session(&self) -> bool {
        self.session.is_some()
    }
}

#[derive(Clone, PartialEq)]
pub struct PaneState {
    input_state: input::InputState,
    term_state: TermEmulatorState,
}

impl tui::AppController<PaneState> for Pane {
    fn initial_state(&self) -> PaneState {
        PaneState {
            input_state: input::InputState::new(),
            term_state: TermEmulatorState::new(),
        }
    }

    fn clean_up(&self, term: &mut tui::Terminal) {
        term.clean_up();
    }

    fn render(&mut self, t: &mut tui::Terminal, state: &PaneState, prev_state: Option<&PaneState>) {
        let mut tt = t.derive("term_emulator".to_string());
        tt.height = t.height - 2;
        self.term_emulator.render(
            &mut tt,
            &state.term_state,
            prev_state.map(|ps| &ps.term_state),
        );

        let mut it = t.derive("input".to_string());
        it.height = 2;
        it.offset_y = t.height - 2;
        AppController::render(
            &mut self.input_component,
            &mut it,
            &state.input_state,
            prev_state.map(|ps| &ps.input_state),
        );
    }

    fn transition(&mut self, state: &PaneState, event: KeyboardEvent) -> Transition<PaneState> {
        if state.input_state.focused {
            return match input::transition(&state.input_state, event) {
                Transition::Updated(new_input_state) => Transition::Updated(PaneState {
                    input_state: new_input_state,
                    term_state: state.term_state.clone(),
                }),
                Transition::Terminate(_) => Transition::Terminate(state.clone()),
                Transition::Nothing => Transition::Nothing,
                Transition::Finished(_) => {
                    let command = state.input_state.value.clone();
                    let mut new_term_state = state.term_state.clone();
                    new_term_state.lines.push(Line::Command(command.clone()));

                    // Skip empty commands
                    if !command.trim().is_empty() {
                        // Spawn the command in a PTY session
                        match Session::spawn(&command, self.cols, self.rows) {
                            Ok(session) => {
                                self.session = Some(session);
                                self.shown_line_count = 0;
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
                })
            }

            _ => Transition::Nothing,
        };
    }
}
