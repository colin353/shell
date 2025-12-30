use crate::{input, terminal, tui};
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
                    // TODO: Execute the command...
                    let mut new_term_state = state.term_state.clone();
                    new_term_state
                        .lines
                        .push(Line::Command(state.input_state.value.clone()));

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
