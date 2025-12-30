use crate::tui;

pub struct TermEmulator {}

impl TermEmulatorState {
    pub fn new() -> Self {
        Self { lines: Vec::new() }
    }
}

#[derive(Clone, PartialEq)]
pub struct TermEmulatorState {
    pub lines: Vec<Line>,
}

#[derive(Clone, PartialEq)]
pub enum Line {
    Command(String),
    Text(String),
}

impl TermEmulator {
    pub fn new() -> Self {
        Self {}
    }
}

impl tui::Component<TermEmulatorState> for TermEmulator {
    fn render(
        &mut self,
        t: &mut tui::Terminal,
        state: &TermEmulatorState,
        _prev_state: Option<&TermEmulatorState>,
    ) -> usize {
        let start_line = t.height.saturating_sub(state.lines.len());
        for y in start_line..t.height {
            let line = state.lines.get(y - start_line).unwrap();

            t.move_cursor_to(0, y);
            t.clear_line();
            match line {
                Line::Command(cmd) => {
                    t.set_bold();
                    t.print(" $ ");
                    t.set_normal();
                    t.print(cmd);
                    t.set_normal();
                }
                Line::Text(text) => {
                    t.print(text);
                }
            }
        }

        t.height
    }
}
