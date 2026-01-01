use emulator::Cell;

pub use emulator::Line;

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
        prev_state: Option<&TermEmulatorState>,
    ) -> usize {
        if prev_state.is_none() {
            for y in 0..t.height {
                t.move_cursor_to(0, y);
                t.clear_line();
            }
        }

        // If we have exactly the terminal height lines, render from the top
        // (this is the case for live session grids that fill the screen)
        // If we have fewer lines, bottom-align them (for command history)
        // If we have more lines, show the last `t.height` lines
        let (start_y, start_idx) = if state.lines.len() == t.height {
            // Full grid - render from top (live session)
            (0, 0)
        } else if state.lines.len() < t.height {
            // Fewer lines than height - bottom-align (history view)
            (t.height - state.lines.len(), 0)
        } else {
            // More lines than height - show last t.height lines
            (0, state.lines.len() - t.height)
        };

        for (y, idx) in (start_y..t.height).zip(start_idx..state.lines.len()) {
            let line = &state.lines[idx];

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
                Line::Cells(cells) => {
                    t.print_cells(cells);
                    t.set_normal();
                }
            }
        }

        t.height
    }
}
