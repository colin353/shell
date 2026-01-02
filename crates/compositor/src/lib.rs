//! Terminal compositor for combining multiple terminal screens into one.

struct Compositor {
    root: PaneCell,
}

struct PaneCell {
    inner: PaneCellInner,
    width: usize,
    height: usize,
    pos_x: usize,
    pos_y: usize,
    focus: bool,
}

struct Pane {
    terminal_emulator: emulator::TerminalEmulator,
    pty: Option<pty::PtyProcess>,
    read_buffer: [u8; 4096],
}

enum PaneCellInner {
    Pane(Pane),
    VSplit(Vec<PaneCell>),
    HSplit(Vec<PaneCell>),
}

impl Compositor {
    pub fn new(width: usize, height: usize) -> Self {
        Self {
            root: PaneCell {
                inner: PaneCellInner::Pane(Pane {
                    terminal_emulator: emulator::TerminalEmulator::new(width, height),
                    pty: Some(
                        pty::PtyProcess::spawn("/bin/bash", width as u16, height as u16).unwrap(),
                    ),
                    read_buffer: [0u8; 4096],
                }),
                width: width,
                height: height,
                pos_x: 0,
                pos_y: 0,
                focus: true,
            },
        }
    }

    pub fn handle_input(&mut self, input: &[u8]) {
        // Direct input to the focused pane
        self.root.handle_input(input);
    }
}

impl PaneCell {
    pub fn handle_input(&mut self, input: &[u8]) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.handle_input(input);
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.handle_input(input);
                    }
                }
            }
        }
    }
}

impl Pane {
    pub fn handle_input(&mut self, input: &[u8]) {
        if let Some(pty) = &mut self.pty {
            let _ = pty.write(input);
        }
    }
}
