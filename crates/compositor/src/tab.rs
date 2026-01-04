use crate::error::CompositorError;
use crate::pane::Pane;
use crate::pane_cell::{PaneCell, PaneCellInner};
use pty;

/// A tab containing a name and its own root pane tree
pub struct Tab {
    /// The display name for this tab
    pub name: String,
    /// The root pane cell for this tab's content
    pub root: PaneCell,
}

impl Tab {
    /// Create a new tab with the given name and dimensions
    pub fn new(name: String, width: usize, height: usize) -> Result<Self, CompositorError> {
        Ok(Self {
            name,
            root: PaneCell {
                inner: PaneCellInner::Pane(Pane {
                    terminal_emulator: emulator::TerminalEmulator::new(width, height),
                    pty: Some(
                        pty::PtyProcess::spawn("/bin/bash", width as u16, height as u16)
                            .map_err(CompositorError::Pty)?,
                    ),
                    read_buffer: [0u8; 4096],
                    scrollback_mode: false,
                    scroll_offset: 0,
                    search_mode: false,
                    search_query: String::new(),
                    search_matches: Vec::new(),
                    current_match_index: None,
                }),
                width,
                height,
                pos_x: 0,
                pos_y: 0,
                focus: true,
            },
        })
    }

    /// Resize the tab's root pane to new dimensions
    pub fn resize(&mut self, width: usize, height: usize) {
        self.root.resize(0, 0, width, height);
    }
}
