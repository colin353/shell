use crate::error::CompositorError;
use crate::pane::Pane;
use crate::pane_cell::{PaneCell, PaneCellInner};

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
                inner: PaneCellInner::Pane(Pane::new(width, height)),
                width,
                height,
                pos_x: 0,
                pos_y: 0,
                focus: true,
            },
        })
    }

    /// Create a new tab with a custom ShellCore (for testing with pre-populated history)
    pub fn with_core(
        name: String,
        width: usize,
        height: usize,
        core: std::sync::Arc<libshell::ShellCore>,
    ) -> Result<Self, CompositorError> {
        Ok(Self {
            name,
            root: PaneCell {
                inner: PaneCellInner::Pane(Pane::with_core(width, height, core)),
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
