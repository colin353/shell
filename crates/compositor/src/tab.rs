use crate::error::CompositorError;
use crate::pane::Pane;
use crate::pane_cell::{PaneCell, PaneCellInner};

/// A tab containing a name and its own root pane tree
pub struct Tab {
    /// The display name for this tab
    pub name: String,
    /// The root pane cell for this tab's content
    pub root: PaneCell,
    /// Whether the focused pane is currently zoomed (temporary fullscreen)
    pub zoomed: bool,
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
            zoomed: false,
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
            zoomed: false,
        })
    }

    /// Resize the tab's root pane to new dimensions
    pub fn resize(&mut self, width: usize, height: usize) {
        self.root.resize(0, 0, width, height);
    }

    /// Toggle zoom state for the focused pane.
    /// When zoomed, the focused pane temporarily takes up the entire tab space.
    /// Returns true if zoom was toggled, false if there's only one pane (nothing to zoom).
    pub fn toggle_zoom(&mut self) -> bool {
        // Only allow zooming if there are multiple panes
        if self.root.pane_count() <= 1 {
            self.zoomed = false;
            return false;
        }
        self.zoomed = !self.zoomed;
        true
    }

    /// Exit zoom mode if currently zoomed.
    /// Called when the focused pane is closed to ensure we don't stay zoomed.
    pub fn exit_zoom(&mut self) {
        self.zoomed = false;
    }
}
