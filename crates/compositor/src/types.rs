/// Events that can occur in the compositor.
#[derive(Debug, Clone)]
pub enum CompositorEvent {
    /// A pane received output from its PTY
    PtyOutput { pane_id: usize },
    /// Keyboard input was processed
    KeyboardInput,
    /// A pane's process exited
    ProcessExited { pane_id: usize },
}

/// Direction for focus movement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Left,
    Right,
    Up,
    Down,
}

/// Direction for splitting a pane.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SplitDirection {
    /// Split horizontally (creates top/bottom panes)
    Horizontal,
    /// Split vertically (creates left/right panes)
    Vertical,
}
