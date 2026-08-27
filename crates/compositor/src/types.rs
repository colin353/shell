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

/// Transient, single-cell status shown at the end of a tab label.
///
/// Variant order is also aggregation priority: a pane that needs input wins
/// over working panes when a tab contains multiple splits.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Badge {
    None,
    ShellPrompt,
    ProgramRunning,
    AgentWorking,
    AgentNeedsInput,
}

impl Default for Badge {
    fn default() -> Self {
        Self::None
    }
}

impl Badge {
    /// Glyph rendered in the tab's reserved badge cell.
    pub const fn glyph(self) -> char {
        match self {
            Self::None => ' ',
            Self::ShellPrompt => '○',
            Self::ProgramRunning => '●',
            Self::AgentWorking => '◆',
            Self::AgentNeedsInput => '◇',
        }
    }
}

impl From<Badge> for protocol::PaneBadge {
    fn from(badge: Badge) -> Self {
        match badge {
            Badge::None => Self::None,
            Badge::ShellPrompt => Self::ShellPrompt,
            Badge::ProgramRunning => Self::ProgramRunning,
            Badge::AgentWorking => Self::AgentWorking,
            Badge::AgentNeedsInput => Self::AgentNeedsInput,
        }
    }
}

impl From<protocol::PaneBadge> for Badge {
    fn from(badge: protocol::PaneBadge) -> Self {
        match badge {
            protocol::PaneBadge::None => Self::None,
            protocol::PaneBadge::ShellPrompt => Self::ShellPrompt,
            protocol::PaneBadge::ProgramRunning => Self::ProgramRunning,
            protocol::PaneBadge::AgentWorking => Self::AgentWorking,
            protocol::PaneBadge::AgentNeedsInput => Self::AgentNeedsInput,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Badge;
    use unicode_width::UnicodeWidthChar;

    #[test]
    fn badge_glyphs_are_exactly_one_cell_wide() {
        for badge in [
            Badge::None,
            Badge::ShellPrompt,
            Badge::ProgramRunning,
            Badge::AgentWorking,
            Badge::AgentNeedsInput,
        ] {
            assert_eq!(badge.glyph().width(), Some(1));
        }
    }

    #[test]
    fn badge_priority_favors_agent_attention() {
        assert!(Badge::ShellPrompt < Badge::ProgramRunning);
        assert!(Badge::ProgramRunning < Badge::AgentWorking);
        assert!(Badge::AgentWorking < Badge::AgentNeedsInput);
    }

    #[test]
    fn protocol_badges_round_trip() {
        for badge in [
            Badge::None,
            Badge::ShellPrompt,
            Badge::ProgramRunning,
            Badge::AgentWorking,
            Badge::AgentNeedsInput,
        ] {
            assert_eq!(Badge::from(protocol::PaneBadge::from(badge)), badge);
        }
    }
}
