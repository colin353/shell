//! Conversion between [`TerminalGrid`] and the wire [`protocol::GridSnapshot`].
//!
//! This is the bridge for tmux-style reattach and (later) remote panes: the
//! authoritative server-side grid is serialized to a snapshot, sent to a client,
//! and either applied to the client's own emulator or rendered to ANSI for a
//! dumb client to paint.

use crate::{compute_delta, TerminalGrid};
use protocol::GridSnapshot;

impl TerminalGrid {
    /// Serialize this grid into a wire snapshot for resync.
    ///
    /// The full grid (cells, cursor, modes) is encoded into `cells`; `cols`,
    /// `rows`, and `alternate_screen` mirror it for cheap inspection without
    /// decoding.
    pub fn to_snapshot(&self) -> GridSnapshot {
        let cells = serde_json::to_vec(self).expect("TerminalGrid is always serializable");
        GridSnapshot {
            cols: self.cols as u16,
            rows: self.rows as u16,
            cells,
            alternate_screen: self.in_alternate_screen,
        }
    }

    /// Reconstruct a grid from a wire snapshot, or `None` if the payload is
    /// malformed.
    pub fn from_snapshot(snap: &GridSnapshot) -> Option<Self> {
        serde_json::from_slice(&snap.cells).ok()
    }
}

/// Produce the ANSI byte stream that paints a freshly-cleared terminal to match
/// `snap`. Used by a dumb client to apply a `GridResync`.
///
/// The client owns its own alternate-screen state (it enters app mode on
/// attach), so the snapshot's `in_alternate_screen` is neutralized here to avoid
/// emitting a `1049h` *after* the cells (which would clear them).
pub fn render_snapshot_to_ansi(snap: &GridSnapshot) -> Vec<u8> {
    let Some(mut grid) = TerminalGrid::from_snapshot(snap) else {
        return Vec::new();
    };
    grid.in_alternate_screen = false;

    let blank = TerminalGrid::new(grid.cols, grid.rows);
    let mut out = Vec::with_capacity(snap.cells.len() / 2 + 16);
    out.extend_from_slice(b"\x1b[2J\x1b[H");
    out.extend(compute_delta(&blank, &grid));
    out
}

#[cfg(test)]
mod tests {
    use crate::{Cell, CellAttributes, Color, TerminalGrid};

    fn sample_grid() -> TerminalGrid {
        let mut g = TerminalGrid::new(20, 5);
        for (i, ch) in "hello".chars().enumerate() {
            g.set_cell(i, 0, Cell::with_char(ch));
        }
        // A styled cell to exercise attribute round-tripping.
        let styled = Cell::new(
            'X',
            CellAttributes {
                fg_color: Some(Color::Rgb(10, 20, 30)),
                bg_color: Some(Color::Indexed(200)),
                bold: true,
                underline: true,
                ..Default::default()
            },
        );
        g.set_cell(0, 1, styled);
        g.cursor_x = 3;
        g.cursor_y = 2;
        g.cursor_visible = false;
        g
    }

    #[test]
    fn snapshot_round_trips_exactly() {
        let grid = sample_grid();
        let snap = grid.to_snapshot();
        assert_eq!(snap.cols, 20);
        assert_eq!(snap.rows, 5);
        let restored = TerminalGrid::from_snapshot(&snap).unwrap();
        assert!(restored == grid, "round-tripped grid must equal the original");
    }

    #[test]
    fn render_to_ansi_reproduces_grid_on_a_blank_emulator() {
        let grid = sample_grid();
        let snap = grid.to_snapshot();
        let ansi = super::render_snapshot_to_ansi(&snap);

        // Feed the ANSI into a fresh emulator and confirm the visible content
        // matches the source grid's text.
        let mut emu = crate::TerminalEmulator::new(20, 5);
        emu.process(&ansi);
        assert_eq!(emu.grid().get_line_text(0), "hello".to_string() + &" ".repeat(15));
        assert_eq!(&emu.grid().get_line_text(1)[..1], "X");
    }

    #[test]
    fn malformed_snapshot_yields_empty_ansi() {
        let snap = protocol::GridSnapshot {
            cols: 10,
            rows: 3,
            cells: vec![1, 2, 3], // not valid JSON
            alternate_screen: false,
        };
        assert!(super::render_snapshot_to_ansi(&snap).is_empty());
        assert!(TerminalGrid::from_snapshot(&snap).is_none());
    }
}
