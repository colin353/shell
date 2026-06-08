//! Terminal emulator module
//!
//! This module provides a virtual terminal that can parse ANSI escape sequences
//! and maintain a grid of cells representing the terminal display.
//!
//! This implementation uses alacritty_terminal as the backend for better
//! compatibility with real-world terminal applications.

mod alacritty_wrapper;
mod cell;
pub mod delta;
mod snapshot;

use serde::{Deserialize, Serialize};

pub use alacritty_wrapper::AlacrittyEmulator;
pub use cell::{Cell, CellAttributes, Color, Line};
pub use delta::compute_delta;
pub use snapshot::render_snapshot_to_ansi;

/// Character set designations (for compatibility)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum CharSet {
    /// US ASCII (B)
    Ascii,
    /// UK/British (A) - # becomes £
    Uk,
    /// DEC Special Graphics and line drawing (0)
    DecSpecialGraphics,
    /// DEC Alternate character ROM standard (1)
    DecAltRomStandard,
    /// DEC Alternate character ROM special graphics (2)
    DecAltRomSpecial,
}

/// Mouse reporting mode requested by the application running in the terminal.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum MouseReportMode {
    #[default]
    None,
    Click,
    Drag,
    Motion,
}

/// Mouse coordinate encoding requested by the application.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum MouseEncoding {
    #[default]
    Normal,
    Utf8,
    Sgr,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct MouseMode {
    pub report: MouseReportMode,
    pub encoding: MouseEncoding,
}

impl Default for CharSet {
    fn default() -> Self {
        CharSet::Ascii
    }
}

/// Compatibility wrapper that provides a TerminalGrid-like interface
/// backed by alacritty_terminal.
#[derive(Clone, Serialize, Deserialize)]
pub struct TerminalGrid {
    /// Cached cells for direct access
    cells: Vec<Vec<Cell>>,
    pub cols: usize,
    pub rows: usize,
    pub cursor_x: usize,
    pub cursor_y: usize,
    pub current_attrs: CellAttributes,
    pub cursor_visible: bool,
    pub scroll_top: usize,
    pub scroll_bottom: usize,
    pub origin_mode: bool,
    pub autowrap: bool,
    pub in_alternate_screen: bool,
    pub charset_g0: CharSet,
    pub charset_g1: CharSet,
    pub gl_is_g1: bool,
}

impl TerminalGrid {
    /// Create a new empty terminal grid
    pub fn new(cols: usize, rows: usize) -> Self {
        let cells = (0..rows)
            .map(|_| (0..cols).map(|_| Cell::empty()).collect())
            .collect();

        Self {
            cells,
            cols,
            rows,
            cursor_x: 0,
            cursor_y: 0,
            current_attrs: CellAttributes::default(),
            cursor_visible: true,
            scroll_top: 0,
            scroll_bottom: rows.saturating_sub(1),
            origin_mode: false,
            autowrap: true,
            in_alternate_screen: false,
            charset_g0: CharSet::Ascii,
            charset_g1: CharSet::Ascii,
            gl_is_g1: false,
        }
    }

    /// Get a cell at the specified position
    pub fn get_cell(&self, x: usize, y: usize) -> &Cell {
        if y < self.cells.len() && x < self.cells[y].len() {
            &self.cells[y][x]
        } else {
            static EMPTY: Cell = Cell {
                character: ' ',
                attrs: CellAttributes {
                    fg_color: None,
                    bg_color: None,
                    bold: false,
                    italic: false,
                    underline: false,
                    strikethrough: false,
                    dim: false,
                    inverse: false,
                    hidden: false,
                },
                is_wide_char_spacer: false,
            };
            &EMPTY
        }
    }

    /// Set a cell at the specified position
    pub fn set_cell(&mut self, x: usize, y: usize, cell: Cell) {
        if y < self.cells.len() && x < self.cells[y].len() {
            self.cells[y][x] = cell;
        }
    }

    /// Get text content of a line (skips wide char spacers for proper text extraction)
    pub fn get_line_text(&self, row: usize) -> String {
        if row >= self.rows {
            return String::new();
        }
        self.cells[row]
            .iter()
            .filter(|c| !c.is_wide_char_spacer)
            .map(|c| c.character)
            .collect()
    }

    /// Resize the grid
    pub fn resize(&mut self, cols: usize, rows: usize) {
        // Resize rows
        while self.cells.len() < rows {
            self.cells.push((0..cols).map(|_| Cell::empty()).collect());
        }
        while self.cells.len() > rows {
            self.cells.pop();
        }

        // Resize columns
        for row in &mut self.cells {
            while row.len() < cols {
                row.push(Cell::empty());
            }
            while row.len() > cols {
                row.pop();
            }
        }

        self.cols = cols;
        self.rows = rows;
        self.scroll_bottom = rows.saturating_sub(1);
        self.cursor_x = self.cursor_x.min(cols.saturating_sub(1));
        self.cursor_y = self.cursor_y.min(rows.saturating_sub(1));
    }

    /// Get the grid as lines for rendering
    pub fn to_lines(&self) -> Vec<Line> {
        self.cells
            .iter()
            .map(|row| Line::Cells(row.clone()))
            .collect()
    }

    /// Update this grid from an AlacrittyEmulator
    pub fn update_from(&mut self, emu: &AlacrittyEmulator) {
        let (cols, rows) = emu.dimensions();
        if self.cols != cols || self.rows != rows {
            self.resize(cols, rows);
        }

        // Copy cells
        for y in 0..rows {
            for x in 0..cols {
                self.cells[y][x] = emu.get_cell(x, y);
            }
        }

        // Copy state
        let (cx, cy) = emu.cursor_position();
        self.cursor_x = cx;
        self.cursor_y = cy;
        self.cursor_visible = emu.cursor_visible();
        self.autowrap = emu.autowrap();
        self.origin_mode = emu.origin_mode();
        self.in_alternate_screen = emu.in_alternate_screen();
        let (top, bottom) = emu.scroll_region();
        self.scroll_top = top;
        self.scroll_bottom = bottom;
    }

    // Scrollback methods - alacritty handles this internally, stub implementations for now

    /// Get the number of lines in the scrollback buffer
    pub fn scrollback_len(&self) -> usize {
        // TODO: Extract from alacritty's history
        0
    }

    /// Get a row from the scrollback buffer (index 0 is most recent)
    pub fn get_scrollback_row(&self, _index: usize) -> Option<&Vec<Cell>> {
        // TODO: Extract from alacritty's history
        None
    }

    /// Get a row from the visible grid
    pub fn get_row(&self, row: usize) -> Option<&Vec<Cell>> {
        self.cells.get(row)
    }
}

impl PartialEq for TerminalGrid {
    fn eq(&self, other: &Self) -> bool {
        self.cols == other.cols
            && self.rows == other.rows
            && self.cursor_x == other.cursor_x
            && self.cursor_y == other.cursor_y
            && self.cursor_visible == other.cursor_visible
            && self.cells == other.cells
    }
}

/// A complete terminal emulator - now backed by alacritty_terminal
pub struct TerminalEmulator {
    inner: AlacrittyEmulator,
    /// Cached grid state for efficient access
    grid_cache: TerminalGrid,
}

impl TerminalEmulator {
    /// Create a new terminal emulator with the given dimensions
    pub fn new(cols: usize, rows: usize) -> Self {
        let inner = AlacrittyEmulator::new(cols, rows);
        let mut grid_cache = TerminalGrid::new(cols, rows);
        grid_cache.update_from(&inner);
        Self { inner, grid_cache }
    }

    /// Process raw bytes from the PTY
    pub fn process(&mut self, bytes: &[u8]) {
        self.inner.process(bytes);
        // Eagerly update the cache
        self.grid_cache.update_from(&self.inner);
    }

    /// Get the current display as lines for rendering
    pub fn to_lines(&self) -> Vec<Line> {
        self.inner.to_lines()
    }

    /// Get the grid for direct access
    pub fn grid(&self) -> &TerminalGrid {
        &self.grid_cache
    }

    /// Get mutable access to the grid (for direct cell manipulation)
    pub fn grid_mut(&mut self) -> &mut TerminalGrid {
        &mut self.grid_cache
    }

    /// Resize the terminal
    pub fn resize(&mut self, cols: usize, rows: usize) {
        self.inner.resize(cols, rows);
        self.grid_cache.resize(cols, rows);
        self.grid_cache.update_from(&self.inner);
    }

    /// Get cursor position
    pub fn cursor_position(&self) -> (usize, usize) {
        self.inner.cursor_position()
    }

    /// Drain queued responses (for DSR and other terminal queries)
    pub fn drain_responses(&mut self) -> Vec<Vec<u8>> {
        self.inner.drain_responses()
    }

    pub fn mouse_mode(&self) -> MouseMode {
        self.inner.mouse_mode()
    }

    /// Get access to the underlying AlacrittyEmulator for debugging
    pub fn inner(&self) -> &AlacrittyEmulator {
        &self.inner
    }

    /// Blit a rectangular region from another terminal emulator into this one.
    pub fn blit_from(
        &mut self,
        source: &TerminalEmulator,
        src_x: usize,
        src_y: usize,
        dst_x: usize,
        dst_y: usize,
        width: usize,
        height: usize,
    ) {
        // Copy cells directly to the grid cache
        for dy in 0..height {
            for dx in 0..width {
                let src_cell = source.inner.get_cell(src_x + dx, src_y + dy);
                self.grid_cache.set_cell(dst_x + dx, dst_y + dy, src_cell);
            }
        }
    }

    /// Get the dimensions of this terminal (cols, rows)
    pub fn dimensions(&self) -> (usize, usize) {
        self.inner.dimensions()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_text() {
        let mut emu = TerminalEmulator::new(80, 24);
        emu.process(b"Hello, World!");

        let line = emu.grid().get_line_text(0);
        assert!(line.starts_with("Hello, World!"));
    }

    #[test]
    fn test_newline() {
        let mut emu = TerminalEmulator::new(80, 24);
        emu.process(b"Line1\r\nLine2");

        assert!(emu.grid().get_line_text(0).starts_with("Line1"));
        assert!(emu.grid().get_line_text(1).starts_with("Line2"));
    }

    #[test]
    fn test_cursor_movement() {
        let mut emu = TerminalEmulator::new(80, 24);
        // Write "Hello", move back 3 columns, overwrite with "X"
        emu.process(b"Hello\x1b[3DX");

        let line = emu.grid().get_line_text(0);
        assert!(line.starts_with("HeXlo"), "Got: {}", line);
    }

    #[test]
    fn test_clear_line() {
        let mut emu = TerminalEmulator::new(80, 24);
        emu.process(b"Hello World\x1b[5D\x1b[K");

        let line = emu.grid().get_line_text(0);
        assert!(line.starts_with("Hello "), "Got: '{}'", line);
        assert!(!line.contains("World"));
    }

    #[test]
    fn test_color() {
        let mut emu = TerminalEmulator::new(80, 24);
        emu.process(b"\x1b[31mRed\x1b[0m");

        let cell = emu.grid().get_cell(0, 0);
        assert_eq!(cell.attrs.fg_color, Some(Color::Red));

        // After reset, color should be None
        let cell_after = emu.grid().get_cell(3, 0);
        assert_eq!(cell_after.attrs.fg_color, None);
    }

    #[test]
    fn test_bold() {
        let mut emu = TerminalEmulator::new(80, 24);
        emu.process(b"\x1b[1mBold\x1b[0m Normal");

        assert!(emu.grid().get_cell(0, 0).attrs.bold);
        assert!(!emu.grid().get_cell(5, 0).attrs.bold);
    }

    /// Test that incremental delta computation works correctly with CJK characters
    #[test]
    fn test_incremental_cjk_delta() {
        let mut emu = TerminalEmulator::new(80, 24);

        // Initial state: empty terminal
        let prev1 = emu.grid().clone();

        // Type some ASCII first
        emu.process(b"echo ");
        let grid1 = emu.grid().clone();

        // Compute delta and verify it's reasonable
        let delta1 = compute_delta(&prev1, &grid1);

        // Now add CJK character
        emu.process("日".as_bytes());
        let grid2 = emu.grid().clone();

        // Compute delta from grid1 to grid2
        let delta2 = compute_delta(&grid1, &grid2);

        // Apply delta2 to grid1 through a simulated terminal and verify result
        // For now, just verify the text is correct
        let line = emu.grid().get_line_text(0);
        assert!(
            line.starts_with("echo 日"),
            "Expected 'echo 日', got '{}'",
            line
        );

        // Verify cursor position is correct
        let (cx, cy) = emu.cursor_position();
        // "echo " is 5 chars, "日" is 2 columns wide, so cursor should be at column 7
        assert_eq!(cx, 7, "Cursor should be at column 7, got {}", cx);
        assert_eq!(cy, 0, "Cursor should be at row 0, got {}", cy);

        // Add more CJK
        emu.process("本".as_bytes());
        let grid3 = emu.grid().clone();

        // Compute delta from grid2 to grid3
        let delta3 = compute_delta(&grid2, &grid3);

        // Verify the text
        let line = emu.grid().get_line_text(0);
        assert!(
            line.starts_with("echo 日本"),
            "Expected 'echo 日本', got '{}'",
            line
        );

        // Cursor should now be at column 9 (5 + 2 + 2)
        let (cx, _) = emu.cursor_position();
        assert_eq!(cx, 9, "Cursor should be at column 9, got {}", cx);

        // Now let's test applying deltas to a separate emulator to simulate
        // what the compositor does
        let mut display_emu = TerminalEmulator::new(80, 24);

        // Apply delta1
        display_emu.process(&delta1);
        let display_line = display_emu.grid().get_line_text(0);
        assert!(
            display_line.starts_with("echo "),
            "After delta1, expected 'echo ', got '{}'",
            display_line
        );

        // Apply delta2
        display_emu.process(&delta2);
        let display_line = display_emu.grid().get_line_text(0);
        assert!(
            display_line.starts_with("echo 日"),
            "After delta2, expected 'echo 日', got '{}'",
            display_line
        );

        // Apply delta3
        display_emu.process(&delta3);
        let display_line = display_emu.grid().get_line_text(0);
        assert!(
            display_line.starts_with("echo 日本"),
            "After delta3, expected 'echo 日本', got '{}'",
            display_line
        );
    }

    /// Test that simulates bash-style echo where chars are typed one at a time
    /// and the terminal receives them with possible cursor movements
    #[test]
    fn test_bash_style_incremental_echo() {
        let mut emu = TerminalEmulator::new(80, 24);

        // Simulate what bash does: for each keystroke, it echoes the char
        // at the current cursor position and advances the cursor.
        // This simulates typing "echo '日本'"

        // Type "e" - bash echoes "e"
        emu.process(b"e");
        let line = emu.grid().get_line_text(0);
        assert!(line.starts_with("e"), "After 'e', got '{}'", line);
        let (cx, _) = emu.cursor_position();
        assert_eq!(cx, 1, "Cursor should be at 1 after 'e'");

        // Type "c" - bash echoes "c"
        emu.process(b"c");
        let line = emu.grid().get_line_text(0);
        assert!(line.starts_with("ec"), "After 'c', got '{}'", line);
        let (cx, _) = emu.cursor_position();
        assert_eq!(cx, 2, "Cursor should be at 2 after 'c'");

        // Type "h" - bash echoes "h"
        emu.process(b"h");
        let line = emu.grid().get_line_text(0);
        assert!(line.starts_with("ech"), "After 'h', got '{}'", line);

        // Type "o" - bash echoes "o"
        emu.process(b"o");
        let line = emu.grid().get_line_text(0);
        assert!(line.starts_with("echo"), "After 'o', got '{}'", line);

        // Type " " - bash echoes " "
        emu.process(b" ");
        let line = emu.grid().get_line_text(0);
        assert!(line.starts_with("echo "), "After ' ', got '{}'", line);

        // Type "'" - bash echoes "'"
        emu.process(b"'");
        let line = emu.grid().get_line_text(0);
        assert!(line.starts_with("echo '"), "After \"'\", got '{}'", line);
        let (cx, _) = emu.cursor_position();
        assert_eq!(cx, 6, "Cursor should be at 6 after \"'\"");

        // Type "日" (CJK char - 3 bytes UTF-8, 2 columns wide) - bash echoes it
        emu.process("日".as_bytes());
        let line = emu.grid().get_line_text(0);
        assert!(line.starts_with("echo '日"), "After '日', got '{}'", line);
        let (cx, _) = emu.cursor_position();
        assert_eq!(cx, 8, "Cursor should be at 8 after '日' (wide char)");

        // Type "本" (another CJK char)
        emu.process("本".as_bytes());
        let line = emu.grid().get_line_text(0);
        assert!(line.starts_with("echo '日本"), "After '本', got '{}'", line);
        let (cx, _) = emu.cursor_position();
        assert_eq!(
            cx, 10,
            "Cursor should be at 10 after '本' (another wide char)"
        );
    }
}
