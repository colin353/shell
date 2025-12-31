//! Terminal emulator module
//! 
//! This module provides a virtual terminal that can parse ANSI escape sequences
//! and maintain a grid of cells representing the terminal display.

mod cell;
mod grid;
mod parser;

pub use cell::{Cell, CellAttributes, Color};
pub use grid::TerminalGrid;
pub use parser::AnsiParser;

use crate::terminal::Line;

/// A complete terminal emulator combining parser and grid
pub struct TerminalEmulator {
    grid: TerminalGrid,
    parser: AnsiParser,
}

impl TerminalEmulator {
    /// Create a new terminal emulator with the given dimensions
    pub fn new(cols: usize, rows: usize) -> Self {
        Self {
            grid: TerminalGrid::new(cols, rows),
            parser: AnsiParser::new(),
        }
    }

    /// Process raw bytes from the PTY
    pub fn process(&mut self, bytes: &[u8]) {
        self.parser.process(bytes, &mut self.grid);
    }

    /// Get the current display as lines for rendering
    pub fn to_lines(&self) -> Vec<Line> {
        self.grid.to_lines()
    }

    /// Get the grid for direct access (useful for testing)
    pub fn grid(&self) -> &TerminalGrid {
        &self.grid
    }

    /// Get mutable grid access
    pub fn grid_mut(&mut self) -> &mut TerminalGrid {
        &mut self.grid
    }

    /// Resize the terminal
    pub fn resize(&mut self, cols: usize, rows: usize) {
        self.grid.resize(cols, rows);
    }

    /// Get cursor position
    pub fn cursor_position(&self) -> (usize, usize) {
        (self.grid.cursor_x, self.grid.cursor_y)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_text() {
        let mut emu = TerminalEmulator::new(80, 24);
        emu.process(b"Hello, World!");
        
        let line = emu.grid.get_line_text(0);
        assert!(line.starts_with("Hello, World!"));
    }

    #[test]
    fn test_newline() {
        let mut emu = TerminalEmulator::new(80, 24);
        emu.process(b"Line1\nLine2");
        
        assert!(emu.grid.get_line_text(0).starts_with("Line1"));
        assert!(emu.grid.get_line_text(1).starts_with("Line2"));
    }

    #[test]
    fn test_cursor_movement() {
        let mut emu = TerminalEmulator::new(80, 24);
        // Write "Hello", move back 3 columns, overwrite with "XXX"
        emu.process(b"Hello\x1b[3DX");
        
        let line = emu.grid.get_line_text(0);
        assert!(line.starts_with("HeXlo"), "Got: {}", line);
    }

    #[test]
    fn test_clear_line() {
        let mut emu = TerminalEmulator::new(80, 24);
        emu.process(b"Hello World\x1b[5D\x1b[K");
        
        let line = emu.grid.get_line_text(0);
        assert!(line.starts_with("Hello "), "Got: '{}'", line);
        assert!(!line.contains("World"));
    }

    #[test]
    fn test_color() {
        let mut emu = TerminalEmulator::new(80, 24);
        emu.process(b"\x1b[31mRed\x1b[0m");
        
        let cell = emu.grid.get_cell(0, 0);
        assert_eq!(cell.attrs.fg_color, Some(Color::Red));
        
        // After reset, color should be None
        let cell_after = emu.grid.get_cell(3, 0);
        assert_eq!(cell_after.attrs.fg_color, None);
    }

    #[test]
    fn test_bold() {
        let mut emu = TerminalEmulator::new(80, 24);
        emu.process(b"\x1b[1mBold\x1b[0m Normal");
        
        assert!(emu.grid.get_cell(0, 0).attrs.bold);
        assert!(!emu.grid.get_cell(5, 0).attrs.bold);
    }
}
