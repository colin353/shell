//! Wrapper around alacritty_terminal for terminal emulation
//!
//! This module provides a bridge between alacritty_terminal's battle-tested
//! terminal emulator and our existing Cell/Line types.

use std::collections::VecDeque;

use alacritty_terminal::event::{Event, EventListener};
use alacritty_terminal::grid::Dimensions;
use alacritty_terminal::index::{Column, Line as AlacLine, Point};
use alacritty_terminal::term::cell::Flags as CellFlags;
use alacritty_terminal::term::{Config, TermMode};
use alacritty_terminal::vte::ansi::{Color as AlacColor, NamedColor, Processor, Rgb};
use alacritty_terminal::Term;

use crate::cell::{Cell, CellAttributes, Color, Line};
use crate::{MouseEncoding, MouseMode, MouseReportMode};

/// Dummy event listener that collects responses
#[derive(Default)]
pub struct EventProxy {
    responses: Vec<Vec<u8>>,
}

impl EventListener for EventProxy {
    fn send_event(&self, _event: Event) {
        // Most events can be ignored for our use case
    }
}

/// Terminal emulator backed by alacritty_terminal
pub struct AlacrittyEmulator {
    term: Term<EventProxy>,
    processor: Processor,
    cols: usize,
    rows: usize,
    /// Queued responses to be sent back to the PTY (e.g., for DSR queries)
    responses: VecDeque<Vec<u8>>,
}

impl AlacrittyEmulator {
    /// Create a new terminal emulator with the given dimensions
    pub fn new(cols: usize, rows: usize) -> Self {
        let config = Config::default();
        let size = TermSize::new(cols, rows);
        let term = Term::new(config, &size, EventProxy::default());

        Self {
            term,
            processor: Processor::new(),
            cols,
            rows,
            responses: VecDeque::new(),
        }
    }

    /// Process raw bytes from the PTY
    pub fn process(&mut self, bytes: &[u8]) {
        self.processor.advance(&mut self.term, bytes);
    }

    /// Get the current display as lines for rendering
    pub fn to_lines(&self) -> Vec<Line> {
        let grid = self.term.grid();
        let mut lines = Vec::with_capacity(self.rows);

        for row_idx in 0..self.rows {
            let line = AlacLine(row_idx as i32);
            let mut cells = Vec::with_capacity(self.cols);

            for col_idx in 0..self.cols {
                let point = Point::new(line, Column(col_idx));
                let alac_cell = &grid[point];
                cells.push(convert_cell(alac_cell));
            }

            lines.push(Line::Cells(cells));
        }

        lines
    }

    /// Get a cell at the specified position
    pub fn get_cell(&self, x: usize, y: usize) -> Cell {
        if x >= self.cols || y >= self.rows {
            return Cell::empty();
        }

        let point = Point::new(AlacLine(y as i32), Column(x));
        let alac_cell = &self.term.grid()[point];
        convert_cell(alac_cell)
    }

    /// Get the text content of a line
    pub fn get_line_text(&self, row: usize) -> String {
        if row >= self.rows {
            return String::new();
        }

        let grid = self.term.grid();
        let line = AlacLine(row as i32);
        let mut text = String::with_capacity(self.cols);

        for col_idx in 0..self.cols {
            let point = Point::new(line, Column(col_idx));
            let cell = &grid[point];
            // Skip wide char spacers (the second half of a double-width character)
            if !cell.flags.contains(CellFlags::WIDE_CHAR_SPACER) {
                text.push(cell.c);
            }
        }

        text
    }

    /// Resize the terminal
    pub fn resize(&mut self, cols: usize, rows: usize) {
        self.cols = cols;
        self.rows = rows;
        let size = TermSize::new(cols, rows);
        self.term.resize(size);
    }

    /// Get cursor position (x, y)
    pub fn cursor_position(&self) -> (usize, usize) {
        let cursor = self.term.grid().cursor.point;
        (cursor.column.0, cursor.line.0 as usize)
    }

    /// Get dimensions (cols, rows)
    pub fn dimensions(&self) -> (usize, usize) {
        (self.cols, self.rows)
    }

    /// Drain queued responses (for DSR and other terminal queries)
    pub fn drain_responses(&mut self) -> Vec<Vec<u8>> {
        self.responses.drain(..).collect()
    }

    /// Check if cursor is visible
    pub fn cursor_visible(&self) -> bool {
        // SHOW_CURSOR means cursor is visible
        self.term.mode().contains(TermMode::SHOW_CURSOR)
    }

    /// Check if autowrap is enabled
    pub fn autowrap(&self) -> bool {
        self.term.mode().contains(TermMode::LINE_WRAP)
    }

    /// Check if origin mode is enabled
    pub fn origin_mode(&self) -> bool {
        self.term.mode().contains(TermMode::ORIGIN)
    }

    /// Check if in alternate screen
    pub fn in_alternate_screen(&self) -> bool {
        self.term.mode().contains(TermMode::ALT_SCREEN)
    }

    /// Return the mouse reporting mode requested by the application.
    pub fn mouse_mode(&self) -> MouseMode {
        let mode = self.term.mode();
        let report = if mode.contains(TermMode::MOUSE_MOTION) {
            MouseReportMode::Motion
        } else if mode.contains(TermMode::MOUSE_DRAG) {
            MouseReportMode::Drag
        } else if mode.contains(TermMode::MOUSE_REPORT_CLICK) {
            MouseReportMode::Click
        } else {
            MouseReportMode::None
        };

        let encoding = if mode.contains(TermMode::SGR_MOUSE) {
            MouseEncoding::Sgr
        } else if mode.contains(TermMode::UTF8_MOUSE) {
            MouseEncoding::Utf8
        } else {
            MouseEncoding::Normal
        };

        MouseMode { report, encoding }
    }

    /// Get scroll region (top, bottom) - 0-indexed
    pub fn scroll_region(&self) -> (usize, usize) {
        // Alacritty stores scroll region internally in the grid
        // For now, return full screen bounds
        // TODO: extract actual scroll region if needed for delta rendering
        (0, self.rows.saturating_sub(1))
    }

    /// Access the underlying alacritty Term (for advanced operations)
    pub fn term(&self) -> &Term<EventProxy> {
        &self.term
    }

    /// Mutable access to the underlying alacritty Term
    pub fn term_mut(&mut self) -> &mut Term<EventProxy> {
        &mut self.term
    }
}

/// Helper struct for terminal sizing
struct TermSize {
    cols: usize,
    rows: usize,
}

impl TermSize {
    fn new(cols: usize, rows: usize) -> Self {
        Self { cols, rows }
    }
}

impl Dimensions for TermSize {
    fn total_lines(&self) -> usize {
        self.rows
    }

    fn screen_lines(&self) -> usize {
        self.rows
    }

    fn columns(&self) -> usize {
        self.cols
    }
}

/// Convert an alacritty cell to our Cell type
fn convert_cell(alac_cell: &alacritty_terminal::term::cell::Cell) -> Cell {
    let character = alac_cell.c;
    let flags = alac_cell.flags;

    let attrs = CellAttributes {
        fg_color: convert_color(&alac_cell.fg),
        bg_color: convert_color(&alac_cell.bg),
        bold: flags.contains(CellFlags::BOLD),
        italic: flags.contains(CellFlags::ITALIC),
        underline: flags.contains(CellFlags::UNDERLINE)
            || flags.contains(CellFlags::DOUBLE_UNDERLINE)
            || flags.contains(CellFlags::UNDERCURL),
        strikethrough: flags.contains(CellFlags::STRIKEOUT),
        dim: flags.contains(CellFlags::DIM),
        inverse: flags.contains(CellFlags::INVERSE),
        hidden: flags.contains(CellFlags::HIDDEN),
    };

    let is_wide_char_spacer = flags.contains(CellFlags::WIDE_CHAR_SPACER);

    Cell {
        character,
        attrs,
        is_wide_char_spacer,
    }
}

/// Convert alacritty color to our Color type
fn convert_color(color: &AlacColor) -> Option<Color> {
    match color {
        AlacColor::Named(named) => convert_named_color(*named),
        AlacColor::Spec(Rgb { r, g, b }) => Some(Color::Rgb(*r, *g, *b)),
        AlacColor::Indexed(idx) => {
            // Handle standard 16 colors specially
            if *idx < 16 {
                convert_indexed_16(*idx)
            } else {
                Some(Color::Indexed(*idx))
            }
        }
    }
}

/// Convert named color to our Color type
fn convert_named_color(named: NamedColor) -> Option<Color> {
    match named {
        NamedColor::Black => Some(Color::Black),
        NamedColor::Red => Some(Color::Red),
        NamedColor::Green => Some(Color::Green),
        NamedColor::Yellow => Some(Color::Yellow),
        NamedColor::Blue => Some(Color::Blue),
        NamedColor::Magenta => Some(Color::Magenta),
        NamedColor::Cyan => Some(Color::Cyan),
        NamedColor::White => Some(Color::White),
        NamedColor::BrightBlack => Some(Color::BrightBlack),
        NamedColor::BrightRed => Some(Color::BrightRed),
        NamedColor::BrightGreen => Some(Color::BrightGreen),
        NamedColor::BrightYellow => Some(Color::BrightYellow),
        NamedColor::BrightBlue => Some(Color::BrightBlue),
        NamedColor::BrightMagenta => Some(Color::BrightMagenta),
        NamedColor::BrightCyan => Some(Color::BrightCyan),
        NamedColor::BrightWhite => Some(Color::BrightWhite),
        NamedColor::Foreground => None, // Default
        NamedColor::Background => None, // Default
        _ => None,
    }
}

/// Convert indexed 16 colors to named colors
fn convert_indexed_16(idx: u8) -> Option<Color> {
    match idx {
        0 => Some(Color::Black),
        1 => Some(Color::Red),
        2 => Some(Color::Green),
        3 => Some(Color::Yellow),
        4 => Some(Color::Blue),
        5 => Some(Color::Magenta),
        6 => Some(Color::Cyan),
        7 => Some(Color::White),
        8 => Some(Color::BrightBlack),
        9 => Some(Color::BrightRed),
        10 => Some(Color::BrightGreen),
        11 => Some(Color::BrightYellow),
        12 => Some(Color::BrightBlue),
        13 => Some(Color::BrightMagenta),
        14 => Some(Color::BrightCyan),
        15 => Some(Color::BrightWhite),
        _ => Some(Color::Indexed(idx)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_text() {
        let mut emu = AlacrittyEmulator::new(80, 24);
        emu.process(b"Hello, World!");

        let line = emu.get_line_text(0);
        assert!(line.starts_with("Hello, World!"));
    }

    #[test]
    fn test_cursor_position() {
        let mut emu = AlacrittyEmulator::new(80, 24);
        emu.process(b"Hello");

        let (x, y) = emu.cursor_position();
        assert_eq!(x, 5);
        assert_eq!(y, 0);
    }

    #[test]
    fn test_newline() {
        let mut emu = AlacrittyEmulator::new(80, 24);
        emu.process(b"Line1\r\nLine2");

        assert!(emu.get_line_text(0).starts_with("Line1"));
        assert!(emu.get_line_text(1).starts_with("Line2"));
    }

    #[test]
    fn test_color() {
        let mut emu = AlacrittyEmulator::new(80, 24);
        emu.process(b"\x1b[31mRed\x1b[0m");

        let cell = emu.get_cell(0, 0);
        assert_eq!(cell.attrs.fg_color, Some(Color::Red));
    }

    #[test]
    fn test_bold() {
        let mut emu = AlacrittyEmulator::new(80, 24);
        emu.process(b"\x1b[1mBold\x1b[0m Normal");

        assert!(emu.get_cell(0, 0).attrs.bold);
        assert!(!emu.get_cell(5, 0).attrs.bold);
    }

    #[test]
    fn test_resize() {
        let mut emu = AlacrittyEmulator::new(80, 24);
        emu.resize(120, 40);

        assert_eq!(emu.dimensions(), (120, 40));
    }

    #[test]
    fn test_mouse_mode_tracking() {
        let mut emu = AlacrittyEmulator::new(80, 24);
        assert_eq!(emu.mouse_mode(), MouseMode::default());

        emu.process(b"\x1b[?1000h");
        assert_eq!(
            emu.mouse_mode(),
            MouseMode {
                report: MouseReportMode::Click,
                encoding: MouseEncoding::Normal,
            }
        );

        emu.process(b"\x1b[?1006h");
        assert_eq!(
            emu.mouse_mode(),
            MouseMode {
                report: MouseReportMode::Click,
                encoding: MouseEncoding::Sgr,
            }
        );

        emu.process(b"\x1b[?1002h");
        assert_eq!(
            emu.mouse_mode(),
            MouseMode {
                report: MouseReportMode::Drag,
                encoding: MouseEncoding::Sgr,
            }
        );

        emu.process(b"\x1b[?1002l\x1b[?1006l");
        assert_eq!(emu.mouse_mode(), MouseMode::default());
    }

    #[test]
    fn test_unicode() {
        let mut emu = AlacrittyEmulator::new(80, 24);
        emu.process("日本語".as_bytes());

        // Wide characters should be stored correctly
        let cell = emu.get_cell(0, 0);
        assert_eq!(cell.character, '日');
    }

    #[test]
    fn test_unicode_cursor_position() {
        let mut emu = AlacrittyEmulator::new(80, 24);
        emu.process("日本語".as_bytes());

        // 3 CJK chars, each 2 columns wide = 6 columns
        let (x, y) = emu.cursor_position();
        assert_eq!(x, 6, "Cursor should be at column 6 after 3 CJK chars");
        assert_eq!(y, 0);

        // Text extraction should give us the 3 chars without spacers
        let text = emu.get_line_text(0);
        assert!(text.starts_with("日本語"), "Text should be '日本語', got: {}", text);
    }

    #[test]
    fn test_unicode_incremental_echo() {
        // Simulate incremental typing with local echo
        let mut emu = AlacrittyEmulator::new(80, 24);
        
        // Type 'a' and cursor should be at 1
        emu.process(b"a");
        let (x, _) = emu.cursor_position();
        assert_eq!(x, 1, "After 'a', cursor at 1");

        // Type '日' (3 UTF-8 bytes: e6 97 a5) and cursor should be at 3 (1 + 2)
        emu.process("日".as_bytes());
        let (x, _) = emu.cursor_position();
        assert_eq!(x, 3, "After 'a日', cursor at 3 (1 + 2 width)");

        // Type 'b' and cursor should be at 4
        emu.process(b"b");
        let (x, _) = emu.cursor_position();
        assert_eq!(x, 4, "After 'a日b', cursor at 4");

        // Check the line content
        let text = emu.get_line_text(0);
        assert!(text.starts_with("a日b"), "Text should be 'a日b', got: {}", text);
    }

    #[test]
    fn test_pty_style_echo_simulation() {
        // This simulates what bash might output when typing "echo '日'"
        // Bash typically just echoes the character at the cursor position
        let mut emu = AlacrittyEmulator::new(80, 24);
        
        // Start with a prompt
        emu.process(b"$ ");
        assert_eq!(emu.cursor_position().0, 2, "Cursor at 2 after prompt");
        
        // Type 'e' - bash echoes 'e'
        emu.process(b"e");
        let line = emu.get_line_text(0);
        assert!(line.starts_with("$ e"), "After 'e', got '{}'", line);
        assert_eq!(emu.cursor_position().0, 3);
        
        // Type 'c' - bash echoes 'c'  
        emu.process(b"c");
        let line = emu.get_line_text(0);
        assert!(line.starts_with("$ ec"), "After 'c', got '{}'", line);
        assert_eq!(emu.cursor_position().0, 4);
        
        // Type 'h' - bash echoes 'h'
        emu.process(b"h");
        let line = emu.get_line_text(0);
        assert!(line.starts_with("$ ech"), "After 'h', got '{}'", line);
        
        // Type 'o' - bash echoes 'o'
        emu.process(b"o");
        let line = emu.get_line_text(0);
        assert!(line.starts_with("$ echo"), "After 'o', got '{}'", line);
        
        // Type ' ' and "'"
        emu.process(b" '");
        let line = emu.get_line_text(0);
        assert!(line.starts_with("$ echo '"), "After \" '\", got '{}'", line);
        // "$ echo '" = 8 chars
        assert_eq!(emu.cursor_position().0, 8);
        
        // Type CJK char - 日 (wide, 2 columns)
        emu.process("日".as_bytes());
        let line = emu.get_line_text(0);
        assert!(line.starts_with("$ echo '日"), "After '日', got '{}'", line);
        // 8 + 2 = 10
        assert_eq!(emu.cursor_position().0, 10, "Cursor at 10 after wide char");
        
        // Type CJK char - 本 (wide, 2 columns)
        emu.process("本".as_bytes());
        let line = emu.get_line_text(0);
        assert!(line.starts_with("$ echo '日本"), "After '本', got '{}'", line);
        // 10 + 2 = 12
        assert_eq!(emu.cursor_position().0, 12, "Cursor at 12 after second wide char");
    }
}
