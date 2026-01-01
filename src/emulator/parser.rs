//! ANSI escape sequence parser using the vte crate

use super::cell::Color;
use super::grid::TerminalGrid;
use vte::{Params, Parser, Perform};

/// Parser for ANSI escape sequences
pub struct AnsiParser {
    parser: Parser,
}

impl AnsiParser {
    pub fn new() -> Self {
        Self {
            parser: Parser::new(),
        }
    }

    /// Process bytes through the parser, updating the grid
    pub fn process(&mut self, bytes: &[u8], grid: &mut TerminalGrid) {
        let mut performer = GridPerformer { grid };
        for byte in bytes {
            self.parser.advance(&mut performer, *byte);
        }
    }
}

/// Performer that applies parsed sequences to the grid
struct GridPerformer<'a> {
    grid: &'a mut TerminalGrid,
}

impl<'a> Perform for GridPerformer<'a> {
    fn print(&mut self, c: char) {
        self.grid.put_char(c);
    }

    fn execute(&mut self, byte: u8) {
        match byte {
            // Bell
            0x07 => {}
            // Backspace
            0x08 => self.grid.backspace(),
            // Tab
            0x09 => self.grid.tab(),
            // Newline (LF) - in terminal emulators, LF typically implies CR as well
            0x0A => {
                self.grid.carriage_return();
                self.grid.newline();
            }
            // Carriage return
            0x0D => self.grid.carriage_return(),
            _ => {}
        }
    }

    fn hook(&mut self, _params: &Params, _intermediates: &[u8], _ignore: bool, _action: char) {
        // DCS (Device Control String) - not commonly needed
    }

    fn put(&mut self, _byte: u8) {
        // For DCS sequences
    }

    fn unhook(&mut self) {
        // End of DCS
    }

    fn osc_dispatch(&mut self, _params: &[&[u8]], _bell_terminated: bool) {
        // OSC (Operating System Command) - e.g., setting window title
        // params[0] is usually the command number
        // For now, we'll ignore these
    }

    fn csi_dispatch(&mut self, params: &Params, intermediates: &[u8], _ignore: bool, action: char) {
        let params: Vec<u16> = params.iter().map(|p| p[0]).collect();
        
        // Check for private mode sequences (CSI ? ... h/l)
        let is_private = intermediates.contains(&b'?');
        
        if is_private {
            match action {
                'h' => {
                    // Set private mode
                    for &param in &params {
                        match param {
                            // Show cursor
                            25 => self.grid.cursor_visible = true,
                            // Alternate screen buffer (with save/restore)
                            1049 => self.grid.enter_alternate_screen(),
                            // Other private modes we might want to handle:
                            // 1 - Application cursor keys
                            // 47 - Alternate screen buffer (without save/restore)
                            // 1047 - Alternate screen buffer
                            47 | 1047 => self.grid.enter_alternate_screen(),
                            _ => {}
                        }
                    }
                    return;
                }
                'l' => {
                    // Reset private mode
                    for &param in &params {
                        match param {
                            // Hide cursor
                            25 => self.grid.cursor_visible = false,
                            1049 | 47 | 1047 => self.grid.leave_alternate_screen(),
                            _ => {}
                        }
                    }
                    return;
                }
                _ => return,
            }
        }
        
        match action {
            // Cursor Up
            'A' => {
                let n = params.first().copied().unwrap_or(1).max(1) as isize;
                self.grid.move_cursor_relative(0, -n);
            }
            // Cursor Down
            'B' => {
                let n = params.first().copied().unwrap_or(1).max(1) as isize;
                self.grid.move_cursor_relative(0, n);
            }
            // Cursor Forward
            'C' => {
                let n = params.first().copied().unwrap_or(1).max(1) as isize;
                self.grid.move_cursor_relative(n, 0);
            }
            // Cursor Back
            'D' => {
                let n = params.first().copied().unwrap_or(1).max(1) as isize;
                self.grid.move_cursor_relative(-n, 0);
            }
            // Cursor Next Line
            'E' => {
                let n = params.first().copied().unwrap_or(1).max(1) as isize;
                self.grid.move_cursor_relative(0, n);
                self.grid.carriage_return();
            }
            // Cursor Previous Line
            'F' => {
                let n = params.first().copied().unwrap_or(1).max(1) as isize;
                self.grid.move_cursor_relative(0, -n);
                self.grid.carriage_return();
            }
            // Cursor Horizontal Absolute
            'G' => {
                let col = params.first().copied().unwrap_or(1).max(1) as usize - 1;
                self.grid.move_cursor_to(col, self.grid.cursor_y);
            }
            // Cursor Position (row;col) - 1-indexed
            'H' | 'f' => {
                let row = params.first().copied().unwrap_or(1).max(1) as usize - 1;
                let col = params.get(1).copied().unwrap_or(1).max(1) as usize - 1;
                self.grid.move_cursor_to(col, row);
            }
            // Erase in Display
            'J' => {
                let mode = params.first().copied().unwrap_or(0);
                match mode {
                    0 => self.grid.clear_to_end_of_screen(),
                    1 => self.grid.clear_to_start_of_screen(),
                    2 | 3 => self.grid.clear_screen(),
                    _ => {}
                }
            }
            // Erase in Line
            'K' => {
                let mode = params.first().copied().unwrap_or(0);
                match mode {
                    0 => self.grid.clear_to_end_of_line(),
                    1 => self.grid.clear_to_start_of_line(),
                    2 => self.grid.clear_line(),
                    _ => {}
                }
            }
            // Insert Lines
            'L' => {
                let n = params.first().copied().unwrap_or(1).max(1) as usize;
                self.grid.insert_lines(n);
            }
            // Delete Lines
            'M' => {
                let n = params.first().copied().unwrap_or(1).max(1) as usize;
                self.grid.delete_lines(n);
            }
            // Erase Characters
            'X' => {
                let n = params.first().copied().unwrap_or(1).max(1) as usize;
                self.grid.erase_chars(n);
            }
            // Scroll Up
            'S' => {
                let n = params.first().copied().unwrap_or(1).max(1) as usize;
                self.grid.scroll_up(n);
            }
            // Scroll Down
            'T' => {
                let n = params.first().copied().unwrap_or(1).max(1) as usize;
                self.grid.scroll_down(n);
            }
            // SGR (Select Graphic Rendition)
            'm' => {
                self.handle_sgr(&params);
            }
            // Cursor Vertical Absolute
            'd' => {
                let row = params.first().copied().unwrap_or(1).max(1) as usize - 1;
                self.grid.move_cursor_to(self.grid.cursor_x, row);
            }
            // Save cursor position
            's' => {
                // TODO: save cursor state
            }
            // Restore cursor position
            'u' => {
                // TODO: restore cursor state
            }
            _ => {
                // Unknown CSI sequence
            }
        }
    }

    fn esc_dispatch(&mut self, _intermediates: &[u8], _ignore: bool, byte: u8) {
        match byte {
            // RIS (Reset to Initial State)
            b'c' => {
                self.grid.clear_screen();
                self.grid.move_cursor_to(0, 0);
                self.grid.current_attrs.reset();
            }
            // IND (Index - move down, scroll if at bottom)
            b'D' => {
                self.grid.newline();
            }
            // NEL (Next Line)
            b'E' => {
                self.grid.carriage_return();
                self.grid.newline();
            }
            // RI (Reverse Index - move up, scroll if at top)
            b'M' => {
                if self.grid.cursor_y == 0 {
                    self.grid.scroll_down(1);
                } else {
                    self.grid.move_cursor_relative(0, -1);
                }
            }
            _ => {}
        }
    }
}

impl<'a> GridPerformer<'a> {
    fn handle_sgr(&mut self, params: &[u16]) {
        if params.is_empty() {
            self.grid.current_attrs.reset();
            return;
        }

        let mut iter = params.iter().peekable();
        
        while let Some(&param) = iter.next() {
            match param {
                // Reset
                0 => self.grid.current_attrs.reset(),
                
                // Bold
                1 => self.grid.current_attrs.bold = true,
                
                // Dim
                2 => self.grid.current_attrs.dim = true,
                
                // Italic
                3 => self.grid.current_attrs.italic = true,
                
                // Underline
                4 => self.grid.current_attrs.underline = true,
                
                // Inverse
                7 => self.grid.current_attrs.inverse = true,
                
                // Hidden
                8 => self.grid.current_attrs.hidden = true,
                
                // Strikethrough
                9 => self.grid.current_attrs.strikethrough = true,
                
                // Normal intensity
                22 => {
                    self.grid.current_attrs.bold = false;
                    self.grid.current_attrs.dim = false;
                }
                
                // Not italic
                23 => self.grid.current_attrs.italic = false,
                
                // Not underlined
                24 => self.grid.current_attrs.underline = false,
                
                // Not inverse
                27 => self.grid.current_attrs.inverse = false,
                
                // Not hidden
                28 => self.grid.current_attrs.hidden = false,
                
                // Not strikethrough
                29 => self.grid.current_attrs.strikethrough = false,
                
                // Foreground colors (30-37)
                30..=37 => {
                    self.grid.current_attrs.fg_color = Color::from_sgr_basic(param);
                }
                
                // Extended foreground color
                38 => {
                    if let Some(color) = self.parse_extended_color(&mut iter) {
                        self.grid.current_attrs.fg_color = Some(color);
                    }
                }
                
                // Default foreground
                39 => self.grid.current_attrs.fg_color = None,
                
                // Background colors (40-47)
                40..=47 => {
                    self.grid.current_attrs.bg_color = Color::from_sgr_basic(param);
                }
                
                // Extended background color
                48 => {
                    if let Some(color) = self.parse_extended_color(&mut iter) {
                        self.grid.current_attrs.bg_color = Some(color);
                    }
                }
                
                // Default background
                49 => self.grid.current_attrs.bg_color = None,
                
                // Bright foreground colors (90-97)
                90..=97 => {
                    self.grid.current_attrs.fg_color = Color::from_sgr_bright(param);
                }
                
                // Bright background colors (100-107)
                100..=107 => {
                    self.grid.current_attrs.bg_color = Color::from_sgr_bright(param);
                }
                
                _ => {}
            }
        }
    }

    fn parse_extended_color<'b, I>(&self, iter: &mut std::iter::Peekable<I>) -> Option<Color>
    where
        I: Iterator<Item = &'b u16>,
    {
        match iter.next() {
            // 256-color palette
            Some(&5) => {
                iter.next().map(|&idx| Color::Indexed(idx as u8))
            }
            // True color RGB
            Some(&2) => {
                let r = iter.next().copied().unwrap_or(0) as u8;
                let g = iter.next().copied().unwrap_or(0) as u8;
                let b = iter.next().copied().unwrap_or(0) as u8;
                Some(Color::Rgb(r, g, b))
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_text() {
        let mut parser = AnsiParser::new();
        let mut grid = TerminalGrid::new(80, 24);
        
        parser.process(b"Hello", &mut grid);
        
        assert_eq!(grid.get_line_text(0).trim(), "Hello");
    }

    #[test]
    fn test_parse_cursor_movement() {
        let mut parser = AnsiParser::new();
        let mut grid = TerminalGrid::new(80, 24);
        
        parser.process(b"\x1b[5;10H", &mut grid); // Move to row 5, col 10 (1-indexed)
        
        assert_eq!(grid.cursor_y, 4); // 0-indexed
        assert_eq!(grid.cursor_x, 9);
    }

    #[test]
    fn test_parse_colors() {
        let mut parser = AnsiParser::new();
        let mut grid = TerminalGrid::new(80, 24);
        
        parser.process(b"\x1b[31m", &mut grid); // Red foreground
        
        assert_eq!(grid.current_attrs.fg_color, Some(Color::Red));
    }

    #[test]
    fn test_parse_clear_screen() {
        let mut parser = AnsiParser::new();
        let mut grid = TerminalGrid::new(80, 24);
        
        parser.process(b"Hello\x1b[2J", &mut grid);
        
        // Screen should be cleared
        assert_eq!(grid.get_line_text(0).trim(), "");
    }
}
