//! Terminal grid - the 2D character buffer

use super::cell::{Cell, CellAttributes};
use crate::terminal::Line;

/// The terminal display grid
pub struct TerminalGrid {
    cells: Vec<Vec<Cell>>,
    pub cols: usize,
    pub rows: usize,
    pub cursor_x: usize,
    pub cursor_y: usize,
    pub current_attrs: CellAttributes,
    /// Lines that have scrolled off the top
    pub scrollback: Vec<Vec<Cell>>,
    /// Maximum scrollback lines to keep
    pub max_scrollback: usize,
}

impl TerminalGrid {
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
            scrollback: Vec::new(),
            max_scrollback: 1000,
        }
    }

    /// Put a character at the cursor position and advance cursor
    pub fn put_char(&mut self, c: char) {
        if self.cursor_x >= self.cols {
            // Wrap to next line
            self.cursor_x = 0;
            self.cursor_y += 1;
        }

        if self.cursor_y >= self.rows {
            self.scroll_up(1);
            self.cursor_y = self.rows - 1;
        }

        if self.cursor_y < self.rows && self.cursor_x < self.cols {
            self.cells[self.cursor_y][self.cursor_x] = Cell::new(c, self.current_attrs.clone());
            self.cursor_x += 1;
        }
    }

    /// Handle newline (move to next line, possibly scroll)
    pub fn newline(&mut self) {
        self.cursor_y += 1;
        if self.cursor_y >= self.rows {
            self.scroll_up(1);
            self.cursor_y = self.rows - 1;
        }
    }

    /// Handle carriage return (move cursor to beginning of line)
    pub fn carriage_return(&mut self) {
        self.cursor_x = 0;
    }

    /// Handle backspace
    pub fn backspace(&mut self) {
        if self.cursor_x > 0 {
            self.cursor_x -= 1;
        }
    }

    /// Handle tab
    pub fn tab(&mut self) {
        // Move to next tab stop (every 8 columns)
        let next_tab = ((self.cursor_x / 8) + 1) * 8;
        self.cursor_x = next_tab.min(self.cols - 1);
    }

    /// Scroll the display up by n lines
    pub fn scroll_up(&mut self, n: usize) {
        for _ in 0..n {
            if !self.cells.is_empty() {
                let line = self.cells.remove(0);
                if self.scrollback.len() >= self.max_scrollback {
                    self.scrollback.remove(0);
                }
                self.scrollback.push(line);
                self.cells.push((0..self.cols).map(|_| Cell::empty()).collect());
            }
        }
    }

    /// Scroll the display down by n lines
    pub fn scroll_down(&mut self, n: usize) {
        for _ in 0..n {
            if !self.cells.is_empty() {
                self.cells.pop();
                self.cells.insert(0, (0..self.cols).map(|_| Cell::empty()).collect());
            }
        }
    }

    /// Move cursor to absolute position
    pub fn move_cursor_to(&mut self, x: usize, y: usize) {
        self.cursor_x = x.min(self.cols.saturating_sub(1));
        self.cursor_y = y.min(self.rows.saturating_sub(1));
    }

    /// Move cursor relative
    pub fn move_cursor_relative(&mut self, dx: isize, dy: isize) {
        let new_x = (self.cursor_x as isize + dx).max(0) as usize;
        let new_y = (self.cursor_y as isize + dy).max(0) as usize;
        self.move_cursor_to(new_x, new_y);
    }

    /// Clear from cursor to end of line
    pub fn clear_to_end_of_line(&mut self) {
        for x in self.cursor_x..self.cols {
            self.cells[self.cursor_y][x] = Cell::empty();
        }
    }

    /// Clear from beginning of line to cursor
    pub fn clear_to_start_of_line(&mut self) {
        for x in 0..=self.cursor_x.min(self.cols - 1) {
            self.cells[self.cursor_y][x] = Cell::empty();
        }
    }

    /// Clear entire line
    pub fn clear_line(&mut self) {
        for x in 0..self.cols {
            self.cells[self.cursor_y][x] = Cell::empty();
        }
    }

    /// Clear from cursor to end of screen
    pub fn clear_to_end_of_screen(&mut self) {
        self.clear_to_end_of_line();
        for y in (self.cursor_y + 1)..self.rows {
            for x in 0..self.cols {
                self.cells[y][x] = Cell::empty();
            }
        }
    }

    /// Clear from beginning of screen to cursor
    pub fn clear_to_start_of_screen(&mut self) {
        self.clear_to_start_of_line();
        for y in 0..self.cursor_y {
            for x in 0..self.cols {
                self.cells[y][x] = Cell::empty();
            }
        }
    }

    /// Clear entire screen
    pub fn clear_screen(&mut self) {
        for y in 0..self.rows {
            for x in 0..self.cols {
                self.cells[y][x] = Cell::empty();
            }
        }
    }

    /// Resize the grid
    pub fn resize(&mut self, cols: usize, rows: usize) {
        // Adjust rows
        while self.cells.len() < rows {
            self.cells.push((0..cols).map(|_| Cell::empty()).collect());
        }
        while self.cells.len() > rows {
            self.cells.pop();
        }

        // Adjust columns
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
        self.cursor_x = self.cursor_x.min(cols.saturating_sub(1));
        self.cursor_y = self.cursor_y.min(rows.saturating_sub(1));
    }

    /// Get a cell at position
    pub fn get_cell(&self, x: usize, y: usize) -> &Cell {
        &self.cells[y][x]
    }

    /// Get text content of a line (trimmed)
    pub fn get_line_text(&self, y: usize) -> String {
        self.cells[y].iter().map(|c| c.character).collect()
    }

    /// Convert to Line enum for rendering
    pub fn to_lines(&self) -> Vec<Line> {
        self.cells
            .iter()
            .map(|row| {
                let text: String = row.iter().map(|c| c.character).collect();
                // Trim trailing spaces for cleaner output
                Line::Text(text.trim_end().to_string())
            })
            .collect()
    }

    /// Erase n characters from cursor position
    pub fn erase_chars(&mut self, n: usize) {
        for i in 0..n {
            let x = self.cursor_x + i;
            if x < self.cols {
                self.cells[self.cursor_y][x] = Cell::empty();
            }
        }
    }

    /// Insert n blank lines at cursor, scrolling down
    pub fn insert_lines(&mut self, n: usize) {
        for _ in 0..n {
            if self.cursor_y < self.rows {
                self.cells.remove(self.rows - 1);
                self.cells.insert(self.cursor_y, (0..self.cols).map(|_| Cell::empty()).collect());
            }
        }
    }

    /// Delete n lines at cursor, scrolling up
    pub fn delete_lines(&mut self, n: usize) {
        for _ in 0..n {
            if self.cursor_y < self.rows {
                self.cells.remove(self.cursor_y);
                self.cells.push((0..self.cols).map(|_| Cell::empty()).collect());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_put_char() {
        let mut grid = TerminalGrid::new(80, 24);
        grid.put_char('H');
        grid.put_char('i');
        
        assert_eq!(grid.get_cell(0, 0).character, 'H');
        assert_eq!(grid.get_cell(1, 0).character, 'i');
        assert_eq!(grid.cursor_x, 2);
    }

    #[test]
    fn test_newline() {
        let mut grid = TerminalGrid::new(80, 24);
        grid.put_char('A');
        grid.carriage_return();
        grid.newline();
        grid.put_char('B');
        
        assert_eq!(grid.get_cell(0, 0).character, 'A');
        assert_eq!(grid.get_cell(0, 1).character, 'B');
    }

    #[test]
    fn test_scroll() {
        let mut grid = TerminalGrid::new(80, 3);
        grid.put_char('1');
        grid.carriage_return();
        grid.newline();
        grid.put_char('2');
        grid.carriage_return();
        grid.newline();
        grid.put_char('3');
        grid.carriage_return();
        grid.newline();
        grid.put_char('4');
        
        // Line '1' should have scrolled off
        assert_eq!(grid.scrollback.len(), 1);
        assert_eq!(grid.get_cell(0, 0).character, '2');
        assert_eq!(grid.get_cell(0, 1).character, '3');
        assert_eq!(grid.get_cell(0, 2).character, '4');
    }

    #[test]
    fn test_clear_line() {
        let mut grid = TerminalGrid::new(80, 24);
        for c in "Hello".chars() {
            grid.put_char(c);
        }
        grid.cursor_x = 2;
        grid.clear_to_end_of_line();
        
        assert_eq!(grid.get_cell(0, 0).character, 'H');
        assert_eq!(grid.get_cell(1, 0).character, 'e');
        assert_eq!(grid.get_cell(2, 0).character, ' ');
        assert_eq!(grid.get_cell(3, 0).character, ' ');
    }
}
