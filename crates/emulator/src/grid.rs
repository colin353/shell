//! Terminal grid - the 2D character buffer

use super::cell::{Cell, CellAttributes, Line};

/// Character set designations
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

impl Default for CharSet {
    fn default() -> Self {
        CharSet::Ascii
    }
}

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
    /// Saved main screen buffer (for alternate screen support)
    saved_screen: Option<SavedScreen>,
    /// Whether we're currently in alternate screen mode
    pub in_alternate_screen: bool,
    /// Whether the cursor is visible
    pub cursor_visible: bool,
    /// Scroll region top margin (0-indexed, inclusive)
    pub scroll_top: usize,
    /// Scroll region bottom margin (0-indexed, inclusive)
    pub scroll_bottom: usize,
    /// Origin mode (DECOM) - when true, cursor positioning is relative to scroll region
    pub origin_mode: bool,
    /// Pending wrap state - when true, next printable character will trigger autowrap
    pending_wrap: bool,
    /// Auto-wrap mode (DECAWM) - when true, cursor wraps at end of line
    pub autowrap: bool,
    /// Tab stops - sorted list of column positions where tabs stop
    tab_stops: Vec<usize>,
    /// Last printed character for REP sequence
    last_char: Option<char>,
    /// Saved cursor state for DECSC/DECRC
    saved_cursor: Option<SavedCursor>,
    /// Character set designated to G0
    pub charset_g0: CharSet,
    /// Character set designated to G1
    pub charset_g1: CharSet,
    /// Whether G1 is active (true) or G0 is active (false)
    /// Controlled by SO (0x0E) and SI (0x0F)
    pub gl_is_g1: bool,
    /// Queued responses to be sent back to the PTY (e.g., for DSR queries)
    responses: Vec<Vec<u8>>,
}

/// Saved cursor state for DECSC/DECRC
#[derive(Clone)]
struct SavedCursor {
    x: usize,
    y: usize,
    attrs: CellAttributes,
    charset_g0: CharSet,
    charset_g1: CharSet,
    gl_is_g1: bool,
}

/// Saved screen state for alternate screen buffer
struct SavedScreen {
    cells: Vec<Vec<Cell>>,
    cursor_x: usize,
    cursor_y: usize,
    scrollback: Vec<Vec<Cell>>,
}

impl TerminalGrid {
    pub fn new(cols: usize, rows: usize) -> Self {
        let cells = (0..rows)
            .map(|_| (0..cols).map(|_| Cell::empty()).collect())
            .collect();

        // Initialize default tab stops at every 8 columns
        let tab_stops: Vec<usize> = (0..cols).filter(|&c| c > 0 && c % 8 == 0).collect();

        Self {
            cells,
            cols,
            rows,
            cursor_x: 0,
            cursor_y: 0,
            current_attrs: CellAttributes::default(),
            scrollback: Vec::new(),
            max_scrollback: 1000,
            saved_screen: None,
            in_alternate_screen: false,
            cursor_visible: true,
            scroll_top: 0,
            scroll_bottom: rows - 1,
            origin_mode: false,
            pending_wrap: false,
            autowrap: true,
            tab_stops,
            last_char: None,
            saved_cursor: None,
            charset_g0: CharSet::Ascii,
            charset_g1: CharSet::Ascii,
            gl_is_g1: false,
            responses: Vec::new(),
        }
    }

    /// Put a character at the cursor position and advance cursor
    pub fn put_char(&mut self, c: char) {
        // Handle pending wrap (delayed autowrap) - only if autowrap is enabled
        if self.pending_wrap && self.autowrap {
            // Wrap to next line
            self.cursor_x = 0;
            self.pending_wrap = false;
            // When wrapping, check if we need to scroll within the scroll region
            if self.cursor_y == self.scroll_bottom {
                self.scroll_region_up(1);
            } else if self.cursor_y < self.rows - 1 {
                self.cursor_y += 1;
            }
        } else if self.pending_wrap && !self.autowrap {
            // If autowrap is disabled, clear pending wrap and overwrite last char
            self.pending_wrap = false;
            // cursor_x is already at cols - 1, so we'll overwrite that position
        }

        if self.cursor_y < self.rows && self.cursor_x < self.cols {
            self.cells[self.cursor_y][self.cursor_x] = Cell::new(c, self.current_attrs.clone());
            self.last_char = Some(c);
            self.cursor_x += 1;

            // If cursor is now past the last column, set pending wrap instead of wrapping immediately
            if self.cursor_x >= self.cols {
                self.cursor_x = self.cols - 1; // Keep cursor at last column
                self.pending_wrap = true;
            }
        }
    }

    /// Repeat the last printed character n times (REP sequence)
    pub fn repeat_char(&mut self, n: usize) {
        if let Some(c) = self.last_char {
            for _ in 0..n {
                self.put_char(c);
            }
        }
    }

    /// Handle newline (move to next line, possibly scroll)
    pub fn newline(&mut self) {
        if self.cursor_y == self.scroll_bottom {
            // At bottom of scroll region, scroll the region up
            self.scroll_region_up(1);
        } else if self.cursor_y < self.rows - 1 {
            self.cursor_y += 1;
        }
    }

    /// Handle carriage return (move cursor to beginning of line)
    pub fn carriage_return(&mut self) {
        self.pending_wrap = false;
        self.cursor_x = 0;
    }

    /// Handle backspace
    pub fn backspace(&mut self) {
        // Backspace clears pending wrap
        if self.pending_wrap {
            self.pending_wrap = false;
            // When in pending wrap state, the cursor is conceptually at index `cols`.
            // Backspace should move it to `cols - 1`.
            // However, our cursor_x is clamped to `cols - 1`.
            // If we just clear pending_wrap, we are at `cols - 1`.
            // But some tests (vttest) seem to expect backspace to move to `cols - 2`
            // effectively unwrapping AND moving left.
            if self.cursor_x > 0 {
                self.cursor_x -= 1;
            }
        } else if self.cursor_x > 0 {
            self.cursor_x -= 1;
        }
    }

    /// Handle tab
    pub fn tab(&mut self) {
        // Tab clears pending wrap
        self.pending_wrap = false;
        // Find the next tab stop after current cursor position
        let next_tab = self
            .tab_stops
            .iter()
            .find(|&&stop| stop > self.cursor_x)
            .copied();
        match next_tab {
            Some(stop) => self.cursor_x = stop.min(self.cols - 1),
            None => self.cursor_x = self.cols - 1, // No more tab stops, move to end
        }
    }

    /// Set a tab stop at the current cursor column (HTS - Horizontal Tab Set)
    pub fn set_tab_stop(&mut self) {
        let col = self.cursor_x;
        // Insert in sorted order if not already present
        if let Err(pos) = self.tab_stops.binary_search(&col) {
            self.tab_stops.insert(pos, col);
        }
    }

    /// Clear the tab stop at the current cursor column (TBC mode 0)
    pub fn clear_tab_stop(&mut self) {
        let col = self.cursor_x;
        if let Ok(pos) = self.tab_stops.binary_search(&col) {
            self.tab_stops.remove(pos);
        }
    }

    /// Clear all tab stops (TBC mode 3)
    pub fn clear_all_tab_stops(&mut self) {
        self.tab_stops.clear();
    }

    /// Save cursor state (DECSC - ESC 7)
    pub fn save_cursor(&mut self) {
        self.saved_cursor = Some(SavedCursor {
            x: self.cursor_x,
            y: self.cursor_y,
            attrs: self.current_attrs.clone(),
            charset_g0: self.charset_g0,
            charset_g1: self.charset_g1,
            gl_is_g1: self.gl_is_g1,
        });
    }

    /// Restore cursor state (DECRC - ESC 8)
    pub fn restore_cursor(&mut self) {
        if let Some(saved) = self.saved_cursor.clone() {
            self.cursor_x = saved.x.min(self.cols.saturating_sub(1));
            self.cursor_y = saved.y.min(self.rows.saturating_sub(1));
            self.current_attrs = saved.attrs;
            self.charset_g0 = saved.charset_g0;
            self.charset_g1 = saved.charset_g1;
            self.gl_is_g1 = saved.gl_is_g1;
            self.pending_wrap = false;
        }
    }

    /// Scroll the display up by n lines (full screen)
    pub fn scroll_up(&mut self, n: usize) {
        for _ in 0..n {
            if !self.cells.is_empty() {
                let line = self.cells.remove(0);
                if self.scrollback.len() >= self.max_scrollback {
                    self.scrollback.remove(0);
                }
                self.scrollback.push(line);
                self.cells
                    .push((0..self.cols).map(|_| Cell::empty()).collect());
            }
        }
    }

    /// Scroll the display down by n lines (full screen)
    pub fn scroll_down(&mut self, n: usize) {
        for _ in 0..n {
            if !self.cells.is_empty() {
                self.cells.pop();
                self.cells
                    .insert(0, (0..self.cols).map(|_| Cell::empty()).collect());
            }
        }
    }

    /// Scroll the scroll region up by n lines
    pub fn scroll_region_up(&mut self, n: usize) {
        for _ in 0..n {
            if self.scroll_top < self.scroll_bottom {
                // Remove the top line of the scroll region
                let line = self.cells.remove(self.scroll_top);
                // Only add to scrollback if scroll region is the full screen
                if self.scroll_top == 0 && self.scroll_bottom == self.rows - 1 {
                    if self.scrollback.len() >= self.max_scrollback {
                        self.scrollback.remove(0);
                    }
                    self.scrollback.push(line);
                }
                // Insert a new empty line at the bottom of the scroll region
                self.cells.insert(
                    self.scroll_bottom,
                    (0..self.cols).map(|_| Cell::empty()).collect(),
                );
            }
        }
    }

    /// Scroll the scroll region down by n lines
    pub fn scroll_region_down(&mut self, n: usize) {
        for _ in 0..n {
            if self.scroll_top < self.scroll_bottom {
                // Remove the bottom line of the scroll region
                self.cells.remove(self.scroll_bottom);
                // Insert a new empty line at the top of the scroll region
                self.cells.insert(
                    self.scroll_top,
                    (0..self.cols).map(|_| Cell::empty()).collect(),
                );
            }
        }
    }

    /// Reverse index - move cursor up, scroll region down if at top of scroll region
    pub fn reverse_index(&mut self) {
        if self.cursor_y == self.scroll_top {
            // At top of scroll region, scroll the region down
            self.scroll_region_down(1);
        } else if self.cursor_y > 0 {
            self.cursor_y -= 1;
        }
    }

    /// Set the scroll region (DECSTBM)
    /// top and bottom are 1-indexed as per VT100 spec
    pub fn set_scroll_region(&mut self, top: usize, bottom: usize) {
        // Convert to 0-indexed
        let top = top.saturating_sub(1);
        let bottom = if bottom == 0 {
            self.rows - 1
        } else {
            (bottom.saturating_sub(1)).min(self.rows - 1)
        };

        if top < bottom && bottom < self.rows {
            self.scroll_top = top;
            self.scroll_bottom = bottom;
        }

        // DECSTBM also moves cursor to home position
        // In origin mode, home is top-left of scroll region; otherwise it's (0,0)
        if self.origin_mode {
            self.cursor_x = 0;
            self.cursor_y = self.scroll_top;
        } else {
            self.cursor_x = 0;
            self.cursor_y = 0;
        }
    }

    /// Set origin mode (DECOM)
    pub fn set_origin_mode(&mut self, enabled: bool) {
        self.origin_mode = enabled;
        // Setting origin mode moves cursor to home position
        if enabled {
            self.cursor_x = 0;
            self.cursor_y = self.scroll_top;
        } else {
            self.cursor_x = 0;
            self.cursor_y = 0;
        }
    }

    /// Move cursor to absolute position
    /// When origin_mode is true, y is relative to the scroll region
    pub fn move_cursor_to(&mut self, x: usize, y: usize) {
        self.pending_wrap = false;
        self.cursor_x = x.min(self.cols.saturating_sub(1));
        if self.origin_mode {
            // In origin mode, y is relative to scroll region top
            let absolute_y = self.scroll_top + y;
            // Cursor is confined to scroll region
            self.cursor_y = absolute_y.min(self.scroll_bottom);
        } else {
            self.cursor_y = y.min(self.rows.saturating_sub(1));
        }
    }

    /// Move cursor to absolute position without origin mode translation
    /// Used for commands that work in absolute screen coordinates
    pub fn move_cursor_to_absolute(&mut self, x: usize, y: usize) {
        self.pending_wrap = false;
        self.cursor_x = x.min(self.cols.saturating_sub(1));
        self.cursor_y = y.min(self.rows.saturating_sub(1));
    }

    /// Move cursor relative
    /// When origin mode is enabled, cursor is constrained to scroll region
    pub fn move_cursor_relative(&mut self, dx: isize, dy: isize) {
        self.pending_wrap = false;
        let new_x = (self.cursor_x as isize + dx).max(0) as usize;
        let new_y = (self.cursor_y as isize + dy).max(0) as usize;

        self.cursor_x = new_x.min(self.cols.saturating_sub(1));

        if self.origin_mode {
            // In origin mode, cursor is constrained to scroll region
            self.cursor_y = new_y.max(self.scroll_top).min(self.scroll_bottom);
        } else {
            self.cursor_y = new_y.min(self.rows.saturating_sub(1));
        }
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

    /// Fill entire screen with a character (used by DECALN)
    pub fn fill_screen_with(&mut self, c: char) {
        for y in 0..self.rows {
            for x in 0..self.cols {
                self.cells[y][x] = Cell::new(c, CellAttributes::default());
            }
        }
    }

    /// Queue a response to be sent back to the PTY
    pub fn queue_response(&mut self, response: Vec<u8>) {
        self.responses.push(response);
    }

    /// Drain all queued responses
    pub fn drain_responses(&mut self) -> Vec<Vec<u8>> {
        std::mem::take(&mut self.responses)
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
        // Reset scroll region to full screen on resize
        self.scroll_top = 0;
        self.scroll_bottom = rows - 1;
    }

    /// Get a cell at position
    pub fn get_cell(&self, x: usize, y: usize) -> &Cell {
        &self.cells[y][x]
    }

    /// Get text content of a line (trimmed)
    pub fn get_line_text(&self, y: usize) -> String {
        self.cells[y].iter().map(|c| c.character).collect()
    }

    /// Convert to Line enum for rendering, preserving cell attributes
    pub fn to_lines(&self) -> Vec<Line> {
        self.cells
            .iter()
            .map(|row| {
                // Find last non-space character to trim trailing spaces
                let last_non_space = row.iter().rposition(|c| c.character != ' ');
                match last_non_space {
                    Some(idx) => Line::Cells(row[..=idx].to_vec()),
                    None => Line::Cells(Vec::new()),
                }
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
                self.cells.insert(
                    self.cursor_y,
                    (0..self.cols).map(|_| Cell::empty()).collect(),
                );
            }
        }
    }

    /// Delete n lines at cursor, scrolling up
    pub fn delete_lines(&mut self, n: usize) {
        for _ in 0..n {
            if self.cursor_y < self.rows {
                self.cells.remove(self.cursor_y);
                self.cells
                    .push((0..self.cols).map(|_| Cell::empty()).collect());
            }
        }
    }

    /// Enter alternate screen buffer (used by vim, less, etc.)
    pub fn enter_alternate_screen(&mut self) {
        if self.in_alternate_screen {
            return; // Already in alternate screen
        }

        // Save current screen state
        self.saved_screen = Some(SavedScreen {
            cells: self.cells.clone(),
            cursor_x: self.cursor_x,
            cursor_y: self.cursor_y,
            scrollback: self.scrollback.clone(),
        });

        // Clear screen for alternate buffer
        self.clear_screen();
        self.move_cursor_to(0, 0);
        self.scrollback.clear();
        self.in_alternate_screen = true;
    }

    /// Leave alternate screen buffer and restore main screen
    pub fn leave_alternate_screen(&mut self) {
        if !self.in_alternate_screen {
            return; // Not in alternate screen
        }

        // Restore saved screen state
        if let Some(saved) = self.saved_screen.take() {
            self.cells = saved.cells;
            self.cursor_x = saved.cursor_x;
            self.cursor_y = saved.cursor_y;
            self.scrollback = saved.scrollback;
        }

        self.in_alternate_screen = false;
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

    #[test]
    fn test_scroll_region() {
        let mut grid = TerminalGrid::new(80, 10);

        // Write content on each row
        for i in 0..10 {
            grid.move_cursor_to_absolute(0, i);
            grid.put_char(('0' as u8 + i as u8) as char);
        }

        // Set scroll region to rows 3-7 (1-indexed) = rows 2-6 (0-indexed)
        grid.set_scroll_region(3, 7);
        assert_eq!(grid.scroll_top, 2);
        assert_eq!(grid.scroll_bottom, 6);

        // Cursor should be at home
        assert_eq!(grid.cursor_x, 0);
        assert_eq!(grid.cursor_y, 0);

        // Move cursor to bottom of scroll region
        grid.move_cursor_to_absolute(0, 6);
        assert_eq!(grid.cursor_y, 6);

        // Trigger scroll by newline
        grid.newline();

        // Cursor should still be at row 6 (scroll_bottom)
        assert_eq!(grid.cursor_y, 6);

        // Row 2 content ('2') should have scrolled out
        // Row 3 ('3') should now be at row 2
        assert_eq!(grid.get_cell(0, 2).character, '3');
        assert_eq!(grid.get_cell(0, 3).character, '4');
        assert_eq!(grid.get_cell(0, 4).character, '5');
        assert_eq!(grid.get_cell(0, 5).character, '6');
        assert_eq!(grid.get_cell(0, 6).character, ' '); // New blank line

        // Content outside scroll region should be unchanged
        assert_eq!(grid.get_cell(0, 0).character, '0');
        assert_eq!(grid.get_cell(0, 1).character, '1');
        assert_eq!(grid.get_cell(0, 7).character, '7');
        assert_eq!(grid.get_cell(0, 8).character, '8');
        assert_eq!(grid.get_cell(0, 9).character, '9');
    }

    #[test]
    fn test_origin_mode() {
        let mut grid = TerminalGrid::new(80, 10);

        // Set scroll region to rows 3-7 (1-indexed) = rows 2-6 (0-indexed)
        grid.set_scroll_region(3, 7);

        // Enable origin mode
        grid.set_origin_mode(true);

        // Cursor should be at scroll_top
        assert_eq!(grid.cursor_y, 2);

        // Move to row 1 (origin-relative) should put cursor at scroll_top
        grid.move_cursor_to(0, 0);
        assert_eq!(grid.cursor_y, 2);

        // Move to row 5 (origin-relative) should put cursor at scroll_top + 4 = 6
        grid.move_cursor_to(0, 4);
        assert_eq!(grid.cursor_y, 6);

        // Move beyond scroll region should clamp to scroll_bottom
        grid.move_cursor_to(0, 10);
        assert_eq!(grid.cursor_y, 6);
    }

    #[test]
    fn test_autowrap_at_scroll_bottom() {
        let mut grid = TerminalGrid::new(10, 5);

        // Set scroll region to rows 2-4 (1-indexed) = rows 1-3 (0-indexed)
        grid.set_scroll_region(2, 4);

        // Write content on each row
        for i in 0..5 {
            grid.move_cursor_to_absolute(0, i);
            grid.put_char(('0' as u8 + i as u8) as char);
        }

        // Move to bottom of scroll region, last column
        grid.move_cursor_to_absolute(9, 3);

        // Write char at last column - cursor stays at last column, pending wrap set
        grid.put_char('A');
        assert_eq!(grid.cursor_x, 9);
        assert!(grid.pending_wrap);
        assert_eq!(grid.cursor_y, 3);

        // Write another char - triggers autowrap + scroll
        grid.put_char('B');

        // Cursor should be at column 1 (after 'B'), row 3 (scroll_bottom)
        assert_eq!(grid.cursor_x, 1);
        assert_eq!(grid.cursor_y, 3);

        // 'B' should be at column 0, row 3
        assert_eq!(grid.get_cell(0, 3).character, 'B');

        // Content should have scrolled within the region
        // Row 1 (was '1') should now be '2'
        assert_eq!(grid.get_cell(0, 1).character, '2');
        // Row 2 (was '2') should now be '3'
        assert_eq!(grid.get_cell(0, 2).character, '3');

        // Content outside scroll region should be unchanged
        assert_eq!(grid.get_cell(0, 0).character, '0');
        assert_eq!(grid.get_cell(0, 4).character, '4');
    }
}
