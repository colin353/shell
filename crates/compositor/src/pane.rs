use pty;

/// A match found during search, storing the line index and column range
#[derive(Clone, Debug, PartialEq)]
pub struct SearchMatch {
    /// Line index (negative values are scrollback lines, counting from -1 as most recent scrollback)
    /// 0 and positive values are grid lines
    pub line_index: isize,
    /// Starting column of the match
    pub start_col: usize,
    /// Ending column of the match (exclusive)
    pub end_col: usize,
}

pub struct Pane {
    pub terminal_emulator: emulator::TerminalEmulator,
    pub pty: Option<pty::PtyProcess>,
    pub read_buffer: [u8; 4096],
    /// Whether the pane is in scrollback mode
    pub scrollback_mode: bool,
    /// Scroll offset (number of lines scrolled up from the bottom)
    pub scroll_offset: usize,
    /// Whether the pane is in search mode (sub-mode of scrollback)
    pub search_mode: bool,
    /// Current search query
    pub search_query: String,
    /// All matches found for the current search
    pub search_matches: Vec<SearchMatch>,
    /// Index of the currently selected match (if any)
    pub current_match_index: Option<usize>,

    /// Vim cursor engine for scrollback navigation (persisted across inputs)
    pub vim_engine: libvim::VimCursorEngine<'static>,
}

impl Pane {
    /// Handle keyboard input by writing to the PTY.
    pub fn handle_input(&mut self, input: &[u8]) {
        if let Some(pty) = &mut self.pty {
            let _ = pty.write(input);
        }
    }

    /// Read available data from the PTY and process it through the emulator.
    pub fn read_and_process(&mut self) {
        if let Some(ref pty) = self.pty {
            // Read all available data
            loop {
                match pty.read(&mut self.read_buffer) {
                    Ok(Some(0)) => break, // EOF
                    Ok(Some(n)) => {
                        // Process through terminal emulator
                        self.terminal_emulator.process(&self.read_buffer[..n]);

                        // Handle any responses from the terminal (e.g., cursor position queries)
                        let responses = self.terminal_emulator.drain_responses();
                        for response in responses {
                            let _ = pty.write(&response);
                        }
                    }
                    Ok(None) => break, // No more data available (EAGAIN)
                    Err(_) => break,   // Error reading
                }
            }
        }
    }

    /// Check if the PTY process is still running.
    pub fn is_running(&self) -> bool {
        self.pty.as_ref().map_or(false, |p| p.is_running())
    }

    /// Enter scrollback mode
    pub fn enter_scrollback_mode(&mut self) {
        // Don't enter scrollback mode if in alternate screen
        if self.terminal_emulator.grid().in_alternate_screen {
            return;
        }
        self.scrollback_mode = true;
        self.scroll_offset = 0;

        // Initialize vim cursor at the terminal's current cursor position
        let grid = self.terminal_emulator.grid();
        let scrollback_len = grid.scrollback_len();

        // Convert grid cursor position to absolute line number
        // Grid cursor_y is relative to the grid, so add scrollback_len to get absolute position
        let abs_row = scrollback_len + grid.cursor_y;
        let col = grid.cursor_x;

        // Initialize vim engine with current lines and cursor position
        let lines = self.get_all_lines();
        let viewport_height = self.viewport_height();
        let viewport_width = self.viewport_width();
        self.vim_engine =
            libvim::VimCursorEngine::new_owned(lines, viewport_height, viewport_width);
        self.vim_engine.cursor = libvim::Position::new(abs_row, col);
        self.vim_engine.scroll_offset_row = scrollback_len.saturating_sub(self.scroll_offset);

        // Start with scroll at bottom (showing the grid, no scrollback)
        self.scroll_offset = 0;
    }

    /// Exit scrollback mode
    pub fn exit_scrollback_mode(&mut self) {
        self.scrollback_mode = false;
        self.scroll_offset = 0;
        // Also clear search state
        self.search_mode = false;
        self.search_query.clear();
        self.search_matches.clear();
        self.current_match_index = None;
        // Reset vim state
        self.vim_engine.mode = libvim::Mode::Normal;
        self.vim_engine.input_state = libvim::InputState::default();
    }

    /// Get the total number of lines (scrollback + grid)
    pub fn total_lines(&self) -> usize {
        let grid = self.terminal_emulator.grid();
        grid.scrollback_len() + grid.rows
    }

    /// Get all lines (scrollback + grid) as strings for vim processing
    fn get_all_lines(&self) -> Vec<String> {
        let grid = self.terminal_emulator.grid();
        let scrollback_len = grid.scrollback_len();
        let grid_rows = grid.rows;
        let mut lines = Vec::with_capacity(scrollback_len + grid_rows);

        // Add scrollback lines (oldest first), trimming trailing whitespace
        for i in 0..scrollback_len {
            if let Some(row) = grid.get_scrollback_row(i) {
                let line: String = row.iter().map(|c| c.character).collect();
                lines.push(line.trim_end().to_string());
            }
        }

        // Add grid lines, trimming trailing whitespace
        for y in 0..grid_rows {
            if let Some(row) = grid.get_row(y) {
                let line: String = row.iter().map(|c| c.character).collect();
                lines.push(line.trim_end().to_string());
            }
        }

        lines
    }

    /// Get the viewport height for vim
    fn viewport_height(&self) -> usize {
        self.terminal_emulator.grid().rows
    }

    /// Get the viewport width for vim
    fn viewport_width(&self) -> usize {
        self.terminal_emulator.grid().cols
    }

    /// Handle vim input in scrollback mode
    /// Returns true if the input was handled, false if it should be passed to search mode
    pub fn handle_vim_input(&mut self, input: &[u8]) -> bool {
        // Update lines in case scrollback has changed
        let lines = self.get_all_lines();
        self.vim_engine.set_lines(lines);

        // Update viewport dimensions in case of resize
        self.vim_engine.viewport_height = self.viewport_height();
        self.vim_engine.viewport_width = self.viewport_width();

        // Calculate scroll_offset_row from our scroll_offset
        // Our scroll_offset is lines from bottom, vim's is lines from top
        let scrollback_len = self.terminal_emulator.grid().scrollback_len();
        // scroll_offset=0 means showing grid (bottom), so scroll_offset_row = scrollback_len
        // scroll_offset=scrollback_len means showing from top
        self.vim_engine.scroll_offset_row = scrollback_len.saturating_sub(self.scroll_offset);

        // Process input
        self.vim_engine.handle_input(input);

        // Convert vim scroll_offset_row back to our scroll_offset
        // vim scroll_offset_row is the first visible line from top
        // we need to convert to lines from bottom
        let new_scroll_row = self.vim_engine.scroll_offset_row;
        let viewport_height = self.viewport_height();
        // The bottom of visible area is at new_scroll_row + viewport_height - 1
        // If that's >= scrollback_len, we're showing some grid
        // scroll_offset = how many scrollback lines are visible at top
        if new_scroll_row + viewport_height <= scrollback_len {
            // All visible lines are in scrollback
            self.scroll_offset = scrollback_len - new_scroll_row;
        } else if new_scroll_row >= scrollback_len {
            // All visible lines are in grid
            self.scroll_offset = 0;
        } else {
            // Mixed: some scrollback, some grid
            self.scroll_offset = scrollback_len - new_scroll_row;
        }

        // Clamp scroll_offset
        self.scroll_offset = self.scroll_offset.min(scrollback_len);

        true
    }

    /// Get vim cursor info for rendering (row, col, visible)
    /// Row is relative to the visible viewport
    pub fn get_vim_cursor_info(&self) -> Option<(usize, usize, bool)> {
        if !self.scrollback_mode {
            return None;
        }

        let scrollback_len = self.terminal_emulator.grid().scrollback_len();
        let viewport_height = self.viewport_height();

        // Calculate which line is at the top of the viewport
        // scroll_offset = number of scrollback lines shown
        // If scroll_offset > 0, first visible line is scrollback[scrollback_len - scroll_offset]
        let first_visible_line = scrollback_len.saturating_sub(self.scroll_offset);
        let last_visible_line = first_visible_line + viewport_height;

        // Check if cursor is visible
        let cursor = &self.vim_engine.cursor;
        if cursor.row >= first_visible_line && cursor.row < last_visible_line {
            let viewport_row = cursor.row - first_visible_line;
            Some((cursor.col, viewport_row, true))
        } else {
            None
        }
    }

    /// Get vim selection info for rendering
    /// Returns (start_row, start_col, end_row, end_col, mode) in absolute line coordinates
    pub fn get_vim_selection_info(
        &self,
    ) -> Option<(libvim::Position, libvim::Position, libvim::Mode)> {
        if !self.scrollback_mode || self.vim_engine.mode == libvim::Mode::Normal {
            return None;
        }
        Some((
            self.vim_engine.selection_start,
            self.vim_engine.selection_end,
            self.vim_engine.mode,
        ))
    }

    /// Get the current vim mode
    pub fn get_vim_mode(&self) -> libvim::Mode {
        self.vim_engine.mode
    }

    /// Check if in scrollback mode
    pub fn is_in_scrollback_mode(&self) -> bool {
        self.scrollback_mode
    }

    /// Scroll up by the given number of lines (increases scroll_offset)
    pub fn scroll_up(&mut self, lines: usize) {
        let max_offset = self.terminal_emulator.grid().scrollback_len();
        self.scroll_offset = (self.scroll_offset + lines).min(max_offset);
    }

    /// Scroll down by the given number of lines (decreases scroll_offset)
    pub fn scroll_down(&mut self, lines: usize) {
        self.scroll_offset = self.scroll_offset.saturating_sub(lines);
    }

    /// Get the current scroll offset
    pub fn scroll_offset(&self) -> usize {
        self.scroll_offset
    }

    /// Get the total number of scrollback lines
    pub fn scrollback_len(&self) -> usize {
        self.terminal_emulator.grid().scrollback_len()
    }

    /// Get the terminal emulator
    pub fn emulator(&self) -> &emulator::TerminalEmulator {
        &self.terminal_emulator
    }

    /// Enter search mode (must be in scrollback mode first)
    pub fn enter_search_mode(&mut self) {
        if self.scrollback_mode {
            self.search_mode = true;
            self.search_query.clear();
            self.search_matches.clear();
            self.current_match_index = None;
        }
    }

    /// Exit search mode (back to scrollback mode)
    pub fn exit_search_mode(&mut self) {
        self.search_mode = false;
        self.search_query.clear();
        self.search_matches.clear();
        self.current_match_index = None;
    }

    /// Check if in search mode
    pub fn is_in_search_mode(&self) -> bool {
        self.search_mode
    }

    /// Get the current search query
    pub fn search_query(&self) -> &str {
        &self.search_query
    }

    /// Get the current match index and total matches
    pub fn search_match_info(&self) -> (Option<usize>, usize) {
        (self.current_match_index, self.search_matches.len())
    }

    /// Handle a character input for the search query
    pub fn search_input_char(&mut self, c: char) {
        self.search_query.push(c);
        self.update_search();
    }

    /// Handle backspace for the search query
    pub fn search_input_backspace(&mut self) {
        self.search_query.pop();
        self.update_search();
    }

    /// Clear the search query
    pub fn search_clear(&mut self) {
        self.search_query.clear();
        self.update_search();
    }

    /// Update search results based on current query
    fn update_search(&mut self) {
        self.search_matches.clear();
        self.current_match_index = None;

        if self.search_query.is_empty() {
            return;
        }

        const MAX_MATCHES: usize = 100;

        let query_lower = self.search_query.to_lowercase();
        let grid = self.terminal_emulator.grid();
        let scrollback_len = grid.scrollback_len();

        // Search from bottom to top (most recent first) so we find the most relevant matches
        // when hitting the MAX_MATCHES limit

        // First, search current grid from bottom to top
        'grid: for y in (0..grid.rows).rev() {
            if let Some(row) = grid.get_row(y) {
                let line_text: String = row.iter().map(|c| c.character).collect();
                let line_lower = line_text.to_lowercase();

                // Find all matches in this line (still left to right within line)
                let mut search_start = 0;
                while let Some(pos) = line_lower[search_start..].find(&query_lower) {
                    let start_col = search_start + pos;
                    let end_col = start_col + self.search_query.len();
                    self.search_matches.push(SearchMatch {
                        line_index: y as isize,
                        start_col,
                        end_col,
                    });
                    if self.search_matches.len() >= MAX_MATCHES {
                        break 'grid;
                    }
                    search_start = start_col + 1;
                    if search_start >= line_lower.len() {
                        break;
                    }
                }
            }
        }

        // Then search scrollback from newest to oldest (only if we haven't hit the limit)
        // Scrollback index scrollback_len-1 is the most recent, 0 is the oldest
        if self.search_matches.len() < MAX_MATCHES {
            'scrollback: for i in (0..scrollback_len).rev() {
                if let Some(row) = grid.get_scrollback_row(i) {
                    let line_text: String = row.iter().map(|c| c.character).collect();
                    let line_lower = line_text.to_lowercase();

                    // Find all matches in this line
                    let mut search_start = 0;
                    while let Some(pos) = line_lower[search_start..].find(&query_lower) {
                        let start_col = search_start + pos;
                        let end_col = start_col + self.search_query.len();
                        // Convert scrollback index to our line_index format:
                        // scrollback line 0 (oldest) -> -(scrollback_len)
                        // scrollback line (scrollback_len - 1) (newest) -> -1
                        let line_index = (i as isize) - (scrollback_len as isize);
                        self.search_matches.push(SearchMatch {
                            line_index,
                            start_col,
                            end_col,
                        });
                        if self.search_matches.len() >= MAX_MATCHES {
                            break 'scrollback;
                        }
                        search_start = start_col + 1;
                        if search_start >= line_lower.len() {
                            break;
                        }
                    }
                }
            }
        }

        // Initially select the first match (which is now the most recent/bottom-most
        // since we searched from bottom to top)
        if !self.search_matches.is_empty() {
            self.current_match_index = Some(0);
            self.jump_to_current_match();
        }
    }

    /// Jump to the next match (toward bottom/more recent)
    pub fn next_match(&mut self) {
        if self.search_matches.is_empty() {
            return;
        }
        match self.current_match_index {
            Some(idx) if idx < self.search_matches.len() - 1 => {
                self.current_match_index = Some(idx + 1);
            }
            _ => {
                // Wrap to start
                self.current_match_index = Some(0);
            }
        }
        self.jump_to_current_match();
    }

    /// Jump to the previous match (toward top/older)
    pub fn prev_match(&mut self) {
        if self.search_matches.is_empty() {
            return;
        }
        match self.current_match_index {
            Some(idx) if idx > 0 => {
                self.current_match_index = Some(idx - 1);
            }
            _ => {
                // Wrap to end
                self.current_match_index = Some(self.search_matches.len() - 1);
            }
        }
        self.jump_to_current_match();
    }

    /// Adjust scroll_offset to make the current match visible and move vim cursor to it
    fn jump_to_current_match(&mut self) {
        if let Some(idx) = self.current_match_index {
            if let Some(m) = self.search_matches.get(idx) {
                let grid = self.terminal_emulator.grid();
                let scrollback_len = grid.scrollback_len();
                let visible_rows = grid.rows;

                // Convert line_index to absolute line number (for vim cursor)
                let abs_line = if m.line_index < 0 {
                    // Scrollback line: line_index = -1 means scrollback_len - 1
                    (scrollback_len as isize + m.line_index) as usize
                } else {
                    // Grid line
                    scrollback_len + m.line_index as usize
                };

                // Move vim cursor to the match
                self.vim_engine.cursor = libvim::Position::new(abs_line, m.start_col);

                // The display model (from composite_into):
                // - scroll_offset = number of scrollback lines shown at the top of the screen
                // - For screen row `row`:
                //   - If row < scroll_offset: shows scrollback[scrollback_len - scroll_offset + row]
                //   - If row >= scroll_offset: shows grid[row - scroll_offset]
                //
                // line_index encoding:
                // - Negative: scrollback line, -1 = most recent (scrollback_idx = scrollback_len - 1)
                // - Non-negative: grid row number

                if m.line_index < 0 {
                    // This is a scrollback line
                    // lines_back = how many lines back from grid start (1 = most recent scrollback)
                    let lines_back = (-m.line_index) as usize;

                    // The scrollback line at `lines_back` has scrollback_idx = scrollback_len - lines_back
                    // It appears at screen row = scroll_offset - lines_back
                    // For it to be visible (row >= 0 and row < visible_rows):
                    //   scroll_offset >= lines_back (so row >= 0)
                    //   scroll_offset - lines_back < visible_rows (so row < visible_rows)
                    //     => scroll_offset < visible_rows + lines_back

                    // Minimum scroll_offset to see this line (appears at bottom of scrollback area)
                    let min_offset = lines_back;

                    // Maximum scroll_offset to see this line (line must appear in visible area)
                    // row = scroll_offset - lines_back < visible_rows
                    // scroll_offset < visible_rows + lines_back
                    let max_offset = (visible_rows + lines_back).saturating_sub(1);

                    // Try to center the line on screen
                    // We want row ≈ visible_rows / 2
                    // scroll_offset - lines_back = visible_rows / 2
                    // scroll_offset = lines_back + visible_rows / 2
                    let ideal_offset = lines_back + visible_rows / 2;

                    // Clamp to valid range
                    self.scroll_offset = ideal_offset
                        .max(min_offset)
                        .min(max_offset)
                        .min(scrollback_len);
                } else {
                    // This is a grid line
                    let grid_line = m.line_index as usize;

                    // Grid row `grid_line` appears at screen row = scroll_offset + grid_line
                    // For it to be visible: scroll_offset + grid_line < visible_rows
                    // So: scroll_offset < visible_rows - grid_line
                    // Max valid scroll_offset = visible_rows - grid_line - 1

                    if grid_line < visible_rows {
                        let max_offset = visible_rows - grid_line - 1;
                        if self.scroll_offset > max_offset {
                            self.scroll_offset = max_offset;
                        }
                    } else {
                        // Grid line is beyond visible area - shouldn't happen normally
                        // but handle gracefully by scrolling to 0
                        self.scroll_offset = 0;
                    }
                }
            }
        }
    }

    /// Get search matches for highlighting
    pub fn get_search_matches(&self) -> &[SearchMatch] {
        &self.search_matches
    }

    /// Get the current match index
    pub fn current_match_index(&self) -> Option<usize> {
        self.current_match_index
    }
}
