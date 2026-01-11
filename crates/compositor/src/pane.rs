use pty;
use regex::Regex;
use std::sync::LazyLock;

/// Regex pattern for matching URLs (based on Alacritty's approach)
/// Matches common URL schemes followed by valid URL characters
static URL_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    // Supported schemes: ipfs, ipns, magnet, mailto, gemini, gopher, https, http, news, file, git, ssh, ftp
    // Excluded characters: C0/C1 control chars, angle brackets, quotes, whitespace, braces, caret, backtick, backslash
    Regex::new(concat!(
        r"(?i)(ipfs:|ipns:|magnet:|mailto:|gemini://|gopher://|https://|http://|news:|file:|git://|ssh:|ftp://)",
        r#"[^\x00-\x1f\x7f-\x9f<>"\s{}\^`\\]+"#
    )).unwrap()
});

/// Post-process a URL match to handle bracket balancing and trailing delimiter trimming.
/// This follows Alacritty's approach for better URL extraction.
fn post_process_url(url: &str) -> &str {
    let mut result = url;

    // Stage 1: Bracket balancing
    // Track opening/closing brackets and truncate if a closer has no matching opener
    result = balance_brackets(result);

    // Stage 2: Trim trailing delimiters that are likely sentence punctuation
    result = trim_trailing_delimiters(result);

    result
}

/// Balance brackets in a URL, truncating if a closing bracket has no matching opener.
fn balance_brackets(url: &str) -> &str {
    let bytes = url.as_bytes();
    let mut paren_depth: i32 = 0; // ()
    let mut bracket_depth: i32 = 0; // []

    let mut last_valid_end = 0;

    for (i, &byte) in bytes.iter().enumerate() {
        match byte {
            b'(' => paren_depth += 1,
            b')' => {
                paren_depth -= 1;
                if paren_depth < 0 {
                    // Unmatched closing paren - truncate here
                    return &url[..last_valid_end];
                }
            }
            b'[' => bracket_depth += 1,
            b']' => {
                bracket_depth -= 1;
                if bracket_depth < 0 {
                    // Unmatched closing bracket - truncate here
                    return &url[..last_valid_end];
                }
            }
            _ => {}
        }
        last_valid_end = i + 1;
    }

    url
}

/// Trim trailing punctuation that's likely sentence delimiters, not part of the URL.
fn trim_trailing_delimiters(url: &str) -> &str {
    let trailing_delimiters = ['.', ',', ':', ';', '?', '!', '(', '[', '\''];

    let mut result = url;
    while !result.is_empty() {
        if let Some(last_char) = result.chars().last() {
            if trailing_delimiters.contains(&last_char) {
                result = &result[..result.len() - last_char.len_utf8()];
            } else {
                break;
            }
        } else {
            break;
        }
    }

    result
}

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

/// A URL match found in the terminal, storing the line index, column range, and the URL text
#[derive(Clone, Debug, PartialEq)]
pub struct UrlMatch {
    /// Line index (negative values are scrollback lines, counting from -1 as most recent scrollback)
    /// 0 and positive values are grid lines
    pub line_index: isize,
    /// Starting column of the match
    pub start_col: usize,
    /// Ending column of the match (exclusive)
    pub end_col: usize,
    /// The actual URL text
    pub url: String,
}

/// Direction for incremental URL search
#[derive(Clone, Copy, PartialEq)]
enum SearchDirection {
    /// Search toward older content (up/back in history)
    Up,
    /// Search toward newer content (down/forward in history)  
    Down,
}

pub struct Pane {
    pub terminal_emulator: emulator::TerminalEmulator,
    /// The embedded shell instance
    pub shell: libshell::Shell,
    /// Currently running subprocess (if any) - takes over PTY when active
    pub subprocess: Option<pty::PtyProcess>,
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
    /// Whether the pane is in URL mode (sub-mode of scrollback)
    pub url_mode: bool,
    /// All URLs found in the terminal content
    pub url_matches: Vec<UrlMatch>,
    /// Index of the currently selected URL (if any)
    pub current_url_index: Option<usize>,

    /// Vim cursor engine for scrollback navigation (persisted across inputs)
    pub vim_engine: libvim::VimCursorEngine<'static>,
}

impl Pane {
    /// Create a new pane with the given dimensions.
    pub fn new(width: usize, height: usize) -> Self {
        let (shell, initial_output) = libshell::Shell::new(width as u16, height as u16);
        let mut terminal_emulator = emulator::TerminalEmulator::new(width, height);

        // Process the initial prompt output
        terminal_emulator.process(&initial_output);

        Pane {
            terminal_emulator,
            shell,
            subprocess: None,
            read_buffer: [0u8; 4096],
            scrollback_mode: false,
            scroll_offset: 0,
            search_mode: false,
            search_query: String::new(),
            search_matches: Vec::new(),
            current_match_index: None,
            url_mode: false,
            url_matches: Vec::new(),
            current_url_index: None,
            vim_engine: libvim::VimCursorEngine::new_owned(Vec::new(), height, width),
        }
    }

    /// Handle keyboard input.
    ///
    /// If a subprocess is running, input goes to it.
    /// Otherwise, input goes to the shell.
    ///
    /// Returns `true` if the terminal content changed and a rerender is needed.
    pub fn handle_input(&mut self, input: &[u8]) -> bool {
        if let Some(ref mut proc) = self.subprocess {
            // Subprocess is active - send input directly to it
            let _ = proc.write(input);
            // Subprocess output will trigger rerender via poll
            false
        } else {
            // Shell is active - process input and handle actions
            match self.shell.handle_input(input) {
                libshell::ShellAction::None => false,
                libshell::ShellAction::Output(data) => {
                    self.terminal_emulator.process(&data);
                    true // Content changed, need rerender
                }
                libshell::ShellAction::SpawnSubprocess {
                    output,
                    command,
                    args,
                    env: _,
                    cwd,
                } => {
                    // First, write any pending output (e.g., the newline after the command)
                    if !output.is_empty() {
                        self.terminal_emulator.process(&output);
                    }

                    // Build the full command string
                    let full_command = if args.is_empty() {
                        command
                    } else {
                        format!("{} {}", command, args.join(" "))
                    };

                    // Spawn the subprocess
                    let width = self.terminal_emulator.grid().cols as u16;
                    let height = self.terminal_emulator.grid().rows as u16;

                    // Change to the shell's cwd before spawning
                    let _ = std::env::set_current_dir(&cwd);

                    match pty::PtyProcess::spawn(&full_command, width, height) {
                        Ok(proc) => {
                            self.subprocess = Some(proc);
                        }
                        Err(e) => {
                            // Failed to spawn - show error and prompt
                            let error_msg = format!("spawn error: {}\r\n", e);
                            self.terminal_emulator.process(error_msg.as_bytes());
                            let prompt = self.shell.subprocess_exited(1);
                            self.terminal_emulator.process(&prompt);
                        }
                    }
                    true // Content changed (at least the newline), need rerender
                }
                libshell::ShellAction::Exit => {
                    // Shell wants to exit - could close the pane
                    // For now, just show a message
                    self.terminal_emulator.process(b"[shell exited]\r\n");
                    true // Content changed, need rerender
                }
            }
        }
    }

    /// Read available data from the subprocess and process it through the emulator.
    /// Also checks if the subprocess has exited and returns control to the shell.
    pub fn read_and_process(&mut self) {
        if let Some(ref proc) = self.subprocess {
            // Read all available data from subprocess
            loop {
                match proc.read(&mut self.read_buffer) {
                    Ok(Some(0)) => break, // EOF
                    Ok(Some(n)) => {
                        // Process through terminal emulator
                        self.terminal_emulator.process(&self.read_buffer[..n]);

                        // Handle any responses from the terminal (e.g., cursor position queries)
                        let responses = self.terminal_emulator.drain_responses();
                        for response in responses {
                            if let Some(ref proc) = self.subprocess {
                                let _ = proc.write(&response);
                            }
                        }
                    }
                    Ok(None) => break, // No more data available (EAGAIN)
                    Err(_) => break,   // Error reading
                }
            }
        }

        // Check if subprocess has exited
        if let Some(ref proc) = self.subprocess {
            if !proc.is_running() {
                // Subprocess exited - we don't have access to exit code directly,
                // so we assume 0 for now. The Drop impl will clean up the process.
                drop(self.subprocess.take());

                // Notify shell and show prompt
                let output = self.shell.subprocess_exited(0);
                self.terminal_emulator.process(&output);
            }
        }
    }

    /// Check if the pane is still active (shell hasn't exited).
    pub fn is_running(&self) -> bool {
        // Pane is running if shell hasn't exited
        !self.shell.should_exit()
    }

    /// Check if a subprocess is currently running.
    pub fn has_subprocess(&self) -> bool {
        self.subprocess.is_some()
    }

    /// Get the subprocess PTY file descriptor for polling (if any).
    pub fn subprocess_fd(&self) -> Option<std::os::fd::RawFd> {
        use std::os::fd::AsRawFd;
        self.subprocess.as_ref().map(|p| p.as_raw_fd())
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
        // Also clear URL mode state
        self.url_mode = false;
        self.url_matches.clear();
        self.current_url_index = None;
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
        // Note: Lines are captured once when entering scrollback mode (in enter_scrollback_mode)
        // and are not updated on each keystroke since scrollback is frozen in this mode.

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

    /// Get the currently selected text in visual mode.
    /// Returns None if not in scrollback mode or not in visual mode.
    pub fn get_selected_text(&self) -> Option<String> {
        if !self.scrollback_mode {
            return None;
        }

        let mode = self.vim_engine.mode;
        if mode == libvim::Mode::Normal {
            return None;
        }

        let start = self.vim_engine.selection_start;
        let end = self.vim_engine.selection_end;

        // Get all lines for extracting text
        let lines = self.get_all_lines_for_selection();

        let mut result = String::new();

        match mode {
            libvim::Mode::Visual => {
                // Character-wise selection
                if start.row == end.row {
                    // Single line selection
                    if let Some(line) = lines.get(start.row) {
                        let start_col = start.col.min(line.len());
                        let end_col = (end.col + 1).min(line.len());
                        if start_col <= end_col {
                            result.push_str(&line[start_col..end_col]);
                        }
                    }
                } else {
                    // Multi-line selection
                    for row in start.row..=end.row {
                        if let Some(line) = lines.get(row) {
                            if row == start.row {
                                // First line: from start_col to end
                                let col = start.col.min(line.len());
                                result.push_str(&line[col..]);
                            } else if row == end.row {
                                // Last line: from start to end_col (inclusive)
                                let end_col = (end.col + 1).min(line.len());
                                result.push_str(&line[..end_col]);
                            } else {
                                // Middle lines: entire line
                                result.push_str(line);
                            }
                        }
                        if row < end.row {
                            result.push('\n');
                        }
                    }
                }
            }
            libvim::Mode::VisualLine => {
                // Line-wise selection: entire lines from start.row to end.row
                for row in start.row..=end.row {
                    if let Some(line) = lines.get(row) {
                        result.push_str(line);
                    }
                    if row < end.row {
                        result.push('\n');
                    }
                }
            }
            libvim::Mode::Normal => unreachable!(),
        }

        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    }

    /// Get all lines for selection extraction (same as get_all_lines but accessible for selection)
    fn get_all_lines_for_selection(&self) -> Vec<String> {
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

    /// Enter URL mode (must be in scrollback mode first)
    /// Uses incremental search - only finds the first URL from the current cursor position
    pub fn enter_url_mode(&mut self) {
        if self.scrollback_mode {
            self.url_mode = true;
            self.url_matches.clear();
            self.current_url_index = None;

            // Find the first URL starting from the current cursor position, searching upward (older)
            // This is typically what users want - find a URL they just saw
            if let Some(url_match) = self.find_url_from_cursor(SearchDirection::Up) {
                self.url_matches.push(url_match);
                self.current_url_index = Some(0);
                self.jump_to_current_url();
            }
        }
    }

    /// Exit URL mode (back to scrollback mode)
    pub fn exit_url_mode(&mut self) {
        self.url_mode = false;
        self.url_matches.clear();
        self.current_url_index = None;
    }

    /// Check if in URL mode
    pub fn is_in_url_mode(&self) -> bool {
        self.url_mode
    }

    /// Convert absolute line number (vim cursor style) to line_index (UrlMatch style)
    /// Vim cursor: 0..scrollback_len are scrollback, scrollback_len..scrollback_len+grid_rows are grid
    /// line_index: negative for scrollback (-scrollback_len..-1), non-negative for grid (0..grid_rows)
    fn abs_line_to_line_index(&self, abs_line: usize) -> isize {
        let scrollback_len = self.terminal_emulator.grid().scrollback_len();
        if abs_line < scrollback_len {
            // Scrollback line: abs_line 0 -> -(scrollback_len), abs_line scrollback_len-1 -> -1
            (abs_line as isize) - (scrollback_len as isize)
        } else {
            // Grid line
            (abs_line - scrollback_len) as isize
        }
    }

    /// Convert line_index (UrlMatch style) to absolute line number (vim cursor style)
    fn line_index_to_abs_line(&self, line_index: isize) -> usize {
        let scrollback_len = self.terminal_emulator.grid().scrollback_len();
        if line_index < 0 {
            // Scrollback line
            (scrollback_len as isize + line_index) as usize
        } else {
            // Grid line
            scrollback_len + line_index as usize
        }
    }

    /// Find a URL starting from the current cursor position in the given direction.
    /// Returns the first URL found, or None if no URL is found.
    fn find_url_from_cursor(&self, direction: SearchDirection) -> Option<UrlMatch> {
        let grid = self.terminal_emulator.grid();
        let scrollback_len = grid.scrollback_len();
        let total_lines = scrollback_len + grid.rows;

        // Get cursor position (absolute line number and column)
        let cursor_abs_line = self.vim_engine.cursor.row;
        let cursor_col = self.vim_engine.cursor.col;

        match direction {
            SearchDirection::Up => {
                // Search from cursor position going up (toward older content)
                // First, search the current line from cursor position backward
                if let Some(url_match) = self.find_url_in_line_before(cursor_abs_line, cursor_col) {
                    return Some(url_match);
                }

                // Then search previous lines (going up)
                if cursor_abs_line > 0 {
                    for abs_line in (0..cursor_abs_line).rev() {
                        if let Some(url_match) = self.find_last_url_in_line(abs_line) {
                            return Some(url_match);
                        }
                    }
                }
            }
            SearchDirection::Down => {
                // Search from cursor position going down (toward newer content)
                // First, search the current line from cursor position forward
                if let Some(url_match) = self.find_url_in_line_after(cursor_abs_line, cursor_col) {
                    return Some(url_match);
                }

                // Then search subsequent lines (going down)
                for abs_line in (cursor_abs_line + 1)..total_lines {
                    if let Some(url_match) = self.find_first_url_in_line(abs_line) {
                        return Some(url_match);
                    }
                }
            }
        }

        None
    }

    /// Find a URL continuing from the current URL position in the given direction.
    /// Used for next_url/prev_url navigation.
    fn find_next_url_from_current(&self, direction: SearchDirection) -> Option<UrlMatch> {
        let current_url = self.url_matches.first()?;
        let current_abs_line = self.line_index_to_abs_line(current_url.line_index);

        let grid = self.terminal_emulator.grid();
        let scrollback_len = grid.scrollback_len();
        let total_lines = scrollback_len + grid.rows;

        match direction {
            SearchDirection::Up => {
                // Search for the next URL going up (toward older content)
                // First check current line for a URL before the current one
                if let Some(url_match) =
                    self.find_url_in_line_before(current_abs_line, current_url.start_col)
                {
                    return Some(url_match);
                }

                // Then search previous lines
                if current_abs_line > 0 {
                    for abs_line in (0..current_abs_line).rev() {
                        if let Some(url_match) = self.find_last_url_in_line(abs_line) {
                            return Some(url_match);
                        }
                    }
                }
            }
            SearchDirection::Down => {
                // Search for the next URL going down (toward newer content)
                // First check current line for a URL after the current one
                if let Some(url_match) =
                    self.find_url_in_line_after(current_abs_line, current_url.end_col)
                {
                    return Some(url_match);
                }

                // Then search subsequent lines
                for abs_line in (current_abs_line + 1)..total_lines {
                    if let Some(url_match) = self.find_first_url_in_line(abs_line) {
                        return Some(url_match);
                    }
                }
            }
        }

        None
    }

    /// Get the line text for an absolute line number
    fn get_line_text(&self, abs_line: usize) -> Option<String> {
        let grid = self.terminal_emulator.grid();
        let scrollback_len = grid.scrollback_len();

        if abs_line < scrollback_len {
            grid.get_scrollback_row(abs_line)
                .map(|row| row.iter().map(|c| c.character).collect())
        } else {
            let grid_row = abs_line - scrollback_len;
            grid.get_row(grid_row)
                .map(|row| row.iter().map(|c| c.character).collect())
        }
    }

    /// Find the first URL in a line
    fn find_first_url_in_line(&self, abs_line: usize) -> Option<UrlMatch> {
        let line_text = self.get_line_text(abs_line)?;
        let line_index = self.abs_line_to_line_index(abs_line);

        for mat in URL_REGEX.find_iter(&line_text) {
            let processed_url = post_process_url(mat.as_str());
            if processed_url.is_empty() {
                continue;
            }
            let processed_end_col = mat.start() + processed_url.len();
            return Some(UrlMatch {
                line_index,
                start_col: mat.start(),
                end_col: processed_end_col,
                url: processed_url.to_string(),
            });
        }

        None
    }

    /// Find the last URL in a line
    fn find_last_url_in_line(&self, abs_line: usize) -> Option<UrlMatch> {
        let line_text = self.get_line_text(abs_line)?;
        let line_index = self.abs_line_to_line_index(abs_line);

        let mut last_match: Option<UrlMatch> = None;
        for mat in URL_REGEX.find_iter(&line_text) {
            let processed_url = post_process_url(mat.as_str());
            if processed_url.is_empty() {
                continue;
            }
            let processed_end_col = mat.start() + processed_url.len();
            last_match = Some(UrlMatch {
                line_index,
                start_col: mat.start(),
                end_col: processed_end_col,
                url: processed_url.to_string(),
            });
        }

        last_match
    }

    /// Find a URL in a line that starts before the given column
    fn find_url_in_line_before(&self, abs_line: usize, before_col: usize) -> Option<UrlMatch> {
        let line_text = self.get_line_text(abs_line)?;
        let line_index = self.abs_line_to_line_index(abs_line);

        let mut last_match: Option<UrlMatch> = None;
        for mat in URL_REGEX.find_iter(&line_text) {
            let processed_url = post_process_url(mat.as_str());
            if processed_url.is_empty() {
                continue;
            }
            // Only consider URLs that start before the given column
            if mat.start() >= before_col {
                break;
            }
            let processed_end_col = mat.start() + processed_url.len();
            last_match = Some(UrlMatch {
                line_index,
                start_col: mat.start(),
                end_col: processed_end_col,
                url: processed_url.to_string(),
            });
        }

        last_match
    }

    /// Find a URL in a line that starts at or after the given column
    fn find_url_in_line_after(&self, abs_line: usize, after_col: usize) -> Option<UrlMatch> {
        let line_text = self.get_line_text(abs_line)?;
        let line_index = self.abs_line_to_line_index(abs_line);

        for mat in URL_REGEX.find_iter(&line_text) {
            let processed_url = post_process_url(mat.as_str());
            if processed_url.is_empty() {
                continue;
            }
            // Only consider URLs that start at or after the given column
            if mat.start() >= after_col {
                let processed_end_col = mat.start() + processed_url.len();
                return Some(UrlMatch {
                    line_index,
                    start_col: mat.start(),
                    end_col: processed_end_col,
                    url: processed_url.to_string(),
                });
            }
        }

        None
    }

    /// Jump to the next URL (toward bottom/more recent, j key)
    /// Uses incremental search - only searches when needed
    pub fn next_url(&mut self) {
        if let Some(url_match) = self.find_next_url_from_current(SearchDirection::Down) {
            self.url_matches.clear();
            self.url_matches.push(url_match);
            self.current_url_index = Some(0);
            self.jump_to_current_url();
        }
        // If no next URL found, stay at current position (no wrapping for incremental search)
    }

    /// Jump to the previous URL (toward top/older, k key)
    /// Uses incremental search - only searches when needed
    pub fn prev_url(&mut self) {
        if let Some(url_match) = self.find_next_url_from_current(SearchDirection::Up) {
            self.url_matches.clear();
            self.url_matches.push(url_match);
            self.current_url_index = Some(0);
            self.jump_to_current_url();
        }
        // If no previous URL found, stay at current position (no wrapping for incremental search)
    }

    /// Adjust scroll_offset to make the current URL visible
    fn jump_to_current_url(&mut self) {
        if let Some(idx) = self.current_url_index {
            if let Some(m) = self.url_matches.get(idx) {
                let grid = self.terminal_emulator.grid();
                let scrollback_len = grid.scrollback_len();
                let visible_rows = grid.rows;

                if m.line_index < 0 {
                    // This is a scrollback line
                    let lines_back = (-m.line_index) as usize;
                    let min_offset = lines_back;
                    let max_offset = (visible_rows + lines_back).saturating_sub(1);
                    let ideal_offset = lines_back + visible_rows / 2;

                    self.scroll_offset = ideal_offset
                        .max(min_offset)
                        .min(max_offset)
                        .min(scrollback_len);
                } else {
                    // This is a grid line
                    let grid_line = m.line_index as usize;

                    if grid_line < visible_rows {
                        let max_offset = visible_rows - grid_line - 1;
                        if self.scroll_offset > max_offset {
                            self.scroll_offset = max_offset;
                        }
                    } else {
                        self.scroll_offset = 0;
                    }
                }
            }
        }
    }

    /// Get the currently selected URL
    pub fn get_current_url(&self) -> Option<&str> {
        self.current_url_index
            .and_then(|idx| self.url_matches.get(idx))
            .map(|m| m.url.as_str())
    }

    /// Get URL info for status bar (current index, total count)
    pub fn url_match_info(&self) -> (Option<usize>, usize) {
        (self.current_url_index, self.url_matches.len())
    }

    /// Get URL matches for highlighting
    pub fn get_url_matches(&self) -> &[UrlMatch] {
        &self.url_matches
    }

    /// Get the current URL index
    pub fn current_url_index(&self) -> Option<usize> {
        self.current_url_index
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_url_regex_basic() {
        // Basic HTTP/HTTPS URLs
        assert!(URL_REGEX.is_match("https://example.com"));
        assert!(URL_REGEX.is_match("http://example.com"));
        assert!(URL_REGEX.is_match("https://example.com/path?query=value"));
    }

    #[test]
    fn test_url_regex_schemes() {
        // Various supported schemes
        assert!(URL_REGEX.is_match("mailto:user@example.com"));
        assert!(URL_REGEX.is_match("ftp://ftp.example.com/file.txt"));
        assert!(URL_REGEX.is_match("ssh://user@host"));
        assert!(URL_REGEX.is_match("git://github.com/user/repo.git"));
        assert!(URL_REGEX.is_match("file:///home/user/doc.txt"));
    }

    #[test]
    fn test_url_regex_case_insensitive() {
        // Case insensitivity
        assert!(URL_REGEX.is_match("HTTPS://EXAMPLE.COM"));
        assert!(URL_REGEX.is_match("Http://Example.Com"));
    }

    #[test]
    fn test_url_regex_invalid() {
        // Invalid URLs (missing scheme or malformed)
        assert!(!URL_REGEX.is_match("example.com")); // No scheme
        assert!(!URL_REGEX.is_match("http://")); // Empty body
        assert!(!URL_REGEX.is_match("not a url"));
    }

    #[test]
    fn test_bracket_balancing_matched_parens() {
        // Matched parentheses should be preserved
        assert_eq!(
            balance_brackets("http://example.com/page_(1)"),
            "http://example.com/page_(1)"
        );
        assert_eq!(
            balance_brackets("http://example.com/wiki/Thing_(concept)"),
            "http://example.com/wiki/Thing_(concept)"
        );
    }

    #[test]
    fn test_bracket_balancing_unmatched_parens() {
        // Unmatched closing paren should truncate
        assert_eq!(
            balance_brackets("http://example.com)"),
            "http://example.com"
        );
        assert_eq!(
            balance_brackets("http://example.com/page)more"),
            "http://example.com/page"
        );
    }

    #[test]
    fn test_bracket_balancing_unmatched_brackets() {
        // Unmatched closing bracket should truncate
        assert_eq!(
            balance_brackets("http://example.com]"),
            "http://example.com"
        );
        assert_eq!(
            balance_brackets("http://example.com/path]rest"),
            "http://example.com/path"
        );
    }

    #[test]
    fn test_bracket_balancing_nested() {
        // Nested brackets should work
        assert_eq!(
            balance_brackets("http://example.com/f(g(x))"),
            "http://example.com/f(g(x))"
        );
    }

    #[test]
    fn test_trailing_delimiter_trimming() {
        // Trailing punctuation should be trimmed
        assert_eq!(
            trim_trailing_delimiters("http://example.com."),
            "http://example.com"
        );
        assert_eq!(
            trim_trailing_delimiters("http://example.com,"),
            "http://example.com"
        );
        assert_eq!(
            trim_trailing_delimiters("http://example.com;"),
            "http://example.com"
        );
        assert_eq!(
            trim_trailing_delimiters("http://example.com:"),
            "http://example.com"
        );
        assert_eq!(
            trim_trailing_delimiters("http://example.com?"),
            "http://example.com"
        );
        assert_eq!(
            trim_trailing_delimiters("http://example.com!"),
            "http://example.com"
        );
    }

    #[test]
    fn test_trailing_delimiter_preserves_query() {
        // Query strings with ? should not be stripped if not trailing
        assert_eq!(
            trim_trailing_delimiters("http://example.com/path?query=1"),
            "http://example.com/path?query=1"
        );
    }

    #[test]
    fn test_trailing_delimiter_multiple() {
        // Multiple trailing delimiters
        assert_eq!(
            trim_trailing_delimiters("http://example.com.."),
            "http://example.com"
        );
        assert_eq!(
            trim_trailing_delimiters("http://example.com.,"),
            "http://example.com"
        );
    }

    #[test]
    fn test_post_process_url_combined() {
        // Combined bracket balancing and trailing trimming
        assert_eq!(
            post_process_url("http://example.com)."),
            "http://example.com"
        );
        assert_eq!(
            post_process_url("http://example.com/page_(1)."),
            "http://example.com/page_(1)"
        );
    }

    #[test]
    fn test_post_process_url_embedded_in_parens() {
        // URL embedded in parentheses - simulates "(see http://example.com)"
        // The regex would match "http://example.com)" and post-processing fixes it
        assert_eq!(
            post_process_url("http://example.com)"),
            "http://example.com"
        );
    }
}
