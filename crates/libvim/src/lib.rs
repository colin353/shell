//! libvim library
//!
//! A vim cursor movement emulator that tracks cursor position and selection state.

use std::borrow::Cow;

/// Vim editing mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Mode {
    /// Normal mode - default mode for navigation
    #[default]
    Normal,
    /// Visual mode (character-wise selection with 'v')
    Visual,
    /// Visual Line mode (line-wise selection with 'V')
    VisualLine,
}

/// Parser state for handling multi-byte sequences and counts
#[derive(Debug, Clone, Default)]
pub struct InputState {
    /// Accumulated count prefix (e.g., "12" in "12j")
    pub count: Option<usize>,
    /// Pending bytes for multi-byte commands (e.g., "g" waiting for "g")
    pub pending: Vec<u8>,
}

/// Cursor position in the document
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Position {
    pub row: usize,
    pub col: usize,
}

impl Position {
    pub fn new(row: usize, col: usize) -> Self {
        Position { row, col }
    }
}

pub struct VimCursorEngine<'a> {
    pub lines: Cow<'a, [String]>,
    pub viewport_height: usize,
    pub viewport_width: usize,
    pub scroll_offset_row: usize,
    pub scroll_offset_col: usize,

    pub cursor: Position,

    /// Anchor point for visual selection (where selection started)
    pub selection_anchor: Position,
    /// Start of visual selection (always <= end, for display)
    pub selection_start: Position,
    /// End of visual selection (always >= start, for display)
    pub selection_end: Position,

    /// Current mode
    pub mode: Mode,

    /// Input parser state
    pub input_state: InputState,
}

impl<'a> VimCursorEngine<'a> {
    pub fn new(lines: &'a [String], viewport_height: usize, viewport_width: usize) -> Self {
        VimCursorEngine {
            lines: Cow::Borrowed(lines),
            viewport_height,
            viewport_width,
            scroll_offset_row: 0,
            scroll_offset_col: 0,
            cursor: Position { row: 0, col: 0 },
            selection_anchor: Position { row: 0, col: 0 },
            selection_start: Position { row: 0, col: 0 },
            selection_end: Position { row: 0, col: 0 },
            mode: Mode::Normal,
            input_state: InputState::default(),
        }
    }

    /// Create a new VimCursorEngine that owns its lines data.
    /// This is useful when you need to persist the engine across calls.
    pub fn new_owned(
        lines: Vec<String>,
        viewport_height: usize,
        viewport_width: usize,
    ) -> VimCursorEngine<'static> {
        VimCursorEngine {
            lines: Cow::Owned(lines),
            viewport_height,
            viewport_width,
            scroll_offset_row: 0,
            scroll_offset_col: 0,
            cursor: Position { row: 0, col: 0 },
            selection_anchor: Position { row: 0, col: 0 },
            selection_start: Position { row: 0, col: 0 },
            selection_end: Position { row: 0, col: 0 },
            mode: Mode::Normal,
            input_state: InputState::default(),
        }
    }

    /// Update the lines data. This is useful when the scrollback content changes.
    pub fn set_lines(&mut self, lines: Vec<String>) {
        self.lines = Cow::Owned(lines);
        // Clamp cursor to valid range
        let max_row = self.lines.len().saturating_sub(1);
        if self.cursor.row > max_row {
            self.cursor.row = max_row;
        }
        let line_len = self.line_len(self.cursor.row);
        if self.cursor.col > line_len.saturating_sub(1) && line_len > 0 {
            self.cursor.col = line_len.saturating_sub(1);
        }
    }

    /// Process input bytes, updating cursor and selection state.
    /// Handles partial input across multiple calls.
    pub fn handle_input(&mut self, input: &[u8]) {
        for &byte in input {
            self.process_byte(byte);
        }
    }

    /// Process a single byte of input
    fn process_byte(&mut self, byte: u8) {
        // Handle count prefix (digits)
        if self.input_state.pending.is_empty() {
            if byte == b'0' && self.input_state.count.is_some() {
                // '0' extends an existing count
                let count = self.input_state.count.unwrap();
                self.input_state.count = Some(count * 10);
                return;
            } else if byte == b'0' {
                // '0' alone is a motion (go to start of line)
                self.execute_motion(b'0', 1);
                return;
            } else if byte.is_ascii_digit() {
                // Start or extend count
                let digit = (byte - b'0') as usize;
                let count = self.input_state.count.unwrap_or(0) * 10 + digit;
                self.input_state.count = Some(count);
                return;
            }
        }

        // Handle pending multi-byte sequences
        if !self.input_state.pending.is_empty() {
            self.input_state.pending.push(byte);
            if let Some(cmd) = self.try_complete_sequence() {
                let count = self.input_state.count.take().unwrap_or(1);
                self.execute_command(&cmd, count);
                self.input_state.pending.clear();
            }
            return;
        }

        // Check if this starts a multi-byte sequence
        if self.starts_multi_byte_sequence(byte) {
            self.input_state.pending.push(byte);
            return;
        }

        // Single-byte command
        let count = self.input_state.count.take().unwrap_or(1);
        self.execute_motion(byte, count);
    }

    /// Check if byte starts a multi-byte sequence
    fn starts_multi_byte_sequence(&self, byte: u8) -> bool {
        // 'i' and 'a' start text object sequences in visual mode
        if self.mode == Mode::Visual || self.mode == Mode::VisualLine {
            matches!(
                byte,
                b'g' | b'z' | b'f' | b'F' | b't' | b'T' | b'i' | b'a' | 0x1b
            )
        } else {
            matches!(byte, b'g' | b'z' | b'f' | b'F' | b't' | b'T' | 0x1b)
        }
    }

    /// Try to complete a pending multi-byte sequence
    fn try_complete_sequence(&self) -> Option<Vec<u8>> {
        let pending = &self.input_state.pending;

        match pending.as_slice() {
            // gg - go to top
            [b'g', b'g'] => Some(pending.clone()),
            // g followed by something else we don't recognize - abort
            [b'g', _] => Some(pending.clone()),

            // z commands (zt, zz, zb for scrolling)
            [b'z', b't' | b'z' | b'b'] => Some(pending.clone()),
            [b'z', _] => Some(pending.clone()),

            // f/F/t/T followed by any character
            [b'f' | b'F' | b't' | b'T', _] => Some(pending.clone()),

            // Escape sequences (arrow keys, etc.)
            [0x1b, b'[', _] => Some(pending.clone()),
            [0x1b, b'['] => None, // Need one more byte
            [0x1b, b'O', _] => Some(pending.clone()),
            [0x1b, b'O'] => None,               // Need one more byte
            [0x1b, _] => Some(pending.clone()), // ESC + other char
            [0x1b] => Some(pending.clone()),    // Bare escape - treat as complete

            // Text objects: i( i) i[ i] i{ i} i< i> i" i' and same with 'a'
            [b'i' | b'a', b'(' | b')' | b'[' | b']' | b'{' | b'}' | b'<' | b'>' | b'"' | b'\''] => {
                Some(pending.clone())
            }
            [b'i' | b'a', _] => Some(pending.clone()), // Complete on any second char
            [b'i'] | [b'a'] => None,                   // Need one more byte

            _ => None,
        }
    }

    /// Execute a completed command sequence
    fn execute_command(&mut self, cmd: &[u8], count: usize) {
        match cmd {
            [b'g', b'g'] => self.motion_gg(count),
            [b'z', b't'] => self.scroll_cursor_to_top(),
            [b'z', b'z'] => self.scroll_cursor_to_center(),
            [b'z', b'b'] => self.scroll_cursor_to_bottom(),
            [b'f', ch] => self.motion_f(*ch, count),
            [b'F', ch] => self.motion_f_back(*ch, count),
            [b't', ch] => self.motion_t(*ch, count),
            [b'T', ch] => self.motion_t_back(*ch, count),
            // Arrow keys
            [0x1b, b'[', b'A'] => self.motion_k(count), // Up
            [0x1b, b'[', b'B'] => self.motion_j(count), // Down
            [0x1b, b'[', b'C'] => self.motion_l(count), // Right
            [0x1b, b'[', b'D'] => self.motion_h(count), // Left
            // Bare Escape - return to normal mode
            [0x1b] => self.enter_normal_mode(),
            // Text objects
            [b'i', ch] => self.text_object_inner(*ch),
            [b'a', ch] => self.text_object_around(*ch),
            _ => {} // Unknown sequence, ignore
        }
    }

    /// Execute a single-byte motion command
    fn execute_motion(&mut self, byte: u8, count: usize) {
        match byte {
            // Basic movement
            b'h' => self.motion_h(count),
            b'j' => self.motion_j(count),
            b'k' => self.motion_k(count),
            b'l' => self.motion_l(count),

            // Line position
            b'0' => self.motion_0(),
            b'^' => self.motion_caret(),
            b'$' => self.motion_dollar(count),

            // Word motions
            b'w' => self.motion_w(count),
            b'W' => self.motion_w_big(count),
            b'b' => self.motion_b(count),
            b'B' => self.motion_b_big(count),
            b'e' => self.motion_e(count),
            b'E' => self.motion_e_big(count),

            // Document navigation
            b'G' => self.motion_g_big(count),

            // Scrolling
            0x04 => self.scroll_down(count), // Ctrl-D
            0x15 => self.scroll_up(count),   // Ctrl-U
            0x06 => self.page_down(count),   // Ctrl-F
            0x02 => self.page_up(count),     // Ctrl-B

            // Screen-relative
            b'H' => self.motion_h_screen(),
            b'M' => self.motion_m_screen(),
            b'L' => self.motion_l_screen(),

            // Visual mode
            b'v' => self.toggle_visual_mode(),
            b'V' => self.toggle_visual_line_mode(),

            // Escape - return to normal mode
            0x1b => self.enter_normal_mode(),

            // Matching bracket
            b'%' => self.motion_percent(),

            // Paragraph motions
            b'}' => self.motion_paragraph_forward(count),
            b'{' => self.motion_paragraph_backward(count),

            _ => {} // Unknown command, ignore
        }
    }

    // ==================== Helper Methods ====================

    /// Get the length of a line (number of characters)
    fn line_len(&self, row: usize) -> usize {
        self.lines.get(row).map(|l| l.chars().count()).unwrap_or(0)
    }

    /// Get the last valid column in a line (0 if empty, len-1 otherwise for normal mode)
    fn last_col(&self, row: usize) -> usize {
        let len = self.line_len(row);
        if len == 0 {
            0
        } else {
            len - 1
        }
    }

    /// Total number of lines
    fn total_lines(&self) -> usize {
        self.lines.len()
    }

    /// Clamp cursor column to valid range for current line
    fn clamp_cursor_col(&mut self) {
        let max_col = self.last_col(self.cursor.row);
        if self.cursor.col > max_col {
            self.cursor.col = max_col;
        }
    }

    /// Ensure cursor is visible by adjusting scroll offset
    fn ensure_cursor_visible(&mut self) {
        // Vim reserves 1-2 rows for status line, so effective viewport is smaller
        let effective_height = self.viewport_height.saturating_sub(2);

        // If cursor is above the viewport, scroll up
        if self.cursor.row < self.scroll_offset_row {
            self.scroll_offset_row = self.cursor.row;
        }
        // If cursor is below the viewport, scroll down
        let visible_bottom = self.scroll_offset_row + effective_height;
        if self.cursor.row > visible_bottom {
            self.scroll_offset_row = self.cursor.row.saturating_sub(effective_height);
        }
    }

    /// Update selection based on cursor movement in visual mode
    fn update_selection(&mut self) {
        match self.mode {
            Mode::Normal => {
                // No selection in normal mode
                self.selection_start = self.cursor;
                self.selection_end = self.cursor;
            }
            Mode::Visual => {
                // Character-wise selection from anchor to cursor
                // Vim's visual mode highlights from anchor to the character BEFORE cursor
                // when cursor > anchor. The cursor sits on/after the last selected char.
                //
                // Special case: when cursor is past end of line (from $), selection
                // should extend to the last character of the line.

                let anchor_col_clamped = self
                    .selection_anchor
                    .col
                    .min(self.last_col(self.selection_anchor.row));
                let anchor = Position::new(self.selection_anchor.row, anchor_col_clamped);

                let line_last_col = self.last_col(self.cursor.row);
                let cursor_past_end = self.cursor.col > line_last_col;

                if (self.cursor.row, self.cursor.col)
                    < (self.selection_anchor.row, self.selection_anchor.col)
                {
                    // Cursor before anchor: cursor sits before the selection
                    // Selection runs from (cursor + 1) to anchor
                    let cursor_clamped = self.cursor.col.min(line_last_col);
                    self.selection_start =
                        Position::new(self.cursor.row, cursor_clamped.saturating_add(1));
                    self.selection_end = anchor;
                } else if (self.cursor.row, self.cursor.col)
                    > (self.selection_anchor.row, self.selection_anchor.col)
                {
                    // Cursor after anchor
                    self.selection_start = anchor;

                    if cursor_past_end {
                        // Cursor is past end of line (e.g., from $)
                        // Selection ends at last character of the line
                        self.selection_end = Position::new(self.cursor.row, line_last_col);
                    } else if self.cursor.row == self.selection_anchor.row {
                        // Same line, cursor within line: selection ends one before cursor
                        self.selection_end =
                            Position::new(self.cursor.row, self.cursor.col.saturating_sub(1));
                    } else {
                        // Different lines, cursor within line
                        if self.cursor.col == 0 {
                            // Cursor at start of line: selection ends at end of previous line
                            // Vim highlights the conceptual newline position (line_len, not len-1)
                            let prev_row = self.cursor.row.saturating_sub(1);
                            self.selection_end = Position::new(prev_row, self.line_len(prev_row));
                        } else {
                            // Selection ends one before cursor
                            self.selection_end =
                                Position::new(self.cursor.row, self.cursor.col.saturating_sub(1));
                        }
                    }
                } else {
                    // Cursor at anchor: single character selection
                    self.selection_start = anchor;
                    self.selection_end = anchor;
                }
            }
            Mode::VisualLine => {
                // Line-wise selection
                let start_row = self.cursor.row.min(self.selection_anchor.row);
                let end_row = self.cursor.row.max(self.selection_anchor.row);
                self.selection_start = Position::new(start_row, 0);
                self.selection_end = Position::new(end_row, self.last_col(end_row));
            }
        }
    }

    /// Check if a character is a word character (alphanumeric or underscore)
    fn is_word_char(ch: char) -> bool {
        ch.is_alphanumeric() || ch == '_'
    }

    /// Get character at position, or None if out of bounds
    fn char_at(&self, row: usize, col: usize) -> Option<char> {
        self.lines.get(row).and_then(|line| line.chars().nth(col))
    }

    // ==================== Mode Changes ====================

    fn enter_normal_mode(&mut self) {
        self.mode = Mode::Normal;
        self.selection_start = self.cursor;
        self.selection_end = self.cursor;
    }

    fn toggle_visual_mode(&mut self) {
        match self.mode {
            Mode::Visual => self.enter_normal_mode(),
            _ => {
                self.mode = Mode::Visual;
                self.selection_anchor = self.cursor;
                self.update_selection();
            }
        }
    }

    fn toggle_visual_line_mode(&mut self) {
        match self.mode {
            Mode::VisualLine => self.enter_normal_mode(),
            _ => {
                self.mode = Mode::VisualLine;
                self.selection_anchor = self.cursor;
                self.update_selection();
            }
        }
    }

    // ==================== Basic Motions ====================

    fn motion_h(&mut self, count: usize) {
        self.cursor.col = self.cursor.col.saturating_sub(count);
        self.update_selection();
    }

    fn motion_l(&mut self, count: usize) {
        let max_col = self.last_col(self.cursor.row);
        self.cursor.col = (self.cursor.col + count).min(max_col);
        self.update_selection();
    }

    fn motion_j(&mut self, count: usize) {
        let max_row = self.total_lines().saturating_sub(1);
        self.cursor.row = (self.cursor.row + count).min(max_row);
        self.clamp_cursor_col();
        self.ensure_cursor_visible();
        self.update_selection();
    }

    fn motion_k(&mut self, count: usize) {
        self.cursor.row = self.cursor.row.saturating_sub(count);
        self.clamp_cursor_col();
        self.ensure_cursor_visible();
        self.update_selection();
    }

    // ==================== Line Position Motions ====================

    fn motion_0(&mut self) {
        self.cursor.col = 0;
        self.update_selection();
    }

    fn motion_caret(&mut self) {
        // Go to first non-blank character
        if let Some(line) = self.lines.get(self.cursor.row) {
            self.cursor.col = line.chars().position(|c| !c.is_whitespace()).unwrap_or(0);
        }
        self.update_selection();
    }

    fn motion_dollar(&mut self, count: usize) {
        // $ goes to end of line. With count, goes to end of line (count-1) below
        if count > 1 {
            let max_row = self.total_lines().saturating_sub(1);
            self.cursor.row = (self.cursor.row + count - 1).min(max_row);
        }
        // In visual mode, $ goes one past the last character (to include it in selection)
        // In normal mode, $ goes to the last character
        let line_len = self.line_len(self.cursor.row);
        if self.mode == Mode::Visual || self.mode == Mode::VisualLine {
            self.cursor.col = line_len; // One past last char
        } else {
            self.cursor.col = if line_len == 0 { 0 } else { line_len - 1 };
        }
        self.update_selection();
    }

    // ==================== Word Motions ====================

    fn motion_w(&mut self, count: usize) {
        for _ in 0..count {
            self.word_forward(false);
        }
        self.update_selection();
    }

    fn motion_w_big(&mut self, count: usize) {
        for _ in 0..count {
            self.word_forward(true);
        }
        self.update_selection();
    }

    fn motion_b(&mut self, count: usize) {
        for _ in 0..count {
            self.word_backward(false);
        }
        self.update_selection();
    }

    fn motion_b_big(&mut self, count: usize) {
        for _ in 0..count {
            self.word_backward(true);
        }
        self.update_selection();
    }

    fn motion_e(&mut self, count: usize) {
        for _ in 0..count {
            self.word_end_forward(false);
        }
        self.update_selection();
    }

    fn motion_e_big(&mut self, count: usize) {
        for _ in 0..count {
            self.word_end_forward(true);
        }
        self.update_selection();
    }

    /// Move to start of next word
    fn word_forward(&mut self, big_word: bool) {
        let total = self.total_lines();
        if total == 0 {
            return;
        }

        let mut row = self.cursor.row;
        let mut col = self.cursor.col;

        // Get current character type
        let is_word = if big_word {
            |c: char| !c.is_whitespace()
        } else {
            Self::is_word_char as fn(char) -> bool
        };

        // Skip current word (same type characters)
        if let Some(ch) = self.char_at(row, col) {
            let in_word = is_word(ch);
            let is_space = ch.is_whitespace();

            if !is_space {
                // Skip rest of current word/punctuation
                while let Some(c) = self.char_at(row, col) {
                    if is_word(c) != in_word || c.is_whitespace() {
                        break;
                    }
                    col += 1;
                }
            }
        }

        // Skip whitespace (possibly across lines)
        loop {
            while let Some(c) = self.char_at(row, col) {
                if !c.is_whitespace() {
                    self.cursor.row = row;
                    self.cursor.col = col;
                    return;
                }
                col += 1;
            }
            // End of line, go to next
            row += 1;
            col = 0;
            if row >= total {
                // End of document
                self.cursor.row = total - 1;
                self.cursor.col = self.last_col(total - 1);
                return;
            }
            // Empty line acts as a word boundary - stop at column 0
            if self.line_len(row) == 0 {
                self.cursor.row = row;
                self.cursor.col = 0;
                return;
            }
        }
    }

    /// Move to start of previous word
    fn word_backward(&mut self, big_word: bool) {
        let mut row = self.cursor.row;
        let mut col = self.cursor.col;

        let is_word = if big_word {
            |c: char| !c.is_whitespace()
        } else {
            Self::is_word_char as fn(char) -> bool
        };

        // Move back one position to start
        if col == 0 {
            if row == 0 {
                return;
            }
            row -= 1;
            col = self.line_len(row);
        }
        if col > 0 {
            col -= 1;
        }

        // Skip whitespace backward
        loop {
            while col > 0 || row > 0 {
                if let Some(c) = self.char_at(row, col) {
                    if !c.is_whitespace() {
                        break;
                    }
                }
                if col == 0 {
                    if row == 0 {
                        self.cursor.row = 0;
                        self.cursor.col = 0;
                        return;
                    }
                    row -= 1;
                    col = self.line_len(row).saturating_sub(1);
                } else {
                    col -= 1;
                }
            }
            break;
        }

        // Now we're on a non-whitespace character. Find the start of this word.
        if let Some(ch) = self.char_at(row, col) {
            let in_word = is_word(ch);

            while col > 0 {
                if let Some(c) = self.char_at(row, col - 1) {
                    if is_word(c) != in_word || c.is_whitespace() {
                        break;
                    }
                    col -= 1;
                } else {
                    break;
                }
            }
        }

        self.cursor.row = row;
        self.cursor.col = col;
    }

    /// Move to end of current/next word
    fn word_end_forward(&mut self, big_word: bool) {
        let total = self.total_lines();
        if total == 0 {
            return;
        }

        let mut row = self.cursor.row;
        let mut col = self.cursor.col;

        let is_word = if big_word {
            |c: char| !c.is_whitespace()
        } else {
            Self::is_word_char as fn(char) -> bool
        };

        // Move forward at least one position
        col += 1;
        if col >= self.line_len(row) {
            row += 1;
            col = 0;
        }

        // Skip whitespace
        loop {
            if row >= total {
                self.cursor.row = total - 1;
                self.cursor.col = self.last_col(total - 1);
                return;
            }

            if let Some(c) = self.char_at(row, col) {
                if !c.is_whitespace() {
                    break;
                }
            }

            col += 1;
            if col >= self.line_len(row) {
                row += 1;
                col = 0;
            }
        }

        // Now on a word. Find its end.
        if let Some(ch) = self.char_at(row, col) {
            let in_word = is_word(ch);

            while let Some(c) = self.char_at(row, col + 1) {
                if is_word(c) != in_word || c.is_whitespace() {
                    break;
                }
                col += 1;
            }
        }

        self.cursor.row = row;
        self.cursor.col = col;
    }

    // ==================== Document Navigation ====================

    fn motion_gg(&mut self, count: usize) {
        // gg goes to line N (1-indexed), default first line
        if self.input_state.count.is_some() || count > 1 {
            self.cursor.row = (count - 1).min(self.total_lines().saturating_sub(1));
        } else {
            self.cursor.row = 0;
        }
        self.clamp_cursor_col();
        self.ensure_cursor_visible();
        self.motion_caret(); // Go to first non-blank
        self.update_selection();
    }

    fn motion_g_big(&mut self, count: usize) {
        // G goes to line N (1-indexed), default last line
        if count > 1 {
            self.cursor.row = (count - 1).min(self.total_lines().saturating_sub(1));
        } else {
            self.cursor.row = self.total_lines().saturating_sub(1);
        }
        self.clamp_cursor_col();
        self.ensure_cursor_visible();
        self.motion_caret();
        self.update_selection();
    }

    // ==================== Find Character Motions ====================

    fn motion_f(&mut self, ch: u8, count: usize) {
        let target = ch as char;
        if let Some(line) = self.lines.get(self.cursor.row) {
            let chars: Vec<char> = line.chars().collect();
            let mut found = 0;
            for i in (self.cursor.col + 1)..chars.len() {
                if chars[i] == target {
                    found += 1;
                    if found == count {
                        self.cursor.col = i;
                        break;
                    }
                }
            }
        }
        self.update_selection();
    }

    fn motion_f_back(&mut self, ch: u8, count: usize) {
        let target = ch as char;
        if let Some(line) = self.lines.get(self.cursor.row) {
            let chars: Vec<char> = line.chars().collect();
            let mut found = 0;
            for i in (0..self.cursor.col).rev() {
                if chars[i] == target {
                    found += 1;
                    if found == count {
                        self.cursor.col = i;
                        break;
                    }
                }
            }
        }
        self.update_selection();
    }

    fn motion_t(&mut self, ch: u8, count: usize) {
        let target = ch as char;
        if let Some(line) = self.lines.get(self.cursor.row) {
            let chars: Vec<char> = line.chars().collect();
            let mut found = 0;
            for i in (self.cursor.col + 1)..chars.len() {
                if chars[i] == target {
                    found += 1;
                    if found == count {
                        self.cursor.col = i.saturating_sub(1);
                        break;
                    }
                }
            }
        }
        self.update_selection();
    }

    fn motion_t_back(&mut self, ch: u8, count: usize) {
        let target = ch as char;
        if let Some(line) = self.lines.get(self.cursor.row) {
            let chars: Vec<char> = line.chars().collect();
            let mut found = 0;
            for i in (0..self.cursor.col).rev() {
                if chars[i] == target {
                    found += 1;
                    if found == count {
                        self.cursor.col = (i + 1).min(chars.len().saturating_sub(1));
                        break;
                    }
                }
            }
        }
        self.update_selection();
    }

    // ==================== Matching Bracket ====================

    fn motion_percent(&mut self) {
        let brackets = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')];

        if let Some(ch) = self.char_at(self.cursor.row, self.cursor.col) {
            // Find if current char is a bracket
            let mut open_bracket = None;
            let mut is_opening = false;

            for (open, close) in &brackets {
                if ch == *open {
                    open_bracket = Some((*open, *close));
                    is_opening = true;
                    break;
                } else if ch == *close {
                    open_bracket = Some((*open, *close));
                    is_opening = false;
                    break;
                }
            }

            if let Some((open, close)) = open_bracket {
                if is_opening {
                    self.find_matching_bracket_forward(open, close);
                } else {
                    self.find_matching_bracket_backward(open, close);
                }
            }
        }
        self.update_selection();
    }

    fn find_matching_bracket_forward(&mut self, open: char, close: char) {
        let mut depth = 1;
        let mut row = self.cursor.row;
        let mut col = self.cursor.col + 1;

        while row < self.total_lines() {
            let line_len = self.line_len(row);
            while col < line_len {
                if let Some(ch) = self.char_at(row, col) {
                    if ch == open {
                        depth += 1;
                    } else if ch == close {
                        depth -= 1;
                        if depth == 0 {
                            self.cursor.row = row;
                            self.cursor.col = col;
                            return;
                        }
                    }
                }
                col += 1;
            }
            row += 1;
            col = 0;
        }
    }

    fn find_matching_bracket_backward(&mut self, open: char, close: char) {
        let mut depth = 1;
        let mut row = self.cursor.row;
        let mut col = self.cursor.col;

        loop {
            if col == 0 {
                if row == 0 {
                    return;
                }
                row -= 1;
                col = self.line_len(row);
            }
            col -= 1;

            if let Some(ch) = self.char_at(row, col) {
                if ch == close {
                    depth += 1;
                } else if ch == open {
                    depth -= 1;
                    if depth == 0 {
                        self.cursor.row = row;
                        self.cursor.col = col;
                        return;
                    }
                }
            }
        }
    }

    // ==================== Scrolling ====================

    fn scroll_down(&mut self, count: usize) {
        // Ctrl-D: scroll down half a page and move cursor
        let scroll_amount = count * (self.viewport_height / 2);
        let max_row = self.total_lines().saturating_sub(1);

        self.cursor.row = (self.cursor.row + scroll_amount).min(max_row);
        self.scroll_offset_row = (self.scroll_offset_row + scroll_amount)
            .min(self.total_lines().saturating_sub(self.viewport_height));
        self.clamp_cursor_col();
        self.update_selection();
    }

    fn scroll_up(&mut self, count: usize) {
        // Ctrl-U: scroll up half a page and move cursor
        let scroll_amount = count * (self.viewport_height / 2);

        self.cursor.row = self.cursor.row.saturating_sub(scroll_amount);
        self.scroll_offset_row = self.scroll_offset_row.saturating_sub(scroll_amount);
        self.clamp_cursor_col();
        self.update_selection();
    }

    fn page_down(&mut self, count: usize) {
        // Ctrl-F: page down
        let scroll_amount = count * self.viewport_height;
        let max_row = self.total_lines().saturating_sub(1);

        self.cursor.row = (self.cursor.row + scroll_amount).min(max_row);
        self.scroll_offset_row = (self.scroll_offset_row + scroll_amount)
            .min(self.total_lines().saturating_sub(self.viewport_height));
        self.clamp_cursor_col();
        self.update_selection();
    }

    fn page_up(&mut self, count: usize) {
        // Ctrl-B: page up
        let scroll_amount = count * self.viewport_height;

        self.cursor.row = self.cursor.row.saturating_sub(scroll_amount);
        self.scroll_offset_row = self.scroll_offset_row.saturating_sub(scroll_amount);
        self.clamp_cursor_col();
        self.update_selection();
    }

    fn scroll_cursor_to_top(&mut self) {
        // zt: scroll so cursor is at top of screen
        self.scroll_offset_row = self.cursor.row;
    }

    fn scroll_cursor_to_center(&mut self) {
        // zz: scroll so cursor is at center of screen
        self.scroll_offset_row = self.cursor.row.saturating_sub(self.viewport_height / 2);
    }

    fn scroll_cursor_to_bottom(&mut self) {
        // zb: scroll so cursor is at bottom of screen
        self.scroll_offset_row = self
            .cursor
            .row
            .saturating_sub(self.viewport_height.saturating_sub(1));
    }

    // ==================== Screen-Relative Motions ====================

    fn motion_h_screen(&mut self) {
        // H: go to top of screen
        self.cursor.row = self.scroll_offset_row;
        self.motion_caret();
        self.update_selection();
    }

    fn motion_m_screen(&mut self) {
        // M: go to middle of screen
        let visible_lines = self
            .viewport_height
            .min(self.total_lines() - self.scroll_offset_row);
        self.cursor.row = self.scroll_offset_row + visible_lines / 2;
        self.motion_caret();
        self.update_selection();
    }

    fn motion_l_screen(&mut self) {
        // L: go to bottom of screen
        let visible_lines = self
            .viewport_height
            .min(self.total_lines() - self.scroll_offset_row);
        self.cursor.row = self.scroll_offset_row + visible_lines.saturating_sub(1);
        self.motion_caret();
        self.update_selection();
    }

    // ==================== Paragraph Motions ====================

    fn motion_paragraph_forward(&mut self, count: usize) {
        // } moves forward to the next blank line (or end of file)
        let total = self.total_lines();
        if total == 0 {
            return;
        }

        for _ in 0..count {
            let mut row = self.cursor.row;

            // Skip any blank lines we're currently on
            while row < total && self.is_blank_line(row) {
                row += 1;
            }

            // Find the next blank line
            while row < total && !self.is_blank_line(row) {
                row += 1;
            }

            self.cursor.row = row.min(total.saturating_sub(1));
        }

        self.cursor.col = 0;
        self.clamp_cursor_col();
        self.ensure_cursor_visible();
        self.update_selection();
    }

    fn motion_paragraph_backward(&mut self, count: usize) {
        // { moves backward to the previous blank line (or beginning of file)
        for _ in 0..count {
            let mut row = self.cursor.row;

            // If we're at the start, stay there
            if row == 0 {
                break;
            }

            // Move up at least one line
            row = row.saturating_sub(1);

            // Skip any blank lines we're currently on
            while row > 0 && self.is_blank_line(row) {
                row -= 1;
            }

            // Find the previous blank line
            while row > 0 && !self.is_blank_line(row) {
                row -= 1;
            }

            self.cursor.row = row;
        }

        self.cursor.col = 0;
        self.clamp_cursor_col();
        self.ensure_cursor_visible();
        self.update_selection();
    }

    /// Check if a line is blank (empty or whitespace only)
    fn is_blank_line(&self, row: usize) -> bool {
        self.lines
            .get(row)
            .map(|l| l.trim().is_empty())
            .unwrap_or(true)
    }

    // ==================== Text Objects ====================

    fn text_object_inner(&mut self, ch: u8) {
        // Only works in visual mode
        if self.mode != Mode::Visual && self.mode != Mode::VisualLine {
            return;
        }

        match ch {
            b'(' | b')' => self.select_inner_bracket('(', ')'),
            b'[' | b']' => self.select_inner_bracket('[', ']'),
            b'{' | b'}' => self.select_inner_bracket('{', '}'),
            b'<' | b'>' => self.select_inner_bracket('<', '>'),
            b'"' => self.select_inner_quote('"'),
            b'\'' => self.select_inner_quote('\''),
            _ => {}
        }
    }

    fn text_object_around(&mut self, ch: u8) {
        // Only works in visual mode
        if self.mode != Mode::Visual && self.mode != Mode::VisualLine {
            return;
        }

        match ch {
            b'(' | b')' => self.select_around_bracket('(', ')'),
            b'[' | b']' => self.select_around_bracket('[', ']'),
            b'{' | b'}' => self.select_around_bracket('{', '}'),
            b'<' | b'>' => self.select_around_bracket('<', '>'),
            b'"' => self.select_around_quote('"'),
            b'\'' => self.select_around_quote('\''),
            _ => {}
        }
    }

    fn select_inner_bracket(&mut self, open: char, close: char) {
        // Find the enclosing bracket pair and select contents inside
        if let Some((start_row, start_col, end_row, end_col)) =
            self.find_enclosing_brackets(open, close)
        {
            // Inner: select from after open bracket to before close bracket
            let inner_start_col = start_col + 1;
            let inner_end_col = if end_col > 0 { end_col - 1 } else { 0 };

            // Handle case where brackets are on different lines
            if start_row == end_row {
                if inner_start_col <= inner_end_col {
                    // In vim visual mode with vi(, the cursor lands on the last char of selection
                    // and selection ends one before the cursor position
                    self.selection_anchor = Position::new(start_row, inner_start_col);
                    // Cursor goes to the last character of inner selection
                    self.cursor = Position::new(end_row, inner_end_col);
                    // Selection covers from inner_start to one before cursor (inner_end - 1)
                    self.selection_start = Position::new(start_row, inner_start_col);
                    self.selection_end = Position::new(end_row, inner_end_col.saturating_sub(1));
                }
            } else {
                self.selection_anchor = Position::new(start_row, inner_start_col);
                self.cursor = Position::new(end_row, inner_end_col);
                self.selection_start = Position::new(start_row, inner_start_col);
                self.selection_end = Position::new(end_row, inner_end_col.saturating_sub(1));
            }
        }
    }

    fn select_around_bracket(&mut self, open: char, close: char) {
        // Find the enclosing bracket pair and select including brackets
        if let Some((start_row, start_col, end_row, end_col)) =
            self.find_enclosing_brackets(open, close)
        {
            self.selection_anchor = Position::new(start_row, start_col);
            self.cursor = Position::new(end_row, end_col + 1);
            self.update_selection();
        }
    }

    fn find_enclosing_brackets(
        &self,
        open: char,
        close: char,
    ) -> Option<(usize, usize, usize, usize)> {
        // First, find the opening bracket by searching backward
        let mut depth = 0;
        let mut start_row = self.cursor.row;
        let mut start_col = self.cursor.col;
        let mut found_open = false;

        // Check if we're on the open bracket
        if self.char_at(start_row, start_col) == Some(open) {
            found_open = true;
        } else {
            // Search backward for opening bracket
            loop {
                if let Some(ch) = self.char_at(start_row, start_col) {
                    if ch == close {
                        depth += 1;
                    } else if ch == open {
                        if depth == 0 {
                            found_open = true;
                            break;
                        }
                        depth -= 1;
                    }
                }

                if start_col == 0 {
                    if start_row == 0 {
                        break;
                    }
                    start_row -= 1;
                    start_col = self.line_len(start_row).saturating_sub(1);
                } else {
                    start_col -= 1;
                }
            }
        }

        if !found_open {
            return None;
        }

        // Now find the closing bracket by searching forward from the open bracket
        let mut end_row = start_row;
        let mut end_col = start_col + 1;
        depth = 1;

        while end_row < self.total_lines() {
            let line_len = self.line_len(end_row);
            while end_col < line_len {
                if let Some(ch) = self.char_at(end_row, end_col) {
                    if ch == open {
                        depth += 1;
                    } else if ch == close {
                        depth -= 1;
                        if depth == 0 {
                            return Some((start_row, start_col, end_row, end_col));
                        }
                    }
                }
                end_col += 1;
            }
            end_row += 1;
            end_col = 0;
        }

        None
    }

    fn select_inner_quote(&mut self, quote: char) {
        // Find quotes on current line and select between them
        if let Some(line) = self.lines.get(self.cursor.row) {
            let chars: Vec<char> = line.chars().collect();
            let col = self.cursor.col;

            // Find quote boundaries
            let mut start = None;
            let mut end = None;

            // Find the opening quote (before or at cursor)
            for i in (0..=col.min(chars.len().saturating_sub(1))).rev() {
                if chars[i] == quote {
                    start = Some(i);
                    break;
                }
            }

            // If no quote before cursor, look for one after
            if start.is_none() {
                for i in col..chars.len() {
                    if chars[i] == quote {
                        start = Some(i);
                        break;
                    }
                }
            }

            if let Some(s) = start {
                // Find the closing quote (after start)
                for i in (s + 1)..chars.len() {
                    if chars[i] == quote {
                        end = Some(i);
                        break;
                    }
                }

                if let Some(e) = end {
                    // Inner: between quotes (not including quotes)
                    self.selection_anchor = Position::new(self.cursor.row, s + 1);
                    self.cursor = Position::new(self.cursor.row, e);
                    self.update_selection();
                }
            }
        }
    }

    fn select_around_quote(&mut self, quote: char) {
        // Find quotes on current line and select including them
        if let Some(line) = self.lines.get(self.cursor.row) {
            let chars: Vec<char> = line.chars().collect();
            let col = self.cursor.col;

            // Find quote boundaries
            let mut start = None;
            let mut end = None;

            // Find the opening quote (before or at cursor)
            for i in (0..=col.min(chars.len().saturating_sub(1))).rev() {
                if chars[i] == quote {
                    start = Some(i);
                    break;
                }
            }

            // If no quote before cursor, look for one after
            if start.is_none() {
                for i in col..chars.len() {
                    if chars[i] == quote {
                        start = Some(i);
                        break;
                    }
                }
            }

            if let Some(s) = start {
                // Find the closing quote (after start)
                for i in (s + 1)..chars.len() {
                    if chars[i] == quote {
                        end = Some(i);
                        break;
                    }
                }

                if let Some(e) = end {
                    // Around: including quotes
                    self.selection_anchor = Position::new(self.cursor.row, s);
                    self.cursor = Position::new(self.cursor.row, e + 1);
                    self.update_selection();
                }
            }
        }
    }
}
