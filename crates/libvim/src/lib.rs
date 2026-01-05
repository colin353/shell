//! libvim library

pub struct Position {
    pub row: usize,
    pub col: usize,
}

pub struct VimCursorEngine<'a> {
    lines: &'a [String],
    viewport_height: usize,
    viewport_width: usize,
    scroll_offset_row: usize,
    scroll_offset_col: usize,

    cursor: Position,

    selection_start: Position,
    selection_end: Position,
}

impl<'a> VimCursorEngine<'a> {
    pub fn new(lines: &'a [String], viewport_height: usize, viewport_width: usize) -> Self {
        VimCursorEngine {
            lines,
            viewport_height,
            viewport_width,
            scroll_offset_row: 0,
            scroll_offset_col: 0,
            cursor: Position { row: 0, col: 0 },
            selection_start: Position { row: 0, col: 0 },
            selection_end: Position { row: 0, col: 0 },
        }
    }

    pub fn handle_input(&mut self, input: &[u8]) {
        // TODO: Handle input bytes to update cursor position and selection
    }
}
