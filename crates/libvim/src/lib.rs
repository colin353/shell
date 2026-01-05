//! libvim library

pub struct Position {
    pub row: usize,
    pub col: usize,
}

pub struct VimCursorEngine<'a> {
    pub lines: &'a [String],
    pub viewport_height: usize,
    pub viewport_width: usize,
    pub scroll_offset_row: usize,
    pub scroll_offset_col: usize,

    pub cursor: Position,

    pub selection_start: Position,
    pub selection_end: Position,
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

    pub fn handle_input(&mut self, _input: &[u8]) {
        // TODO: Handle input bytes to update cursor position and selection
    }
}
