//! Delta rendering - compute minimal terminal escape sequences to transition between states
//!
//! This module provides functionality to compare two terminal grid states and produce
//! an efficient sequence of ANSI escape codes to transform the first state into the second.

use crate::cell::{Cell, CellAttributes, Color};
use crate::grid::TerminalGrid;

/// Compute the minimal sequence of terminal commands to transition from `prev` to `next`.
///
/// Returns a `Vec<u8>` containing ANSI escape sequences that, when processed by a terminal
/// starting in the `prev` state, will result in the `next` state.
pub fn compute_delta(prev: &TerminalGrid, next: &TerminalGrid) -> Vec<u8> {
    let mut output = Vec::new();

    // Track current attributes to minimize SGR commands
    let mut current_attrs = CellAttributes::default();

    // Track cursor position to minimize movement commands
    let mut cursor_x = prev.cursor_x;
    let mut cursor_y = prev.cursor_y;

    // Compare cells and emit changes
    let rows = prev.rows.min(next.rows);
    let cols = prev.cols.min(next.cols);

    for y in 0..rows {
        // Track if we have pending changes on this line
        let mut line_start: Option<usize> = None;
        let mut pending_cells: Vec<&Cell> = Vec::new();

        for x in 0..cols {
            let prev_cell = prev.get_cell(x, y);
            let next_cell = next.get_cell(x, y);

            if prev_cell != next_cell {
                // Cell differs - we need to update it
                if line_start.is_none() {
                    line_start = Some(x);
                }
                pending_cells.push(next_cell);
            } else if !pending_cells.is_empty() {
                // Cell is same but we have pending changes - flush them
                emit_cell_batch(
                    &mut output,
                    &mut cursor_x,
                    &mut cursor_y,
                    &mut current_attrs,
                    line_start.unwrap(),
                    y,
                    &pending_cells,
                    cols,
                    rows,
                    next.autowrap,
                );
                line_start = None;
                pending_cells.clear();
            }
        }

        // Flush any remaining pending cells at end of line
        if !pending_cells.is_empty() {
            emit_cell_batch(
                &mut output,
                &mut cursor_x,
                &mut cursor_y,
                &mut current_attrs,
                line_start.unwrap(),
                y,
                &pending_cells,
                cols,
                rows,
                next.autowrap,
            );
        }
    }

    // Handle cursor position change
    if cursor_x != next.cursor_x || cursor_y != next.cursor_y {
        emit_cursor_move(&mut output, next.cursor_x, next.cursor_y);
    }

    // Handle cursor visibility change
    if prev.cursor_visible != next.cursor_visible {
        if next.cursor_visible {
            output.extend_from_slice(b"\x1b[?25h"); // Show cursor (DECTCEM)
        } else {
            output.extend_from_slice(b"\x1b[?25l"); // Hide cursor (DECTCEM)
        }
    }

    // Handle autowrap mode change
    if prev.autowrap != next.autowrap {
        if next.autowrap {
            output.extend_from_slice(b"\x1b[?7h"); // Enable autowrap (DECAWM)
        } else {
            output.extend_from_slice(b"\x1b[?7l"); // Disable autowrap (DECAWM)
        }
    }

    // Handle origin mode change
    if prev.origin_mode != next.origin_mode {
        if next.origin_mode {
            output.extend_from_slice(b"\x1b[?6h"); // Enable origin mode (DECOM)
        } else {
            output.extend_from_slice(b"\x1b[?6l"); // Disable origin mode (DECOM)
        }
    }

    // Handle alternate screen change
    if prev.in_alternate_screen != next.in_alternate_screen {
        if next.in_alternate_screen {
            output.extend_from_slice(b"\x1b[?1049h"); // Enter alternate screen
        } else {
            output.extend_from_slice(b"\x1b[?1049l"); // Leave alternate screen
        }
    }

    // Handle scroll region change
    if prev.scroll_top != next.scroll_top || prev.scroll_bottom != next.scroll_bottom {
        // DECSTBM - Set Top and Bottom Margins
        // ESC [ top ; bottom r
        let top = next.scroll_top + 1; // 1-indexed
        let bottom = next.scroll_bottom + 1; // 1-indexed
        output.extend_from_slice(format!("\x1b[{};{}r", top, bottom).as_bytes());
    }

    // Handle character set changes
    if prev.charset_g0 != next.charset_g0 {
        emit_charset_designation(&mut output, 0, next.charset_g0);
    }
    if prev.charset_g1 != next.charset_g1 {
        emit_charset_designation(&mut output, 1, next.charset_g1);
    }

    // Handle GL charset shift
    if prev.gl_is_g1 != next.gl_is_g1 {
        if next.gl_is_g1 {
            output.push(0x0E); // SO - Shift Out (activate G1)
        } else {
            output.push(0x0F); // SI - Shift In (activate G0)
        }
    }

    // Reset attributes at the end to leave terminal in clean state
    if current_attrs != CellAttributes::default() {
        output.extend_from_slice(b"\x1b[0m");
    }

    output
}

/// Emit a batch of consecutive cells starting at (start_x, y)
fn emit_cell_batch(
    output: &mut Vec<u8>,
    cursor_x: &mut usize,
    cursor_y: &mut usize,
    current_attrs: &mut CellAttributes,
    start_x: usize,
    y: usize,
    cells: &[&Cell],
    cols: usize,
    rows: usize,
    autowrap: bool,
) {
    if cells.is_empty() {
        return;
    }

    // Move cursor to start position if needed
    if *cursor_x != start_x || *cursor_y != y {
        emit_cursor_move(output, start_x, y);
        *cursor_x = start_x;
        *cursor_y = y;
    }

    // Emit each cell
    for (i, cell) in cells.iter().enumerate() {
        let current_x = start_x + i;
        let is_last_cell = current_x == cols - 1 && y == rows - 1;

        // If we are writing to the last cell and autowrap is on, disable it temporarily
        // to prevent scrolling
        if is_last_cell && autowrap {
            output.extend_from_slice(b"\x1b[?7l");
        }

        // Update attributes if needed
        if cell.attrs != *current_attrs {
            emit_sgr_transition(output, current_attrs, &cell.attrs);
            *current_attrs = cell.attrs.clone();
        }

        // Emit the character
        let mut buf = [0u8; 4];
        let s = cell.character.encode_utf8(&mut buf);
        output.extend_from_slice(s.as_bytes());
        *cursor_x += 1;

        // Re-enable autowrap if we disabled it
        if is_last_cell && autowrap {
            output.extend_from_slice(b"\x1b[?7h");
        }
    }
}

/// Emit cursor movement command (CUP - Cursor Position)
fn emit_cursor_move(output: &mut Vec<u8>, x: usize, y: usize) {
    // CUP uses 1-indexed positions
    let row = y + 1;
    let col = x + 1;
    output.extend_from_slice(format!("\x1b[{};{}H", row, col).as_bytes());
}

/// Emit SGR (Select Graphic Rendition) commands to transition between attribute states
fn emit_sgr_transition(output: &mut Vec<u8>, from: &CellAttributes, to: &CellAttributes) {
    let mut params: Vec<u16> = Vec::new();

    // Check if we need a full reset
    let needs_reset = (from.bold && !to.bold)
        || (from.italic && !to.italic)
        || (from.underline && !to.underline)
        || (from.strikethrough && !to.strikethrough)
        || (from.dim && !to.dim)
        || (from.inverse && !to.inverse)
        || (from.hidden && !to.hidden)
        || (from.fg_color.is_some() && to.fg_color.is_none())
        || (from.bg_color.is_some() && to.bg_color.is_none());

    if needs_reset {
        // Reset all attributes first
        params.push(0);

        // Then set all the attributes we need
        if to.bold {
            params.push(1);
        }
        if to.dim {
            params.push(2);
        }
        if to.italic {
            params.push(3);
        }
        if to.underline {
            params.push(4);
        }
        if to.inverse {
            params.push(7);
        }
        if to.hidden {
            params.push(8);
        }
        if to.strikethrough {
            params.push(9);
        }

        // Set colors
        if let Some(ref fg) = to.fg_color {
            emit_color_params(&mut params, fg, true);
        }
        if let Some(ref bg) = to.bg_color {
            emit_color_params(&mut params, bg, false);
        }
    } else {
        // Incremental update - only set new attributes
        if !from.bold && to.bold {
            params.push(1);
        }
        if !from.dim && to.dim {
            params.push(2);
        }
        if !from.italic && to.italic {
            params.push(3);
        }
        if !from.underline && to.underline {
            params.push(4);
        }
        if !from.inverse && to.inverse {
            params.push(7);
        }
        if !from.hidden && to.hidden {
            params.push(8);
        }
        if !from.strikethrough && to.strikethrough {
            params.push(9);
        }

        // Handle color changes
        if from.fg_color != to.fg_color {
            if let Some(ref fg) = to.fg_color {
                emit_color_params(&mut params, fg, true);
            }
        }
        if from.bg_color != to.bg_color {
            if let Some(ref bg) = to.bg_color {
                emit_color_params(&mut params, bg, false);
            }
        }
    }

    if !params.is_empty() {
        output.extend_from_slice(b"\x1b[");
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                output.push(b';');
            }
            output.extend_from_slice(param.to_string().as_bytes());
        }
        output.push(b'm');
    }
}

/// Add color parameters to the SGR params list
fn emit_color_params(params: &mut Vec<u16>, color: &Color, is_foreground: bool) {
    let base = if is_foreground { 30 } else { 40 };

    match color {
        Color::Black => params.push(base),
        Color::Red => params.push(base + 1),
        Color::Green => params.push(base + 2),
        Color::Yellow => params.push(base + 3),
        Color::Blue => params.push(base + 4),
        Color::Magenta => params.push(base + 5),
        Color::Cyan => params.push(base + 6),
        Color::White => params.push(base + 7),
        Color::BrightBlack => params.push(base + 60), // 90 or 100
        Color::BrightRed => params.push(base + 61),
        Color::BrightGreen => params.push(base + 62),
        Color::BrightYellow => params.push(base + 63),
        Color::BrightBlue => params.push(base + 64),
        Color::BrightMagenta => params.push(base + 65),
        Color::BrightCyan => params.push(base + 66),
        Color::BrightWhite => params.push(base + 67),
        Color::Indexed(idx) => {
            // 256-color: ESC[38;5;{idx}m for fg, ESC[48;5;{idx}m for bg
            params.push(if is_foreground { 38 } else { 48 });
            params.push(5);
            params.push(*idx as u16);
        }
        Color::Rgb(r, g, b) => {
            // True color: ESC[38;2;{r};{g};{b}m for fg, ESC[48;2;{r};{g};{b}m for bg
            params.push(if is_foreground { 38 } else { 48 });
            params.push(2);
            params.push(*r as u16);
            params.push(*g as u16);
            params.push(*b as u16);
        }
    }
}

/// Emit character set designation sequence
fn emit_charset_designation(output: &mut Vec<u8>, g: u8, charset: crate::grid::CharSet) {
    use crate::grid::CharSet;

    let designator = match g {
        0 => b'(',
        1 => b')',
        _ => return,
    };

    let charset_char = match charset {
        CharSet::Ascii => b'B',
        CharSet::Uk => b'A',
        CharSet::DecSpecialGraphics => b'0',
        CharSet::DecAltRomStandard => b'1',
        CharSet::DecAltRomSpecial => b'2',
    };

    output.push(0x1b);
    output.push(designator);
    output.push(charset_char);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TerminalEmulator;

    #[test]
    fn test_empty_delta() {
        // Two identical grids should produce no output (except possibly a reset)
        let grid1 = TerminalGrid::new(80, 24);
        let grid2 = TerminalGrid::new(80, 24);

        let delta = compute_delta(&grid1, &grid2);
        // Should be empty or just a final reset
        assert!(delta.is_empty() || delta == b"\x1b[0m");
    }

    #[test]
    fn test_single_char_change() {
        let mut emu1 = TerminalEmulator::new(80, 24);
        let mut emu2 = TerminalEmulator::new(80, 24);

        emu1.process(b"Hello");
        emu2.process(b"Jello");

        let delta = compute_delta(emu1.grid(), emu2.grid());

        // Apply delta to emu1 and verify it matches emu2
        emu1.process(&delta);

        // Check that the first character is now 'J'
        assert_eq!(emu1.grid().get_cell(0, 0).character, 'J');
    }

    #[test]
    fn test_cursor_move() {
        let grid1 = TerminalGrid::new(80, 24);
        let mut grid2 = TerminalGrid::new(80, 24);

        grid2.cursor_x = 10;
        grid2.cursor_y = 5;

        let delta = compute_delta(&grid1, &grid2);

        // Should contain cursor movement
        assert!(delta.windows(4).any(|w| w == b"\x1b[6;"));
    }

    #[test]
    fn test_cursor_visibility() {
        let grid1 = TerminalGrid::new(80, 24);
        let mut grid2 = TerminalGrid::new(80, 24);

        // grid1.cursor_visible is true by default
        grid2.cursor_visible = false;

        let delta = compute_delta(&grid1, &grid2);

        // Should contain hide cursor sequence
        assert!(delta.windows(6).any(|w| w == b"\x1b[?25l"));
    }
}
