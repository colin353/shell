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
    output.push(charset_ctar);
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
        // `Special comment 7 with backticks`
        let grid1 = TerminalGrid::new(80, 24);
        let mut grid2 = TerminalGrid::new(80, 24);

        // grid1.cursor_visible is true by default
        grid2.cursor_visible = false;

        let delta = compute_delta(&grid1, &grid2);

        // Should contain hide cursor sequence
        assert!(delta.windows(6).any(|w| w == b"\x1b[?25l"));
    }
}
