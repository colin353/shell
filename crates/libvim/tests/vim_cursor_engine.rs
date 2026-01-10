//! Integration tests for VimCursorEngine by comparing against real vim
//!
//! These tests spawn real vim with a test file, send input sequences,
//! and compare the cursor position and selection state with VimCursorEngine.

use emulator::TerminalEmulator;
use libvim::VimCursorEngine;
use pty::PtyProcess;
use std::fs;
use std::path::Path;
use std::thread;
use std::time::Duration;

/// Cursor and selection position information
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CursorInfo {
    /// Cursor row (0-indexed)
    row: usize,
    /// Cursor column (0-indexed)
    col: usize,
    /// Whether a selection is active
    has_selection: bool,
    /// Start of selection (row, col), if active
    selection_start: Option<(usize, usize)>,
    /// End of selection (row, col), if active
    selection_end: Option<(usize, usize)>,
}

/// Run an arbitrary command with given input sequences and return the emulator state.
fn run_pty_test(command: &str, cols: u16, rows: u16, inputs: &[&[u8]]) -> TerminalEmulator {
    let pty = PtyProcess::spawn(command, cols, rows)
        .unwrap_or_else(|e| panic!("Failed to spawn {}: {}", command, e));

    let mut emulator = TerminalEmulator::new(cols as usize, rows as usize);
    let mut buf = [0u8; 8192];

    // Helper to drain all available PTY output
    let drain_pty = |pty: &PtyProcess, emulator: &mut TerminalEmulator, buf: &mut [u8]| loop {
        match pty.read(buf) {
            Ok(Some(n)) if n > 0 => {
                emulator.process(&buf[..n]);

                // Send any responses back to the PTY (e.g., DSR responses)
                for response in emulator.drain_responses() {
                    let _ = pty.write(&response);
                }
            }
            _ => break,
        }
    };

    // Wait for vim to start
    for _ in 0..50 {
        thread::sleep(Duration::from_millis(20));
        drain_pty(&pty, &mut emulator, &mut buf);
    }

    // Send each input sequence with a wait period between
    for input in inputs {
        pty.write(input).expect("Failed to write to pty");
        for _ in 0..20 {
            thread::sleep(Duration::from_millis(20));
            drain_pty(&pty, &mut emulator, &mut buf);
        }
    }

    // Return emulator state BEFORE quitting vim, so we capture
    // the cursor position while vim is still running.
    // The pty process will be killed when it goes out of scope.
    emulator
}

/// Detect the cursor position and any selection region in the emulator.
///
/// Returns CursorInfo with:
/// - Cursor position (always present)
/// - Selection boundaries if any cell has inverse attribute or non-default background color
fn detect_cursor_info(emulator: &TerminalEmulator) -> CursorInfo {
    let grid = emulator.grid();
    let (cursor_col, cursor_row) = (grid.cursor_x, grid.cursor_y);

    // Find all cells with inverse attribute or background color (indicates selection in vim)
    let mut selection_cells = Vec::new();

    for y in 0..grid.rows {
        for x in 0..grid.cols {
            if let Some(cell) =
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| grid.get_cell(x, y))).ok()
            {
                // Vim indicates selection via inverse attribute OR background color
                if cell.attrs.inverse || cell.attrs.bg_color.is_some() {
                    selection_cells.push((x, y));
                }
            }
        }
    }

    let (has_selection, selection_start, selection_end) = if selection_cells.is_empty() {
        (false, None, None)
    } else {
        // Find the bounds of the selection region
        let min_row = selection_cells
            .iter()
            .map(|(_, y)| y)
            .min()
            .copied()
            .unwrap_or(0);
        let max_row = selection_cells
            .iter()
            .map(|(_, y)| y)
            .max()
            .copied()
            .unwrap_or(0);
        let min_col = selection_cells
            .iter()
            .filter(|(_, y)| *y == min_row)
            .map(|(x, _)| x)
            .min()
            .copied()
            .unwrap_or(0);
        let max_col = selection_cells
            .iter()
            .filter(|(_, y)| *y == max_row)
            .map(|(x, _)| x)
            .max()
            .copied()
            .unwrap_or(0);

        (true, Some((min_row, min_col)), Some((max_row, max_col)))
    };

    CursorInfo {
        row: cursor_row,
        col: cursor_col,
        has_selection,
        selection_start,
        selection_end,
    }
}

/// Load test file contents
fn load_test_file() -> Vec<String> {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let fixture_path = Path::new(manifest_dir)
        .join("fixtures")
        .join("test_code.rs");

    let content = fs::read_to_string(&fixture_path).expect("Failed to read test_code.rs fixture");

    content.lines().map(|s| s.to_string()).collect()
}

/// Visualize the vim emulator's grid with cursor and selection highlighted using colors
fn visualize_vim_grid(emulator: &TerminalEmulator, cursor_info: &CursorInfo) -> String {
    let grid = emulator.grid();
    let mut output = String::new();

    output.push_str("=== VIM EMULATOR GRID ===\n");
    output.push_str(
        "Cursor: \x1b[44m  \x1b[0m  Selection: \x1b[46m  \x1b[0m  Both: \x1b[42m  \x1b[0m\n",
    );

    for y in 0..std::cmp::min(grid.rows, 20) {
        output.push_str(&format!("{:2} ", y));

        for x in 0..std::cmp::min(grid.cols, 80) {
            let cell = grid.get_cell(x, y);
            let is_cursor = x == cursor_info.col && y == cursor_info.row;
            let is_selected = if let (Some((s_r, s_c)), Some((e_r, e_c))) =
                (cursor_info.selection_start, cursor_info.selection_end)
            {
                (y >= s_r && y <= e_r)
                    && (if s_r == e_r {
                        x >= s_c && x <= e_c
                    } else if y == s_r {
                        x >= s_c
                    } else if y == e_r {
                        x <= e_c
                    } else {
                        true
                    })
            } else {
                false
            };

            let ch = if cell.character == ' ' {
                '·'
            } else {
                cell.character
            };

            if is_cursor && is_selected {
                output.push_str(&format!("\x1b[42m{}\x1b[0m", ch));
            } else if is_cursor {
                output.push_str(&format!("\x1b[44m{}\x1b[0m", ch));
            } else if is_selected {
                output.push_str(&format!("\x1b[46m{}\x1b[0m", ch));
            } else {
                output.push(ch);
            }
        }
        output.push('\n');
    }

    output.push_str(&format!(
        "Cursor: ({},{})",
        cursor_info.row, cursor_info.col
    ));
    if let (Some((sr, sc)), Some((er, ec))) =
        (cursor_info.selection_start, cursor_info.selection_end)
    {
        output.push_str(&format!(" | Selection: ({},{}) to ({},{})", sr, sc, er, ec));
    }
    output.push('\n');

    output
}

/// Visualize the VimCursorEngine's state with cursor and selection highlighted using colors
fn visualize_engine_grid(lines: &[String], engine: &VimCursorEngine) -> String {
    let mut output = String::new();

    output.push_str("=== VIM CURSOR ENGINE GRID ===\n");
    output.push_str(
        "Cursor: \x1b[44m  \x1b[0m  Selection: \x1b[46m  \x1b[0m  Both: \x1b[42m  \x1b[0m\n",
    );

    for (y, line) in lines.iter().enumerate().take(20) {
        output.push_str(&format!("{:2} ", y));

        let is_cursor_row = y == engine.cursor.row;
        let has_selection = engine.selection_start.row != engine.selection_end.row
            || engine.selection_start.col != engine.selection_end.col;

        for (x, ch) in line.chars().enumerate().take(80) {
            let is_cursor = is_cursor_row && x == engine.cursor.col;
            let is_selected = if has_selection {
                let (s_r, s_c) = (engine.selection_start.row, engine.selection_start.col);
                let (e_r, e_c) = (engine.selection_end.row, engine.selection_end.col);
                (y >= s_r && y <= e_r)
                    && (if s_r == e_r {
                        x >= s_c && x <= e_c
                    } else if y == s_r {
                        x >= s_c
                    } else if y == e_r {
                        x <= e_c
                    } else {
                        true
                    })
            } else {
                false
            };

            let display_ch = if ch == ' ' { '·' } else { ch };

            if is_cursor && is_selected {
                output.push_str(&format!("\x1b[42m{}\x1b[0m", display_ch));
            } else if is_cursor {
                output.push_str(&format!("\x1b[44m{}\x1b[0m", display_ch));
            } else if is_selected {
                output.push_str(&format!("\x1b[46m{}\x1b[0m", display_ch));
            } else {
                output.push(display_ch);
            }
        }
        output.push('\n');
    }

    output.push_str(&format!(
        "Cursor: ({},{})",
        engine.cursor.row, engine.cursor.col
    ));
    if engine.selection_start.row != engine.selection_end.row
        || engine.selection_start.col != engine.selection_end.col
    {
        output.push_str(&format!(
            " | Selection: ({},{}) to ({},{})",
            engine.selection_start.row,
            engine.selection_start.col,
            engine.selection_end.row,
            engine.selection_end.col
        ));
    }
    output.push('\n');

    output
}

/// Helper function to compare VimCursorEngine against real vim.
///
/// This function:
/// 1. Spawns real vim with the test fixture file
/// 2. Sends the given input sequences to vim
/// 3. Creates a VimCursorEngine with the same file content
/// 4. Sends the same inputs to the engine
/// 5. Compares cursor position and selection state
///
/// Panics with detailed visualization if there's a mismatch.
fn assert_vim_engine_match(inputs: &[&[u8]]) {
    // Check if vim is available
    if !std::process::Command::new("which")
        .arg("vim")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        eprintln!("vim not found, skipping test");
        return;
    }

    let cols = 100u16;
    let rows = 32u16;

    // Get the test file path
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let fixture_path = Path::new(manifest_dir)
        .join("fixtures")
        .join("test_code.rs");

    // Run real vim
    let vim_command = format!("vim -n {}", fixture_path.display());
    let vim_emulator = run_pty_test(&vim_command, cols, rows, inputs);
    let vim_cursor_info = detect_cursor_info(&vim_emulator);

    // Load test file for VimCursorEngine
    let test_lines = load_test_file();

    // Initialize VimCursorEngine
    let mut engine = VimCursorEngine::new(&test_lines, rows as usize, cols as usize);

    // Send the same inputs to VimCursorEngine
    for input in inputs {
        engine.handle_input(input);
    }

    // Get cursor info from engine (using screen-relative positions for comparison with vim's display)
    // Vim's cursor position from the terminal emulator is screen-relative, not document-relative
    let screen_row = engine.cursor.row.saturating_sub(engine.scroll_offset_row);
    let screen_col = engine.cursor.col.saturating_sub(engine.scroll_offset_col);

    // For selection, we also need to convert to screen-relative coordinates
    let selection_start_screen = (
        engine
            .selection_start
            .row
            .saturating_sub(engine.scroll_offset_row),
        engine
            .selection_start
            .col
            .saturating_sub(engine.scroll_offset_col),
    );
    let selection_end_screen = (
        engine
            .selection_end
            .row
            .saturating_sub(engine.scroll_offset_row),
        engine
            .selection_end
            .col
            .saturating_sub(engine.scroll_offset_col),
    );

    let engine_cursor_info = CursorInfo {
        row: screen_row,
        col: screen_col,
        has_selection: engine.selection_start.row != engine.selection_end.row
            || engine.selection_start.col != engine.selection_end.col,
        selection_start: if engine.selection_start.row != engine.selection_end.row
            || engine.selection_start.col != engine.selection_end.col
        {
            Some(selection_start_screen)
        } else {
            None
        },
        selection_end: if engine.selection_start.row != engine.selection_end.row
            || engine.selection_start.col != engine.selection_end.col
        {
            Some(selection_end_screen)
        } else {
            None
        },
    };

    // Format input sequence for error messages
    let input_str: String = inputs
        .iter()
        .map(|i| String::from_utf8_lossy(i).to_string())
        .collect::<Vec<_>>()
        .join(" ");

    println!("Testing input sequence: {}", input_str);
    println!("  Vim cursor: {:?}", vim_cursor_info);
    println!("  Engine cursor: {:?}", engine_cursor_info);

    // Compare cursor positions
    if vim_cursor_info.row != engine_cursor_info.row {
        eprintln!("\n{}", visualize_vim_grid(&vim_emulator, &vim_cursor_info));
        eprintln!("{}", visualize_engine_grid(&test_lines, &engine));
        panic!(
            "Cursor row mismatch for inputs [{}]: vim={}, engine={}",
            input_str, vim_cursor_info.row, engine_cursor_info.row
        );
    }

    if vim_cursor_info.col != engine_cursor_info.col {
        eprintln!("\n{}", visualize_vim_grid(&vim_emulator, &vim_cursor_info));
        eprintln!("{}", visualize_engine_grid(&test_lines, &engine));
        panic!(
            "Cursor column mismatch for inputs [{}]: vim={}, engine={}",
            input_str, vim_cursor_info.col, engine_cursor_info.col
        );
    }

    // Compare selection state
    // Only compare if the engine is in visual mode (actual selection).
    // Vim may show bg_color for other reasons (matchparen highlighting) that
    // aren't true selections.
    let engine_in_visual_mode = engine.mode != libvim::Mode::Normal;

    if engine_in_visual_mode {
        if vim_cursor_info.has_selection != engine_cursor_info.has_selection {
            eprintln!("\n{}", visualize_vim_grid(&vim_emulator, &vim_cursor_info));
            eprintln!("{}", visualize_engine_grid(&test_lines, &engine));
            panic!(
                "Selection state mismatch for inputs [{}]: vim has_selection={}, engine has_selection={}",
                input_str, vim_cursor_info.has_selection, engine_cursor_info.has_selection
            );
        }

        if vim_cursor_info.has_selection {
            // For visual line mode, only compare row ranges since empty lines
            // won't show bg_color and column detection may be off
            let is_visual_line = engine.mode == libvim::Mode::VisualLine;

            if is_visual_line {
                // Compare rows only for visual line mode
                let vim_start_row = vim_cursor_info.selection_start.map(|(r, _)| r);
                let engine_start_row = engine_cursor_info.selection_start.map(|(r, _)| r);
                if vim_start_row != engine_start_row {
                    eprintln!("\n{}", visualize_vim_grid(&vim_emulator, &vim_cursor_info));
                    eprintln!("{}", visualize_engine_grid(&test_lines, &engine));
                    panic!(
                        "Selection start row mismatch for inputs [{}]: vim={:?}, engine={:?}",
                        input_str, vim_start_row, engine_start_row
                    );
                }
                // For visual line mode, the end row comparison needs to account for
                // empty lines not being detected. The engine's end row should be >= vim's.
                let vim_end_row = vim_cursor_info.selection_end.map(|(r, _)| r);
                let engine_end_row = engine_cursor_info.selection_end.map(|(r, _)| r);
                if let (Some(vim_end), Some(engine_end)) = (vim_end_row, engine_end_row) {
                    // Engine end should match or exceed vim (empty trailing lines)
                    if engine_end < vim_end {
                        eprintln!("\n{}", visualize_vim_grid(&vim_emulator, &vim_cursor_info));
                        eprintln!("{}", visualize_engine_grid(&test_lines, &engine));
                        panic!(
                            "Selection end row mismatch for inputs [{}]: vim={}, engine={}",
                            input_str, vim_end, engine_end
                        );
                    }
                }
            } else {
                // Regular visual mode: compare exact positions
                if vim_cursor_info.selection_start != engine_cursor_info.selection_start {
                    eprintln!("\n{}", visualize_vim_grid(&vim_emulator, &vim_cursor_info));
                    eprintln!("{}", visualize_engine_grid(&test_lines, &engine));
                    panic!(
                        "Selection start mismatch for inputs [{}]: vim={:?}, engine={:?}",
                        input_str,
                        vim_cursor_info.selection_start,
                        engine_cursor_info.selection_start
                    );
                }

                if vim_cursor_info.selection_end != engine_cursor_info.selection_end {
                    eprintln!("\n{}", visualize_vim_grid(&vim_emulator, &vim_cursor_info));
                    eprintln!("{}", visualize_engine_grid(&test_lines, &engine));
                    panic!(
                        "Selection end mismatch for inputs [{}]: vim={:?}, engine={:?}",
                        input_str, vim_cursor_info.selection_end, engine_cursor_info.selection_end
                    );
                }
            }
        }
    }
}

#[test]
fn test_vim_cursor_movement_and_selection() {
    // Move down 4 lines, enter visual mode, then move to end of line
    // This should select from col 0 to end of line
    assert_vim_engine_match(&[b"jjjj", b"v$"]);
}

#[test]
fn test_vim_basic_cursor_movement() {
    // Simple movement: down 5 lines, right 3 columns
    assert_vim_engine_match(&[b"5j", b"3l"]);
}

// ==================== Basic Movement Tests ====================

#[test]
fn test_vim_hjkl_movement() {
    // Basic hjkl navigation
    assert_vim_engine_match(&[b"jjj", b"lllll", b"k", b"hh"]);
}

#[test]
fn test_vim_movement_with_counts() {
    // Movement with numeric counts
    assert_vim_engine_match(&[b"10j", b"5l"]);
}

#[test]
fn test_vim_movement_with_paragraph_moves() {
    assert_vim_engine_match(&[b"}}"]);
}

#[test]
fn test_vim_movement_with_paragraph_moves_2() {
    assert_vim_engine_match(&[b"{{"]);
}

#[test]
fn test_vim_selection_inside() {
    // vi( selects inside parentheses
    assert_vim_engine_match(&[b"jfC", b"vi("]);
}

#[test]
fn test_vim_movement_large_count() {
    // Large count that exceeds document bounds should clamp
    assert_vim_engine_match(&[b"999j"]);
}

#[test]
fn test_vim_left_at_start_of_line() {
    // h at start of line should stay at column 0
    assert_vim_engine_match(&[b"j", b"hhhhh"]);
}

#[test]
fn test_vim_up_at_top() {
    // k at top of document should stay at row 0
    assert_vim_engine_match(&[b"kkkkk"]);
}

// ==================== Line Position Tests ====================

#[test]
fn test_vim_start_of_line() {
    // 0 goes to start of line
    assert_vim_engine_match(&[b"jjj", b"lllllllll", b"0"]);
}

#[test]
fn test_vim_end_of_line() {
    // $ goes to end of line
    assert_vim_engine_match(&[b"jjj", b"$"]);
}

#[test]
fn test_vim_first_non_blank() {
    // ^ goes to first non-blank character
    assert_vim_engine_match(&[b"jjjjj", b"^"]);
}

#[test]
fn test_vim_dollar_with_count() {
    // 3$ goes to end of line 2 lines below
    assert_vim_engine_match(&[b"3$"]);
}

// ==================== Word Motion Tests ====================

#[test]
fn test_vim_word_forward() {
    // w moves to start of next word
    assert_vim_engine_match(&[b"w"]);
}

#[test]
fn test_vim_word_forward_multiple() {
    // Multiple w movements
    assert_vim_engine_match(&[b"www"]);
}

#[test]
fn test_vim_word_forward_with_count() {
    // 5w moves forward 5 words
    assert_vim_engine_match(&[b"5w"]);
}

#[test]
fn test_vim_word_backward() {
    // b moves to start of previous word
    assert_vim_engine_match(&[b"5w", b"b"]);
}

#[test]
fn test_vim_word_backward_multiple() {
    // Multiple b movements
    assert_vim_engine_match(&[b"10w", b"bbb"]);
}

#[test]
fn test_vim_word_end() {
    // e moves to end of word
    assert_vim_engine_match(&[b"e"]);
}

#[test]
fn test_vim_word_end_multiple() {
    // Multiple e movements
    assert_vim_engine_match(&[b"eee"]);
}

#[test]
fn test_vim_big_word_forward() {
    // W moves to start of next WORD (non-whitespace)
    assert_vim_engine_match(&[b"jj", b"W"]);
}

#[test]
fn test_vim_big_word_backward() {
    // B moves to start of previous WORD
    assert_vim_engine_match(&[b"jj", b"5W", b"B"]);
}

// ==================== Document Navigation Tests ====================

#[test]
fn test_vim_go_to_top() {
    // gg goes to top of document
    assert_vim_engine_match(&[b"10j", b"gg"]);
}

#[test]
fn test_vim_go_to_bottom() {
    // G goes to bottom of document
    assert_vim_engine_match(&[b"G"]);
}

#[test]
fn test_vim_go_to_line_number() {
    // 5gg goes to line 5
    assert_vim_engine_match(&[b"5gg"]);
}

#[test]
fn test_vim_go_to_line_with_g() {
    // 10G goes to line 10
    assert_vim_engine_match(&[b"10G"]);
}

// ==================== Find Character Tests ====================

#[test]
fn test_vim_find_char_forward() {
    // f finds character forward on line
    assert_vim_engine_match(&[b"jj", b"fe"]);
}

#[test]
fn test_vim_find_char_backward() {
    // F finds character backward on line
    assert_vim_engine_match(&[b"jj", b"$", b"Fc"]);
}

#[test]
fn test_vim_till_char_forward() {
    // t moves till (before) character
    assert_vim_engine_match(&[b"jj", b"te"]);
}

#[test]
fn test_vim_till_char_backward() {
    // T moves till (after) character backward
    assert_vim_engine_match(&[b"jj", b"$", b"Tc"]);
}

#[test]
fn test_vim_find_char_with_count() {
    // 2fe finds second 'e' on line
    assert_vim_engine_match(&[b"jj", b"2fe"]);
}

// ==================== Visual Mode Tests ====================

#[test]
fn test_vim_visual_mode_basic() {
    // v enters visual mode, movement extends selection
    assert_vim_engine_match(&[b"jj", b"v", b"lll"]);
}

#[test]
fn test_vim_visual_mode_word() {
    // Visual mode with word motion
    assert_vim_engine_match(&[b"jj", b"v", b"w"]);
}

#[test]
fn test_vim_cursor_moves() {
    // Visual mode with word motion
    assert_vim_engine_match(&[b"4j", b"fd", b"f "]);
    assert_vim_engine_match(&[b"4j", b"V", b""]);
}

#[test]
fn test_vim_visual_mode_multiple_words() {
    // Visual mode selecting multiple words
    assert_vim_engine_match(&[b"jj", b"v", b"3w"]);
}

#[test]
fn test_vim_visual_mode_backward() {
    // Visual mode with backward movement
    assert_vim_engine_match(&[b"jj", b"$", b"v", b"b"]);
}

#[test]
fn test_vim_visual_mode_multiline() {
    // Visual mode across multiple lines
    assert_vim_engine_match(&[b"jj", b"v", b"jj"]);
}

#[test]
fn test_vim_visual_line_mode() {
    // V enters visual line mode
    assert_vim_engine_match(&[b"jj", b"V", b"j"]);
}

#[test]
fn test_vim_visual_mode_escape() {
    // Escape exits visual mode
    assert_vim_engine_match(&[b"jj", b"v", b"lll", b"\x1b"]);
}

#[test]
fn test_vim_select_inside() {
    assert_vim_engine_match(&[b"72G", b"f7", b"vi`"]);
}

// ==================== Matching Bracket Tests ====================

#[test]
fn test_vim_matching_bracket_paren() {
    // % jumps to matching bracket
    assert_vim_engine_match(&[b"j", b"f(", b"%"]);
}

#[test]
fn test_vim_matching_bracket_brace() {
    // % with curly brace
    assert_vim_engine_match(&[b"jjjj", b"f{", b"%"]);
}

// ==================== Combined Motion Tests ====================

#[test]
fn test_vim_complex_navigation() {
    // Complex navigation sequence
    assert_vim_engine_match(&[b"5j", b"w", b"w", b"$", b"b"]);
}

#[test]
fn test_vim_visual_then_motion() {
    // Visual mode with complex motion
    assert_vim_engine_match(&[b"jjj", b"v", b"2w", b"l"]);
}

#[test]
fn test_vim_navigate_to_end_and_back() {
    // Navigate to end of document and back up
    assert_vim_engine_match(&[b"G", b"5k", b"^"]);
}

// ==================== Edge Case Tests ====================

#[test]
fn test_vim_empty_line_handling() {
    // Navigate to and from empty line (line 3 in fixture is empty after `use` block)
    assert_vim_engine_match(&[b"3j", b"$"]);
}

#[test]
fn test_vim_word_across_lines() {
    // Word motion that crosses line boundary
    assert_vim_engine_match(&[b"$", b"w"]);
}

#[test]
fn test_vim_visual_to_end_of_line() {
    // Visual select to end, then extend further
    assert_vim_engine_match(&[b"jj", b"v", b"$", b"j"]);
}
