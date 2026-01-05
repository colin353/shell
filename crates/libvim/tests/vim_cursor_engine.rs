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
/// - Selection boundaries if any cell has inverse attribute
fn detect_cursor_info(emulator: &TerminalEmulator) -> CursorInfo {
    let grid = emulator.grid();
    let (cursor_col, cursor_row) = (grid.cursor_x, grid.cursor_y);

    // Find all cells with inverse attribute (indicates selection in vim)
    let mut selection_cells = Vec::new();

    for y in 0..grid.rows {
        for x in 0..grid.cols {
            if let Some(cell) =
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| grid.get_cell(x, y))).ok()
            {
                if cell.attrs.inverse {
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

    // Get cursor info from engine
    let engine_cursor_info = CursorInfo {
        row: engine.cursor.row,
        col: engine.cursor.col,
        has_selection: engine.selection_start.row != engine.selection_end.row
            || engine.selection_start.col != engine.selection_end.col,
        selection_start: if engine.selection_start.row != engine.selection_end.row
            || engine.selection_start.col != engine.selection_end.col
        {
            Some((engine.selection_start.row, engine.selection_start.col))
        } else {
            None
        },
        selection_end: if engine.selection_start.row != engine.selection_end.row
            || engine.selection_start.col != engine.selection_end.col
        {
            Some((engine.selection_end.row, engine.selection_end.col))
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
    if vim_cursor_info.has_selection != engine_cursor_info.has_selection {
        eprintln!("\n{}", visualize_vim_grid(&vim_emulator, &vim_cursor_info));
        eprintln!("{}", visualize_engine_grid(&test_lines, &engine));
        panic!(
            "Selection state mismatch for inputs [{}]: vim has_selection={}, engine has_selection={}",
            input_str, vim_cursor_info.has_selection, engine_cursor_info.has_selection
        );
    }

    if vim_cursor_info.has_selection {
        if vim_cursor_info.selection_start != engine_cursor_info.selection_start {
            eprintln!("\n{}", visualize_vim_grid(&vim_emulator, &vim_cursor_info));
            eprintln!("{}", visualize_engine_grid(&test_lines, &engine));
            panic!(
                "Selection start mismatch for inputs [{}]: vim={:?}, engine={:?}",
                input_str, vim_cursor_info.selection_start, engine_cursor_info.selection_start
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

#[test]
fn test_vim_cursor_movement_and_selection() {
    // Move down 4 lines, to end of line, then visual select to end
    assert_vim_engine_match(&[b"jjjj", b"$", b"v$"]);
}

#[test]
fn test_vim_basic_cursor_movement() {
    // Simple movement: down 5 lines, right 3 columns
    assert_vim_engine_match(&[b"5j", b"3l"]);
}
