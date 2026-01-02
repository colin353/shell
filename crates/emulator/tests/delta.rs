//! Integration tests for delta rendering using fixture files
//!
//! These tests load terminal states from fixture files, compute deltas between them,
//! and verify that applying the delta to one state produces the other.

use emulator::{compute_delta, Cell, CellAttributes, Color, TerminalEmulator, TerminalGrid};
use pty::PtyProcess;
use serde::Deserialize;
use std::fs;
use std::thread;
use std::time::Duration;

// Fixture deserialization structures

#[derive(Debug, Deserialize)]
struct Fixture {
    columns: usize,
    lines: usize,
    cells: Vec<Vec<FixtureCell>>,
    #[serde(default)]
    cursor: Option<FixtureCursor>,
    #[serde(default)]
    modes: Option<FixtureModes>,
}

#[derive(Debug, Deserialize)]
struct FixtureCell {
    character: String,
    #[serde(default)]
    flags: FixtureFlags,
    #[serde(default)]
    fg: Option<FixtureColor>,
    #[serde(default)]
    bg: Option<FixtureColor>,
}

#[derive(Debug, Deserialize, Default)]
struct FixtureFlags {
    #[serde(default)]
    inverse: bool,
    #[serde(default)]
    bold: bool,
    #[serde(default)]
    italic: bool,
    #[serde(default)]
    underline: bool,
    #[serde(default)]
    dim: bool,
    #[serde(default)]
    hidden: bool,
    #[serde(default)]
    strikeout: bool,
}

#[derive(Debug, Deserialize)]
struct FixtureColor {
    #[serde(default)]
    semantic: Option<SemanticColor>,
    #[serde(default)]
    rgb: Option<RgbColor>,
}

#[derive(Debug, Deserialize)]
struct SemanticColor {
    #[serde(rename = "type")]
    color_type: String,
    #[serde(default)]
    value: Option<String>,
}

#[derive(Debug, Deserialize)]
struct RgbColor {
    r: u8,
    g: u8,
    b: u8,
}

#[derive(Debug, Deserialize)]
struct FixtureCursor {
    line: usize,
    column: usize,
}

#[derive(Debug, Deserialize, Default)]
struct FixtureModes {
    #[serde(default)]
    show_cursor: bool,
    #[serde(default)]
    line_wrap: bool,
    #[serde(default)]
    origin: bool,
    #[serde(default)]
    alt_screen: bool,
}

/// Load a fixture file and convert it to a TerminalGrid
fn load_fixture_to_grid(name: &str) -> TerminalGrid {
    let fixture_path = format!("{}/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name);
    let content = fs::read_to_string(&fixture_path)
        .unwrap_or_else(|e| panic!("Failed to load fixture {}: {}", fixture_path, e));

    let fixture: Fixture = serde_json::from_str(&content)
        .unwrap_or_else(|e| panic!("Failed to parse fixture {}: {}", fixture_path, e));

    fixture_to_grid(&fixture)
}

/// Convert a Fixture to a TerminalGrid
fn fixture_to_grid(fixture: &Fixture) -> TerminalGrid {
    let mut grid = TerminalGrid::new(fixture.columns, fixture.lines);

    // Set cells
    for (y, row) in fixture.cells.iter().enumerate() {
        for (x, cell) in row.iter().enumerate() {
            if x < fixture.columns && y < fixture.lines {
                let character = cell.character.chars().next().unwrap_or(' ');
                let attrs = fixture_flags_to_attrs(&cell.flags, &cell.fg, &cell.bg);
                let new_cell = Cell::new(character, attrs);
                grid.set_cell(x, y, new_cell);
            }
        }
    }

    // Set cursor position
    if let Some(ref cursor) = fixture.cursor {
        grid.cursor_x = cursor.column;
        grid.cursor_y = cursor.line;
    }

    // Set modes
    if let Some(ref modes) = fixture.modes {
        grid.cursor_visible = modes.show_cursor;
        grid.autowrap = modes.line_wrap;
        grid.origin_mode = modes.origin;
        // Note: alt_screen transition is complex, we just track the flag
        grid.in_alternate_screen = modes.alt_screen;
    }

    grid
}

/// Convert fixture flags to CellAttributes
fn fixture_flags_to_attrs(
    flags: &FixtureFlags,
    fg: &Option<FixtureColor>,
    bg: &Option<FixtureColor>,
) -> CellAttributes {
    let mut attrs = CellAttributes::default();

    attrs.bold = flags.bold;
    attrs.italic = flags.italic;
    attrs.underline = flags.underline;
    attrs.strikethrough = flags.strikeout;
    attrs.dim = flags.dim;
    attrs.inverse = flags.inverse;
    attrs.hidden = flags.hidden;

    // Convert colors - for now we use RGB values if available
    if let Some(ref fg_color) = fg {
        attrs.fg_color = fixture_color_to_color(fg_color);
    }
    if let Some(ref bg_color) = bg {
        attrs.bg_color = fixture_color_to_color(bg_color);
    }

    attrs
}

/// Convert a fixture color to our Color type
fn fixture_color_to_color(fc: &FixtureColor) -> Option<Color> {
    // First try semantic colors
    if let Some(ref semantic) = fc.semantic {
        match semantic.color_type.as_str() {
            "Named" => {
                if let Some(ref value) = semantic.value {
                    return match value.as_str() {
                        "Foreground" | "Background" => None, // Default colors
                        "Black" => Some(Color::Black),
                        "Red" => Some(Color::Red),
                        "Green" => Some(Color::Green),
                        "Yellow" => Some(Color::Yellow),
                        "Blue" => Some(Color::Blue),
                        "Magenta" => Some(Color::Magenta),
                        "Cyan" => Some(Color::Cyan),
                        "White" => Some(Color::White),
                        "BrightBlack" => Some(Color::BrightBlack),
                        "BrightRed" => Some(Color::BrightRed),
                        "BrightGreen" => Some(Color::BrightGreen),
                        "BrightYellow" => Some(Color::BrightYellow),
                        "BrightBlue" => Some(Color::BrightBlue),
                        "BrightMagenta" => Some(Color::BrightMagenta),
                        "BrightCyan" => Some(Color::BrightCyan),
                        "BrightWhite" => Some(Color::BrightWhite),
                        _ => None,
                    };
                }
            }
            _ => {}
        }
    }

    // Fall back to RGB if available
    if let Some(ref rgb) = fc.rgb {
        return Some(Color::Rgb(rgb.r, rgb.g, rgb.b));
    }

    None
}

/// Compare two grids, checking only the aspects that delta rendering handles
fn grids_match(a: &TerminalGrid, b: &TerminalGrid) -> Result<(), String> {
    let cols = a.cols.min(b.cols);
    let rows = a.rows.min(b.rows);

    // Compare cells
    for y in 0..rows {
        for x in 0..cols {
            let cell_a = a.get_cell(x, y);
            let cell_b = b.get_cell(x, y);

            if cell_a.character != cell_b.character {
                return Err(format!(
                    "Cell ({}, {}) character mismatch: expected {:?}, got {:?}",
                    x, y, cell_b.character, cell_a.character
                ));
            }

            if cell_a.attrs != cell_b.attrs {
                return Err(format!(
                    "Cell ({}, {}) attrs mismatch:\n  expected: {:?}\n  got: {:?}",
                    x, y, cell_b.attrs, cell_a.attrs
                ));
            }
        }
    }

    // Compare cursor position
    if a.cursor_x != b.cursor_x || a.cursor_y != b.cursor_y {
        return Err(format!(
            "Cursor position mismatch: expected ({}, {}), got ({}, {})",
            b.cursor_x, b.cursor_y, a.cursor_x, a.cursor_y
        ));
    }

    // Compare cursor visibility
    if a.cursor_visible != b.cursor_visible {
        return Err(format!(
            "Cursor visibility mismatch: expected {}, got {}",
            b.cursor_visible, a.cursor_visible
        ));
    }

    // Compare modes
    if a.autowrap != b.autowrap {
        return Err(format!(
            "Autowrap mismatch: expected {}, got {}",
            b.autowrap, a.autowrap
        ));
    }

    if a.origin_mode != b.origin_mode {
        return Err(format!(
            "Origin mode mismatch: expected {}, got {}",
            b.origin_mode, a.origin_mode
        ));
    }

    Ok(())
}

/// Test helper that:
/// 1. Loads initial and final states from fixtures
/// 2. Computes the delta
/// 3. Creates an emulator at the initial state
/// 4. Applies the delta
/// 5. Verifies the result matches the final state
fn test_delta_transition(initial_fixture: &str, final_fixture: &str) {
    let initial_grid = load_fixture_to_grid(initial_fixture);
    let final_grid = load_fixture_to_grid(final_fixture);

    // Compute the delta
    let delta = compute_delta(&initial_grid, &final_grid);

    // Create an emulator with the initial state
    let mut emu = TerminalEmulator::new(initial_grid.cols, initial_grid.rows);

    // Copy initial grid state into the emulator
    for y in 0..initial_grid.rows {
        for x in 0..initial_grid.cols {
            let cell = initial_grid.get_cell(x, y).clone();
            emu.grid_mut().set_cell(x, y, cell);
        }
    }
    emu.grid_mut().cursor_x = initial_grid.cursor_x;
    emu.grid_mut().cursor_y = initial_grid.cursor_y;
    emu.grid_mut().cursor_visible = initial_grid.cursor_visible;
    emu.grid_mut().autowrap = initial_grid.autowrap;
    emu.grid_mut().origin_mode = initial_grid.origin_mode;

    // Apply the delta
    emu.process(&delta);

    // Verify the result matches the expected final state
    if let Err(msg) = grids_match(emu.grid(), &final_grid) {
        panic!(
            "Delta transition from {} to {} failed:\n{}\n\nDelta ({} bytes): {:?}",
            initial_fixture,
            final_fixture,
            msg,
            delta.len(),
            String::from_utf8_lossy(&delta)
        );
    }
}

// Test transitions between fixture files

#[test]
fn test_delta_vttest_attrs_0_to_1() {
    test_delta_transition("vttest-attributes-0.json", "vttest-attributes-1.json");
}

#[test]
fn test_delta_vttest_attrs_1_to_0() {
    test_delta_transition("vttest-attributes-1.json", "vttest-attributes-0.json");
}

#[test]
fn test_delta_vttest_attrs_0_to_2() {
    test_delta_transition("vttest-attributes-0.json", "vttest-attributes-2.json");
}

#[test]
fn test_delta_vttest_attrs_2_to_0() {
    test_delta_transition("vttest-attributes-2.json", "vttest-attributes-0.json");
}

#[test]
fn test_delta_identity() {
    // Applying delta from a state to itself should result in same state
    let grid = load_fixture_to_grid("vttest-attributes-0.json");

    let delta = compute_delta(&grid, &grid);

    // Create an emulator with the state
    let mut emu = TerminalEmulator::new(grid.cols, grid.rows);
    for y in 0..grid.rows {
        for x in 0..grid.cols {
            let cell = grid.get_cell(x, y).clone();
            emu.grid_mut().set_cell(x, y, cell);
        }
    }
    emu.grid_mut().cursor_x = grid.cursor_x;
    emu.grid_mut().cursor_y = grid.cursor_y;
    emu.grid_mut().cursor_visible = grid.cursor_visible;
    emu.grid_mut().autowrap = grid.autowrap;
    emu.grid_mut().origin_mode = grid.origin_mode;

    // Apply the delta
    emu.process(&delta);

    // Verify it still matches
    if let Err(msg) = grids_match(emu.grid(), &grid) {
        panic!("Identity delta changed the grid: {}", msg);
    }
}

// ============================================================================
// PTY-based delta tests
//
// These tests run actual terminal programs and validate that the delta logic
// correctly produces escape sequences that transition between states.
// ============================================================================

/// Check if a command is available on the system
fn command_available(command: &str) -> bool {
    std::process::Command::new("which")
        .arg(command)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Clone the state of a TerminalGrid into a new one
fn clone_grid_state(source: &TerminalGrid) -> TerminalGrid {
    let mut dest = TerminalGrid::new(source.cols, source.rows);

    for y in 0..source.rows {
        for x in 0..source.cols {
            let cell = source.get_cell(x, y).clone();
            dest.set_cell(x, y, cell);
        }
    }

    dest.cursor_x = source.cursor_x;
    dest.cursor_y = source.cursor_y;
    dest.cursor_visible = source.cursor_visible;
    dest.autowrap = source.autowrap;
    dest.origin_mode = source.origin_mode;
    dest.in_alternate_screen = source.in_alternate_screen;
    dest.scroll_top = source.scroll_top;
    dest.scroll_bottom = source.scroll_bottom;
    dest.charset_g0 = source.charset_g0;
    dest.charset_g1 = source.charset_g1;
    dest.gl_is_g1 = source.gl_is_g1;

    dest
}

/// Test helper for PTY-based delta tests:
/// 1. Runs a command with initial_inputs to get initial state
/// 2. Continues with additional_inputs to get final state
/// 3. Computes the delta from initial to final
/// 4. Applies the delta to a fresh emulator at initial state
/// 5. Verifies the result matches the final state
fn test_pty_delta_transition(
    command: &str,
    cols: u16,
    rows: u16,
    initial_inputs: &[&[u8]],
    additional_inputs: &[&[u8]],
) {
    test_pty_delta_transition_debug(
        command,
        cols,
        rows,
        initial_inputs,
        additional_inputs,
        false,
    )
}

fn test_pty_delta_transition_debug(
    command: &str,
    cols: u16,
    rows: u16,
    initial_inputs: &[&[u8]],
    additional_inputs: &[&[u8]],
    debug: bool,
) {
    let pty = PtyProcess::spawn(command, cols, rows)
        .unwrap_or_else(|e| panic!("Failed to spawn {}: {}", command, e));

    let mut emulator = TerminalEmulator::new(cols as usize, rows as usize);
    let mut buf = [0u8; 8192];

    let drain_pty =
        |pty: &PtyProcess, emulator: &mut TerminalEmulator, buf: &mut [u8], debug: bool| loop {
            match pty.read(buf) {
                Ok(Some(n)) if n > 0 => {
                    if debug {
                        eprintln!("=== Received {} bytes ===", n);
                        for byte in &buf[..n] {
                            if *byte == 0x1b {
                                eprint!("ESC");
                            } else if *byte >= 0x20 && *byte < 0x7f {
                                eprint!("{}", *byte as char);
                            } else {
                                eprint!("[{:02x}]", byte);
                            }
                        }
                        eprintln!();
                    }
                    emulator.process(&buf[..n]);
                    for response in emulator.drain_responses() {
                        if debug {
                            eprintln!("=== Sending response: {:?} ===", response);
                        }
                        let _ = pty.write(&response);
                    }
                }
                _ => break,
            }
        };

    let wait_and_drain = |pty: &PtyProcess,
                          emulator: &mut TerminalEmulator,
                          buf: &mut [u8],
                          iterations: usize,
                          debug: bool| {
        for _ in 0..iterations {
            thread::sleep(Duration::from_millis(20));
            drain_pty(pty, emulator, buf, debug);
        }
    };

    // Wait for program to start
    wait_and_drain(&pty, &mut emulator, &mut buf, 30, debug);

    // Send initial inputs
    for input in initial_inputs {
        pty.write(input).expect("Failed to write to pty");
        wait_and_drain(&pty, &mut emulator, &mut buf, 20, debug);
    }

    // Capture the initial state
    let initial_grid = clone_grid_state(emulator.grid());

    if debug {
        eprintln!("=== Initial state captured ===");
        for y in 0..rows.min(10) as usize {
            eprintln!("  {}", emulator.grid().get_line_text(y));
        }
    }

    // Send additional inputs to get final state
    for input in additional_inputs {
        pty.write(input).expect("Failed to write to pty");
        wait_and_drain(&pty, &mut emulator, &mut buf, 20, debug);
    }

    let final_grid = emulator.grid();

    if debug {
        eprintln!("=== Final state captured ===");
        for y in 0..rows.min(10) as usize {
            eprintln!("  {}", final_grid.get_line_text(y));
        }
    }

    // Compute the delta
    let delta = compute_delta(&initial_grid, final_grid);

    if debug {
        eprintln!("=== Delta ({} bytes) ===", delta.len());
        eprintln!("{:?}", String::from_utf8_lossy(&delta));
    }

    // Create a fresh emulator at the initial state
    let mut test_emu = TerminalEmulator::new(cols as usize, rows as usize);
    for y in 0..initial_grid.rows {
        for x in 0..initial_grid.cols {
            let cell = initial_grid.get_cell(x, y).clone();
            test_emu.grid_mut().set_cell(x, y, cell);
        }
    }
    test_emu.grid_mut().cursor_x = initial_grid.cursor_x;
    test_emu.grid_mut().cursor_y = initial_grid.cursor_y;
    test_emu.grid_mut().cursor_visible = initial_grid.cursor_visible;
    test_emu.grid_mut().autowrap = initial_grid.autowrap;
    test_emu.grid_mut().origin_mode = initial_grid.origin_mode;
    test_emu.grid_mut().in_alternate_screen = initial_grid.in_alternate_screen;
    test_emu.grid_mut().scroll_top = initial_grid.scroll_top;
    test_emu.grid_mut().scroll_bottom = initial_grid.scroll_bottom;
    test_emu.grid_mut().charset_g0 = initial_grid.charset_g0;
    test_emu.grid_mut().charset_g1 = initial_grid.charset_g1;
    test_emu.grid_mut().gl_is_g1 = initial_grid.gl_is_g1;

    // Apply the delta
    test_emu.process(&delta);

    // Verify the result matches the expected final state
    if let Err(msg) = grids_match(test_emu.grid(), final_grid) {
        panic!(
            "PTY delta transition failed:\n{}\n\nDelta ({} bytes): {:?}\n\nCommand: {}\nInitial inputs: {:?}\nAdditional inputs: {:?}",
            msg,
            delta.len(),
            String::from_utf8_lossy(&delta),
            command,
            initial_inputs,
            additional_inputs
        );
    }
}

// ============================================================================
// Vim delta tests - specifically testing the Ctrl+D scroll scenario
// ============================================================================

#[test]
fn test_delta_vim_ctrl_d() {
    if !command_available("vim") {
        eprintln!("Skipping test: vim not available");
        return;
    }

    // This test replicates the bug scenario:
    // 1. Open vim with a file
    // 2. Capture initial state
    // 3. Send Ctrl+D to scroll down
    // 4. Verify delta correctly transitions between states
    let command = format!(
        "bash -c \"cd {}/../.. && vim -n crates/emulator/fixtures/terminal.rs\"",
        env!("CARGO_MANIFEST_DIR")
    );

    test_pty_delta_transition(
        &command,
        84,
        31,
        &[],        // initial: just vim startup
        &[b"\x04"], // additional: Ctrl+D
    );
}

#[test]
fn test_delta_vim_ctrl_d_multiple() {
    if !command_available("vim") {
        eprintln!("Skipping test: vim not available");
        return;
    }

    // Test multiple Ctrl+D presses
    let command = format!(
        "bash -c \"cd {}/../.. && vim -n crates/emulator/fixtures/terminal.rs\"",
        env!("CARGO_MANIFEST_DIR")
    );

    test_pty_delta_transition(
        &command,
        84,
        31,
        &[b"\x04"], // initial: vim + one Ctrl+D
        &[b"\x04"], // additional: another Ctrl+D
    );
}

#[test]
fn test_delta_vim_ctrl_u() {
    if !command_available("vim") {
        eprintln!("Skipping test: vim not available");
        return;
    }

    // Test Ctrl+U (scroll up) after Ctrl+D
    let command = format!(
        "bash -c \"cd {}/../.. && vim -n crates/emulator/fixtures/terminal.rs\"",
        env!("CARGO_MANIFEST_DIR")
    );

    test_pty_delta_transition(
        &command,
        84,
        31,
        &[b"\x04"], // initial: vim + Ctrl+D
        &[b"\x15"], // additional: Ctrl+U
    );
}

#[test]
fn test_delta_vim_page_down() {
    if !command_available("vim") {
        eprintln!("Skipping test: vim not available");
        return;
    }

    // Test Page Down (Ctrl+F)
    let command = format!(
        "bash -c \"cd {}/../.. && vim -n crates/emulator/fixtures/terminal.rs\"",
        env!("CARGO_MANIFEST_DIR")
    );

    test_pty_delta_transition(
        &command,
        84,
        31,
        &[],        // initial: just vim
        &[b"\x06"], // additional: Ctrl+F (page down)
    );
}

#[test]
fn test_delta_vim_j_movement() {
    if !command_available("vim") {
        eprintln!("Skipping test: vim not available");
        return;
    }

    // Test simple j (down) movement
    let command = format!(
        "bash -c \"cd {}/../.. && vim -n crates/emulator/fixtures/terminal.rs\"",
        env!("CARGO_MANIFEST_DIR")
    );

    test_pty_delta_transition(
        &command,
        84,
        31,
        &[],         // initial: just vim
        &[b"jjjjj"], // additional: move down 5 lines
    );
}

#[test]
fn test_delta_vim_search() {
    if !command_available("vim") {
        eprintln!("Skipping test: vim not available");
        return;
    }

    // Test search highlighting
    let command = format!(
        "bash -c \"cd {}/../.. && vim -n crates/emulator/fixtures/terminal.rs\"",
        env!("CARGO_MANIFEST_DIR")
    );

    test_pty_delta_transition(
        &command,
        84,
        31,
        &[],          // initial: just vim
        &[b"/pub\n"], // additional: search for "pub"
    );
}

#[test]
fn test_delta_vim_insert_mode() {
    if !command_available("vim") {
        eprintln!("Skipping test: vim not available");
        return;
    }

    // Test entering and exiting insert mode
    let command = format!(
        "bash -c \"cd {}/../.. && vim -n crates/emulator/fixtures/terminal.rs\"",
        env!("CARGO_MANIFEST_DIR")
    );

    test_pty_delta_transition(
        &command,
        84,
        31,
        &[b"i"],    // initial: enter insert mode
        &[b"\x1b"], // additional: exit insert mode
    );
}

// ============================================================================
// Simple terminal delta tests
// ============================================================================

#[test]
fn test_delta_echo_simple() {
    // Simple test: echo some text, then echo more
    test_pty_delta_transition(
        "bash",
        80,
        24,
        &[b"echo hello\n"], // initial
        &[b"echo world\n"], // additional
    );
}

#[test]
fn test_delta_clear_screen() {
    // Test clear screen transition
    test_pty_delta_transition(
        "bash",
        80,
        24,
        &[b"echo line1\necho line2\n"], // initial
        &[b"clear\n"],                  // additional: clear screen
    );
}

// ============================================================================
// Debug test - run with RUST_TEST_THREADS=1 cargo test test_delta_vim_ctrl_d_debug -- --nocapture
// to see detailed output
// ============================================================================

#[test]
#[ignore] // Run manually with --ignored flag for debugging
fn test_delta_vim_ctrl_d_debug() {
    if !command_available("vim") {
        eprintln!("Skipping test: vim not available");
        return;
    }

    let command = format!(
        "bash -c \"cd {}/../.. && vim -n crates/emulator/fixtures/terminal.rs\"",
        env!("CARGO_MANIFEST_DIR")
    );

    test_pty_delta_transition_debug(
        &command,
        84,
        31,
        &[],        // initial: just vim startup
        &[b"\x04"], // additional: Ctrl+D
        true,       // enable debug output
    );
}

/// More detailed compositor flow test with debug output
#[test]
#[ignore]
fn test_compositor_delta_flow_debug() {
    if !command_available("vim") {
        eprintln!("Skipping test: vim not available");
        return;
    }

    let cols = 84;
    let rows = 31;

    let command = format!(
        "bash -c \"cd {}/../.. && vim -n crates/emulator/fixtures/terminal.rs\"",
        env!("CARGO_MANIFEST_DIR")
    );

    let pty = PtyProcess::spawn(&command, cols as u16, rows as u16).expect("Failed to spawn vim");

    let mut pane_emulator = TerminalEmulator::new(cols, rows);
    let mut real_terminal = TerminalEmulator::new(cols, rows);
    let mut prev_frame = TerminalGrid::new(cols, rows);

    let mut buf = [0u8; 8192];

    let drain_and_process = |pty: &PtyProcess, pane_emu: &mut TerminalEmulator, buf: &mut [u8]| loop {
        match pty.read(buf) {
            Ok(Some(n)) if n > 0 => {
                pane_emu.process(&buf[..n]);
                for response in pane_emu.drain_responses() {
                    let _ = pty.write(&response);
                }
            }
            _ => break,
        }
    };

    let render_and_apply = |pane_emu: &TerminalEmulator,
                            prev_frame: &mut TerminalGrid,
                            real_terminal: &mut TerminalEmulator,
                            debug: bool|
     -> Vec<u8> {
        let mut global_emulator = TerminalEmulator::new(cols, rows);
        global_emulator.blit_from(pane_emu, 0, 0, 0, 0, cols, rows);

        {
            let (cx, cy) = pane_emu.cursor_position();
            let pane_grid = pane_emu.grid();
            let global_grid = global_emulator.grid_mut();
            global_grid.cursor_x = cx;
            global_grid.cursor_y = cy;
            global_grid.cursor_visible = pane_grid.cursor_visible;
        }

        let delta = compute_delta(prev_frame, global_emulator.grid());

        if debug {
            eprintln!("\n=== Delta ({} bytes) ===", delta.len());
            // Print readable delta
            for byte in &delta {
                if *byte == 0x1b {
                    eprint!("ESC");
                } else if *byte >= 0x20 && *byte < 0x7f {
                    eprint!("{}", *byte as char);
                } else {
                    eprint!("[{:02x}]", byte);
                }
            }
            eprintln!();
        }

        real_terminal.process(&delta);
        *prev_frame = global_emulator.grid().clone();

        delta
    };

    // Wait for vim to start
    for _ in 0..30 {
        thread::sleep(Duration::from_millis(20));
        drain_and_process(&pty, &mut pane_emulator, &mut buf);
    }

    eprintln!("=== After vim startup ===");
    for y in 0..10 {
        eprintln!("  Line {}: {}", y, pane_emulator.grid().get_line_text(y));
    }

    // Initial render
    let _ = render_and_apply(&pane_emulator, &mut prev_frame, &mut real_terminal, true);

    // Send Ctrl+D
    eprintln!("\n=== Sending Ctrl+D ===");
    pty.write(b"\x04").expect("Failed to write Ctrl+D");

    for _ in 0..20 {
        thread::sleep(Duration::from_millis(20));
        drain_and_process(&pty, &mut pane_emulator, &mut buf);
    }

    eprintln!("\n=== After Ctrl+D ===");
    for y in 0..10 {
        eprintln!("  Line {}: {}", y, pane_emulator.grid().get_line_text(y));
    }

    // Render after Ctrl+D
    let _delta2 = render_and_apply(&pane_emulator, &mut prev_frame, &mut real_terminal, true);

    // Check result
    if let Err(msg) = grids_match(real_terminal.grid(), pane_emulator.grid()) {
        eprintln!("\n=== MISMATCH DETECTED ===");
        eprintln!("{}", msg);

        eprintln!("\n=== Real terminal lines ===");
        for y in 0..10 {
            eprintln!("  Line {}: {}", y, real_terminal.grid().get_line_text(y));
        }

        panic!("Delta flow mismatch: {}", msg);
    }

    eprintln!("\n=== Real terminal matches pane emulator ===");
}

/// Print a visual diff of two grids for debugging
#[allow(dead_code)]
fn print_grid_diff(a: &TerminalGrid, b: &TerminalGrid, label_a: &str, label_b: &str) {
    eprintln!("\n=== Grid comparison: {} vs {} ===", label_a, label_b);
    eprintln!("Dimensions: {}x{} vs {}x{}", a.cols, a.rows, b.cols, b.rows);
    eprintln!(
        "Cursor: ({},{}) vs ({},{})",
        a.cursor_x, a.cursor_y, b.cursor_x, b.cursor_y
    );
    eprintln!(
        "Cursor visible: {} vs {}",
        a.cursor_visible, b.cursor_visible
    );
    eprintln!(
        "Scroll region: ({},{}) vs ({},{})",
        a.scroll_top, a.scroll_bottom, b.scroll_top, b.scroll_bottom
    );
    eprintln!();

    let rows = a.rows.min(b.rows);
    for y in 0..rows {
        let line_a = a.get_line_text(y);
        let line_b = b.get_line_text(y);
        if line_a != line_b {
            eprintln!("Line {} differs:", y);
            eprintln!("  {}: {:?}", label_a, line_a);
            eprintln!("  {}: {:?}", label_b, line_b);
        }
    }
}

// ============================================================================
// Tests for scroll region handling in delta
// These test the theory that scroll regions affect delta correctness
// ============================================================================

/// This test verifies behavior when delta is applied to a terminal with
/// a pre-existing scroll region (simulating the real terminal state)
#[test]
fn test_delta_with_scroll_region_mismatch() {
    // Create initial state (what compositor thinks terminal looks like)
    let mut prev_grid = TerminalGrid::new(80, 24);
    for y in 0..24 {
        for x in 0..80 {
            prev_grid.set_cell(
                x,
                y,
                Cell::new(((y % 26) as u8 + b'A') as char, CellAttributes::default()),
            );
        }
    }

    // Create next state (what compositor wants terminal to look like)
    // Simulate vim scrolling: lines shift up
    let mut next_grid = TerminalGrid::new(80, 24);
    for y in 0..24 {
        let shifted_y = (y + 12) % 24; // Lines shifted
        for x in 0..80 {
            next_grid.set_cell(
                x,
                y,
                Cell::new(
                    ((shifted_y % 26) as u8 + b'A') as char,
                    CellAttributes::default(),
                ),
            );
        }
    }

    // Compute delta (compositor assumes default scroll regions)
    let delta = compute_delta(&prev_grid, &next_grid);

    // Now simulate what happens when delta is applied to a REAL terminal
    // that has a scroll region set (e.g., from vim)
    let mut real_terminal = TerminalEmulator::new(80, 24);

    // Set up real terminal to match prev_grid
    for y in 0..24 {
        for x in 0..80 {
            let cell = prev_grid.get_cell(x, y).clone();
            real_terminal.grid_mut().set_cell(x, y, cell);
        }
    }

    // CRITICAL: Set a scroll region on the real terminal (simulating vim's effect)
    // This is the state the real terminal has, but compositor doesn't know about
    real_terminal.grid_mut().scroll_top = 1;
    real_terminal.grid_mut().scroll_bottom = 22;

    // Apply the delta
    real_terminal.process(&delta);

    // Check if result matches expected
    if let Err(msg) = grids_match(real_terminal.grid(), &next_grid) {
        eprintln!("SCROLL REGION MISMATCH BUG REPRODUCED!");
        eprintln!("Delta applied to terminal with scroll region doesn't match expected.");
        eprintln!("{}", msg);
        eprintln!(
            "\nDelta ({} bytes): {:?}",
            delta.len(),
            String::from_utf8_lossy(&delta)
        );

        // This test documents the bug - when fixed, remove the expect below
        // For now, we expect this to fail if scroll regions affect the output
        panic!("Scroll region mismatch caused incorrect rendering: {}", msg);
    }
}

/// Test that delta explicitly resets scroll regions to ensure consistent state
#[test]
fn test_delta_resets_scroll_region() {
    let prev_grid = TerminalGrid::new(80, 24);
    let next_grid = TerminalGrid::new(80, 24);

    let delta = compute_delta(&prev_grid, &next_grid);

    // The delta should probably reset scroll region to ensure known state
    // Check if it contains DECSTBM reset sequence \x1b[r or \x1b[1;24r
    let delta_str = String::from_utf8_lossy(&delta);

    // This test documents expected behavior - update based on design decision
    eprintln!("Delta for identity: {:?}", delta_str);
}

/// This test simulates the compositor's rendering flow more precisely:
/// 1. Pane emulator processes vim output
/// 2. Cells are blitted to a fresh global_emulator
/// 3. Delta is computed from prev_frame to global_emulator
/// 4. Delta is applied to a "real terminal" emulator
///
/// The key difference from other tests: we simulate what the compositor does
/// where it creates a FRESH global_emulator each frame and only blits cells.
#[test]
fn test_compositor_delta_flow() {
    if !command_available("vim") {
        eprintln!("Skipping test: vim not available");
        return;
    }

    let cols = 84;
    let rows = 31;

    // This simulates what the compositor does
    let command = format!(
        "bash -c \"cd {}/../.. && vim -n crates/emulator/fixtures/terminal.rs\"",
        env!("CARGO_MANIFEST_DIR")
    );

    let pty = PtyProcess::spawn(&command, cols as u16, rows as u16).expect("Failed to spawn vim");

    // The pane's terminal emulator (like in compositor)
    let mut pane_emulator = TerminalEmulator::new(cols, rows);

    // The "real terminal" - what actually displays on screen
    // This receives delta output
    let mut real_terminal = TerminalEmulator::new(cols, rows);

    // Previous frame for delta computation
    let mut prev_frame = TerminalGrid::new(cols, rows);

    let mut buf = [0u8; 8192];

    let drain_and_process = |pty: &PtyProcess, pane_emu: &mut TerminalEmulator, buf: &mut [u8]| loop {
        match pty.read(buf) {
            Ok(Some(n)) if n > 0 => {
                pane_emu.process(&buf[..n]);
                for response in pane_emu.drain_responses() {
                    let _ = pty.write(&response);
                }
            }
            _ => break,
        }
    };

    // Simulate compositor's render cycle
    let render_and_apply = |pane_emu: &TerminalEmulator,
                            prev_frame: &mut TerminalGrid,
                            real_terminal: &mut TerminalEmulator| {
        // Create fresh global emulator (like compositor does)
        let mut global_emulator = TerminalEmulator::new(cols, rows);

        // Blit pane cells into global emulator
        global_emulator.blit_from(pane_emu, 0, 0, 0, 0, cols, rows);

        // Copy cursor info (like compositor does)
        {
            let (cx, cy) = pane_emu.cursor_position();
            let pane_grid = pane_emu.grid();
            let global_grid = global_emulator.grid_mut();
            global_grid.cursor_x = cx;
            global_grid.cursor_y = cy;
            global_grid.cursor_visible = pane_grid.cursor_visible;
        }

        // Compute delta from prev_frame to global_emulator
        let delta = compute_delta(prev_frame, global_emulator.grid());

        // Apply delta to real terminal
        real_terminal.process(&delta);

        // Save current frame as prev_frame
        *prev_frame = global_emulator.grid().clone();

        delta
    };

    // Wait for vim to start
    for _ in 0..30 {
        thread::sleep(Duration::from_millis(20));
        drain_and_process(&pty, &mut pane_emulator, &mut buf);
    }

    // Initial render
    let _delta1 = render_and_apply(&pane_emulator, &mut prev_frame, &mut real_terminal);

    // Verify initial state matches
    if let Err(msg) = grids_match(real_terminal.grid(), pane_emulator.grid()) {
        panic!("Initial render mismatch: {}", msg);
    }

    // Send Ctrl+D
    pty.write(b"\x04").expect("Failed to write Ctrl+D");

    // Wait for vim to process
    for _ in 0..20 {
        thread::sleep(Duration::from_millis(20));
        drain_and_process(&pty, &mut pane_emulator, &mut buf);
    }

    // Render after Ctrl+D
    let delta2 = render_and_apply(&pane_emulator, &mut prev_frame, &mut real_terminal);

    // Verify real terminal matches pane emulator
    if let Err(msg) = grids_match(real_terminal.grid(), pane_emulator.grid()) {
        eprintln!("BUG REPRODUCED: After Ctrl+D, real terminal doesn't match pane emulator!");
        eprintln!("{}", msg);
        eprintln!(
            "\nDelta ({} bytes): {:?}",
            delta2.len(),
            String::from_utf8_lossy(&delta2)
        );

        // Print some diagnostic info
        eprintln!(
            "\nPane emulator scroll region: ({}, {})",
            pane_emulator.grid().scroll_top,
            pane_emulator.grid().scroll_bottom
        );
        eprintln!(
            "Real terminal scroll region: ({}, {})",
            real_terminal.grid().scroll_top,
            real_terminal.grid().scroll_bottom
        );

        panic!("Compositor delta flow failed after Ctrl+D: {}", msg);
    }
}
