//! Integration tests for delta rendering using fixture files
//!
//! These tests load terminal states from fixture files, compute deltas between them,
//! and verify that applying the delta to one state produces the other.

use emulator::{compute_delta, Cell, CellAttributes, Color, TerminalEmulator, TerminalGrid};
use serde::Deserialize;
use std::fs;

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
