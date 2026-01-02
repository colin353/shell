//! Integration tests for terminal emulator using vttest.
//!
//! These tests spawn the `vttest` program and verify that the emulator
//! correctly renders the various test screens.

use emulator::TerminalEmulator;
use pty::PtyProcess;
use serde::Deserialize;
use std::thread;
use std::time::Duration;

/// Check if vttest is available on the system
fn vttest_available() -> bool {
    std::process::Command::new("which")
        .arg("vttest")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Run vttest with given input sequences and return the emulator state.
/// Each input in the slice is sent separately with a wait period in between.
fn run_vttest_test(cols: u16, rows: u16, inputs: &[&[u8]]) -> TerminalEmulator {
    run_vttest_test_debug(cols, rows, inputs, false)
}

fn run_vttest_test_debug(cols: u16, rows: u16, inputs: &[&[u8]], debug: bool) -> TerminalEmulator {
    let pty = PtyProcess::spawn("vttest", cols, rows).expect("Failed to spawn vttest");

    // Create emulator
    let mut emulator = TerminalEmulator::new(cols as usize, rows as usize);
    let mut buf = [0u8; 8192];

    // Helper to drain all available PTY output and send responses back
    let drain_pty =
        |pty: &PtyProcess, emulator: &mut TerminalEmulator, buf: &mut [u8], debug: bool| loop {
            match pty.read(buf) {
                Ok(Some(n)) if n > 0 => {
                    if debug {
                        // Log the raw bytes in a readable format
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

                    // Send any responses back to the PTY (e.g., DSR responses)
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

    // Helper to wait and drain
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

    // Wait for vttest to start and show the main menu
    wait_and_drain(&pty, &mut emulator, &mut buf, 30, debug);

    // Send each input sequence with a wait period between
    for input in inputs {
        pty.write(input).expect("Failed to write to vttest");
        wait_and_drain(&pty, &mut emulator, &mut buf, 20, debug);
    }

    emulator
}

/// Extract text content from emulator grid as a vector of lines
fn grid_to_lines(emulator: &TerminalEmulator, rows: usize) -> Vec<String> {
    (0..rows)
        .map(|y| emulator.grid().get_line_text(y))
        .collect()
}

/// Load fixture file and return as vector of lines
fn load_fixture(name: &str) -> Vec<String> {
    let fixture_path = format!("{}/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name);
    let content = std::fs::read_to_string(&fixture_path)
        .unwrap_or_else(|e| panic!("Failed to load fixture {}: {}", fixture_path, e));

    content.lines().map(|s| s.to_string()).collect()
}

// Structs for deserializing the JSON attributes fixture
#[derive(Debug, Deserialize)]
struct AttributesFixture {
    columns: usize,
    lines: usize,
    cells: Vec<Vec<FixtureCell>>,
    #[serde(default)]
    cursor: Option<FixtureCursor>,
    #[serde(default)]
    modes: Option<FixtureModes>,
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

#[derive(Debug, Deserialize)]
struct FixtureCell {
    character: String,
    flags: FixtureFlags,
}

#[derive(Debug, Deserialize)]
struct FixtureFlags {
    inverse: bool,
    bold: bool,
    italic: bool,
    underline: bool,
    dim: bool,
    hidden: bool,
    strikeout: bool,
}

/// Load a JSON attributes fixture file
fn load_attributes_fixture(name: &str) -> AttributesFixture {
    let fixture_path = format!("{}/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name);
    let content = std::fs::read_to_string(&fixture_path)
        .unwrap_or_else(|e| panic!("Failed to load fixture {}: {}", fixture_path, e));

    serde_json::from_str(&content)
        .unwrap_or_else(|e| panic!("Failed to parse JSON fixture {}: {}", fixture_path, e))
}

/// Compare grid cell attributes with JSON fixture, providing detailed diff on failure
fn assert_grid_matches_attributes_fixture(
    emulator: &TerminalEmulator,
    fixture_name: &str,
    rows: usize,
) {
    let fixture = load_attributes_fixture(fixture_name);
    let grid = emulator.grid();

    let cols = grid.cols.min(fixture.columns);
    let rows = rows.min(fixture.lines);

    let mut mismatches = Vec::new();

    for y in 0..rows {
        for x in 0..cols {
            let actual = grid.get_cell(x, y);
            let expected = &fixture.cells[y][x];

            // Compare character
            let expected_char = expected.character.chars().next().unwrap_or(' ');
            let char_matches = actual.character == expected_char;

            // Compare flags
            let flags_match = actual.attrs.bold == expected.flags.bold
                && actual.attrs.italic == expected.flags.italic
                && actual.attrs.underline == expected.flags.underline
                && actual.attrs.inverse == expected.flags.inverse
                && actual.attrs.dim == expected.flags.dim
                && actual.attrs.hidden == expected.flags.hidden
                && actual.attrs.strikethrough == expected.flags.strikeout;

            if !char_matches || !flags_match {
                let mut diffs = Vec::new();

                if !char_matches {
                    diffs.push(format!(
                        "char: expected {:?}, got {:?}",
                        expected_char, actual.character
                    ));
                }
                if actual.attrs.bold != expected.flags.bold {
                    diffs.push(format!(
                        "bold: expected {}, got {}",
                        expected.flags.bold, actual.attrs.bold
                    ));
                }
                if actual.attrs.italic != expected.flags.italic {
                    diffs.push(format!(
                        "italic: expected {}, got {}",
                        expected.flags.italic, actual.attrs.italic
                    ));
                }
                if actual.attrs.underline != expected.flags.underline {
                    diffs.push(format!(
                        "underline: expected {}, got {}",
                        expected.flags.underline, actual.attrs.underline
                    ));
                }
                if actual.attrs.inverse != expected.flags.inverse {
                    diffs.push(format!(
                        "inverse: expected {}, got {}",
                        expected.flags.inverse, actual.attrs.inverse
                    ));
                }
                if actual.attrs.dim != expected.flags.dim {
                    diffs.push(format!(
                        "dim: expected {}, got {}",
                        expected.flags.dim, actual.attrs.dim
                    ));
                }
                if actual.attrs.hidden != expected.flags.hidden {
                    diffs.push(format!(
                        "hidden: expected {}, got {}",
                        expected.flags.hidden, actual.attrs.hidden
                    ));
                }
                if actual.attrs.strikethrough != expected.flags.strikeout {
                    diffs.push(format!(
                        "strikethrough: expected {}, got {}",
                        expected.flags.strikeout, actual.attrs.strikethrough
                    ));
                }

                mismatches.push((y, x, diffs));
            }
        }
    }

    if !mismatches.is_empty() {
        let mut msg = format!(
            "Grid attributes do not match fixture '{}'. {} mismatched cells:\n\n",
            fixture_name,
            mismatches.len()
        );

        // Limit output to first 50 mismatches to avoid huge error messages
        for (y, x, diffs) in mismatches.iter().take(50) {
            msg.push_str(&format!("Cell ({}, {}): {}\n", x, y, diffs.join(", ")));
        }

        if mismatches.len() > 50 {
            msg.push_str(&format!(
                "\n... and {} more mismatches\n",
                mismatches.len() - 50
            ));
        }

        panic!("{}", msg);
    }

    // Check cursor position if specified in fixture
    if let Some(ref cursor) = fixture.cursor {
        let actual_x = grid.cursor_x;
        let actual_y = grid.cursor_y;
        if actual_x != cursor.column || actual_y != cursor.line {
            panic!(
                "Cursor position mismatch in '{}': expected ({}, {}), got ({}, {})",
                fixture_name, cursor.column, cursor.line, actual_x, actual_y
            );
        }
    }

    // Check terminal modes if specified in fixture
    if let Some(ref modes) = fixture.modes {
        let mut mode_mismatches = Vec::new();

        if grid.cursor_visible != modes.show_cursor {
            mode_mismatches.push(format!(
                "show_cursor: expected {}, got {}",
                modes.show_cursor, grid.cursor_visible
            ));
        }
        if grid.autowrap != modes.line_wrap {
            mode_mismatches.push(format!(
                "line_wrap: expected {}, got {}",
                modes.line_wrap, grid.autowrap
            ));
        }
        if grid.origin_mode != modes.origin {
            mode_mismatches.push(format!(
                "origin: expected {}, got {}",
                modes.origin, grid.origin_mode
            ));
        }
        if grid.in_alternate_screen != modes.alt_screen {
            mode_mismatches.push(format!(
                "alt_screen: expected {}, got {}",
                modes.alt_screen, grid.in_alternate_screen
            ));
        }

        if !mode_mismatches.is_empty() {
            panic!(
                "Terminal modes do not match fixture '{}': {}",
                fixture_name,
                mode_mismatches.join(", ")
            );
        }
    }
}

/// Compare grid lines with fixture, providing detailed diff on failure
fn assert_grid_matches_fixture(emulator: &TerminalEmulator, fixture_name: &str, rows: usize) {
    let actual = grid_to_lines(emulator, rows);
    let expected = load_fixture(fixture_name);

    // Pad expected to match rows if needed
    let expected: Vec<String> = expected
        .into_iter()
        .chain(std::iter::repeat(String::new()))
        .take(rows)
        .collect();

    let mut mismatches = Vec::new();
    for (i, (actual_line, expected_line)) in actual.iter().zip(expected.iter()).enumerate() {
        // Trim trailing spaces for comparison since fixtures may vary
        let actual_trimmed = actual_line.trim_end();
        let expected_trimmed = expected_line.trim_end();

        if actual_trimmed != expected_trimmed {
            mismatches.push((i, actual_line.clone(), expected_line.clone()));
        }
    }

    if !mismatches.is_empty() {
        let mut msg = format!(
            "Grid does not match fixture '{}'. {} mismatched lines:\n\n",
            fixture_name,
            mismatches.len()
        );

        for (line_num, actual, expected) in &mismatches {
            msg.push_str(&format!("Line {}:\n", line_num));
            msg.push_str(&format!("  Expected: {:?}\n", expected));
            msg.push_str(&format!("  Actual:   {:?}\n\n", actual));
        }

        panic!("{}", msg);
    }
}

#[test]
fn test_vttest_0_screen_alignment() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    // vttest.0.txt: Screen alignment test (option 1 from main menu)
    // Terminal size: 80x33
    let emulator = run_vttest_test(80, 33, &[b"1\n"]);

    assert_grid_matches_fixture(&emulator, "vttest.0.txt", 33);
}

#[test]
fn test_vttest_1_screen_alignment_132col() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    // vttest.1.txt: Screen alignment test in 132-column mode
    // Terminal size: 132x33, input: 1<enter> then <enter> to advance to 132-col test
    let emulator = run_vttest_test(132, 33, &[b"1\n", b"\n"]);

    assert_grid_matches_fixture(&emulator, "vttest.1.txt", 33);
}

#[test]
fn test_vttest_2_cursor_movement() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(80, 33, &[b"1\n", b"\n", b"\n"]);
    assert_grid_matches_fixture(&emulator, "vttest.2.txt", 33);
}

#[test]
fn test_vttest_3_cursor_movement() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(132, 33, &[b"1\n", b"\n", b"\n", b"\n"]);
    assert_grid_matches_fixture(&emulator, "vttest.3.txt", 33);
}

#[test]
fn test_vttest_4_cursor_movement() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(80, 33, &[b"1\n", b"\n", b"\n", b"\n", b"\n"]);
    assert_grid_matches_fixture(&emulator, "vttest.4.txt", 33);
}

#[test]
fn test_vttest_5_cursor_movement() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(80, 33, &[b"1\n", b"\n", b"\n", b"\n", b"\n", b"\n"]);
    assert_grid_matches_fixture(&emulator, "vttest.5.txt", 33);
}

#[test]
fn test_vttest_screen_features_1() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(80, 33, &[b"2\n"]);
    assert_grid_matches_fixture(&emulator, "vttest.2.0.txt", 33);
}

#[test]
fn test_vttest_screen_features_2() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(80, 33, &[b"2\n", b"\n"]);
    assert_grid_matches_fixture(&emulator, "vttest.2.1.txt", 33);
}

#[test]
fn test_vttest_screen_features_3() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(132, 33, &[b"2\n", b"\n", b"\n"]);
    assert_grid_matches_fixture(&emulator, "vttest.2.2.txt", 33);
}

#[test]
fn test_vttest_screen_features_4() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(80, 33, &[b"2\n", b"\n", b"\n", b"\n"]);
    assert_grid_matches_fixture(&emulator, "vttest.2.3.txt", 33);
}

#[test]
fn test_vttest_screen_features_5() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(80, 33, &[b"2\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n"]);
    assert_grid_matches_fixture(&emulator, "vttest.2.4.txt", 33);
}

#[test]
fn test_vttest_screen_features_6() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(
        80,
        33,
        &[b"2\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n"],
    );
    assert_grid_matches_fixture(&emulator, "vttest.2.5.txt", 33);
}

#[test]
fn test_vttest_screen_features_7() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(
        80,
        33,
        &[
            b"2\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n",
        ],
    );
    assert_grid_matches_fixture(&emulator, "vttest.2.6.txt", 33);
}

#[test]
fn test_vttest_screen_features_8() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(
        80,
        33,
        &[
            b"2\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n",
        ],
    );
    assert_grid_matches_fixture(&emulator, "vttest.2.7.txt", 33);
}

#[test]
fn test_vttest_screen_features_9() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(
        80,
        33,
        &[
            b"2\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n",
        ],
    );
    assert_grid_matches_fixture(&emulator, "vttest.2.8.txt", 33);
}

#[test]
fn test_vttest_screen_features_10() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(
        80,
        33,
        &[
            b"2\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n",
        ],
    );
    assert_grid_matches_fixture(&emulator, "vttest.2.9.txt", 33);
}

#[test]
fn test_vttest_screen_features_11() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(
        80,
        33,
        &[
            b"2\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n",
            b"\n",
        ],
    );
    assert_grid_matches_fixture(&emulator, "vttest.2.10.txt", 33);
}

#[test]
fn test_vttest_screen_features_12() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(
        80,
        33,
        &[
            b"2\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n",
            b"\n", b"\n", b"\n",
        ],
    );
    assert_grid_matches_fixture(&emulator, "vttest.2.11.txt", 33);
}

#[test]
fn test_vttest_character_sets() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(80, 33, &[b"3\n"]);
    assert_grid_matches_fixture(&emulator, "vttest.3.0.txt", 33);
}

#[test]
fn test_vttest_terminal_reports() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    // DSR tests need more time because they involve multiple query/response exchanges
    // Empty slices add extra wait time without sending characters that could
    // interfere with vttest reading the DSR responses
    let emulator = run_vttest_test(80, 33, &[b"6\n", b"3\n", b"", b"", b""]);
    assert_grid_matches_fixture(&emulator, "vttest.6.3.txt", 33);
}

#[test]
fn test_vttest_attributes_0() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(
        80,
        33,
        &[
            b"2\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n",
            b"\n",
        ],
    );
    assert_grid_matches_attributes_fixture(&emulator, "vttest-attributes-0.json", 33);
}

#[test]
fn test_vttest_attributes_1() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(
        80,
        33,
        &[
            b"2\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n", b"\n",
            b"\n", b"\n", b"\n",
        ],
    );
    assert_grid_matches_attributes_fixture(&emulator, "vttest-attributes-1.json", 33);
}

#[test]
fn test_vttest_attributes_2() {
    if !vttest_available() {
        eprintln!("Skipping test: vttest not available");
        return;
    }

    let emulator = run_vttest_test(80, 33, &[b""]);
    assert_grid_matches_attributes_fixture(&emulator, "vttest-attributes-2.json", 33);
}
