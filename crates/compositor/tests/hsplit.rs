//! Tests for horizontal split compositing
//!
//! These tests verify that the compositor correctly composites multiple terminal
//! panes and produces the expected output.

use compositor::{Compositor, CompositorError};
use std::io::Write;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

/// A simple in-memory writer for capturing compositor output
#[derive(Clone, Default)]
struct MemoryWriter {
    buffer: Arc<Mutex<Vec<u8>>>,
}

impl MemoryWriter {
    fn new() -> Self {
        Self {
            buffer: Arc::new(Mutex::new(Vec::new())),
        }
    }
}

impl Write for MemoryWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.buffer.lock().unwrap().extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

/// Load fixture file and return as vector of lines
#[allow(dead_code)]
fn load_fixture(name: &str) -> Vec<String> {
    let fixture_path = format!("{}/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name);
    let content = std::fs::read_to_string(&fixture_path)
        .unwrap_or_else(|e| panic!("Failed to load fixture {}: {}", fixture_path, e));

    content.lines().map(|s| s.to_string()).collect()
}

/// Save the current compositor state as a fixture for debugging
#[allow(dead_code)]
fn save_fixture(name: &str, lines: &[String]) {
    let fixture_path = format!("{}/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name);
    let content = lines.join("\n");
    std::fs::write(&fixture_path, content).expect("Failed to write fixture");
}

/// Wait for PTY output and process it
fn wait_for_output(compositor: &mut Compositor, timeout_ms: u64) {
    let start = std::time::Instant::now();
    while start.elapsed() < Duration::from_millis(timeout_ms) {
        match compositor.poll_once(10) {
            Ok(true) => {
                // Got some events, keep polling briefly for more
                thread::sleep(Duration::from_millis(5));
            }
            Ok(false) => {
                // No events, wait a bit
                thread::sleep(Duration::from_millis(10));
            }
            Err(_) => break,
        }
    }
}

#[test]
fn test_hsplit_basic() -> Result<(), CompositorError> {
    // Create a compositor with horizontal split (80x24, split into two 80x12 panes)
    let writer = MemoryWriter::new();
    let mut compositor =
        Compositor::with_hsplit_and_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for bash to start up in both panes
    wait_for_output(&mut compositor, 500);

    // Render to capture the initial state
    let output = compositor.render_to_vec();

    // The output should not be empty (bash prompts should appear)
    // Note: Exact content depends on bash configuration
    assert!(
        output.len() > 0
            || compositor
                .get_text_lines()
                .iter()
                .any(|l| !l.trim().is_empty()),
        "Expected some output from bash initialization"
    );

    Ok(())
}

#[test]
fn test_hsplit_echo_command() -> Result<(), CompositorError> {
    // Create a compositor with horizontal split
    let writer = MemoryWriter::new();
    let mut compositor =
        Compositor::with_hsplit_and_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Send "echo hello" to the focused (top) pane
    compositor.handle_input(b"echo hello\n");

    // Wait for the command to execute and output to appear
    wait_for_output(&mut compositor, 500);

    // Render to update the global emulator
    compositor.render_to_vec();

    // Get the text lines and verify "hello" appears in the top half
    let lines = compositor.get_text_lines();

    // The top pane is rows 0-11, bottom is 12-23
    let top_text: String = lines[..12].join("\n");

    assert!(
        top_text.contains("hello"),
        "Expected 'hello' in top pane output. Got:\n{}",
        top_text
    );

    Ok(())
}

#[test]
fn test_hsplit_separate_panes() -> Result<(), CompositorError> {
    // Create a compositor with horizontal split
    let writer = MemoryWriter::new();
    let mut compositor =
        Compositor::with_hsplit_and_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Send "echo TOP" to the top pane (initially focused)
    compositor.handle_input(b"echo TOP\n");
    wait_for_output(&mut compositor, 300);

    // Switch focus to bottom pane using Ctrl+j (vim-style down)
    compositor.handle_input(&[0x0a]);

    // Send "echo BOTTOM" to the bottom pane
    compositor.handle_input(b"echo BOTTOM\n");
    wait_for_output(&mut compositor, 300);

    // Render to update the global emulator
    compositor.render_to_vec();

    // Get the text lines
    let lines = compositor.get_text_lines();

    // The top pane is rows 0-11, bottom is 12-23
    let top_text: String = lines[..12].join("\n");
    let bottom_text: String = lines[12..].join("\n");

    assert!(
        top_text.contains("TOP"),
        "Expected 'TOP' in top pane. Got:\n{}",
        top_text
    );

    assert!(
        bottom_text.contains("BOTTOM"),
        "Expected 'BOTTOM' in bottom pane. Got:\n{}",
        bottom_text
    );

    // Verify they don't appear in each other's panes
    assert!(
        !top_text.contains("BOTTOM"),
        "'BOTTOM' should not appear in top pane"
    );
    assert!(
        !bottom_text.contains("TOP"),
        "'TOP' should not appear in bottom pane"
    );

    Ok(())
}

#[test]
fn test_render_output_format() -> Result<(), CompositorError> {
    // Create a compositor with horizontal split
    let writer = MemoryWriter::new();
    let mut compositor =
        Compositor::with_hsplit_and_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // First render - should produce output to set up initial state
    let first_render = compositor.render_to_vec();

    // Second render without changes - should produce minimal or no output (delta rendering)
    let second_render = compositor.render_to_vec();

    // The second render should be smaller than or equal to the first
    // (since nothing changed, delta should be minimal)
    assert!(
        second_render.len() <= first_render.len(),
        "Delta rendering should produce less output when nothing changed. First: {}, Second: {}",
        first_render.len(),
        second_render.len()
    );

    Ok(())
}

#[test]
fn test_render_and_replay() -> Result<(), CompositorError> {
    // Create a compositor with horizontal split
    let writer = MemoryWriter::new();
    let mut compositor =
        Compositor::with_hsplit_and_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Send commands to both panes to create a known state
    // Use printf with a known string to avoid bash prompt variability
    compositor.handle_input(b"printf 'TOP_PANE_MARKER\\n'\n");
    wait_for_output(&mut compositor, 300);

    // Switch focus to bottom pane using Ctrl+j (vim-style down)
    compositor.handle_input(&[0x0a]);
    compositor.handle_input(b"printf 'BOTTOM_PANE_MARKER\\n'\n");
    wait_for_output(&mut compositor, 300);

    // Render to get the full output
    let render_output = compositor.render_to_vec();

    // Now create a fresh emulator and replay the render output onto it
    let mut replay_emulator = emulator::TerminalEmulator::new(80, 24);
    replay_emulator.process(&render_output);

    // Compare the replay emulator with the compositor's global emulator
    let compositor_lines = compositor.get_text_lines();
    let replay_lines: Vec<String> = (0..24)
        .map(|y| replay_emulator.grid().get_line_text(y))
        .collect();

    save_fixture("hsplit_replay_fixture.txt", &replay_lines);

    // The replay should contain the same markers
    let replay_text: String = replay_lines.join("\n");
    let compositor_text: String = compositor_lines.join("\n");

    assert!(
        replay_text.contains("TOP_PANE_MARKER"),
        "Replay should contain TOP_PANE_MARKER.\nReplay:\n{}\nCompositor:\n{}",
        replay_text,
        compositor_text
    );

    assert!(
        replay_text.contains("BOTTOM_PANE_MARKER"),
        "Replay should contain BOTTOM_PANE_MARKER.\nReplay:\n{}\nCompositor:\n{}",
        replay_text,
        compositor_text
    );

    Ok(())
}

#[test]
fn test_vsplit_render_and_replay() -> Result<(), CompositorError> {
    // Create a compositor with horizontal split
    let writer = MemoryWriter::new();
    let mut compositor =
        Compositor::with_vsplit_and_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Send commands to both panes to create a known state
    // Use printf with a known string to avoid bash prompt variability
    compositor.handle_input(b"printf 'LEFT_PANE_MARKER\\n'\n");
    wait_for_output(&mut compositor, 300);

    // Switch focus to right pane using Ctrl+l (vim-style right)
    compositor.handle_input(&[0x0c]);
    compositor.handle_input(b"printf 'RIGHT_PANE_MARKER\\n'\n");
    wait_for_output(&mut compositor, 300);

    // Render to get the full output
    let render_output = compositor.render_to_vec();

    // Now create a fresh emulator and replay the render output onto it
    let mut replay_emulator = emulator::TerminalEmulator::new(80, 24);
    replay_emulator.process(&render_output);

    // Compare the replay emulator with the compositor's global emulator
    let compositor_lines = compositor.get_text_lines();
    let replay_lines: Vec<String> = (0..24)
        .map(|y| replay_emulator.grid().get_line_text(y))
        .collect();

    save_fixture("vsplit_replay_fixture.txt", &replay_lines);

    // The replay should contain the same markers
    let replay_text: String = replay_lines.join("\n");
    let compositor_text: String = compositor_lines.join("\n");

    assert!(
        replay_text.contains("LEFT_PANE_MARKER"),
        "Replay should contain LEFT_PANE_MARKER.\nReplay:\n{}\nCompositor:\n{}",
        replay_text,
        compositor_text
    );

    assert!(
        replay_text.contains("RIGHT_PANE_MARKER"),
        "Replay should contain RIGHT_PANE_MARKER.\nReplay:\n{}\nCompositor:\n{}",
        replay_text,
        compositor_text
    );

    Ok(())
}

/// Compare compositor state against a fixture file
#[allow(dead_code)]
fn compare_with_fixture(compositor: &Compositor, fixture_name: &str) {
    let compositor_lines = compositor.get_text_lines();
    let fixture_lines = load_fixture(fixture_name);

    // Compare line by line
    for (i, (comp_line, fix_line)) in compositor_lines
        .iter()
        .zip(fixture_lines.iter())
        .enumerate()
    {
        assert_eq!(
            comp_line.trim_end(),
            fix_line.trim_end(),
            "Line {} differs.\nExpected: '{}'\nGot: '{}'",
            i,
            fix_line,
            comp_line
        );
    }
}
