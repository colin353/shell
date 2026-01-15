//! Tests for horizontal split compositing
//!
//! These tests verify that the compositor correctly composites multiple terminal
//! panes and produces the expected output.

use compositor::{Compositor, CompositorError};
use std::io::Write;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

/// Fixed time used for all tests to avoid fixture churn
fn fixed_test_time() -> chrono::DateTime<chrono::Local> {
    use chrono::TimeZone;
    chrono::Local.with_ymd_and_hms(2025, 1, 1, 12, 0, 0).unwrap()
}

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
    // Create a compositor and split it horizontally using Ctrl+b "
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for bash to start up
    wait_for_output(&mut compositor, 500);

    // Create horizontal split with Ctrl+b "
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "

    // Wait for the new pane's bash to start
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
    // Create a compositor and split it horizontally
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Create horizontal split with Ctrl+b "
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "

    // Wait for the new pane's bash to start
    wait_for_output(&mut compositor, 500);

    // Send "echo hello" to the focused (bottom, newly created) pane
    compositor.handle_input(b"echo hello\n");

    // Wait for the command to execute and output to appear
    wait_for_output(&mut compositor, 500);

    // Render to update the global emulator
    compositor.render_to_vec();

    // Get the text lines and verify "hello" appears in the bottom half
    let lines = compositor.get_text_lines();

    // The top pane is rows 0-11, bottom is 12-23
    let bottom_text: String = lines[12..].join("\n");

    assert!(
        bottom_text.contains("hello"),
        "Expected 'hello' in bottom pane output. Got:\n{}",
        bottom_text
    );

    Ok(())
}

#[test]
fn test_hsplit_separate_panes() -> Result<(), CompositorError> {
    // Create a compositor and split it horizontally
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Create horizontal split with Ctrl+b "
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "

    // Wait for the new pane's bash to start
    wait_for_output(&mut compositor, 500);

    // After split, focus is on bottom pane. Switch to top pane first.
    compositor.handle_input(&[0x0b]); // Ctrl+k - move focus up

    // Send "echo TOP" to the top pane
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
    // Create a compositor and split it horizontally
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Create horizontal split with Ctrl+b "
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "

    // Wait for the new pane's bash to start
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
    // Create a compositor and split it horizontally using Ctrl+b "
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Create horizontal split with Ctrl+b "
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "

    // Wait for the new pane's bash to start
    wait_for_output(&mut compositor, 500);

    // After split, focus is on bottom pane. Switch to top pane first.
    compositor.handle_input(&[0x0b]); // Ctrl+k - move focus up

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
    // Create a compositor and split it vertically using Ctrl+b %
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Create vertical split with Ctrl+b %
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'%']); // %

    // Wait for the new pane's bash to start
    wait_for_output(&mut compositor, 500);

    // After split, focus is on right pane. Switch to left pane first.
    compositor.handle_input(&[0x08]); // Ctrl+h - move focus left

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

#[test]
fn test_render_and_replay_hvsplit() -> Result<(), CompositorError> {
    // Create a compositor and split it horizontally using Ctrl+b "
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Create horizontal split with Ctrl+b "
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "

    // Create a vertical split with Ctrl+b %
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'%']); // %

    // Wait for the new pane's bash to start
    wait_for_output(&mut compositor, 500);

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

    save_fixture("hvsplit_replay_fixture.txt", &replay_lines);

    // The replay should contain the same markers
    let replay_text: String = replay_lines.join("\n");
    let compositor_text: String = compositor_lines.join("\n");

    assert!(
        replay_text.contains("┬"),
        "Replay should contain ┬.\nReplay:\n{}\nCompositor:\n{}",
        replay_text,
        compositor_text
    );

    Ok(())
}

#[test]
fn test_history_search_navigation() -> Result<(), CompositorError> {
    use std::sync::Arc;

    // Create a temporary directory for the test history file with a unique name
    let unique_id = format!(
        "{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let temp_dir = std::env::temp_dir().join(format!("shell_test_{}", unique_id));
    std::fs::create_dir_all(&temp_dir).unwrap();
    let history_path = temp_dir.join("test_history.log");

    // Create a ShellCore with our custom history path
    // This will NOT import from zsh/bash because the file doesn't exist yet
    // and we're not in a normal home directory context
    let core = Arc::new(
        libshell::ShellCore::with_history_path(history_path.clone())
            .expect("Failed to create ShellCore"),
    );

    // Pre-populate the history with fake commands
    let history = core.history();

    // Record some fake commands with varying properties
    let id1 = history
        .record_command("git status".to_string(), libshell::CommandSource::Human)
        .unwrap();
    history.record_exit(&id1, 0, 100).unwrap(); // Successful

    let id2 = history
        .record_command(
            "git commit -m 'Initial commit'".to_string(),
            libshell::CommandSource::Human,
        )
        .unwrap();
    history.record_exit(&id2, 0, 500).unwrap(); // Successful

    let id3 = history
        .record_command(
            "git push origin main".to_string(),
            libshell::CommandSource::Ai,
        )
        .unwrap();
    history.record_exit(&id3, 1, 200).unwrap(); // Failed (AI command)

    let id4 = history
        .record_command(
            "cargo build --release".to_string(),
            libshell::CommandSource::Human,
        )
        .unwrap();
    history.record_exit(&id4, 0, 5000).unwrap(); // Successful

    let id5 = history
        .record_command("cargo test".to_string(), libshell::CommandSource::Human)
        .unwrap();
    history.record_exit(&id5, 0, 3000).unwrap(); // Successful

    let id6 = history
        .record_command(
            "git log --oneline".to_string(),
            libshell::CommandSource::Human,
        )
        .unwrap();
    history.record_exit(&id6, 0, 50).unwrap(); // Successful

    // Create a compositor with the custom core
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_core(80, 24, Arc::new(Mutex::new(writer.clone())), core)?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // No need to wait for subprocess - we're using the embedded shell
    // Render the initial state
    compositor.render_to_vec();

    let initial_lines = compositor.get_text_lines();
    save_fixture("history_search_initial.txt", &initial_lines);

    // Send Ctrl+R to enter history search mode
    compositor.handle_input(&[0x12]); // Ctrl+R
    compositor.render_to_vec();

    let search_mode_lines = compositor.get_text_lines();
    save_fixture("history_search_empty_query.txt", &search_mode_lines);

    // Verify we see the search indicator
    let search_text: String = search_mode_lines.join("\n");
    assert!(
        search_text.contains("reverse-i-search") || search_text.contains("search"),
        "Expected search indicator in output. Got:\n{}",
        search_text
    );

    // Type "git" to search for git commands
    compositor.handle_input(b"git");
    compositor.render_to_vec();

    let git_search_lines = compositor.get_text_lines();
    save_fixture("history_search_git_query.txt", &git_search_lines);

    // Verify we see git-related results
    let git_search_text: String = git_search_lines.join("\n");
    assert!(
        git_search_text.contains("git"),
        "Expected 'git' in search results. Got:\n{}",
        git_search_text
    );

    // Press Down arrow to navigate to next result
    compositor.handle_input(&[0x1b, b'[', b'B']); // Down arrow
    compositor.render_to_vec();

    let nav_down_lines = compositor.get_text_lines();
    save_fixture("history_search_nav_down.txt", &nav_down_lines);

    // Press Down arrow again
    compositor.handle_input(&[0x1b, b'[', b'B']); // Down arrow
    compositor.render_to_vec();

    let nav_down2_lines = compositor.get_text_lines();
    save_fixture("history_search_nav_down2.txt", &nav_down2_lines);

    // Press Up arrow to go back
    compositor.handle_input(&[0x1b, b'[', b'A']); // Up arrow
    compositor.render_to_vec();

    let nav_up_lines = compositor.get_text_lines();
    save_fixture("history_search_nav_up.txt", &nav_up_lines);

    // Press Enter to select the current result
    compositor.handle_input(&[b'\r']); // Enter

    // Force a full re-render to ensure the terminal state is updated
    compositor.force_render();

    let after_select_lines = compositor.get_text_lines();
    save_fixture("history_search_selected.txt", &after_select_lines);

    // The input line should now contain one of the git commands
    // Check that the prompt line contains a git command
    let after_select_text: String = after_select_lines.join("\n");
    assert!(
        after_select_text.contains("git"),
        "Expected selected git command in input line. Got:\n{}",
        after_select_text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

#[test]
fn test_history_search_escape() -> Result<(), CompositorError> {
    use std::sync::Arc;

    // Create a temporary directory for the test history file
    let unique_id = format!(
        "{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let temp_dir = std::env::temp_dir().join(format!("shell_test_esc_{}", unique_id));
    std::fs::create_dir_all(&temp_dir).unwrap();
    let history_path = temp_dir.join("test_history.log");

    // Create a ShellCore with our custom history path
    let core = Arc::new(
        libshell::ShellCore::with_history_path(history_path.clone())
            .expect("Failed to create ShellCore"),
    );

    // Pre-populate the history with some commands
    let history = core.history();
    let id1 = history
        .record_command("echo hello".to_string(), libshell::CommandSource::Human)
        .unwrap();
    history.record_exit(&id1, 0, 10).unwrap();

    let id2 = history
        .record_command("ls -la".to_string(), libshell::CommandSource::Human)
        .unwrap();
    history.record_exit(&id2, 0, 20).unwrap();

    // Create a compositor with the custom core
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_core(80, 24, Arc::new(Mutex::new(writer.clone())), core)?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Render initial state
    compositor.render_to_vec();

    // Type something first to have input to preserve
    compositor.handle_input(b"my_command");
    compositor.render_to_vec();

    // Send Ctrl+R to enter history search mode
    compositor.handle_input(&[0x12]); // Ctrl+R
    compositor.render_to_vec();

    let search_lines = compositor.get_text_lines();
    let search_text: String = search_lines.join("\n");
    assert!(
        search_text.contains("reverse-i-search") || search_text.contains("search"),
        "Should be in search mode"
    );

    // Press Escape to cancel search
    compositor.handle_input(&[0x1b]); // ESC
                                      // Need a small delay or another byte to distinguish from escape sequence
    compositor.handle_input(&[0x1b]); // Send ESC again to confirm it's not a sequence start
    compositor.render_to_vec();

    let after_escape_lines = compositor.get_text_lines();
    save_fixture("history_search_escaped.txt", &after_escape_lines);

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

#[test]
fn test_nonzero_exit_code_display() -> Result<(), CompositorError> {
    // Create a compositor
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for shell to initialize
    wait_for_output(&mut compositor, 500);

    // Run a command that will exit with code 1 (false always returns 1)
    compositor.handle_input(b"false\n");

    // Wait for the command to execute
    wait_for_output(&mut compositor, 500);

    // Render to update the display
    compositor.render_to_vec();

    // Get the text lines and verify the exit code message appears
    let lines = compositor.get_text_lines();
    let all_text: String = lines.join("\n");

    assert!(
        all_text.contains("exit 1"),
        "Expected 'exit 1' to appear after command with non-zero exit code. Got:\n{}",
        all_text
    );

    Ok(())
}

#[test]
fn test_ctrl_c_display() -> Result<(), CompositorError> {
    // Create a compositor
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for shell to initialize
    wait_for_output(&mut compositor, 500);

    // Run a command that will sleep for a while (sleep 10)
    compositor.handle_input(b"sleep 10\n");

    // Wait for the command to start
    wait_for_output(&mut compositor, 200);

    // Send CTRL+C to interrupt the command
    compositor.handle_input(&[0x03]); // CTRL+C

    // Wait for the command to be killed
    wait_for_output(&mut compositor, 500);

    // Render to update the display
    compositor.render_to_vec();

    // Get the text lines and verify CTRL+C message appears (not exit 130)
    let lines = compositor.get_text_lines();
    let all_text: String = lines.join("\n");

    assert!(
        all_text.contains("CTRL+C"),
        "Expected 'CTRL+C' to appear after killing command with CTRL+C. Got:\n{}",
        all_text
    );

    // Make sure it doesn't show "exit 130"
    assert!(
        !all_text.contains("exit 130"),
        "Should not show 'exit 130' when killed by CTRL+C. Got:\n{}",
        all_text
    );

    Ok(())
}
