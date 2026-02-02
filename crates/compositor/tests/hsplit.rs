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
    chrono::Local
        .with_ymd_and_hms(2025, 1, 1, 12, 0, 0)
        .unwrap()
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

    #[allow(dead_code)]
    fn get_buffer(&self) -> Vec<u8> {
        self.buffer.lock().unwrap().clone()
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

#[test]
fn test_multiline_command_with_backslash_continuation() -> Result<(), CompositorError> {
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
    let temp_dir = std::env::temp_dir().join(format!("shell_test_multiline_{}", unique_id));
    std::fs::create_dir_all(&temp_dir).unwrap();
    let history_path = temp_dir.join("test_history.log");

    // Create a ShellCore with our custom history path
    let core = Arc::new(
        libshell::ShellCore::with_history_path(history_path.clone())
            .expect("Failed to create ShellCore"),
    );

    // Create a compositor with the custom core (using embedded shell, not bash)
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_core(80, 24, Arc::new(Mutex::new(writer.clone())), core)?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Render initial state
    compositor.render_to_vec();

    // Type "ls \" (backslash at end to continue to next line)
    compositor.handle_input(b"ls \\");
    compositor.render_to_vec();

    let before_enter_lines = compositor.get_text_lines();
    save_fixture("multiline_before_enter.txt", &before_enter_lines);

    // Press Enter - this should NOT execute the command, but continue to next line
    compositor.handle_input(b"\r");
    compositor.render_to_vec();

    let after_enter_lines = compositor.get_text_lines();
    save_fixture("multiline_after_enter.txt", &after_enter_lines);

    // Now type "-lah" on the continuation line
    compositor.handle_input(b"-lah");
    compositor.render_to_vec();

    let after_continuation_lines = compositor.get_text_lines();
    save_fixture(
        "multiline_after_continuation.txt",
        &after_continuation_lines,
    );

    // The output should show both lines of the multi-line command
    let text: String = after_continuation_lines.join("\n");

    // Verify that the continuation line shows the input
    assert!(
        text.contains("-lah"),
        "Expected '-lah' to appear on the continuation line. Got:\n{}",
        text
    );

    // Verify that the command has NOT executed yet (no spawn error or output)
    assert!(
        !text.contains("spawn error") && !text.contains("No such file"),
        "Command should not have executed yet. Got:\n{}",
        text
    );

    // Now press Enter to execute the complete command
    compositor.handle_input(b"\r");
    wait_for_output(&mut compositor, 500);
    compositor.render_to_vec();

    let after_execute_lines = compositor.get_text_lines();
    save_fixture("multiline_after_execute.txt", &after_execute_lines);

    // The command should have executed and we should see a new prompt
    let executed_text: String = after_execute_lines.join("\n");

    // Verify we see the continuation line with -lah before execution
    // and a new prompt after (the command ran)
    assert!(
        executed_text.contains("➜"),
        "Expected a new prompt after command execution. Got:\n{}",
        executed_text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

#[test]
fn test_multiline_backspace_across_newline() -> Result<(), CompositorError> {
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
    let temp_dir = std::env::temp_dir().join(format!("shell_test_multiline_bs_{}", unique_id));
    std::fs::create_dir_all(&temp_dir).unwrap();
    let history_path = temp_dir.join("test_history.log");

    // Create a ShellCore with our custom history path
    let core = Arc::new(
        libshell::ShellCore::with_history_path(history_path.clone())
            .expect("Failed to create ShellCore"),
    );

    // Create a compositor with the custom core (using embedded shell, not bash)
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_core(80, 24, Arc::new(Mutex::new(writer.clone())), core)?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Render initial state
    compositor.render_to_vec();

    // Type "echo \" (backslash at end to continue to next line)
    compositor.handle_input(b"echo \\");
    compositor.render_to_vec();

    // Press Enter to go to continuation line
    compositor.handle_input(b"\r");
    compositor.render_to_vec();

    // Type "hello" on the continuation line
    compositor.handle_input(b"hello");
    compositor.render_to_vec();

    let before_backspace_lines = compositor.get_text_lines();
    save_fixture("multiline_bs_before.txt", &before_backspace_lines);

    // Verify we have the multi-line input
    let text_before: String = before_backspace_lines.join("\n");
    assert!(
        text_before.contains("echo") && text_before.contains("hello"),
        "Expected 'echo' and 'hello' before backspace. Got:\n{}",
        text_before
    );

    // Now press backspace 6 times to delete "hello" and the newline
    for _ in 0..6 {
        compositor.handle_input(&[0x7f]); // Backspace
        compositor.render_to_vec();
    }

    let after_backspace_lines = compositor.get_text_lines();
    save_fixture("multiline_bs_after.txt", &after_backspace_lines);

    // The output should now show just "echo " on a single line (back to first line)
    let text_after: String = after_backspace_lines.join("\n");

    // Verify that "hello" is gone
    assert!(
        !text_after.contains("hello"),
        "Expected 'hello' to be deleted after backspace. Got:\n{}",
        text_after
    );

    // Verify that "echo " is still there on the first line
    assert!(
        text_after.contains("echo "),
        "Expected 'echo ' to still be present. Got:\n{}",
        text_after
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

#[test]
fn test_cd_tilde_expansion() -> Result<(), CompositorError> {
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
    let temp_dir = std::env::temp_dir().join(format!("shell_test_cd_tilde_{}", unique_id));
    std::fs::create_dir_all(&temp_dir).unwrap();
    let history_path = temp_dir.join("test_history.log");

    // Create a ShellCore with our custom history path
    let core = Arc::new(
        libshell::ShellCore::with_history_path(history_path.clone())
            .expect("Failed to create ShellCore"),
    );

    // Create a compositor with the custom core (using embedded shell, not bash)
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_core(80, 24, Arc::new(Mutex::new(writer.clone())), core)?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Render initial state
    compositor.render_to_vec();

    // First, get the home directory for verification
    let home = std::env::var("HOME").expect("HOME not set");

    // Test cd ~ (should work, but let's verify)
    compositor.handle_input(b"cd ~\r");
    compositor.render_to_vec();

    let lines_after_cd_home = compositor.get_text_lines();
    let text = lines_after_cd_home.join("\n");

    // Should see ~ in the prompt (which is how home dir is displayed)
    assert!(
        text.contains("~ "),
        "Expected '~ ' in prompt after cd ~. Got:\n{}",
        text
    );

    // Now test cd ~/Documents (or any subdir of home that exists)
    // Use a directory we know exists - let's check for common ones
    let test_dirs = ["Documents", "Desktop", "Downloads", "."];
    let mut found_dir = None;
    for dir in &test_dirs {
        let path = std::path::PathBuf::from(&home).join(dir);
        if path.is_dir() {
            found_dir = Some(*dir);
            break;
        }
    }

    if let Some(dir) = found_dir {
        let cmd = format!("cd ~/{}\r", dir);
        compositor.handle_input(cmd.as_bytes());
        compositor.render_to_vec();

        let lines_after = compositor.get_text_lines();
        save_fixture("cd_tilde_expansion.txt", &lines_after);
        let text_after = lines_after.join("\n");

        // Should NOT see "no such directory" error
        assert!(
            !text_after.contains("no such directory"),
            "cd ~/{} should work. Got:\n{}",
            dir,
            text_after
        );

        // The prompt should show the directory name (not ~)
        if dir != "." {
            assert!(
                text_after.contains(dir),
                "Expected '{}' in prompt after cd ~/{}. Got:\n{}",
                dir,
                dir,
                text_after
            );
        }
    }

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

#[test]
fn test_ctrl_x_ctrl_e_edit_multiline_command() -> Result<(), CompositorError> {
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
    let temp_dir = std::env::temp_dir().join(format!("shell_test_ctrlxe_{}", unique_id));
    std::fs::create_dir_all(&temp_dir).unwrap();
    let history_path = temp_dir.join("test_history.log");

    // Create a ShellCore with our custom history path
    let core = Arc::new(
        libshell::ShellCore::with_history_path(history_path.clone())
            .expect("Failed to create ShellCore"),
    );

    // Create a compositor with the custom core (using embedded shell, not bash)
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_core(80, 24, Arc::new(Mutex::new(writer.clone())), core)?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Render initial state
    compositor.render_to_vec();

    let initial_lines = compositor.get_text_lines();
    save_fixture("ctrlxe_initial.txt", &initial_lines);

    // Type some initial text
    compositor.handle_input(b"initial text");
    compositor.render_to_vec();

    // Create a temp file to simulate what vim would write (multi-line content)
    let temp_file = temp_dir.join(format!("shell_edit_{}.sh", std::process::id()));
    let multiline_content = "echo line1\necho line2\necho line3";
    std::fs::write(&temp_file, multiline_content).unwrap();

    // Now simulate the editor exiting by directly calling the shell's editor_exited
    // Get mutable access to the pane's shell
    let pane = compositor
        .get_focused_pane_mut()
        .expect("Expected focused pane");
    let output = pane.shell.editor_exited(&temp_file);
    pane.terminal_emulator.process(&output);

    // Render the result
    compositor.render_to_vec();

    let after_editor_lines = compositor.get_text_lines();
    save_fixture("ctrlxe_after_editor.txt", &after_editor_lines);

    // The output should show the multi-line command properly rendered
    let text: String = after_editor_lines.join("\n");

    // Verify the multi-line content appears correctly
    // The prompt should show the multi-line input
    assert!(
        text.contains("echo line1"),
        "Expected 'echo line1' in output after editor exit. Got:\n{}",
        text
    );

    // For a multi-line command, subsequent lines should also be visible
    // (either on continuation lines or as part of a wrapped input)
    assert!(
        text.contains("line2") || text.contains("line3"),
        "Expected multi-line content to be preserved. Got:\n{}",
        text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

#[test]
fn test_hsplit_history_ctrl_a() -> Result<(), CompositorError> {
    // Test that using up arrow to recall history and Ctrl+A to jump to start of line
    // works correctly with bash connected to the terminal.
    //
    // Bug: After recalling a command with up arrow and pressing Ctrl+A, inserting text
    // at the beginning of the line should show the inserted text followed by the
    // original command. The display should show "asdfecho hello world".

    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    compositor.handle_input(b"bash\n");
    wait_for_output(&mut compositor, 500);

    // Type "echo hello world" and press enter
    compositor.handle_input(b"echo hello world\n");
    wait_for_output(&mut compositor, 500);

    // Press up arrow to recall the previous command
    compositor.handle_input(&[0x1b, b'[', b'A']); // Up arrow
    wait_for_output(&mut compositor, 200);

    // Press Ctrl+A to jump to start of line
    compositor.handle_input(&[0x01]); // Ctrl+A
    wait_for_output(&mut compositor, 200);

    // Type "asdf" at the beginning
    compositor.handle_input(b"asdf");
    wait_for_output(&mut compositor, 200);

    // Render to update the global emulator
    compositor.render_to_vec();

    // Get the text lines
    let lines = compositor.get_text_lines();
    let bottom_text: String = lines.join("\n");

    // The line should now show "asdfecho hello world"
    assert!(
        bottom_text.contains("asdfecho hello world"),
        "Expected 'asdfecho hello world' in bottom pane after Ctrl+A insert. Got:\n{}",
        bottom_text
    );

    Ok(())
}

/// Test that unicode characters render correctly in a horizontal split.
///
/// This tests that various unicode characters (CJK, emoji, symbols) are
/// properly rendered and that the incremental delta rendering produces
/// output that matches a full redraw.
#[test]
fn test_hsplit_unicode_rendering() -> Result<(), CompositorError> {
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
    let temp_dir = std::env::temp_dir().join(format!("shell_test_unicode_{}", unique_id));
    std::fs::create_dir_all(&temp_dir).unwrap();
    let history_path = temp_dir.join("test_history.log");

    // Create a ShellCore with our custom history path
    let core = Arc::new(
        libshell::ShellCore::with_history_path(history_path.clone())
            .expect("Failed to create ShellCore"),
    );

    // Create a compositor with the custom core (using embedded shell, not bash)
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_core(80, 24, Arc::new(Mutex::new(writer.clone())), core)?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Render initial state
    compositor.render_to_vec();

    // Create horizontal split with Ctrl+b "
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "
    compositor.render_to_vec();

    // Switch to top pane
    compositor.handle_input(&[0x0b]); // Ctrl+k - move focus up
    compositor.render_to_vec();

    // Echo a string with various unicode characters:
    // - Basic ASCII
    // - CJK characters (wide - 2 columns each)
    // - Emoji (various widths)
    // - Mathematical symbols
    // - Box drawing characters
    let unicode_string = "echo 'Hello 你好 こんにちは 🎉🚀 α β γ ∞ ≠ ≈ ┌─┐'";
    compositor.handle_input(unicode_string.as_bytes());
    compositor.handle_input(b"\r");
    compositor.render_to_vec();

    // Get the compositor's internal state
    let compositor_lines = compositor.get_text_lines();
    let compositor_text: String = compositor_lines.join("\n");

    // Verify the unicode content is present in the compositor state
    assert!(
        compositor_text.contains("Hello"),
        "Expected 'Hello' in output. Got:\n{}",
        compositor_text
    );
    assert!(
        compositor_text.contains("你好"),
        "Expected Chinese '你好' in output. Got:\n{}",
        compositor_text
    );
    assert!(
        compositor_text.contains("こんにちは"),
        "Expected Japanese 'こんにちは' in output. Got:\n{}",
        compositor_text
    );
    assert!(
        compositor_text.contains("🎉"),
        "Expected emoji '🎉' in output. Got:\n{}",
        compositor_text
    );
    assert!(
        compositor_text.contains("∞"),
        "Expected infinity symbol '∞' in output. Got:\n{}",
        compositor_text
    );

    // Now test that the render output can be replayed to produce the same state
    let render_output = compositor.render_to_vec();

    // Create a fresh emulator and replay the render output
    let mut replay_emulator = emulator::TerminalEmulator::new(80, 24);
    replay_emulator.process(&render_output);

    // Get text from both
    let replay_lines: Vec<String> = (0..24)
        .map(|y| replay_emulator.grid().get_line_text(y))
        .collect();
    let replay_text: String = replay_lines.join("\n");

    // The replay should contain the same unicode content
    assert!(
        replay_text.contains("Hello"),
        "Replay should contain 'Hello'.\nReplay:\n{}\nCompositor:\n{}",
        replay_text,
        compositor_text
    );
    assert!(
        replay_text.contains("你好"),
        "Replay should contain Chinese '你好'.\nReplay:\n{}\nCompositor:\n{}",
        replay_text,
        compositor_text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

/// Test unicode rendering in a vertical split to stress column positioning.
///
/// Vertical splits are more sensitive to column width issues because the
/// panes are side by side, and wide characters that span columns can cause
/// rendering artifacts at the split border.
#[test]
fn test_vsplit_unicode_rendering() -> Result<(), CompositorError> {
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
    let temp_dir = std::env::temp_dir().join(format!("shell_test_unicode_vsplit_{}", unique_id));
    std::fs::create_dir_all(&temp_dir).unwrap();
    let history_path = temp_dir.join("test_history.log");

    // Create a ShellCore with our custom history path
    let core = Arc::new(
        libshell::ShellCore::with_history_path(history_path.clone())
            .expect("Failed to create ShellCore"),
    );

    // Create a compositor with the custom core
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_core(80, 24, Arc::new(Mutex::new(writer.clone())), core)?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Render initial state
    compositor.render_to_vec();

    // Create vertical split with Ctrl+b %
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'%']); // %
    compositor.render_to_vec();

    // Switch to left pane
    compositor.handle_input(&[0x08]); // Ctrl+h - move focus left
    compositor.render_to_vec();

    // Echo CJK characters (each should be 2 columns wide)
    compositor.handle_input(b"echo '\xe4\xb8\xad\xe6\x96\x87\xe5\xad\x97\xe7\xac\xa6'");  // 中文字符
    compositor.handle_input(b"\r");
    compositor.render_to_vec();

    // Get the compositor's internal state
    let compositor_lines = compositor.get_text_lines();
    let compositor_text: String = compositor_lines.join("\n");

    // Verify the CJK content is present
    assert!(
        compositor_text.contains("中文字符"),
        "Expected CJK '中文字符' in output. Got:\n{}",
        compositor_text
    );

    // Switch to right pane and add more unicode
    compositor.handle_input(&[0x0c]); // Ctrl+l - move focus right
    compositor.render_to_vec();

    compositor.handle_input(b"echo '\xce\xb1\xce\xb2\xce\xb3'");  // αβγ
    compositor.handle_input(b"\r");
    compositor.render_to_vec();

    let compositor_lines = compositor.get_text_lines();
    let compositor_text: String = compositor_lines.join("\n");

    // Both panes should show their content
    assert!(
        compositor_text.contains("中文字符"),
        "Left pane should still contain CJK. Got:\n{}",
        compositor_text
    );
    assert!(
        compositor_text.contains("αβγ"),
        "Right pane should contain Greek letters. Got:\n{}",
        compositor_text
    );

    // Test replay of the delta render
    let render_output = compositor.render_to_vec();
    let mut replay_emulator = emulator::TerminalEmulator::new(80, 24);
    replay_emulator.process(&render_output);

    let replay_lines: Vec<String> = (0..24)
        .map(|y| replay_emulator.grid().get_line_text(y))
        .collect();
    let replay_text: String = replay_lines.join("\n");

    assert!(
        replay_text.contains("中文字符"),
        "Replay should contain CJK content. Replay:\n{}",
        replay_text
    );
    assert!(
        replay_text.contains("αβγ"),
        "Replay should contain Greek letters. Replay:\n{}",
        replay_text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

/// Test that incremental rendering produces the same result as a full redraw
/// when unicode characters are involved.
///
/// This test specifically exercises the bug where incremental delta rendering
/// may produce artifacts with unicode characters, but Ctrl+B r (full redraw)
/// produces correct output.
#[test]
fn test_unicode_incremental_vs_full_redraw() -> Result<(), CompositorError> {
    use std::sync::Arc;

    let unique_id = format!(
        "{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let temp_dir = std::env::temp_dir().join(format!("shell_test_unicode_redraw_{}", unique_id));
    std::fs::create_dir_all(&temp_dir).unwrap();
    let history_path = temp_dir.join("test_history.log");

    let core = Arc::new(
        libshell::ShellCore::with_history_path(history_path.clone())
            .expect("Failed to create ShellCore"),
    );

    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_core(80, 24, Arc::new(Mutex::new(writer.clone())), core)?;
    compositor.set_fixed_time(fixed_test_time());

    // Create horizontal split
    compositor.render_to_vec();
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "
    compositor.render_to_vec();

    // Print a mix of narrow and wide characters
    // This sequence is designed to stress-test column tracking:
    // - ASCII (1 column each)
    // - CJK (2 columns each)
    // - Emoji (varies, often 2 columns)
    // - Combining characters
    let test_strings = [
        "echo 'a中b国c人d'",          // Interleaved ASCII and CJK
        "echo '→←↑↓'",               // Arrows
        "echo '①②③④⑤'",             // Circled numbers (narrow)
        "echo '🇺🇸🇬🇧'",               // Flag emoji (complex graphemes)
        "echo 'café résumé naïve'",   // Latin with diacritics
    ];

    for s in &test_strings {
        compositor.handle_input(s.as_bytes());
        compositor.handle_input(b"\r");
        compositor.render_to_vec();
    }

    // Get the state after incremental rendering
    let incremental_lines = compositor.get_text_lines();
    let incremental_text: String = incremental_lines.join("\n");

    // Now force a full redraw (simulating Ctrl+B r)
    compositor.force_full_redraw();

    // Get the state after full redraw
    let full_redraw_lines = compositor.get_text_lines();
    let full_redraw_text: String = full_redraw_lines.join("\n");

    // The text content should be identical
    assert_eq!(
        incremental_text, full_redraw_text,
        "Incremental render should match full redraw.\nIncremental:\n{}\n\nFull redraw:\n{}",
        incremental_text, full_redraw_text
    );

    // Also verify specific content
    assert!(
        full_redraw_text.contains("中") && full_redraw_text.contains("国"),
        "Should contain CJK characters. Got:\n{}",
        full_redraw_text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

/// Test rendering of a dense block of CJK text that fills most of the terminal width.
///
/// This exercises edge cases where wide characters might extend past the
/// expected column boundaries or cause off-by-one errors in cursor positioning.
#[test]
fn test_dense_cjk_rendering() -> Result<(), CompositorError> {
    use std::sync::Arc;

    let unique_id = format!(
        "{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let temp_dir = std::env::temp_dir().join(format!("shell_test_dense_cjk_{}", unique_id));
    std::fs::create_dir_all(&temp_dir).unwrap();
    let history_path = temp_dir.join("test_history.log");

    let core = Arc::new(
        libshell::ShellCore::with_history_path(history_path.clone())
            .expect("Failed to create ShellCore"),
    );

    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_core(80, 24, Arc::new(Mutex::new(writer.clone())), core)?;
    compositor.set_fixed_time(fixed_test_time());
    compositor.render_to_vec();

    // Create a horizontal split
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "
    compositor.render_to_vec();

    // Echo a long string of CJK characters
    // 20 CJK characters = 40 columns (should fit in 80-column terminal)
    let cjk_text = "日本語漢字中国語韓国語台湾語香港語新加坡語馬來西亞";
    let cmd = format!("echo '{}'", cjk_text);
    compositor.handle_input(cmd.as_bytes());
    compositor.handle_input(b"\r");
    compositor.render_to_vec();

    let compositor_lines = compositor.get_text_lines();
    let compositor_text: String = compositor_lines.join("\n");

    // Verify the CJK text appears correctly
    assert!(
        compositor_text.contains("日本語"),
        "Should contain Japanese. Got:\n{}",
        compositor_text
    );
    assert!(
        compositor_text.contains("中国語"),
        "Should contain Chinese. Got:\n{}",
        compositor_text
    );

    // Test replay
    let render_output = compositor.render_to_vec();
    let mut replay_emulator = emulator::TerminalEmulator::new(80, 24);
    replay_emulator.process(&render_output);

    let replay_lines: Vec<String> = (0..24)
        .map(|y| replay_emulator.grid().get_line_text(y))
        .collect();
    let replay_text: String = replay_lines.join("\n");

    assert!(
        replay_text.contains("日本語"),
        "Replay should contain Japanese.\nReplay:\n{}\nCompositor:\n{}",
        replay_text,
        compositor_text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

/// Test that emoji sequences render correctly in splits.
///
/// Emoji can be particularly tricky because:
/// - Some emoji are 2 columns wide
/// - Some are sequences of multiple codepoints (ZWJ sequences)
/// - Skin tone modifiers add complexity
#[test]
fn test_emoji_rendering_in_split() -> Result<(), CompositorError> {
    use std::sync::Arc;

    let unique_id = format!(
        "{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let temp_dir = std::env::temp_dir().join(format!("shell_test_emoji_{}", unique_id));
    std::fs::create_dir_all(&temp_dir).unwrap();
    let history_path = temp_dir.join("test_history.log");

    let core = Arc::new(
        libshell::ShellCore::with_history_path(history_path.clone())
            .expect("Failed to create ShellCore"),
    );

    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_core(80, 24, Arc::new(Mutex::new(writer.clone())), core)?;
    compositor.set_fixed_time(fixed_test_time());
    compositor.render_to_vec();

    // Create horizontal split
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "
    compositor.render_to_vec();

    // Test simple emoji first
    compositor.handle_input("echo '😀😃😄😁😆'".as_bytes());
    compositor.handle_input(b"\r");
    compositor.render_to_vec();

    let lines = compositor.get_text_lines();
    let text: String = lines.join("\n");

    // Verify the emoji appears in output (either in command echo or as typed)
    assert!(
        text.contains("😀"),
        "Expected '😀' in output. Got:\n{}",
        text
    );

    // Verify delta render matches compositor state
    let render_output = compositor.render_to_vec();
    let mut replay_emulator = emulator::TerminalEmulator::new(80, 24);
    replay_emulator.process(&render_output);

    let _compositor_lines = compositor.get_text_lines();
    let replay_lines: Vec<String> = (0..24)
        .map(|y| replay_emulator.grid().get_line_text(y))
        .collect();

    // At least check that some emoji made it through
    let replay_text: String = replay_lines.join("\n");
    assert!(
        replay_text.contains("😀") || replay_text.contains("👍") || replay_text.contains("🔴"),
        "Replay should contain at least some emoji. Got:\n{}",
        replay_text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

/// Test unicode rendering with bash subprocess in a horizontal split.
///
/// This test uses the bash subprocess (not the embedded shell) to print
/// unicode characters, which tests the terminal emulation and incremental
/// rendering path without going through the embedded shell's input handling.
///
/// The user reported that Ctrl+B r (force redraw) clears rendering artifacts,
/// which suggests the terminal emulation is correct but incremental drawing
/// to the real terminal has issues with unicode character widths.
#[test]
fn test_hsplit_unicode_via_bash() -> Result<(), CompositorError> {
    // Use bash subprocess instead of embedded shell
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Create horizontal split
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "

    // Wait for the new pane's bash to start
    wait_for_output(&mut compositor, 500);

    // Switch to top pane
    compositor.handle_input(&[0x0b]); // Ctrl+k

    // Use printf to output unicode via bash - this bypasses the embedded shell's
    // input handling and tests pure terminal emulation
    compositor.handle_input(b"printf '\\xe4\\xb8\\xad\\xe6\\x96\\x87\\n'\n");  // 中文 in UTF-8
    wait_for_output(&mut compositor, 300);

    // Also print some box drawing and symbols
    compositor.handle_input(b"printf '\\xe2\\x94\\x8c\\xe2\\x94\\x80\\xe2\\x94\\x90\\n'\n");  // ┌─┐
    wait_for_output(&mut compositor, 300);

    // Get compositor state after incremental rendering
    compositor.render_to_vec();
    let compositor_lines = compositor.get_text_lines();
    let compositor_text = compositor_lines.join("\n");

    // The compositor should contain the CJK characters
    assert!(
        compositor_text.contains("中文"),
        "Compositor should contain CJK '中文' from bash printf. Got:\n{}",
        compositor_text
    );

    // Now test the delta rendering by replaying to a fresh emulator
    let render_output = compositor.render_to_vec();
    let mut replay_emulator = emulator::TerminalEmulator::new(80, 24);
    replay_emulator.process(&render_output);

    let replay_lines: Vec<String> = (0..24)
        .map(|y| replay_emulator.grid().get_line_text(y))
        .collect();
    let replay_text = replay_lines.join("\n");

    // Replay should also contain the CJK characters
    assert!(
        replay_text.contains("中文"),
        "Replay should contain CJK '中文'.\nReplay:\n{}\nCompositor:\n{}",
        replay_text,
        compositor_text
    );

    // Box drawing should also work
    assert!(
        replay_text.contains("┌") || compositor_text.contains("┌"),
        "Should contain box drawing character.\nReplay:\n{}\nCompositor:\n{}",
        replay_text,
        compositor_text
    );

    Ok(())
}

/// Test that dense wide characters in vertical split don't cause rendering artifacts.
///
/// Vertical splits are especially prone to issues because:
/// 1. Panes are side-by-side with precise column boundaries
/// 2. Wide characters (2 columns) can extend past the expected column
/// 3. The border between panes must be drawn precisely
#[test]
fn test_vsplit_dense_unicode_via_bash() -> Result<(), CompositorError> {
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    compositor.set_fixed_time(fixed_test_time());
    wait_for_output(&mut compositor, 500);

    // Create vertical split
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'%']); // %

    wait_for_output(&mut compositor, 500);

    // Switch to left pane
    compositor.handle_input(&[0x08]); // Ctrl+h

    // Print a line of CJK characters near the border
    // In a 80-column terminal split vertically, each pane is ~39 columns
    // 15 CJK chars = 30 columns, should fit but tests width calculation
    compositor.handle_input(b"printf '\\xe6\\x97\\xa5\\xe6\\x9c\\xac\\xe8\\xaa\\x9e\\xe4\\xb8\\xad\\xe5\\x9b\\xbd\\xe8\\xaa\\x9e\\xe9\\x9f\\x93\\xe5\\x9b\\xbd\\xe8\\xaa\\x9e\\n'\n");  // 日本語中国語韓国語
    wait_for_output(&mut compositor, 300);

    compositor.render_to_vec();
    let compositor_lines = compositor.get_text_lines();
    let compositor_text = compositor_lines.join("\n");

    // Verify CJK rendered in left pane
    assert!(
        compositor_text.contains("日本語") || compositor_text.contains("中国語"),
        "Left pane should contain CJK. Got:\n{}",
        compositor_text
    );

    // Switch to right pane and print different content
    compositor.handle_input(&[0x0c]); // Ctrl+l
    compositor.handle_input(b"printf 'RIGHT_PANE\\n'\n");
    wait_for_output(&mut compositor, 300);

    compositor.render_to_vec();
    let compositor_lines = compositor.get_text_lines();
    let compositor_text = compositor_lines.join("\n");

    // Both panes should show their content
    assert!(
        compositor_text.contains("RIGHT_PANE"),
        "Right pane should contain marker. Got:\n{}",
        compositor_text
    );

    // Test incremental vs full redraw
    let incremental_lines = compositor.get_text_lines();
    let incremental_text = incremental_lines.join("\n");

    compositor.force_full_redraw();

    let full_redraw_lines = compositor.get_text_lines();
    let full_redraw_text = full_redraw_lines.join("\n");

    // Content should be identical after both render methods
    assert_eq!(
        incremental_text, full_redraw_text,
        "Incremental and full redraw should match.\nIncremental:\n{}\n\nFull:\n{}",
        incremental_text, full_redraw_text
    );

    Ok(())
}

/// Test vim Ctrl+D scroll behavior in a split.
///
/// This tests that after opening vim and pressing Ctrl+D (scroll down half page),
/// the terminal state is rendered correctly without corruption.
///
/// There's a known bug where the status line gets corrupted after Ctrl+D,
/// showing something like: `#[cfg(test)]im/fixtures/test_code.rs" 84L, 2343B`
/// which is a mix of file content and vim's status line.
#[test]
fn test_vim_ctrl_d_scroll() -> Result<(), CompositorError> {
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    compositor.set_fixed_time(fixed_test_time());
    wait_for_output(&mut compositor, 500);

    // Create horizontal split
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "
    wait_for_output(&mut compositor, 500);

    // Get absolute path to the test file using workspace root
    // CARGO_MANIFEST_DIR points to crates/compositor, go up to workspace root
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let workspace_root = std::path::Path::new(manifest_dir).parent().unwrap().parent().unwrap();
    let test_file = workspace_root.join("crates/libvim/fixtures/test_code.rs");
    
    // Open vim on the test file
    let vim_cmd = format!("vim {}\n", test_file.display());
    compositor.handle_input(vim_cmd.as_bytes());
    wait_for_output(&mut compositor, 1000);

    // Get initial state before Ctrl+D
    compositor.render_to_vec();
    let initial_lines = compositor.get_text_lines();
    let initial_text = initial_lines.join("\n");

    // Verify vim opened the file (should see file content)
    assert!(
        initial_text.contains("emit_charset_designation") || initial_text.contains("fn "),
        "Vim should display file content. Got:\n{}",
        initial_text
    );

    // Press Ctrl+D to scroll down half a page
    compositor.handle_input(&[0x04]); // Ctrl+D
    wait_for_output(&mut compositor, 500);

    // Render and capture state
    compositor.render_to_vec();
    let after_ctrl_d_lines = compositor.get_text_lines();
    save_fixture("vim_ctrl_d_fixture.txt", &after_ctrl_d_lines);

    let after_ctrl_d_text = after_ctrl_d_lines.join("\n");

    // Look for corruption patterns: file content mixed with status line
    // The corrupted line might look like:
    // - `#[cfg(test)]im/fixtures/test_code.rs" 84L, 2343B`
    // - `<ocuments/code/shell/crates/libvim/fixtures/test_code.rs" 84L, 2343B`
    // - Other patterns where vim status line gets mixed with code
    let has_corruption = after_ctrl_d_text.contains("#[cfg(test)]im/fixtures")
        || after_ctrl_d_text.contains("]im/fixtures/test_code.rs")
        || after_ctrl_d_text.contains("<ocuments/code")
        || after_ctrl_d_text.contains("cuments/code/shell");

    // For now, just document the corruption - this test demonstrates the bug
    if has_corruption {
        eprintln!("DETECTED CORRUPTION: Status line mixed with file content");
        eprintln!("Output:\n{}", after_ctrl_d_text);
    }

    // The status line should NOT contain file content mixed in
    // This assertion will fail if the bug exists
    assert!(
        !has_corruption,
        "Status line should not be corrupted with file content. Got:\n{}",
        after_ctrl_d_text
    );

    // Quit vim
    compositor.handle_input(b":q!\n");
    wait_for_output(&mut compositor, 300);

    Ok(())
}

/// Test that unicode characters are properly cleared from the terminal.
///
/// This test demonstrates a bug where delta rendering fails to properly clear
/// wide unicode characters. After echoing CJK text, typing more text, and
/// running `clear`, the screen should be blank but unicode artifacts remain.
#[test]
fn test_unicode_clear_bug() -> Result<(), CompositorError> {
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    compositor.set_fixed_time(fixed_test_time());
    wait_for_output(&mut compositor, 500);

    // Create horizontal split
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "
    wait_for_output(&mut compositor, 500);

    // Echo a string with lots of CJK characters (each is 2 columns wide)
    compositor.handle_input(b"echo '\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e\xe6\xbc\xa2\xe5\xad\x97\xe4\xb8\xad\xe5\x9b\xbd\xe8\xaa\x9e\xe9\x9f\x93\xe5\x9b\xbd\xe8\xaa\x9e\xe5\x8f\xb0\xe6\xb9\xbe\xe8\xaa\x9e\xe9\xa6\x99\xe6\xb8\xaf\xe8\xaa\x9e\xe6\x96\xb0\xe5\x8a\xa0\xe5\x9d\xa1\xe8\xaa\x9e\xe9\xa6\xac\xe4\xbe\x86\xe8\xa5\xbf\xe4\xba\x9e'\n");
    // This is: 日本語漢字中国語韓国語台湾語香港語新加坡語馬來西亞
    wait_for_output(&mut compositor, 500);

    // Render after echo
    compositor.render_to_vec();
    let after_echo_lines = compositor.get_text_lines();
    let after_echo_text = after_echo_lines.join("\n");

    // Verify the CJK text appeared
    assert!(
        after_echo_text.contains("日本語") || after_echo_text.contains("中国語"),
        "Should contain CJK text after echo. Got:\n{}",
        after_echo_text
    );

    // Type hello world and press enter
    compositor.handle_input(b"hello world\n");
    wait_for_output(&mut compositor, 300);

    // Render and snapshot the "garbled" state
    compositor.render_to_vec();
    let garbled_lines = compositor.get_text_lines();
    save_fixture("unicode_clear_before.txt", &garbled_lines);

    // Now run clear to clear the screen
    compositor.handle_input(b"clear\n");
    wait_for_output(&mut compositor, 500);

    // Render after clear
    compositor.render_to_vec();
    let after_clear_lines = compositor.get_text_lines();
    save_fixture("unicode_clear_after.txt", &after_clear_lines);

    let after_clear_text = after_clear_lines.join("\n");

    // The screen should NOT contain any CJK characters after clear
    let has_cjk_artifacts = after_clear_text.contains("日")
        || after_clear_text.contains("本")
        || after_clear_text.contains("語")
        || after_clear_text.contains("漢")
        || after_clear_text.contains("中")
        || after_clear_text.contains("国");

    if has_cjk_artifacts {
        eprintln!("DETECTED UNICODE CLEAR BUG: CJK characters remain after clear");
        eprintln!("After clear:\n{}", after_clear_text);
    }

    // This assertion will fail if the bug exists
    assert!(
        !has_cjk_artifacts,
        "Screen should be clear of CJK characters after `clear`. Got:\n{}",
        after_clear_text
    );

    Ok(())
}

/// Test that delta rendering properly clears unicode characters.
///
/// This test uses bash subprocess to echo unicode characters, then runs `clear`
/// to clear the screen, verifying that no unicode artifacts remain.
#[test]
fn test_unicode_delta_clear() -> Result<(), CompositorError> {
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;
    compositor.set_fixed_time(fixed_test_time());

    // Wait for bash to start
    wait_for_output(&mut compositor, 500);

    // Create horizontal split
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "
    wait_for_output(&mut compositor, 500);

    // Echo CJK characters using printf (bash)
    compositor.handle_input(b"printf '\\xe6\\x97\\xa5\\xe6\\x9c\\xac\\xe8\\xaa\\x9e\\xe6\\xbc\\xa2\\xe5\\xad\\x97\\xe4\\xb8\\xad\\xe5\\x9b\\xbd\\xe8\\xaa\\x9e\\n'\n");
    wait_for_output(&mut compositor, 500);

    compositor.render_to_vec();
    let after_echo_lines = compositor.get_text_lines();
    let after_echo_text = after_echo_lines.join("\n");
    save_fixture("unicode_delta_after_echo.txt", &after_echo_lines);

    // Verify CJK appeared
    assert!(
        after_echo_text.contains("日本語") || after_echo_text.contains("中国語"),
        "Should contain CJK after printf. Got:\n{}",
        after_echo_text
    );

    // Type hello world
    compositor.handle_input(b"echo 'hello world'\n");
    wait_for_output(&mut compositor, 300);

    compositor.render_to_vec();
    let after_hello_lines = compositor.get_text_lines();
    save_fixture("unicode_delta_after_hello.txt", &after_hello_lines);

    // Run clear command
    compositor.handle_input(b"clear\n");
    wait_for_output(&mut compositor, 500);

    compositor.render_to_vec();
    let after_clear_lines = compositor.get_text_lines();
    save_fixture("unicode_delta_after_clear.txt", &after_clear_lines);

    let after_clear_text = after_clear_lines.join("\n");

    // Check for CJK artifacts
    let has_cjk = after_clear_text.contains("日")
        || after_clear_text.contains("本")
        || after_clear_text.contains("語")
        || after_clear_text.contains("漢")
        || after_clear_text.contains("中")
        || after_clear_text.contains("国");

    if has_cjk {
        eprintln!("DETECTED UNICODE DELTA CLEAR BUG");
        eprintln!("After clear:\n{}", after_clear_text);
    }

    assert!(
        !has_cjk,
        "Screen should be clear after `clear` command. Got:\n{}",
        after_clear_text
    );

    // Test what the delta renderer produces
    // Create a fresh emulator and apply the delta
    let delta = compositor.render_to_vec();
    let mut replay_emulator = emulator::TerminalEmulator::new(80, 24);
    replay_emulator.process(&delta);

    let replay_lines: Vec<String> = (0..24)
        .map(|y| replay_emulator.grid().get_line_text(y))
        .collect();
    let replay_text = replay_lines.join("\n");

    let replay_has_cjk = replay_text.contains("日")
        || replay_text.contains("本")
        || replay_text.contains("語");

    assert!(
        !replay_has_cjk,
        "Replay of delta should not contain CJK after clear. Got:\n{}",
        replay_text
    );

    Ok(())
}

/// Test that delta rendering properly clears wide unicode characters.
///
/// This is a unit test for the delta rendering logic. When we transition from
/// a grid with wide characters to a grid with spaces, the delta must output
/// enough spaces to cover the full width of the previous characters.
#[test]
fn test_delta_render_clears_wide_chars() {
    // Create an emulator and write some wide CJK characters
    let mut emu_with_cjk = emulator::TerminalEmulator::new(80, 24);
    // Write CJK characters - each is 2 columns wide
    emu_with_cjk.process("日本語中国語".as_bytes());
    
    // Create a blank emulator (simulating cleared screen)
    let blank_emu = emulator::TerminalEmulator::new(80, 24);
    
    // Compute delta from CJK grid to blank grid
    let delta = emulator::compute_delta(emu_with_cjk.grid(), blank_emu.grid());
    
    // Apply the delta to the CJK emulator
    emu_with_cjk.process(&delta);
    
    // Get the text from the first line
    let line_text = emu_with_cjk.grid().get_line_text(0);
    
    // The line should be blank (no CJK characters)
    let has_cjk = line_text.contains("日") 
        || line_text.contains("本")
        || line_text.contains("語")
        || line_text.contains("中")
        || line_text.contains("国");
    
    assert!(
        !has_cjk,
        "After applying delta to blank, line should not contain CJK. Got: '{}'",
        line_text
    );
    
    // The line should be all spaces or empty
    assert!(
        line_text.trim().is_empty(),
        "Line should be blank after clearing. Got: '{}'",
        line_text
    );
}

/// Test delta rendering with a more complex scenario: wide chars followed by narrow.
#[test]
fn test_delta_render_wide_to_narrow() {
    // Emulator with wide CJK characters
    let mut emu1 = emulator::TerminalEmulator::new(80, 24);
    emu1.process("日本語中".as_bytes()); // 4 chars = 8 columns
    
    // Emulator with narrow ASCII characters
    let mut emu2 = emulator::TerminalEmulator::new(80, 24);
    emu2.process(b"ABCDEFGH"); // 8 chars = 8 columns
    
    // Compute delta
    let delta = emulator::compute_delta(emu1.grid(), emu2.grid());
    
    // Apply delta to emu1
    emu1.process(&delta);
    
    // Get the text
    let line_text = emu1.grid().get_line_text(0);
    
    // Should contain the ASCII text, not CJK
    assert!(
        line_text.starts_with("ABCDEFGH"),
        "Should contain 'ABCDEFGH' after delta. Got: '{}'",
        line_text
    );
    
    // Should NOT contain any CJK
    let has_cjk = line_text.contains("日") || line_text.contains("本");
    assert!(
        !has_cjk,
        "Should not contain CJK after replacing with ASCII. Got: '{}'",
        line_text
    );
}

/// Test that replacing wide chars with narrow chars at same position works correctly.
#[test]
fn test_delta_overwrite_wide_char() {
    // Emulator with a wide character at position 0
    let mut emu1 = emulator::TerminalEmulator::new(80, 24);
    emu1.process("日".as_bytes()); // 1 wide char = 2 columns
    
    // Emulator with two narrow characters at positions 0 and 1
    let mut emu2 = emulator::TerminalEmulator::new(80, 24);
    emu2.process(b"AB"); // 2 chars = 2 columns
    
    // Compute delta
    let delta = emulator::compute_delta(emu1.grid(), emu2.grid());
    
    // Apply delta to emu1
    emu1.process(&delta);
    
    // Get cell contents
    let cell0 = emu1.grid().get_cell(0, 0).character;
    let cell1 = emu1.grid().get_cell(1, 0).character;
    
    // Both cells should now be A and B
    assert_eq!(cell0, 'A', "Cell 0 should be 'A', got '{}'", cell0);
    assert_eq!(cell1, 'B', "Cell 1 should be 'B', got '{}'", cell1);
}

/// Test that Ctrl+W (delete word backward) works correctly with unicode characters.
///
/// This test reproduces a crash where:
/// 1. Type `echo 日本語漢字`
/// 2. Press Ctrl+E (move to end of line)
/// 3. Press Ctrl+W (delete word backward)
/// The shell should not crash when deleting unicode words.
#[test]
fn test_ctrl_w_unicode_crash() -> Result<(), CompositorError> {
    use std::sync::Arc;

    let unique_id = format!(
        "{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let temp_dir = std::env::temp_dir().join(format!("shell_test_ctrlw_unicode_{}", unique_id));
    std::fs::create_dir_all(&temp_dir).unwrap();
    let history_path = temp_dir.join("test_history.log");

    let core = Arc::new(
        libshell::ShellCore::with_history_path(history_path.clone())
            .expect("Failed to create ShellCore"),
    );

    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_core(80, 24, Arc::new(Mutex::new(writer.clone())), core)?;
    compositor.set_fixed_time(fixed_test_time());

    // Initial render
    compositor.render_to_vec();

    // Create horizontal split
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'"']); // "
    compositor.render_to_vec();

    // Type echo with CJK characters
    compositor.handle_input("echo 日本語漢字".as_bytes());
    compositor.render_to_vec();

    let before_lines = compositor.get_text_lines();
    let before_text = before_lines.join("\n");

    // Verify the CJK text was typed
    assert!(
        before_text.contains("日本語漢字"),
        "Should contain CJK text before Ctrl+W. Got:\n{}",
        before_text
    );

    // Press Ctrl+E to move to end of line
    compositor.handle_input(&[0x05]); // Ctrl+E
    compositor.render_to_vec();

    // Press Ctrl+W to delete word backward - this should NOT crash
    compositor.handle_input(&[0x17]); // Ctrl+W
    compositor.render_to_vec();

    let after_lines = compositor.get_text_lines();
    let after_text = after_lines.join("\n");

    // After Ctrl+W, the CJK word should be deleted
    // The line should now just show "echo " or similar
    assert!(
        !after_text.contains("日本語漢字"),
        "CJK text should be deleted after Ctrl+W. Got:\n{}",
        after_text
    );

    // "echo " should still be present
    assert!(
        after_text.contains("echo"),
        "'echo' should still be present after Ctrl+W. Got:\n{}",
        after_text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

/// Test that clearing wide CJK characters works correctly with delta rendering.
///
/// This reproduces the bug where after running `clear`, only half of each CJK
/// character is cleared because the delta renderer was not accounting for
/// wide character widths.
#[test]
fn test_wide_char_clear_delta() {
    // Create an emulator and write CJK characters
    let mut emu = emulator::TerminalEmulator::new(80, 24);
    // Write a line of CJK characters (each is 2 columns wide)
    emu.process("日本語漢字中国語\n".as_bytes());
    
    // Now create a "cleared" state - all spaces
    let cleared = emulator::TerminalEmulator::new(80, 24);
    
    // Compute the delta from CJK-filled to cleared
    let delta = emulator::compute_delta(emu.grid(), cleared.grid());
    
    // Apply the delta to the CJK emulator
    emu.process(&delta);
    
    // Get the first line's text
    let line0 = emu.grid().get_line_text(0);
    let line1 = emu.grid().get_line_text(1);
    
    // Both lines should be blank - no CJK remnants
    let has_cjk_line0 = line0.contains("日") || line0.contains("本") || line0.contains("語");
    let has_cjk_line1 = line1.contains("日") || line1.contains("本") || line1.contains("語");
    
    assert!(
        !has_cjk_line0,
        "Line 0 should be cleared of CJK. Got: '{}'",
        line0
    );
    
    assert!(
        !has_cjk_line1,
        "Line 1 should be cleared of CJK. Got: '{}'",
        line1
    );
    
    // All cells in the first row should be spaces
    for x in 0..20 {
        let cell = emu.grid().get_cell(x, 0);
        assert_eq!(
            cell.character, ' ',
            "Cell at ({}, 0) should be space, got '{}'",
            x, cell.character
        );
    }
}

/// Test echo of CJK characters - reproduces corruption bug
#[test]
fn test_cjk_echo_corruption() -> Result<(), CompositorError> {
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);
    
    // Check pane's terminal state BEFORE typing
    {
        let pane = compositor.get_focused_pane_mut().unwrap();
        // Check both grid_cache and inner alacritty state
        let cache_line = pane.terminal_emulator.grid().get_line_text(0);
        let inner_line = pane.terminal_emulator.inner().get_line_text(0);
        eprintln!("BEFORE typing:");
        eprintln!("  Cache line 0: {}", cache_line);
        eprintln!("  Inner line 0: {}", inner_line);
    }

    // Echo CJK characters
    compositor.handle_input(b"echo '\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e\xe6\xbc\xa2\xe5\xad\x97\xe4\xb8\xad\xe5\x9b\xbd\xe8\xaa\x9e\xe9\x9f\x93\xe5\x9b\xbd\xe8\xaa\x9e\xe5\x8f\xb0\xe6\xb9\xbe'\n");
    wait_for_output(&mut compositor, 500);
    
    // Check pane's terminal state AFTER typing - compare cache vs inner
    {
        let pane = compositor.get_focused_pane_mut().unwrap();
        let cache_line0 = pane.terminal_emulator.grid().get_line_text(0);
        let cache_line1 = pane.terminal_emulator.grid().get_line_text(1);
        let inner_line0 = pane.terminal_emulator.inner().get_line_text(0);
        let inner_line1 = pane.terminal_emulator.inner().get_line_text(1);
        eprintln!("AFTER typing - Pane terminal state:");
        eprintln!("  Cache line 0: {}", cache_line0);
        eprintln!("  Cache line 1: {}", cache_line1);
        eprintln!("  Inner line 0: {}", inner_line0);
        eprintln!("  Inner line 1: {}", inner_line1);
    }

    // Get terminal state
    let lines = compositor.get_text_lines();
    let line0 = &lines[0];

    eprintln!("GLOBAL emulator state after echo:");
    for (i, line) in lines.iter().enumerate().take(5) {
        eprintln!("  Line {}: {}", i, line);
    }

    // The first line should NOT have repeated echo fragments like "eecechechoecho"
    // This pattern indicates keystroke echo corruption
    // Note: "echo" legitimately contains "ech", so we check for the repeated patterns only
    assert!(
        !line0.contains("eec") && !line0.contains("echoecho"),
        "Line 0 should not have corrupted echo fragments. Got:\n{}",
        line0
    );

    // The first line should be something like "compositor ➜ echo '日本語漢字中国語韓国語台湾'"
    // (the prompt followed by the command we typed)
    assert!(
        line0.contains("echo '") || line0.contains("echo \""),
        "Line 0 should contain the echo command. Got:\n{}",
        line0
    );
    
    // Verify the CJK characters are present
    assert!(
        line0.contains("日本語") && line0.contains("韓国語"),
        "Line 0 should contain CJK characters. Got:\n{}",
        line0
    );

    Ok(())
}

#[test]
fn test_cjk_with_cat() -> Result<(), CompositorError> {
    // This test uses cat to see simple echo behavior
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    compositor.set_fixed_time(fixed_test_time());
    wait_for_output(&mut compositor, 500);
    
    // Run cat as a subprocess
    compositor.handle_input(b"cat\n");
    wait_for_output(&mut compositor, 300);
    
    // Type some CJK characters
    compositor.handle_input("日本語\n".as_bytes());
    wait_for_output(&mut compositor, 300);
    
    // Check pane state
    {
        let pane = compositor.get_focused_pane_mut().unwrap();
        let inner_line0 = pane.terminal_emulator.inner().get_line_text(0);
        let inner_line1 = pane.terminal_emulator.inner().get_line_text(1);
        let inner_line2 = pane.terminal_emulator.inner().get_line_text(2);
        eprintln!("After cat with CJK:");
        eprintln!("  Inner line 0: {}", inner_line0);
        eprintln!("  Inner line 1: {}", inner_line1);
        eprintln!("  Inner line 2: {}", inner_line2);
    }
    
    // Exit cat
    compositor.handle_input(b"\x04"); // Ctrl+D
    wait_for_output(&mut compositor, 300);
    
    Ok(())
}
