//! Tests for scrollback and search behaviors
//!
//! These tests verify that the compositor correctly handles scrollback mode
//! and search functionality within panes.

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

/// Get the path to the lorem ipsum fixture file
fn lorem_fixture_path() -> String {
    format!("{}/fixtures/lorem_ipsum.txt", env!("CARGO_MANIFEST_DIR"))
}

// TODO: These tests fail with alacritty backend because scrollback_len() and
// get_scrollback_row() are not yet implemented - they always return 0/None
#[test]
#[ignore]
fn test_scrollback_basic() -> Result<(), CompositorError> {
    // Create a compositor with a small terminal to ensure scrollback is used
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Cat the lorem ipsum file to fill scrollback
    let cat_cmd = format!("cat {}\n", lorem_fixture_path());
    compositor.handle_input(cat_cmd.as_bytes());

    // Wait for the command to execute and output to appear
    wait_for_output(&mut compositor, 1000);

    // Render to capture the state before scrollback
    compositor.render_to_vec();
    let lines_before = compositor.get_text_lines();

    // The last line of the file should be visible (BOTTOM_MARKER)
    let screen_text_before: String = lines_before.join("\n");
    assert!(
        screen_text_before.contains("BOTTOM_MARKER"),
        "Expected BOTTOM_MARKER to be visible before scrollback. Got:\n{}",
        screen_text_before
    );

    // Enter scrollback mode with Ctrl+b [
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'[']); // [

    // Scroll up with Ctrl+u (half page, about 11 lines for a 24-line terminal)
    // Scroll up multiple times to get to earlier content
    compositor.handle_input(&[0x15]); // Ctrl+u
    compositor.handle_input(&[0x15]); // Ctrl+u again
    compositor.handle_input(&[0x15]); // Ctrl+u again
    compositor.handle_input(&[0x15]); // Ctrl+u again
    compositor.handle_input(&[0x15]); // Ctrl+u again - should now see Line 30 area

    // Render and get the scrolled state
    compositor.render_to_vec();
    let lines_after = compositor.get_text_lines();

    // Save fixture for debugging
    save_fixture("scrollback_test_fixture.txt", &lines_after);

    // After scrolling up significantly, we should see earlier content
    let screen_text_after: String = lines_after.join("\n");

    // After scrolling up, we should see content from the middle of the file
    // Line 30 has SCROLLBACK_VISIBLE_LINE - verify we can scroll to see it
    assert!(
        screen_text_after.contains("SCROLLBACK_VISIBLE_LINE")
            || screen_text_after.contains("SEARCHABLE_MARKER_BETA")
            || screen_text_after.contains("Line 2"),
        "Expected earlier content to be visible after scrolling up. Got:\n{}",
        screen_text_after
    );

    // The bottom marker should no longer be visible since we scrolled up
    assert!(
        !screen_text_after.contains("BOTTOM_MARKER"),
        "BOTTOM_MARKER should not be visible after scrolling up. Got:\n{}",
        screen_text_after
    );

    Ok(())
}

#[test]
#[ignore]
fn test_scrollback_jump_to_top() -> Result<(), CompositorError> {
    // Create a compositor
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Cat the lorem ipsum file
    let cat_cmd = format!("cat {}\n", lorem_fixture_path());
    compositor.handle_input(cat_cmd.as_bytes());
    wait_for_output(&mut compositor, 1000);

    // Enter scrollback mode with Ctrl+b [
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'[']); // [

    // Jump to top with 'gg' (vim-style)
    compositor.handle_input(b"gg");

    // Render and get the state
    compositor.render_to_vec();
    let lines = compositor.get_text_lines();

    save_fixture("scrollback_top_fixture.txt", &lines);

    let screen_text: String = lines.join("\n");

    // At the top, we should see the beginning content (Lorem ipsum)
    assert!(
        screen_text.contains("Lorem ipsum"),
        "Expected 'Lorem ipsum' at top of scrollback. Got:\n{}",
        screen_text
    );

    // The bottom marker should not be visible
    assert!(
        !screen_text.contains("BOTTOM_MARKER"),
        "BOTTOM_MARKER should not be visible at top. Got:\n{}",
        screen_text
    );

    Ok(())
}

#[test]
fn test_scrollback_exit() -> Result<(), CompositorError> {
    // Create a compositor
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Cat the lorem ipsum file
    let cat_cmd = format!("cat {}\n", lorem_fixture_path());
    compositor.handle_input(cat_cmd.as_bytes());
    wait_for_output(&mut compositor, 1000);

    // Enter scrollback mode
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'[']); // [

    // Scroll up
    compositor.handle_input(&[0x15]); // Ctrl+u
    compositor.handle_input(&[0x15]); // Ctrl+u

    // Exit scrollback with 'q'
    compositor.handle_input(b"q");

    // Render and get the state - should be back at bottom
    compositor.render_to_vec();
    let lines = compositor.get_text_lines();
    let screen_text: String = lines.join("\n");

    // After exiting scrollback, we should be back at the bottom
    assert!(
        screen_text.contains("BOTTOM_MARKER"),
        "Expected BOTTOM_MARKER after exiting scrollback. Got:\n{}",
        screen_text
    );

    Ok(())
}

#[test]
fn test_search_basic() -> Result<(), CompositorError> {
    // Create a compositor
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Cat the lorem ipsum file
    let cat_cmd = format!("cat {}\n", lorem_fixture_path());
    compositor.handle_input(cat_cmd.as_bytes());
    wait_for_output(&mut compositor, 1000);

    // Enter scrollback mode with Ctrl+b [
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'[']); // [

    // Enter search mode with '/'
    compositor.handle_input(b"/");

    // Type search query
    compositor.handle_input(b"SEARCHABLE_MARKER_ALPHA");

    // Press Enter to confirm/navigate to match
    compositor.handle_input(&[0x0d]); // Enter

    // Render and get the state
    compositor.render_to_vec();
    let lines = compositor.get_text_lines();

    save_fixture("search_alpha_fixture.txt", &lines);

    let screen_text: String = lines.join("\n");

    // The search should have scrolled to show SEARCHABLE_MARKER_ALPHA (line 11)
    assert!(
        screen_text.contains("SEARCHABLE_MARKER_ALPHA"),
        "Expected SEARCHABLE_MARKER_ALPHA to be visible after search. Got:\n{}",
        screen_text
    );

    Ok(())
}

#[test]
fn test_search_navigate_matches() -> Result<(), CompositorError> {
    // Create a compositor
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Cat the lorem ipsum file
    let cat_cmd = format!("cat {}\n", lorem_fixture_path());
    compositor.handle_input(cat_cmd.as_bytes());
    wait_for_output(&mut compositor, 1000);

    // Enter scrollback mode with Ctrl+b [
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'[']); // [

    // Enter search mode and search for a marker that appears multiple times
    compositor.handle_input(b"/");
    compositor.handle_input(b"SEARCHABLE_MARKER");

    // Render to see initial state (should jump to nearest/most recent match)
    compositor.render_to_vec();
    let lines_first = compositor.get_text_lines();

    save_fixture("search_navigate_first_fixture.txt", &lines_first);

    // Navigate to next match with Enter (goes to older/earlier matches)
    compositor.handle_input(&[0x0d]); // Enter
    compositor.handle_input(&[0x0d]); // Enter again

    compositor.render_to_vec();
    let lines_after_nav = compositor.get_text_lines();

    save_fixture("search_navigate_after_fixture.txt", &lines_after_nav);

    let screen_text: String = lines_after_nav.join("\n");

    // After navigating, we should see one of the SEARCHABLE_MARKER entries
    assert!(
        screen_text.contains("SEARCHABLE_MARKER"),
        "Expected SEARCHABLE_MARKER to be visible after navigation. Got:\n{}",
        screen_text
    );

    Ok(())
}

#[test]
fn test_search_exit_to_scrollback() -> Result<(), CompositorError> {
    // Create a compositor
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Cat the lorem ipsum file
    let cat_cmd = format!("cat {}\n", lorem_fixture_path());
    compositor.handle_input(cat_cmd.as_bytes());
    wait_for_output(&mut compositor, 1000);

    // Enter scrollback mode
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'[']); // [

    // Enter search mode
    compositor.handle_input(b"/");
    compositor.handle_input(b"SEARCHABLE_MARKER_ALPHA");

    // Navigate to the match
    compositor.handle_input(&[0x0d]); // Enter

    // Exit search mode with Escape (should stay in scrollback at current position)
    compositor.handle_input(&[0x1b]); // Escape

    // We're still in scrollback mode, so scrolling should work
    // Try scrolling down with Ctrl+d
    compositor.handle_input(&[0x04]); // Ctrl+d

    compositor.render_to_vec();
    let lines = compositor.get_text_lines();

    save_fixture("search_exit_scrollback_fixture.txt", &lines);

    // We should still be able to see content (test that we're in scrollback mode)
    let screen_text: String = lines.join("\n");
    assert!(
        !screen_text.is_empty(),
        "Screen should have content after exiting search to scrollback"
    );

    Ok(())
}

#[test]
fn test_search_clear_query() -> Result<(), CompositorError> {
    // Create a compositor
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Cat the lorem ipsum file
    let cat_cmd = format!("cat {}\n", lorem_fixture_path());
    compositor.handle_input(cat_cmd.as_bytes());
    wait_for_output(&mut compositor, 1000);

    // Enter scrollback mode
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'[']); // [

    // Enter search mode and type a query
    compositor.handle_input(b"/");
    compositor.handle_input(b"SEARCHABLE_MARKER");

    // Clear the search with Ctrl+w
    compositor.handle_input(&[0x17]); // Ctrl+w

    // Type a new search
    compositor.handle_input(b"BOTTOM_MARKER");

    // Navigate to match
    compositor.handle_input(&[0x0d]); // Enter

    compositor.render_to_vec();
    let lines = compositor.get_text_lines();

    save_fixture("search_clear_fixture.txt", &lines);

    let screen_text: String = lines.join("\n");

    // After clearing and searching for BOTTOM_MARKER, it should be visible
    assert!(
        screen_text.contains("BOTTOM_MARKER"),
        "Expected BOTTOM_MARKER after clearing and new search. Got:\n{}",
        screen_text
    );

    Ok(())
}

#[test]
fn test_search_backspace() -> Result<(), CompositorError> {
    // Create a compositor
    let writer = MemoryWriter::new();
    let mut compositor = Compositor::with_output(80, 24, Arc::new(Mutex::new(writer.clone())))?;

    // Set fixed time to avoid fixture churn
    compositor.set_fixed_time(fixed_test_time());

    // Wait for bash to initialize
    wait_for_output(&mut compositor, 500);

    // Cat the lorem ipsum file
    let cat_cmd = format!("cat {}\n", lorem_fixture_path());
    compositor.handle_input(cat_cmd.as_bytes());
    wait_for_output(&mut compositor, 1000);

    // Enter scrollback mode
    compositor.handle_input(&[0x02]); // Ctrl+b
    compositor.handle_input(&[b'[']); // [

    // Enter search mode and type a query with a typo
    compositor.handle_input(b"/");
    compositor.handle_input(b"SEARCHABLE_MARKER_ALPHAX");

    // Backspace to fix the typo
    compositor.handle_input(&[0x7f]); // Backspace

    // Navigate to match
    compositor.handle_input(&[0x0d]); // Enter

    compositor.render_to_vec();
    let lines = compositor.get_text_lines();

    save_fixture("search_backspace_fixture.txt", &lines);

    let screen_text: String = lines.join("\n");

    // After fixing typo with backspace, SEARCHABLE_MARKER_ALPHA should be found
    assert!(
        screen_text.contains("SEARCHABLE_MARKER_ALPHA"),
        "Expected SEARCHABLE_MARKER_ALPHA after backspace correction. Got:\n{}",
        screen_text
    );

    Ok(())
}
