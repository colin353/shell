//! Tests for syntax highlighting in the shell
//!
//! These tests verify that syntax highlighting works correctly when typing
//! commands at the shell prompt.

use compositor::{Compositor, CompositorError};
use std::io::Write;
use std::sync::{Arc, Mutex};

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

/// Save raw bytes as a fixture for debugging (shows ANSI codes)
#[allow(dead_code)]
fn save_raw_fixture(name: &str, data: &[u8]) {
    let fixture_path = format!("{}/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name);
    // Convert to string, escaping non-printable characters
    let escaped: String = data
        .iter()
        .map(|&b| {
            if b == 0x1b {
                "\\x1b".to_string()
            } else if b == b'\n' {
                "\\n".to_string()
            } else if b == b'\r' {
                "\\r".to_string()
            } else if b >= 0x20 && b < 0x7f {
                (b as char).to_string()
            } else {
                format!("\\x{:02x}", b)
            }
        })
        .collect();
    std::fs::write(&fixture_path, escaped).expect("Failed to write fixture");
}

#[test]
fn test_syntax_highlighting_echo() -> Result<(), CompositorError> {
    // Create a temporary directory for the test history file
    let unique_id = format!(
        "{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let temp_dir = std::env::temp_dir().join(format!("shell_test_syntax_{}", unique_id));
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
    save_fixture("syntax_highlight_initial.txt", &initial_lines);

    // Type "echo hello world" character by character
    compositor.handle_input(b"echo hello world");

    // Capture the raw render output with ANSI codes
    let raw_output = compositor.render_to_vec();
    save_raw_fixture("syntax_highlight_echo_hello_world_raw.txt", &raw_output);

    let after_typing_lines = compositor.get_text_lines();
    save_fixture("syntax_highlight_echo_hello_world.txt", &after_typing_lines);

    // Verify the text appears correctly
    let text: String = after_typing_lines.join("\n");
    assert!(
        text.contains("echo hello world"),
        "Expected 'echo hello world' in output. Got:\n{}",
        text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

#[test]
fn test_syntax_highlighting_git_command() -> Result<(), CompositorError> {
    // Create a temporary directory for the test history file
    let unique_id = format!(
        "{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let temp_dir = std::env::temp_dir().join(format!("shell_test_syntax_git_{}", unique_id));
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

    // Type "git status --short"
    compositor.handle_input(b"git status --short");
    compositor.render_to_vec();

    let lines = compositor.get_text_lines();
    save_fixture("syntax_highlight_git_status.txt", &lines);

    // Verify the text appears correctly
    let text: String = lines.join("\n");
    assert!(
        text.contains("git status --short"),
        "Expected 'git status --short' in output. Got:\n{}",
        text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

#[test]
fn test_syntax_highlighting_env_var() -> Result<(), CompositorError> {
    // Create a temporary directory for the test history file
    let unique_id = format!(
        "{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let temp_dir = std::env::temp_dir().join(format!("shell_test_syntax_env_{}", unique_id));
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

    // Type "echo $HOME"
    compositor.handle_input(b"echo $HOME");
    compositor.render_to_vec();

    let lines = compositor.get_text_lines();
    save_fixture("syntax_highlight_env_var.txt", &lines);

    // Verify the text appears correctly
    let text: String = lines.join("\n");
    assert!(
        text.contains("echo $HOME"),
        "Expected 'echo $HOME' in output. Got:\n{}",
        text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

#[test]
fn test_syntax_highlighting_ctrl_w() -> Result<(), CompositorError> {
    // Create a temporary directory for the test history file
    let unique_id = format!(
        "{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let temp_dir = std::env::temp_dir().join(format!("shell_test_syntax_ctrlw_{}", unique_id));
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

    // Type "echo hello world"
    compositor.handle_input(b"echo hello world");
    compositor.render_to_vec();

    let before_lines = compositor.get_text_lines();
    save_fixture("syntax_highlight_before_ctrl_w.txt", &before_lines);

    // Press Ctrl+W to delete "world"
    compositor.handle_input(&[0x17]); // Ctrl+W
    compositor.render_to_vec();

    let after_ctrl_w_lines = compositor.get_text_lines();
    save_fixture("syntax_highlight_after_ctrl_w.txt", &after_ctrl_w_lines);

    // Verify the text now shows "echo hello " (with trailing space)
    let text: String = after_ctrl_w_lines.join("\n");
    assert!(
        text.contains("echo hello ") && !text.contains("world"),
        "Expected 'echo hello ' without 'world' in output. Got:\n{}",
        text
    );

    // Verify prompt is still there
    assert!(
        text.contains("➜"),
        "Expected prompt arrow in output. Got:\n{}",
        text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}

#[test]
fn test_syntax_highlighting_backspace() -> Result<(), CompositorError> {
    // Create a temporary directory for the test history file
    let unique_id = format!(
        "{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let temp_dir = std::env::temp_dir().join(format!("shell_test_syntax_bs_{}", unique_id));
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

    // Type "echoX" then backspace to fix typo
    compositor.handle_input(b"echoX");
    compositor.render_to_vec();

    // Press backspace to delete the X
    compositor.handle_input(&[0x7f]); // Backspace
    compositor.render_to_vec();

    let after_backspace_lines = compositor.get_text_lines();
    save_fixture(
        "syntax_highlight_after_backspace.txt",
        &after_backspace_lines,
    );

    // Verify the text now shows "echo" (not "echoX")
    let text: String = after_backspace_lines.join("\n");
    assert!(
        text.contains("compositor ➜ echo") && !text.contains("echoX"),
        "Expected 'echo' without 'X' in output. Got:\n{}",
        text
    );

    // Clean up
    let _ = std::fs::remove_file(&history_path);
    let _ = std::fs::remove_dir_all(&temp_dir);

    Ok(())
}
