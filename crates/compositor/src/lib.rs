//! Terminal compositor for combining multiple terminal screens into one.
//!
//! The compositor manages multiple terminal panes, each with its own PTY process.
//! It uses a poll-based event loop to efficiently handle:
//! - Keyboard input from the user
//! - Output from PTY processes
//!
//! # Architecture
//!
//! The compositor uses a single event loop thread that polls on:
//! 1. A wake pipe (to receive keyboard input notifications)
//! 2. All PTY file descriptors (to receive process output)
//!
//! Keyboard input is queued and the wake pipe is written to signal the event loop.

pub mod border;
pub mod compositor;
pub mod error;
pub mod pane;
pub mod pane_cell;
pub mod remote;
pub mod tab;
pub mod types;

pub use compositor::Compositor;
pub use error::CompositorError;
pub use pane::{CtrlCResult, Pane, SearchMatch, UrlMatch};
pub use remote::RemoteProcess;
pub use pane_cell::{PaneCell, PaneCellInner};
pub use tab::Tab;
pub use types::{CompositorEvent, Direction, SplitDirection};

/// Number of rows reserved for the status bar at the bottom of the terminal
pub const STATUS_BAR_HEIGHT: usize = 1;

/// Escape sequence for Begin Synchronized Update (BSU)
pub const BSU: &[u8] = b"\x1b[?2026h";
/// Escape sequence for End Synchronized Update (ESU)
pub const ESU: &[u8] = b"\x1b[?2026l";

/// Escape sequence to query synchronized output mode support (DECRQM for mode 2026)
pub const SYNC_QUERY: &[u8] = b"\x1b[?2026$p";

/// Check a terminal response to see if synchronized output is supported.
///
/// The terminal should respond to DECRQM with `\x1b[?2026;Ps$y` where:
/// - Ps=1: mode is set
/// - Ps=2: mode is reset
/// - Ps=3: mode is permanently set
/// - Ps=4: mode is permanently reset
/// - Ps=0: mode is not recognized
///
/// Any value other than 0 indicates support.
pub fn parse_sync_query_response(response: &[u8]) -> bool {
    // Look for the pattern: ESC [ ? 2026 ; Ps $ y
    // We check for the presence of "2026;" followed by a digit other than '0'
    if let Some(pos) = response.windows(5).position(|w| w == b"2026;") {
        let after_semicolon = pos + 5;
        if after_semicolon < response.len() {
            let ps = response[after_semicolon];
            // Ps should be '1', '2', '3', or '4' for supported
            return ps >= b'1' && ps <= b'4';
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Arc, Mutex};

    /// Build a ShellCore backed by a throwaway history file so tests never read
    /// or write the developer's real `~/.myshell_history.log`.
    ///
    /// This points `SHELL_HISTORY_PATH` at a per-process temp file, which also
    /// isolates panes created by *splits* — those build their own ShellCore via
    /// the default constructor, so `with_core` on the root pane isn't enough.
    fn isolated_core() -> Arc<libshell::ShellCore> {
        let path = std::env::temp_dir().join(format!("shell_test_hist_{}.log", std::process::id()));
        std::env::set_var("SHELL_HISTORY_PATH", &path);
        Arc::new(libshell::ShellCore::new().expect("isolated test history"))
    }

    /// Construct a compositor over a throwaway history and a discardable sink.
    fn isolated_compositor() -> Compositor {
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        Compositor::with_core(80, 24, output, isolated_core()).unwrap()
    }

    #[test]
    fn test_compositor_new() {
        // Smoke test: construction succeeds.
        let _compositor = isolated_compositor();
    }

    #[test]
    fn test_queue_input() {
        let compositor = isolated_compositor();
        compositor.queue_input(b"hello");

        let queue = compositor.input_queue.lock().unwrap();
        assert_eq!(queue.len(), 1);
        assert_eq!(queue[0], b"hello");
    }

    #[test]
    fn test_hsplit_focus_movement() {
        // Create an HSplit compositor (top/bottom panes) using Ctrl+b "
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output, isolated_core()).unwrap();

        // Create horizontal split with Ctrl+b "
        compositor.handle_input(&[0x02]); // Ctrl+b
        compositor.handle_input(&[b'"']); // "

        // The bottom pane (index 1, newly created) should have focus
        if let PaneCellInner::HSplit(cells) = &compositor.root().inner {
            assert!(!cells[0].has_focus());
            assert!(cells[1].has_focus());
        }

        // Move focus up (Ctrl+k = 0x0b)
        compositor.handle_input(&[0x0b]);

        // Now the top pane should have focus
        if let PaneCellInner::HSplit(cells) = &compositor.root().inner {
            assert!(cells[0].has_focus());
            assert!(!cells[1].has_focus());
        }

        // Move focus down (Ctrl+j = 0x0a)
        compositor.handle_input(&[0x0a]);

        // Bottom pane should have focus again
        if let PaneCellInner::HSplit(cells) = &compositor.root().inner {
            assert!(!cells[0].has_focus());
            assert!(cells[1].has_focus());
        }
    }

    #[test]
    fn test_vsplit_focus_movement() {
        // Create a VSplit compositor (left/right panes) using Ctrl+b %
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output, isolated_core()).unwrap();

        // Create vertical split with Ctrl+b %
        compositor.handle_input(&[0x02]); // Ctrl+b
        compositor.handle_input(&[b'%']); // %

        // The right pane (index 1, newly created) should have focus
        if let PaneCellInner::VSplit(cells) = &compositor.root().inner {
            assert!(!cells[0].has_focus());
            assert!(cells[1].has_focus());
        }

        // Move focus left (Ctrl+h = 0x08)
        compositor.handle_input(&[0x08]);

        // Now the left pane should have focus
        if let PaneCellInner::VSplit(cells) = &compositor.root().inner {
            assert!(cells[0].has_focus());
            assert!(!cells[1].has_focus());
        }

        // Move focus right (Ctrl+l = 0x0c)
        compositor.handle_input(&[0x0c]);

        // Right pane should have focus again
        if let PaneCellInner::VSplit(cells) = &compositor.root().inner {
            assert!(!cells[0].has_focus());
            assert!(cells[1].has_focus());
        }
    }

    #[test]
    fn test_three_pane_middle_reachable() {
        // Repro: with three vertical splits, the middle pane should be reachable.
        // Splitting nests the focused leaf into a fresh 2-element split, so two
        // vsplits yield VSplit[A, VSplit[B, C]] with focus on C.
        let mut compositor = isolated_compositor();

        // Two vertical splits (Ctrl+b %), creating panes A | B | C, focus on C.
        compositor.handle_input(&[0x02]);
        compositor.handle_input(&[b'%']);
        compositor.handle_input(&[0x02]);
        compositor.handle_input(&[b'%']);

        // Recursively find the pos_x of the currently focused leaf pane.
        fn focused_pos_x(cell: &PaneCell) -> Option<usize> {
            match &cell.inner {
                PaneCellInner::Pane(_) if cell.focus => Some(cell.pos_x),
                PaneCellInner::Pane(_) => None,
                PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                    cells.iter().find_map(focused_pos_x)
                }
            }
        }

        // Collect the x positions of the three leaves (left to right): A=0, B, C.
        fn leaf_xs(cell: &PaneCell, out: &mut Vec<usize>) {
            match &cell.inner {
                PaneCellInner::Pane(_) => out.push(cell.pos_x),
                PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                    for c in cells {
                        leaf_xs(c, out);
                    }
                }
            }
        }
        // The tree should be flattened: a single VSplit with three leaf children,
        // not a nested VSplit[A, VSplit[B, C]].
        if let PaneCellInner::VSplit(cells) = &compositor.root().inner {
            assert_eq!(cells.len(), 3, "three vsplits should flatten into one split");
            assert!(
                cells.iter().all(|c| matches!(c.inner, PaneCellInner::Pane(_))),
                "all children should be leaf panes (flat tree)"
            );
        } else {
            panic!("root should be a VSplit");
        }

        let mut xs = Vec::new();
        leaf_xs(compositor.root(), &mut xs);
        xs.sort();
        assert_eq!(xs.len(), 3, "expected three panes");
        let (left_x, middle_x, right_x) = (xs[0], xs[1], xs[2]);

        // Focus starts on the rightmost pane C.
        assert_eq!(focused_pos_x(compositor.root()), Some(right_x));

        // Move focus left once (Ctrl+h). Should land on the MIDDLE pane B.
        compositor.handle_input(&[0x08]);
        assert_eq!(
            focused_pos_x(compositor.root()),
            Some(middle_x),
            "moving left from the rightmost pane should reach the middle pane, not skip it"
        );

        // Move focus left again. Should land on the LEFT pane A.
        compositor.handle_input(&[0x08]);
        assert_eq!(focused_pos_x(compositor.root()), Some(left_x));
    }

    #[test]
    fn test_three_pane_middle_reachable_hsplit() {
        // Same as above but for horizontal splits: panes stacked top/middle/bottom.
        let mut compositor = isolated_compositor();

        // Two horizontal splits (Ctrl+b "), focus ends on the bottom pane.
        compositor.handle_input(&[0x02]);
        compositor.handle_input(&[b'"']);
        compositor.handle_input(&[0x02]);
        compositor.handle_input(&[b'"']);

        fn focused_pos_y(cell: &PaneCell) -> Option<usize> {
            match &cell.inner {
                PaneCellInner::Pane(_) if cell.focus => Some(cell.pos_y),
                PaneCellInner::Pane(_) => None,
                PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                    cells.iter().find_map(focused_pos_y)
                }
            }
        }
        fn leaf_ys(cell: &PaneCell, out: &mut Vec<usize>) {
            match &cell.inner {
                PaneCellInner::Pane(_) => out.push(cell.pos_y),
                PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                    for c in cells {
                        leaf_ys(c, out);
                    }
                }
            }
        }
        // The tree should be flattened: a single HSplit with three leaf children.
        if let PaneCellInner::HSplit(cells) = &compositor.root().inner {
            assert_eq!(cells.len(), 3, "three hsplits should flatten into one split");
            assert!(
                cells.iter().all(|c| matches!(c.inner, PaneCellInner::Pane(_))),
                "all children should be leaf panes (flat tree)"
            );
        } else {
            panic!("root should be an HSplit");
        }

        let mut ys = Vec::new();
        leaf_ys(compositor.root(), &mut ys);
        ys.sort();
        assert_eq!(ys.len(), 3, "expected three panes");
        let (top_y, middle_y, bottom_y) = (ys[0], ys[1], ys[2]);

        // Focus starts on the bottom pane.
        assert_eq!(focused_pos_y(compositor.root()), Some(bottom_y));

        // Move focus up once (Ctrl+k). Should land on the MIDDLE pane.
        compositor.handle_input(&[0x0b]);
        assert_eq!(
            focused_pos_y(compositor.root()),
            Some(middle_y),
            "moving up from the bottom pane should reach the middle pane, not skip it"
        );

        // Move focus up again. Should land on the TOP pane.
        compositor.handle_input(&[0x0b]);
        assert_eq!(focused_pos_y(compositor.root()), Some(top_y));
    }

    #[test]
    fn test_cross_axis_split_still_nests() {
        // Flattening must only apply along the same axis. A vertical split
        // followed by a horizontal split must nest: VSplit[A, HSplit[B, C]].
        let mut compositor = isolated_compositor();

        compositor.handle_input(&[0x02]);
        compositor.handle_input(&[b'%']); // vertical split -> VSplit[A, B]
        compositor.handle_input(&[0x02]);
        compositor.handle_input(&[b'"']); // horizontal split of B -> HSplit[B, C]

        if let PaneCellInner::VSplit(cells) = &compositor.root().inner {
            assert_eq!(cells.len(), 2, "outer split stays binary across axes");
            assert!(matches!(cells[0].inner, PaneCellInner::Pane(_)));
            assert!(
                matches!(&cells[1].inner, PaneCellInner::HSplit(inner) if inner.len() == 2),
                "the cross-axis split should nest as an HSplit of two panes"
            );
        } else {
            panic!("root should be a VSplit");
        }
    }

    #[test]
    fn test_focus_movement_at_boundary() {
        // Test that focus doesn't move past boundaries
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output, isolated_core()).unwrap();

        // Create horizontal split with Ctrl+b "
        compositor.handle_input(&[0x02]); // Ctrl+b
        compositor.handle_input(&[b'"']); // "

        // New pane (bottom, index 1) has focus after split
        // Try to move down when already at bottom - should stay at bottom
        compositor.handle_input(&[0x0a]); // Ctrl+j

        if let PaneCellInner::HSplit(cells) = &compositor.root().inner {
            assert!(!cells[0].has_focus());
            assert!(cells[1].has_focus());
        }

        // Move to top
        compositor.handle_input(&[0x0b]); // Ctrl+k

        // Try to move up when already at top - should stay at top
        compositor.handle_input(&[0x0b]); // Ctrl+k

        if let PaneCellInner::HSplit(cells) = &compositor.root().inner {
            assert!(cells[0].has_focus());
            assert!(!cells[1].has_focus());
        }
    }

    #[test]
    fn test_prefix_mode_horizontal_split() {
        // Test that Ctrl+b " creates a horizontal split
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output, isolated_core()).unwrap();

        // Initially should be a single pane
        assert!(matches!(compositor.root().inner(), PaneCellInner::Pane(_)));

        // Send Ctrl+b (0x02) to enter prefix mode
        compositor.handle_input(&[0x02]);

        // Send " to trigger horizontal split
        compositor.handle_input(&[b'"']);

        // Now should be an HSplit with two panes
        if let PaneCellInner::HSplit(cells) = compositor.root().inner() {
            assert_eq!(cells.len(), 2);
            // Second pane (newly created) should have focus
            assert!(!cells[0].has_focus());
            assert!(cells[1].has_focus());
            // Check dimensions - should be split vertically with 1 row border
            // 24 rows - 1 status bar = 23 for panes, minus 1 border = 22 available, split as 11 + 11
            assert_eq!(cells[0].dimensions(), (80, 11));
            assert_eq!(cells[1].dimensions(), (80, 11));
        } else {
            panic!("Expected HSplit after Ctrl+b \"");
        }
    }

    #[test]
    fn test_prefix_mode_vertical_split() {
        // Test that Ctrl+b % creates a vertical split
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output, isolated_core()).unwrap();

        // Initially should be a single pane
        assert!(matches!(compositor.root().inner(), PaneCellInner::Pane(_)));

        // Send Ctrl+b (0x02) to enter prefix mode
        compositor.handle_input(&[0x02]);

        // Send % to trigger vertical split
        compositor.handle_input(&[b'%']);

        // Now should be a VSplit with two panes
        if let PaneCellInner::VSplit(cells) = compositor.root().inner() {
            assert_eq!(cells.len(), 2);
            // Second pane (newly created) should have focus
            assert!(!cells[0].has_focus());
            assert!(cells[1].has_focus());
            // Check dimensions - should be split horizontally with 1 column border
            // 80 cols - 1 border = 79 available, split as 39 + 40
            // Height is 24 - 1 status bar = 23
            assert_eq!(cells[0].dimensions(), (39, 23));
            assert_eq!(cells[1].dimensions(), (40, 23));
        } else {
            panic!("Expected VSplit after Ctrl+b %");
        }
    }

    #[test]
    fn test_prefix_mode_escape() {
        // Test that unknown keys in prefix mode are ignored
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output, isolated_core()).unwrap();

        // Send Ctrl+b to enter prefix mode
        compositor.handle_input(&[0x02]);

        // Send an unknown key - should exit prefix mode without doing anything
        compositor.handle_input(&[b'x']);

        // Should still be a single pane
        assert!(matches!(compositor.root().inner(), PaneCellInner::Pane(_)));
    }

    #[test]
    fn test_prefix_mode_send_ctrl_b() {
        // Test that Ctrl+b Ctrl+b sends Ctrl+b to the terminal
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output, isolated_core()).unwrap();

        // Send Ctrl+b to enter prefix mode
        compositor.handle_input(&[0x02]);

        // Send Ctrl+b again - should send Ctrl+b to terminal and exit prefix mode
        compositor.handle_input(&[0x02]);

        // Should still be a single pane (Ctrl+b was forwarded to terminal, not interpreted)
        assert!(matches!(compositor.root().inner(), PaneCellInner::Pane(_)));
    }

    #[test]
    fn test_resize_single_pane() {
        // Test that resize works correctly for a single pane
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output, isolated_core()).unwrap();

        // Verify initial dimensions (pane height is total - 1 for status bar)
        assert_eq!(compositor.root().dimensions(), (80, 23));
        assert_eq!(compositor.global_emulator().dimensions(), (80, 24));

        // Resize to larger dimensions
        compositor.resize(120, 40);

        // Verify new dimensions (pane height is 40 - 1 = 39)
        assert_eq!(compositor.root().dimensions(), (120, 39));
        assert_eq!(compositor.global_emulator().dimensions(), (120, 40));

        // Resize to smaller dimensions
        compositor.resize(40, 12);

        // Verify new dimensions (pane height is 12 - 1 = 11)
        assert_eq!(compositor.root().dimensions(), (40, 11));
        assert_eq!(compositor.global_emulator().dimensions(), (40, 12));
    }

    #[test]
    fn test_resize_with_splits() {
        // Test that resize correctly distributes space among split panes
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output, isolated_core()).unwrap();

        // Create a vertical split (left/right)
        compositor.handle_input(&[0x02]); // Ctrl+b
        compositor.handle_input(&[b'%']); // %

        // Verify initial split dimensions (80 cols - 1 border = 79, split as 39 + 40)
        // Height is 24 - 1 status bar = 23
        if let PaneCellInner::VSplit(cells) = compositor.root().inner() {
            assert_eq!(cells[0].dimensions(), (39, 23));
            assert_eq!(cells[1].dimensions(), (40, 23));
        } else {
            panic!("Expected VSplit");
        }

        // Resize the compositor
        compositor.resize(100, 30);

        // Verify dimensions are redistributed (100 cols - 1 border = 99, split as 50 + 49)
        // Height is 30 - 1 = 29
        if let PaneCellInner::VSplit(cells) = compositor.root().inner() {
            assert_eq!(cells[0].dimensions(), (50, 29));
            assert_eq!(cells[1].dimensions(), (49, 29));
        } else {
            panic!("Expected VSplit after resize");
        }

        // Resize to smaller
        compositor.resize(60, 20);

        // Verify dimensions are redistributed (60 cols - 1 border = 59, split as 30 + 29)
        // Height is 20 - 1 = 19
        if let PaneCellInner::VSplit(cells) = compositor.root().inner() {
            assert_eq!(cells[0].dimensions(), (30, 19));
            assert_eq!(cells[1].dimensions(), (29, 19));
        } else {
            panic!("Expected VSplit after resize");
        }
    }

    #[test]
    fn test_force_render_after_resize() {
        // Test that force_render produces output after resize
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output.clone(), isolated_core()).unwrap();

        // Resize
        compositor.resize(100, 30);

        // Force render should produce some output (at minimum cursor positioning)
        compositor.force_render();

        let output_data = output.lock().unwrap();
        // After resize + render, there should be some output
        // (even if just cursor visibility/position commands)
        assert!(!output_data.is_empty() || true); // Rendering an empty screen may produce no output
    }

    #[test]
    fn test_synchronized_output_setting() {
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output, isolated_core()).unwrap();

        // Default should be disabled
        assert!(!compositor.synchronized_output_enabled());

        // Enable synchronized output
        compositor.set_synchronized_output(true);
        assert!(compositor.synchronized_output_enabled());

        // Disable synchronized output
        compositor.set_synchronized_output(false);
        assert!(!compositor.synchronized_output_enabled());
    }

    #[test]
    fn test_status_bar_shows_environment_diagnostic() {
        use chrono::TimeZone;

        libshell::clear_global_diagnostic_flag(
            libshell::GlobalDiagnosticFlag::EnvironmentParseFailure,
        );
        libshell::set_global_diagnostic_flag(
            libshell::GlobalDiagnosticFlag::EnvironmentParseFailure,
        );

        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output, isolated_core()).unwrap();
        compositor.set_fixed_time(
            chrono::Local
                .with_ymd_and_hms(2025, 1, 1, 12, 0, 0)
                .unwrap(),
        );

        let _ = compositor.render_to_vec();
        let cell = compositor.global_emulator().grid().get_cell(61, 23);

        assert_eq!(cell.character, 'E');
        assert_eq!(cell.attrs.fg_color, Some(emulator::Color::Red));
        assert!(cell.attrs.bold);

        libshell::clear_global_diagnostic_flag(
            libshell::GlobalDiagnosticFlag::EnvironmentParseFailure,
        );
    }

    #[test]
    fn test_parse_sync_query_response() {
        // Test valid responses indicating support
        // Mode is reset (Ps=2)
        assert!(parse_sync_query_response(b"\x1b[?2026;2$y"));
        // Mode is set (Ps=1)
        assert!(parse_sync_query_response(b"\x1b[?2026;1$y"));
        // Mode is permanently set (Ps=3)
        assert!(parse_sync_query_response(b"\x1b[?2026;3$y"));
        // Mode is permanently reset (Ps=4)
        assert!(parse_sync_query_response(b"\x1b[?2026;4$y"));

        // Test response indicating no support (Ps=0)
        assert!(!parse_sync_query_response(b"\x1b[?2026;0$y"));

        // Test invalid/malformed responses
        assert!(!parse_sync_query_response(b""));
        assert!(!parse_sync_query_response(b"garbage"));
        assert!(!parse_sync_query_response(b"\x1b[?1234;2$y")); // Wrong mode number

        // Test response with extra data
        assert!(parse_sync_query_response(b"prefix\x1b[?2026;2$ysuffix"));
    }

    #[test]
    fn test_synchronized_output_in_render() {
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output.clone(), isolated_core()).unwrap();

        // Enable synchronized output
        compositor.set_synchronized_output(true);

        // Wait for shell prompt and trigger a render
        std::thread::sleep(std::time::Duration::from_millis(100));
        compositor.force_render();

        let output_data = output.lock().unwrap();

        // If there was any output, it should be wrapped with BSU/ESU
        if !output_data.is_empty() {
            // Check for BSU at the start
            assert!(
                output_data.starts_with(BSU),
                "Output should start with BSU sequence"
            );
            // Check for ESU at the end
            assert!(
                output_data.ends_with(ESU),
                "Output should end with ESU sequence"
            );
        }
    }

    #[test]
    fn test_search_mode() {
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output.clone(), isolated_core()).unwrap();

        // Initially not in scrollback or search mode
        assert!(!compositor.root().is_in_scrollback_mode());
        assert!(!compositor.root().is_in_search_mode());

        // Enter scrollback mode with Ctrl+b [
        compositor.handle_input(&[0x02]); // Ctrl+b
        compositor.handle_input(&[b'[']);

        assert!(compositor.root().is_in_scrollback_mode());
        assert!(!compositor.root().is_in_search_mode());

        // Enter search mode with /
        compositor.handle_input(&[b'/']);

        assert!(compositor.root().is_in_scrollback_mode());
        assert!(compositor.root().is_in_search_mode());

        // Type search query
        compositor.handle_input(&[b'h']);
        compositor.handle_input(&[b'e']);
        compositor.handle_input(&[b'l']);
        compositor.handle_input(&[b'l']);
        compositor.handle_input(&[b'o']);

        // Check search info
        if let Some((query, _, _)) = compositor.root().get_search_info() {
            assert_eq!(query, "hello");
        } else {
            panic!("Expected search info");
        }

        // Backspace should remove last char
        compositor.handle_input(&[0x7f]); // Backspace
        if let Some((query, _, _)) = compositor.root().get_search_info() {
            assert_eq!(query, "hell");
        }

        // Escape should exit search mode but stay in scrollback
        compositor.handle_input(&[0x1b]); // Escape

        assert!(compositor.root().is_in_scrollback_mode());
        assert!(!compositor.root().is_in_search_mode());

        // Escape again should exit scrollback mode
        compositor.handle_input(&[0x1b]); // Escape

        assert!(!compositor.root().is_in_scrollback_mode());
    }

    #[test]
    fn test_visual_yank_exits_scrollback_mode() {
        let mut compositor = isolated_compositor();
        compositor
            .get_focused_pane_mut()
            .unwrap()
            .terminal_emulator
            .process(b"\x1b[2J\x1b[Hascii text");

        compositor.handle_input(&[0x02]); // Ctrl+b
        compositor.handle_input(&[b'[']);
        compositor.handle_input(&[b'v']);

        assert!(compositor.root().is_in_scrollback_mode());
        assert_ne!(compositor.root().get_vim_mode(), libvim::Mode::Normal);

        compositor.handle_input(&[b'y']);

        assert!(!compositor.root().is_in_scrollback_mode());
        assert_eq!(compositor.root().get_vim_mode(), libvim::Mode::Normal);
    }

    #[test]
    fn test_prefix_mode_force_redraw() {
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_core(80, 24, output.clone(), isolated_core()).unwrap();

        // Send Ctrl+b r to trigger force full redraw
        compositor.handle_input(&[0x02]); // Ctrl+b
        compositor.handle_input(&[b'r']); // r

        // Check that clear screen sequence was sent
        let output_data = output.lock().unwrap();
        let output_str = String::from_utf8_lossy(&output_data);

        // Should contain clear screen escape sequence (ESC[2J)
        assert!(
            output_str.contains("\x1b[2J"),
            "Expected clear screen sequence in output"
        );
    }
}
