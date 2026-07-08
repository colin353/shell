use crate::error::CompositorError;
use crate::pane::{CtrlCResult, Pane, PaneInputResult};
use crate::pane_cell::PaneCell;
use crate::tab::Tab;
use crate::types::{Direction, SplitDirection};
use crate::{BSU, ESU, STATUS_BAR_HEIGHT};
use emulator::{MouseEncoding, MouseMode, MouseReportMode};
use nix::unistd::{read, write};
use std::collections::VecDeque;
use std::io::Write;
use std::os::fd::{AsRawFd, OwnedFd, RawFd};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

/// Clock function type for getting current time (allows mocking in tests)
pub type ClockFn = Box<dyn Fn() -> chrono::DateTime<chrono::Local> + Send + Sync>;

/// Display width of a string in terminal columns (wide chars count as 2).
fn str_display_width(text: &str) -> usize {
    use unicode_width::UnicodeWidthStr;
    text.width()
}

/// Write `text` into `grid` starting at column `x` on row `y`, painting every cell
/// with `attrs`. Stops before reaching `max_x` (exclusive) and returns the column
/// after the last cell written.
///
/// Wide (2-column) characters are written as a leading glyph cell followed by a
/// `is_wide_char_spacer` cell, mirroring how the terminal emulator stores them.
/// This is required for correctness: `compute_delta` advances its cursor by each
/// glyph's display width, so a wide char placed without its spacer would shift
/// every following cell by one column (and a full repaint would not repair it).
/// Zero-width / control characters are skipped.
fn write_str_to_grid(
    grid: &mut emulator::TerminalGrid,
    mut x: usize,
    y: usize,
    text: &str,
    attrs: &emulator::CellAttributes,
    max_x: usize,
) -> usize {
    use unicode_width::UnicodeWidthChar;
    for ch in text.chars() {
        let w = ch.width().unwrap_or(0);
        if w == 0 {
            continue;
        }
        // Don't paint a glyph that would overflow the available region.
        if x + w > max_x {
            break;
        }
        grid.set_cell(x, y, emulator::Cell::new(ch, attrs.clone()));
        if w == 2 {
            let mut spacer = emulator::Cell::new(' ', attrs.clone());
            spacer.is_wide_char_spacer = true;
            grid.set_cell(x + 1, y, spacer);
        }
        x += w;
    }
    x
}

/// Truncate `text` so its display width does not exceed `max_width`, appending an
/// ellipsis ("...") when truncation occurs. Operates on whole characters, so it
/// never splits a multi-byte UTF-8 sequence.
fn truncate_to_width(text: &str, max_width: usize) -> String {
    use unicode_width::UnicodeWidthChar;
    if str_display_width(text) <= max_width {
        return text.to_string();
    }
    if max_width <= 3 {
        // No room for content alongside the ellipsis; just emit as many dots as fit.
        return ".".repeat(max_width);
    }
    let budget = max_width - 3;
    let mut out = String::new();
    let mut used = 0;
    for ch in text.chars() {
        let w = ch.width().unwrap_or(0);
        if used + w > budget {
            break;
        }
        out.push(ch);
        used += w;
    }
    out.push_str("...");
    out
}

fn find_prefix_key(input: &[u8]) -> Option<(usize, usize)> {
    for index in 0..input.len() {
        if input[index] == 0x02 {
            return Some((index, 1));
        }

        if let Some((0x02, len)) = decode_input_key(&input[index..]) {
            return Some((index, len));
        }
    }

    None
}

fn decode_input_key(input: &[u8]) -> Option<(u8, usize)> {
    let (params, final_byte, len) = decode_csi(input)?;

    match final_byte {
        b'u' => {
            let codepoint = params.first().copied()?;
            let modifiers = params.get(1).copied().unwrap_or(1);
            if is_ctrl_b(codepoint, modifiers) {
                Some((0x02, len))
            } else if modifiers <= 1 && (0x20..=0x7e).contains(&codepoint) {
                Some((codepoint as u8, len))
            } else {
                None
            }
        }
        b'~' => {
            let modifiers = params.get(1).copied()?;
            let codepoint = params.get(2).copied()?;
            if params.first().copied() == Some(27) && is_ctrl_b(codepoint, modifiers) {
                Some((0x02, len))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn is_ctrl_b(codepoint: u16, modifiers: u16) -> bool {
    (codepoint == b'b' as u16 || codepoint == b'B' as u16) && modifiers & 4 != 0
}

fn decode_csi(input: &[u8]) -> Option<(Vec<u16>, u8, usize)> {
    let params_start = if input.starts_with(b"\x1b[") {
        2
    } else if input.first().copied() == Some(0x9b) {
        1
    } else {
        return None;
    };

    let mut final_index = params_start;
    while final_index < input.len() {
        let byte = input[final_index];
        if (0x40..=0x7e).contains(&byte) {
            break;
        }

        if !byte.is_ascii_digit() && byte != b';' {
            return None;
        }
        final_index += 1;
    }

    if final_index == input.len() {
        return None;
    }

    let params = std::str::from_utf8(&input[params_start..final_index])
        .ok()?
        .split(';')
        .map(|param| {
            if param.is_empty() {
                Some(0)
            } else {
                param.parse::<u16>().ok()
            }
        })
        .collect::<Option<Vec<_>>>()?;

    Some((params, input[final_index], final_index + 1))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct SgrMouseEvent {
    button: u16,
    x: usize,
    y: usize,
    final_byte: u8,
    len: usize,
}

fn parse_sgr_mouse_event(input: &[u8]) -> Option<SgrMouseEvent> {
    if !input.starts_with(b"\x1b[<") {
        return None;
    }

    let final_index = input
        .iter()
        .position(|&byte| byte == b'M' || byte == b'm')?;
    let params = std::str::from_utf8(&input[3..final_index]).ok()?;
    let mut parts = params.split(';');
    let button = parts.next()?.parse::<u16>().ok()?;
    let x = parts.next()?.parse::<usize>().ok()?.checked_sub(1)?;
    let y = parts.next()?.parse::<usize>().ok()?.checked_sub(1)?;
    if parts.next().is_some() {
        return None;
    }

    Some(SgrMouseEvent {
        button,
        x,
        y,
        final_byte: input[final_index],
        len: final_index + 1,
    })
}

fn encode_mouse_event(
    event: SgrMouseEvent,
    local_x: usize,
    local_y: usize,
    encoding: MouseEncoding,
) -> Option<Vec<u8>> {
    match encoding {
        MouseEncoding::Sgr => Some(
            format!(
                "\x1b[<{};{};{}{}",
                event.button,
                local_x + 1,
                local_y + 1,
                event.final_byte as char
            )
            .into_bytes(),
        ),
        MouseEncoding::Normal | MouseEncoding::Utf8 => {
            let legacy_button = if event.final_byte == b'm' {
                (event.button & !0b11) | 0b11
            } else {
                event.button
            };
            let button = legacy_button.checked_add(32)?;
            let x = u16::try_from(local_x + 1).ok()?.checked_add(32)?;
            let y = u16::try_from(local_y + 1).ok()?.checked_add(32)?;
            if button > u8::MAX as u16 || x > u8::MAX as u16 || y > u8::MAX as u16 {
                return None;
            }
            Some(vec![0x1b, b'[', b'M', button as u8, x as u8, y as u8])
        }
    }
}

fn host_mouse_sequence(mode: MouseMode) -> Vec<u8> {
    let mut sequence =
        Vec::from(b"\x1b[?1000l\x1b[?1002l\x1b[?1003l\x1b[?1005l\x1b[?1006l".as_slice());

    let report_sequence = match mode.report {
        MouseReportMode::None => return sequence,
        MouseReportMode::Click => b"\x1b[?1000h".as_slice(),
        MouseReportMode::Drag => b"\x1b[?1002h".as_slice(),
        MouseReportMode::Motion => b"\x1b[?1003h".as_slice(),
    };

    sequence.extend_from_slice(b"\x1b[?1006h");
    sequence.extend_from_slice(report_sequence);
    sequence
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn truncate_to_width_leaves_short_text() {
        assert_eq!(truncate_to_width("https://x", 20), "https://x");
    }

    #[test]
    fn truncate_to_width_adds_ellipsis() {
        assert_eq!(truncate_to_width("abcdefghij", 7), "abcd...");
        assert_eq!(str_display_width(&truncate_to_width("abcdefghij", 7)), 7);
    }

    #[test]
    fn truncate_to_width_never_splits_multibyte() {
        // A URL with wide (2-col) characters: truncation must respect display width
        // and never panic on a byte boundary.
        let url = "https://例え.test/路径/page";
        for max in 0..=str_display_width(url) + 2 {
            let out = truncate_to_width(url, max);
            assert!(out.is_char_boundary(out.len())); // trivially true, but proves no panic
            assert!(str_display_width(&out) <= max.max(0));
        }
    }

    #[test]
    fn write_str_to_grid_inserts_wide_char_spacer() {
        let mut grid = emulator::TerminalGrid::new(12, 1);
        let attrs = emulator::CellAttributes::default();
        // "中" is width 2; the next written cell must land at column 2, not 1.
        let next_x = write_str_to_grid(&mut grid, 0, 0, "中A", &attrs, 12);
        assert_eq!(next_x, 3);
        assert_eq!(grid.get_cell(0, 0).character, '中');
        assert!(!grid.get_cell(0, 0).is_wide_char_spacer);
        assert!(grid.get_cell(1, 0).is_wide_char_spacer);
        assert_eq!(grid.get_cell(2, 0).character, 'A');
    }

    #[test]
    fn write_str_to_grid_respects_max_x() {
        let mut grid = emulator::TerminalGrid::new(12, 1);
        let attrs = emulator::CellAttributes::default();
        // Only room for 1 column before max_x; a width-2 glyph must not be placed.
        let next_x = write_str_to_grid(&mut grid, 0, 0, "中", &attrs, 1);
        assert_eq!(next_x, 0);
        assert_eq!(grid.get_cell(0, 0).character, ' ');
    }

    #[test]
    fn status_bar_style_wide_text_survives_delta_roundtrip() {
        // Regression for the status-bar desync: a wide char placed via write_str_to_grid
        // must reproduce exactly through compute_delta applied to a real terminal, with
        // every following cell aligned. (Placing it without a spacer shifted them.)
        let cols = 20;
        let mut grid = emulator::TerminalGrid::new(cols, 1);
        let attrs = emulator::CellAttributes::default();
        write_str_to_grid(&mut grid, 0, 0, " 0 例 shell ", &attrs, cols);

        let blank = emulator::TerminalGrid::new(cols, 1);
        let delta = emulator::compute_delta(&blank, &grid);
        let mut real = emulator::TerminalEmulator::new(cols, 1);
        real.process(&delta);

        for x in 0..cols {
            let r = real.grid().get_cell(x, 0);
            let m = grid.get_cell(x, 0);
            let rc = if r.character.is_control() {
                ' '
            } else {
                r.character
            };
            let mc = if m.character.is_control() {
                ' '
            } else {
                m.character
            };
            assert_eq!(rc, mc, "char mismatch at column {x}");
            assert_eq!(r.attrs, m.attrs, "attr mismatch at column {x}");
        }
    }

    #[test]
    fn parses_sgr_mouse_event() {
        let event = parse_sgr_mouse_event(b"\x1b[<64;10;5Mabc").unwrap();
        assert_eq!(
            event,
            SgrMouseEvent {
                button: 64,
                x: 9,
                y: 4,
                final_byte: b'M',
                len: 11,
            }
        );
    }

    #[test]
    fn translates_sgr_mouse_coordinates() {
        let event = parse_sgr_mouse_event(b"\x1b[<64;10;5M").unwrap();
        let translated = encode_mouse_event(event, 2, 3, MouseEncoding::Sgr).unwrap();
        assert_eq!(translated, b"\x1b[<64;3;4M");
    }

    #[test]
    fn translates_release_to_legacy_mouse_encoding() {
        let event = parse_sgr_mouse_event(b"\x1b[<0;10;5m").unwrap();
        let translated = encode_mouse_event(event, 2, 3, MouseEncoding::Normal).unwrap();
        assert_eq!(translated, vec![0x1b, b'[', b'M', 35, 35, 36]);
    }

    #[test]
    fn host_mouse_sequence_uses_sgr_reporting() {
        let sequence = host_mouse_sequence(MouseMode {
            report: MouseReportMode::Drag,
            encoding: MouseEncoding::Normal,
        });

        assert!(sequence.ends_with(b"\x1b[?1006h\x1b[?1002h"));
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn prefers_wayland_clipboard_when_wayland_display_is_set() {
        let commands = linux_clipboard_commands_for(Some(std::ffi::OsStr::new("wayland-0")));

        assert_eq!(commands[0].program, "wl-copy");
        assert_eq!(commands[1].program, "xclip");
        assert_eq!(commands[2].program, "xsel");
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn uses_x11_clipboard_commands_without_wayland_display() {
        let commands = linux_clipboard_commands_for(None);

        assert_eq!(commands[0].program, "xclip");
        assert_eq!(commands[1].program, "xsel");
    }
}

/// The main compositor that manages terminal panes and the event loop.
pub struct Compositor {
    /// List of tabs, each with its own root pane tree
    pub tabs: Vec<Tab>,
    /// Index of the currently active tab
    pub active_tab: usize,
    /// Total terminal width
    pub width: usize,
    /// Total terminal height (including status bar)
    pub height: usize,

    // Global terminal emulator for compositing
    pub global_emulator: emulator::TerminalEmulator,
    // Previous frame for delta rendering
    pub prev_frame: emulator::TerminalGrid,
    // Output writer for rendering
    pub output: Arc<Mutex<dyn Write + Send>>,

    // Event loop communication
    pub wake_read: OwnedFd,
    pub wake_write: OwnedFd,
    pub input_queue: Mutex<VecDeque<Vec<u8>>>,

    // Prefix mode for tmux-style commands (Ctrl+b)
    pub prefix_mode: bool,

    // Whether the terminal supports synchronized output mode
    pub synchronized_output: bool,

    // Whether to draw (and reserve a row for) the status bar. Disabled in
    // `--bare` mode so a remote daemon embedded in a pane has no chrome.
    status_bar_visible: bool,

    host_mouse_mode: MouseMode,

    exit_requested: bool,

    // Clock function for getting current time (mockable for tests)
    clock: ClockFn,
}

impl Compositor {
    /// Create a new compositor with the given dimensions.
    ///
    /// This spawns a default shell in the root pane.
    /// Output will be written to stdout by default.
    pub fn new(width: usize, height: usize) -> Result<Self, CompositorError> {
        Self::with_output(width, height, Arc::new(Mutex::new(std::io::stdout())))
    }

    /// Create a new compositor with the given dimensions and custom output writer.
    ///
    /// This spawns a default shell in the root pane.
    pub fn with_output(
        width: usize,
        height: usize,
        output: Arc<Mutex<dyn Write + Send>>,
    ) -> Result<Self, CompositorError> {
        // Create the wake pipe for signaling keyboard input
        let (wake_read, wake_write) = nix::unistd::pipe().map_err(CompositorError::Pipe)?;

        // Set wake_read to non-blocking
        use nix::fcntl::{fcntl, FcntlArg, OFlag};
        let flags =
            fcntl(wake_read.as_raw_fd(), FcntlArg::F_GETFL).map_err(CompositorError::Fcntl)?;
        let new_flags = OFlag::from_bits_truncate(flags) | OFlag::O_NONBLOCK;
        fcntl(wake_read.as_raw_fd(), FcntlArg::F_SETFL(new_flags))
            .map_err(CompositorError::Fcntl)?;

        // Calculate pane height (total height minus status bar)
        let pane_height = height.saturating_sub(STATUS_BAR_HEIGHT);

        // Create the initial tab
        let tab = Tab::new("bash".to_string(), width, pane_height)?;

        Ok(Self {
            tabs: vec![tab],
            active_tab: 0,
            width,
            height,
            global_emulator: emulator::TerminalEmulator::new(width, height),
            prev_frame: emulator::TerminalGrid::new(width, height),
            output,
            wake_read,
            wake_write,
            input_queue: Mutex::new(VecDeque::new()),
            prefix_mode: false,
            synchronized_output: false,
            status_bar_visible: true,
            host_mouse_mode: MouseMode::default(),
            exit_requested: false,
            clock: Box::new(|| chrono::Local::now()),
        })
    }

    /// Create a new compositor with a custom ShellCore (for testing with pre-populated history).
    pub fn with_core(
        width: usize,
        height: usize,
        output: Arc<Mutex<dyn Write + Send>>,
        core: Arc<libshell::ShellCore>,
    ) -> Result<Self, CompositorError> {
        // Create the wake pipe for signaling keyboard input
        let (wake_read, wake_write) = nix::unistd::pipe().map_err(CompositorError::Pipe)?;

        // Set wake_read to non-blocking
        use nix::fcntl::{fcntl, FcntlArg, OFlag};
        let flags =
            fcntl(wake_read.as_raw_fd(), FcntlArg::F_GETFL).map_err(CompositorError::Fcntl)?;
        let new_flags = OFlag::from_bits_truncate(flags) | OFlag::O_NONBLOCK;
        fcntl(wake_read.as_raw_fd(), FcntlArg::F_SETFL(new_flags))
            .map_err(CompositorError::Fcntl)?;

        // Calculate pane height (total height minus status bar)
        let pane_height = height.saturating_sub(STATUS_BAR_HEIGHT);

        // Create the initial tab with the custom core
        let tab = Tab::with_core("shell".to_string(), width, pane_height, core)?;

        Ok(Self {
            tabs: vec![tab],
            active_tab: 0,
            width,
            height,
            global_emulator: emulator::TerminalEmulator::new(width, height),
            prev_frame: emulator::TerminalGrid::new(width, height),
            output,
            wake_read,
            wake_write,
            input_queue: Mutex::new(VecDeque::new()),
            prefix_mode: false,
            synchronized_output: false,
            status_bar_visible: true,
            host_mouse_mode: MouseMode::default(),
            exit_requested: false,
            clock: Box::new(|| chrono::Local::now()),
        })
    }

    /// Set a fixed time for rendering (useful for tests to avoid fixture churn).
    pub fn set_fixed_time(&mut self, time: chrono::DateTime<chrono::Local>) {
        self.clock = Box::new(move || time);
    }

    /// Queue keyboard input to be processed by the event loop.
    ///
    /// This method is thread-safe and can be called from any thread.
    /// It queues the input and wakes the event loop.
    pub fn queue_input(&self, input: &[u8]) {
        // Queue the input
        {
            let mut queue = self.input_queue.lock().unwrap();
            queue.push_back(input.to_vec());
        }

        // Wake the event loop by writing to the wake pipe
        let _ = write(&self.wake_write, &[1u8]);
    }

    /// Handle input directly (for synchronous usage).
    ///
    /// This immediately sends the input to the focused pane's PTY.
    /// Intercepts Ctrl+h/j/k/l to move focus between panes (vim-style navigation).
    /// Supports tmux-style Ctrl+b prefix for compositor commands:
    /// - Ctrl+b " : Split horizontally (top/bottom)
    /// - Ctrl+b % : Split vertically (left/right)
    /// - Ctrl+b c : Create new tab
    /// - Ctrl+b 1-9 : Switch to tab 1-9
    /// - Ctrl+b [ : Enter scrollback mode
    /// - Ctrl+b r : Force full screen redraw
    ///
    /// Returns `true` if the compositor should exit, `false` otherwise.
    pub fn handle_input(&mut self, input: &[u8]) -> bool {
        if input.is_empty() {
            return false;
        }

        if let Some(event) = parse_sgr_mouse_event(input) {
            self.handle_mouse_event(event);
            if event.len < input.len() {
                return self.handle_input(&input[event.len..]);
            }
            return false;
        }

        // Check if we're in scrollback mode
        if self.active_tab().root.is_in_scrollback_mode() {
            self.handle_scrollback_input(input);
            return false;
        }

        // Handle prefix mode commands
        if self.prefix_mode {
            self.prefix_mode = false;
            let (command, command_len) = decode_input_key(input).unwrap_or((input[0], 1));
            self.handle_prefix_command(command);
            if command_len < input.len() {
                return self.handle_input(&input[command_len..]);
            }
            return false;
        }

        // Check for prefix key (Ctrl+b = 0x02)
        if let Some((prefix_index, prefix_len)) = find_prefix_key(input) {
            if prefix_index > 0 && self.handle_non_prefix_input(&input[..prefix_index]) {
                return true;
            }
            self.prefix_mode = true;
            let remaining_index = prefix_index + prefix_len;
            if remaining_index < input.len() {
                return self.handle_input(&input[remaining_index..]);
            }
            return false;
        }

        self.handle_non_prefix_input(input)
    }

    fn handle_prefix_command(&mut self, command: u8) {
        match command {
            b'"' => {
                // Ctrl+b " - horizontal split (top/bottom)
                let _ = self.split_focused_pane(SplitDirection::Horizontal);
                self.render();
            }
            b'%' => {
                // Ctrl+b % - vertical split (left/right)
                let _ = self.split_focused_pane(SplitDirection::Vertical);
                self.render();
            }
            b'c' => {
                // Ctrl+b c - create new tab
                let _ = self.create_tab();
            }
            b'n' => {
                // Ctrl+b n - next tab
                self.next_tab();
            }
            b'p' => {
                // Ctrl+b p - previous tab
                self.prev_tab();
            }
            b'0'..=b'9' => {
                // Ctrl+b 0-9 - switch to tab 0-9
                let tab_index = (command - b'0') as usize;
                self.switch_to_tab(tab_index);
            }
            b'[' => {
                // Ctrl+b [ - enter scrollback mode
                self.active_tab_mut().root.enter_scrollback_mode();
                self.render();
            }
            b'u' => {
                // Ctrl+b u - enter URL mode (automatically enters scrollback mode first)
                self.active_tab_mut().root.enter_scrollback_mode();
                self.active_tab_mut().root.enter_url_mode();
                self.render();
            }
            b'z' => {
                // Ctrl+b z - toggle zoom (temporary fullscreen) for focused pane
                self.toggle_zoom();
            }
            b'r' => {
                // Ctrl+b r - force full screen redraw. For a remote pane, also
                // pull a fresh authoritative repaint from the remote, since the
                // corruption may be in the local pane emulator's own state
                // (which a local redraw would just faithfully reproduce).
                if let Some(pane) = self.get_focused_pane_mut() {
                    pane.request_remote_resync();
                }
                self.force_full_redraw();
            }
            0x02 => {
                // Ctrl+b Ctrl+b - send Ctrl+b to the terminal
                let result = self.active_tab_mut().root.handle_input(&[0x02]);
                self.handle_pane_input_result(result);
            }
            _ => {
                // Unknown command, ignore
            }
        }
    }

    fn handle_non_prefix_input(&mut self, input: &[u8]) -> bool {
        // Check for CTRL+C (0x03) - cascading close behavior with SIGINT
        if input.len() == 1 && input[0] == 0x03 {
            return self.handle_ctrl_c();
        }

        // Check for CTRL+D (0x04) - cascading close behavior with EOF
        if input.len() == 1 && input[0] == 0x04 {
            return self.handle_ctrl_d();
        }

        // Check for focus movement shortcuts (Ctrl+h/j/k/l)
        // Ctrl+h = 0x08, Ctrl+j = 0x0a, Ctrl+k = 0x0b, Ctrl+l = 0x0c
        if input.len() == 1 {
            match input[0] {
                0x08 => {
                    // Ctrl+h - move focus left
                    self.move_focus(Direction::Left);
                    self.render();
                    return false;
                }
                0x0a => {
                    // Ctrl+j - move focus down
                    self.move_focus(Direction::Down);
                    self.render();
                    return false;
                }
                0x0b => {
                    // Ctrl+k - move focus up
                    self.move_focus(Direction::Up);
                    self.render();
                    return false;
                }
                0x0c => {
                    // Ctrl+l - move focus right
                    self.move_focus(Direction::Right);
                    self.render();
                    return false;
                }
                _ => {}
            }
        }
        let result = self.active_tab_mut().root.handle_input(input);
        self.handle_pane_input_result(result);
        self.close_requested_panes();
        self.exit_requested
    }

    fn handle_mouse_event(&mut self, event: SgrMouseEvent) {
        let pane_height = self.height.saturating_sub(self.status_bar_height());
        if event.y >= pane_height {
            return;
        }

        let (local_x, local_y, mode, should_focus) = if self.active_tab().zoomed {
            let mode = self.active_tab().root.focused_mouse_mode();
            (event.x, event.y, mode, false)
        } else if let Some(target) = self.active_tab().root.mouse_target_info(event.x, event.y) {
            (target.local_x, target.local_y, target.mode, true)
        } else {
            return;
        };

        if mode.report == MouseReportMode::None {
            return;
        }

        let Some(translated) = encode_mouse_event(event, local_x, local_y, mode.encoding) else {
            return;
        };

        if should_focus {
            self.active_tab_mut().root.focus_pane_at(event.x, event.y);
        }

        let result = self.active_tab_mut().root.handle_input(&translated);
        self.handle_pane_input_result(result);
    }

    fn toggle_zoom(&mut self) {
        let was_zoomed = self.active_tab().zoomed;
        let width = self.width;
        let pane_height = self.height.saturating_sub(self.status_bar_height());
        self.active_tab_mut().toggle_zoom();
        let is_zoomed = self.active_tab().zoomed;
        if is_zoomed && !was_zoomed {
            // Entering zoom mode: resize focused pane's PTY to fullscreen
            self.active_tab_mut()
                .root
                .resize_focused_pty(width, pane_height);
        } else if was_zoomed && !is_zoomed {
            // Exiting zoom mode: restore the original pane layout
            self.active_tab_mut().resize(width, pane_height);
        }
        self.render();
    }

    /// Handle the result of pane input processing
    fn handle_pane_input_result(&mut self, result: PaneInputResult) {
        match result {
            PaneInputResult::None => {}
            PaneInputResult::Rerender => self.render(),
            PaneInputResult::RenameWindow(name) => {
                self.tabs[self.active_tab].name = name;
                self.render();
            }
            PaneInputResult::ConnectedRemote { target, title } => {
                // The tab is now remote-owned: future splits auto-connect there.
                self.tabs[self.active_tab].remote_host = Some(target);
                if let Some(title) = title {
                    self.tabs[self.active_tab].name = title;
                }
                self.render();
            }
        }
    }

    /// Handle input while in scrollback mode.
    ///
    /// Uses libvim for vim-style navigation:
    /// - h/j/k/l: Basic cursor movement
    /// - w/b/e: Word motions
    /// - 0/$: Line start/end
    /// - gg/G: Document start/end
    /// - Ctrl+u/d: Half-page scroll
    /// - Ctrl+f/b: Full-page scroll
    /// - v/V: Visual mode / Visual line mode
    /// - y: Yank (copy) selected text to clipboard (in visual mode)
    /// - /: Enter search mode
    /// - Escape/q: Exit scrollback mode (or visual mode first)
    fn handle_scrollback_input(&mut self, input: &[u8]) {
        // Check if we're in search mode
        if self.active_tab().root.is_in_search_mode() {
            self.handle_search_input(input);
            return;
        }

        // Check if we're in URL mode
        if self.active_tab().root.is_in_url_mode() {
            self.handle_url_input(input);
            return;
        }

        if input.len() == 1 {
            match input[0] {
                b'/' => {
                    // / - enter search mode
                    self.active_tab_mut().root.enter_search_mode();
                    self.render();
                    return;
                }
                b'u' => {
                    // u - enter URL mode
                    self.active_tab_mut().root.enter_url_mode();
                    self.render();
                    return;
                }
                b'y' => {
                    // y - yank selected text to clipboard (if in visual mode)
                    let vim_mode = self.active_tab().root.get_vim_mode();
                    if vim_mode != libvim::Mode::Normal {
                        if let Some(text) = self.active_tab().root.get_selected_text() {
                            let _ = copy_to_clipboard(&text);
                        }
                        // Copying from visual selection completes selection mode.
                        self.active_tab_mut().root.exit_scrollback_mode();
                        self.render();
                        return;
                    }
                }
                0x1b | b'q' => {
                    // Escape or 'q' - exit scrollback mode (or exit visual mode first)
                    // Check if in visual mode - if so, return to normal mode
                    let vim_mode = self.active_tab().root.get_vim_mode();
                    if vim_mode != libvim::Mode::Normal {
                        // Send Escape to vim engine to exit visual mode
                        self.active_tab_mut().root.handle_vim_input(&[0x1b]);
                        self.render();
                        return;
                    }
                    self.active_tab_mut().root.exit_scrollback_mode();
                    self.render();
                    return;
                }
                _ => {}
            }
        }

        // Delegate all other input to the vim engine
        self.active_tab_mut().root.handle_vim_input(input);
        self.render();
    }

    /// Handle input while in search mode.
    fn handle_search_input(&mut self, input: &[u8]) {
        if input.is_empty() {
            return;
        }

        // Check if search input is focused
        let input_focused = self.active_tab().root.is_search_input_focused();

        // Handle unfocused state: n/p/j/k for navigation, / to re-focus, Escape to exit
        if !input_focused {
            if input.len() == 1 {
                match input[0] {
                    b'n' | b'j' => {
                        // n/j - go DOWN toward terminal grid (more recent)
                        self.active_tab_mut().root.prev_match();
                        self.render();
                        return;
                    }
                    b'p' | b'k' | b'N' => {
                        // p/k/N (shift+n) - go UP into scrollback (older)
                        self.active_tab_mut().root.next_match();
                        self.render();
                        return;
                    }
                    0x0e => {
                        // Ctrl+N - go DOWN toward terminal grid (more recent)
                        self.active_tab_mut().root.prev_match();
                        self.render();
                        return;
                    }
                    0x10 => {
                        // Ctrl+P - go UP into scrollback (older)
                        self.active_tab_mut().root.next_match();
                        self.render();
                        return;
                    }
                    b'/' => {
                        // / - re-focus the search input
                        self.active_tab_mut().root.focus_search_input();
                        self.render();
                        return;
                    }
                    0x1b | b'q' => {
                        // Escape or q - exit search mode (back to scrollback mode)
                        self.active_tab_mut().root.exit_search_mode();
                        self.render();
                        return;
                    }
                    _ => {
                        // Ignore other keys when unfocused
                        return;
                    }
                }
            }
            // Ignore multi-byte sequences when unfocused
            return;
        }

        // Search input is focused - handle typing and search navigation

        // Check for escape sequences (CSI sequences starting with ESC [)
        if input.len() >= 3 && input[0] == 0x1b && input[1] == b'[' {
            // Check for up/down arrow keys for match navigation
            if input.len() == 3 {
                match input[2] {
                    b'A' => {
                        // Up arrow - go to next match (older / up on screen)
                        self.active_tab_mut().root.next_match();
                        self.render();
                        return;
                    }
                    b'B' => {
                        // Down arrow - go to previous match (more recent / down on screen)
                        self.active_tab_mut().root.prev_match();
                        self.render();
                        return;
                    }
                    _ => {}
                }
            }
            // Other escape sequences - ignore
            return;
        }

        // Single byte input
        if input.len() == 1 {
            match input[0] {
                0x1b => {
                    // Escape - exit search mode (back to scrollback mode)
                    self.active_tab_mut().root.exit_search_mode();
                    self.render();
                }
                0x0d => {
                    // Enter - unfocus search input (stay in search mode for n/p navigation)
                    self.active_tab_mut().root.unfocus_search_input();
                    self.render();
                }
                0x10 => {
                    // Ctrl+P - go UP into scrollback (older)
                    self.active_tab_mut().root.next_match();
                    self.render();
                }
                0x0e => {
                    // Ctrl+N - go DOWN toward terminal grid (more recent)
                    self.active_tab_mut().root.prev_match();
                    self.render();
                }
                0x17 => {
                    // Ctrl+W - clear search input
                    self.active_tab_mut().root.search_clear();
                    self.render();
                }
                0x7f => {
                    // Backspace (0x7f) - delete last character
                    self.active_tab_mut().root.search_input_backspace();
                    self.render();
                }
                b if b >= 0x20 && b < 0x7f => {
                    // Printable ASCII character
                    self.active_tab_mut().root.search_input_char(b as char);
                    self.render();
                }
                _ => {
                    // Ignore other control characters
                }
            }
        } else if input.len() == 2 && input[0] == 0x1b {
            // Alt+key or other 2-byte sequences
            // Check for Shift+Enter or Ctrl+Enter (some terminals send this as ESC + Enter)
            if input[1] == 0x0d || input[1] == 0x0a {
                // Shift+Enter or Ctrl+Enter - go to previous match (more recent / down on screen)
                self.active_tab_mut().root.prev_match();
                self.render();
            }
        } else if input.len() >= 4 && input[0] == 0x1b && input[1] == b'[' {
            // Some terminals send CSI sequences for modified Enter
            // e.g., ESC [13;5u for Ctrl+Enter in kitty keyboard protocol
            // For now, check if it looks like a modified Enter sequence
            if input.ends_with(b"5u") || input.ends_with(b"5~") {
                // Ctrl+Enter variant - go to previous match
                self.active_tab_mut().root.prev_match();
                self.render();
            }
        } else {
            // Try to interpret as UTF-8 string for multi-byte characters
            if let Ok(s) = std::str::from_utf8(input) {
                for c in s.chars() {
                    if c >= ' ' && c != '\x7f' {
                        self.active_tab_mut().root.search_input_char(c);
                    }
                }
                self.render();
            }
        }
    }

    /// Handle input while in URL mode.
    ///
    /// Navigation keys:
    /// - j: Next URL (toward bottom/more recent)
    /// - k: Previous URL (toward top/older)
    /// - Enter: Open the selected URL in default browser
    /// - y: Yank (copy) the selected URL to clipboard
    /// - Escape/q: Exit URL mode (back to scrollback mode)
    fn handle_url_input(&mut self, input: &[u8]) {
        if input.is_empty() {
            return;
        }

        if input.len() == 1 {
            match input[0] {
                0x1b | b'q' => {
                    // Escape or 'q' - exit URL mode (back to scrollback mode)
                    self.active_tab_mut().root.exit_url_mode();
                    self.render();
                }
                b'j' => {
                    // j - next URL (toward bottom/more recent)
                    self.active_tab_mut().root.next_url();
                    self.render();
                }
                b'k' => {
                    // k - previous URL (toward top/older)
                    self.active_tab_mut().root.prev_url();
                    self.render();
                }
                0x0d => {
                    // Enter - open the selected URL in default browser
                    if let Some(url) = self.active_tab().root.get_current_url() {
                        let _ = open_url(&url);
                    }
                    // Exit URL mode after opening
                    self.active_tab_mut().root.exit_url_mode();
                    self.active_tab_mut().root.exit_scrollback_mode();
                    self.render();
                }
                b'y' => {
                    // y - yank (copy) the selected URL to clipboard
                    if let Some(url) = self.active_tab().root.get_current_url() {
                        let _ = copy_to_clipboard(&url);
                    }
                    // Exit URL mode after yanking
                    self.active_tab_mut().root.exit_url_mode();
                    self.active_tab_mut().root.exit_scrollback_mode();
                    self.render();
                }
                _ => {
                    // Ignore other keys
                }
            }
        }
    }
    /// Get a reference to the currently active tab
    #[allow(dead_code)]
    pub fn active_tab(&self) -> &Tab {
        &self.tabs[self.active_tab]
    }

    /// Get a mutable reference to the currently active tab
    pub fn active_tab_mut(&mut self) -> &mut Tab {
        &mut self.tabs[self.active_tab]
    }

    /// Create a new tab and switch to it
    pub fn create_tab(&mut self) -> Result<(), CompositorError> {
        let pane_height = self.height.saturating_sub(self.status_bar_height());
        let tab = Tab::new("bash".to_string(), self.width, pane_height)?;
        self.tabs.push(tab);
        self.active_tab = self.tabs.len() - 1;
        self.render();
        Ok(())
    }

    /// Switch to the tab at the given index (0-based)
    pub fn switch_to_tab(&mut self, index: usize) {
        if index < self.tabs.len() {
            self.active_tab = index;
            self.render();
        }
    }

    /// Switch to the next tab (wraps around)
    pub fn next_tab(&mut self) {
        if !self.tabs.is_empty() {
            self.active_tab = (self.active_tab + 1) % self.tabs.len();
            self.render();
        }
    }

    /// Switch to the previous tab (wraps around)
    pub fn prev_tab(&mut self) {
        if !self.tabs.is_empty() {
            self.active_tab = if self.active_tab == 0 {
                self.tabs.len() - 1
            } else {
                self.active_tab - 1
            };
            self.render();
        }
    }

    /// Get the number of tabs
    pub fn tab_count(&self) -> usize {
        self.tabs.len()
    }

    /// Get the active tab index
    pub fn active_tab_index(&self) -> usize {
        self.active_tab
    }

    /// Split the currently focused pane.
    ///
    /// Creates a new pane by splitting the focused pane either horizontally or vertically.
    pub fn split_focused_pane(&mut self, direction: SplitDirection) -> Result<(), CompositorError> {
        self.exit_zoom_if_needed();
        let remote_cwd = self
            .active_tab()
            .remote_host
            .as_ref()
            .and_then(|_| self.get_focused_pane())
            .and_then(|pane| pane.remote())
            .and_then(|remote| remote.cwd().map(PathBuf::from));
        self.active_tab_mut().root.split_focused(direction)?;

        // In a remote-owned tab, the new (now focused) pane joins the same host.
        if let Some(host) = self.active_tab().remote_host.clone() {
            let env = libshell::shell_env_snapshot();
            if let Some(pane) = self.get_focused_pane_mut() {
                if let Err(e) = pane.connect_remote_with_cwd(&host, &env, remote_cwd.as_deref()) {
                    let msg = format!("connect error: {}\r\n", e);
                    pane.terminal_emulator.process(msg.as_bytes());
                }
            }
        }
        Ok(())
    }

    fn exit_zoom_if_needed(&mut self) {
        if self.active_tab().zoomed {
            let width = self.width;
            let pane_height = self.height.saturating_sub(self.status_bar_height());
            self.active_tab_mut().exit_zoom();
            self.active_tab_mut().resize(width, pane_height);
        }
    }

    /// Move focus in the specified direction.
    ///
    /// Uses vim-style navigation:
    /// - Left (h): Move to the pane on the left
    /// - Down (j): Move to the pane below
    /// - Up (k): Move to the pane above
    /// - Right (l): Move to the pane on the right
    pub fn move_focus(&mut self, direction: Direction) {
        self.active_tab_mut().root.move_focus(direction);
    }

    /// Handle CTRL+C with cascading behavior.
    ///
    /// The behavior depends on the current state:
    /// 1. If a subprocess is running → forward CTRL+C to it via PTY (letting it decide how to handle)
    /// 2. If the shell has input → clear the input  
    /// 3. If input is already empty → close the focused pane
    /// 4. If there are no other panes → return true to signal the entire app should exit
    ///
    /// Returns `true` if the entire compositor should exit, `false` otherwise.
    pub fn handle_ctrl_c(&mut self) -> bool {
        self.handle_interrupt_key(true)
    }

    /// Handle CTRL+D with cascading behavior (same as CTRL+C but sends EOF instead of SIGINT).
    pub fn handle_ctrl_d(&mut self) -> bool {
        self.handle_interrupt_key(false)
    }

    /// Handle an interrupt key (CTRL+C or CTRL+D) with cascading behavior.
    fn handle_interrupt_key(&mut self, is_ctrl_c: bool) -> bool {
        let result = if is_ctrl_c {
            self.active_tab_mut().root.handle_ctrl_c()
        } else {
            self.active_tab_mut().root.handle_ctrl_d()
        };

        match result {
            CtrlCResult::KilledSubprocess | CtrlCResult::ClearedInput => {
                // Just re-render, don't close anything
                self.render();
                false
            }
            CtrlCResult::ClosePane => {
                // Try to close the focused pane.
                self.close_focused_pane_or_exit()
            }
        }
    }

    fn close_focused_pane_or_exit(&mut self) -> bool {
        let pane_count = self.active_tab().root.pane_count();

        if pane_count <= 1 {
            // This is the last pane in this tab
            if self.tabs.len() <= 1 {
                // This is the last tab - exit the entire compositor
                self.exit_requested = true;
                true
            } else {
                // Close this tab and switch to another
                self.tabs.remove(self.active_tab);
                if self.active_tab >= self.tabs.len() {
                    self.active_tab = self.tabs.len() - 1;
                }
                self.render();
                false
            }
        } else {
            // Close just the focused pane and exit zoom mode
            let was_zoomed = self.active_tab().zoomed;
            let width = self.width;
            let pane_height = self.height.saturating_sub(self.status_bar_height());
            self.active_tab_mut().exit_zoom();
            self.active_tab_mut().root.close_focused_pane();
            // If we were zoomed, restore the pane layout
            if was_zoomed {
                self.active_tab_mut().resize(width, pane_height);
            }
            self.render();
            false
        }
    }

    fn close_requested_panes(&mut self) -> bool {
        let mut closed_any = false;
        let mut idx = 0;
        while idx < self.tabs.len() {
            if self.tabs[idx].root.close_requested_panes() {
                idx += 1;
            } else {
                self.tabs.remove(idx);
                closed_any = true;
                if idx < self.active_tab {
                    self.active_tab -= 1;
                }
            }
        }

        if self.tabs.is_empty() {
            self.active_tab = 0;
            self.exit_requested = true;
            return closed_any;
        }

        if self.active_tab >= self.tabs.len() {
            self.active_tab = self.tabs.len() - 1;
        }

        closed_any
    }

    /// Run the event loop. This blocks and handles all events.
    ///
    /// The loop will:
    /// 1. Poll all PTY file descriptors and the wake pipe
    /// 2. Process any PTY output (feed to emulators)
    /// 3. Process any queued keyboard input
    /// 4. Render the compositor
    ///
    /// Returns when all panes have exited or an error occurs.
    pub fn run(&mut self) -> Result<(), CompositorError> {
        // Initial render to show the shell prompt
        self.render();

        loop {
            // Collect all file descriptors to poll
            let mut poll_fds: Vec<libc::pollfd> = Vec::new();
            let mut fd_to_pane: Vec<Option<*mut Pane>> = Vec::new();

            // Add wake pipe fd
            poll_fds.push(libc::pollfd {
                fd: self.wake_read.as_raw_fd(),
                events: libc::POLLIN,
                revents: 0,
            });
            fd_to_pane.push(None);

            // Collect PTY fds from all tabs
            for tab in &mut self.tabs {
                tab.root.collect_poll_fds(&mut poll_fds, &mut fd_to_pane);
            }

            // If no PTYs are left, we're done
            if poll_fds.len() == 1 {
                return Ok(());
            }

            // Poll with no timeout (block until something happens)
            let n =
                unsafe { libc::poll(poll_fds.as_mut_ptr(), poll_fds.len() as libc::nfds_t, -1) };

            if n < 0 {
                let err = std::io::Error::last_os_error();
                // EINTR means we were interrupted by a signal - just continue
                if err.kind() == std::io::ErrorKind::Interrupted {
                    continue;
                }
                return Err(CompositorError::Poll(err));
            }

            if n == 0 {
                continue;
            }

            // Process ready file descriptors
            for (i, pfd) in poll_fds.iter().enumerate() {
                if pfd.revents == 0 {
                    continue;
                }

                if i == 0 {
                    // Wake pipe - drain it
                    let mut buf = [0u8; 64];
                    while let Ok(_) = read(self.wake_read.as_raw_fd(), &mut buf) {}
                } else if let Some(pane_ptr) = fd_to_pane[i] {
                    // PTY output - read and process
                    // SAFETY: The pointer is valid for the duration of this loop iteration
                    let pane = unsafe { &mut *pane_ptr };

                    if pfd.revents & libc::POLLIN != 0 {
                        pane.read_and_process();
                    }

                    if pfd.revents & libc::POLLHUP != 0 {
                        // Process exited, but we might still have data to read
                        pane.read_and_process();
                    }
                }
            }

            self.close_requested_panes();
            if self.exit_requested {
                return Ok(());
            }

            // Process queued keyboard input
            self.process_keyboard_queue();
            if self.exit_requested {
                return Ok(());
            }

            // Render the compositor
            self.render();
        }
    }

    /// Run one iteration of the event loop with a timeout.
    ///
    /// Returns true if any events were processed.
    pub fn poll_once(&mut self, timeout_ms: i32) -> Result<bool, CompositorError> {
        // Collect all file descriptors to poll
        let mut poll_fds: Vec<libc::pollfd> = Vec::new();
        let mut fd_to_pane: Vec<Option<*mut Pane>> = Vec::new();

        // Add wake pipe fd
        poll_fds.push(libc::pollfd {
            fd: self.wake_read.as_raw_fd(),
            events: libc::POLLIN,
            revents: 0,
        });
        fd_to_pane.push(None);

        // Collect PTY fds from all tabs
        for tab in &mut self.tabs {
            tab.root.collect_poll_fds(&mut poll_fds, &mut fd_to_pane);
        }

        // Poll with timeout
        let n = unsafe {
            libc::poll(
                poll_fds.as_mut_ptr(),
                poll_fds.len() as libc::nfds_t,
                timeout_ms,
            )
        };

        if n < 0 {
            let err = std::io::Error::last_os_error();
            // EINTR means we were interrupted by a signal - just return no events
            if err.kind() == std::io::ErrorKind::Interrupted {
                return Ok(false);
            }
            return Err(CompositorError::Poll(err));
        }

        if n == 0 {
            return Ok(false);
        }

        let mut had_events = false;

        // Process ready file descriptors
        for (i, pfd) in poll_fds.iter().enumerate() {
            if pfd.revents == 0 {
                continue;
            }

            had_events = true;

            if i == 0 {
                // Wake pipe - drain it
                let mut buf = [0u8; 64];
                while let Ok(_) = read(self.wake_read.as_raw_fd(), &mut buf) {}
            } else if let Some(pane_ptr) = fd_to_pane[i] {
                // PTY output - read and process
                let pane = unsafe { &mut *pane_ptr };

                if pfd.revents & libc::POLLIN != 0 {
                    pane.read_and_process();
                }

                if pfd.revents & libc::POLLHUP != 0 {
                    pane.read_and_process();
                }
            }
        }

        self.close_requested_panes();
        if self.exit_requested {
            return Ok(had_events);
        }

        // A remote session may have pushed a window rename; apply it to the tab
        // owning the focused (remote) pane.
        let remote_title = self
            .get_focused_pane_mut()
            .and_then(|p| p.take_remote_title());
        if let Some(name) = remote_title {
            self.tabs[self.active_tab].name = name;
        }

        // Process queued keyboard input
        self.process_keyboard_queue();
        if self.exit_requested {
            return Ok(had_events);
        }

        // Render the compositor
        self.render();

        Ok(had_events)
    }

    /// Process all queued keyboard input.
    /// Returns `true` if any input caused terminal content to change.
    fn process_keyboard_queue(&mut self) -> bool {
        let inputs: Vec<Vec<u8>> = {
            let mut queue = self.input_queue.lock().unwrap();
            queue.drain(..).collect()
        };

        let mut needs_render = false;
        for input in inputs {
            let result = self.active_tab_mut().root.handle_input(&input);
            match result {
                PaneInputResult::None => {}
                PaneInputResult::Rerender => needs_render = true,
                PaneInputResult::RenameWindow(name) => {
                    self.tabs[self.active_tab].name = name;
                    needs_render = true;
                }
                PaneInputResult::ConnectedRemote { target, title } => {
                    self.tabs[self.active_tab].remote_host = Some(target);
                    if let Some(title) = title {
                        self.tabs[self.active_tab].name = title;
                    }
                    needs_render = true;
                }
            }
        }
        needs_render
    }

    /// Render the compositor to the terminal.
    ///
    /// This traverses all panes, taking their grid contents and compositing them into
    /// a single terminal emulator. Then, it uses delta rendering to output only the changed parts.
    pub fn render(&mut self) {
        self.sync_host_mouse_mode();

        // Clear the global emulator to prepare for compositing
        let (cols, rows) = self.global_emulator.dimensions();
        self.global_emulator = emulator::TerminalEmulator::new(cols, rows);

        // Calculate pane area (excluding status bar)
        let pane_height = rows.saturating_sub(self.status_bar_height());

        // Composite the active tab's panes into the global emulator
        let tab = &mut self.tabs[self.active_tab];
        if tab.zoomed {
            // Zoom mode: composite the focused cell at fullscreen position
            // The PTY has already been resized to fullscreen when entering zoom mode
            if let Some(cell) = tab.root.get_focused_cell_mut() {
                cell.composite_into_at(&mut self.global_emulator, 0, 0, cols, pane_height);
            }
        } else {
            // Normal mode: render all panes with borders
            tab.root.composite_into(&mut self.global_emulator);
        }

        // Render the status bar at the bottom (skipped in bare mode)
        if self.status_bar_visible {
            self.render_status_bar();
        }

        // Set the cursor position and visibility from the focused pane
        if let Some((cursor_x, cursor_y, cursor_visible)) =
            self.tabs[self.active_tab].root.get_focused_cursor_info()
        {
            let grid = self.global_emulator.grid_mut();
            // In zoom mode, cursor should be at (0,0) offset since pane fills screen
            // get_focused_cursor_info returns global coords (pos_x + cursor_x),
            // so we need to subtract the cell's position to get the screen position
            if self.tabs[self.active_tab].zoomed {
                if let Some(cell) = self.tabs[self.active_tab].root.get_focused_cell_mut() {
                    grid.cursor_x = cursor_x.saturating_sub(cell.pos_x);
                    grid.cursor_y = cursor_y.saturating_sub(cell.pos_y);
                } else {
                    grid.cursor_x = cursor_x;
                    grid.cursor_y = cursor_y;
                }
            } else {
                grid.cursor_x = cursor_x;
                grid.cursor_y = cursor_y;
            }
            grid.cursor_visible = cursor_visible;
        }

        // Compute the delta between the previous frame and current frame
        let delta = emulator::compute_delta(&self.prev_frame, self.global_emulator.grid());

        // Write the delta to the output, wrapped in BSU/ESU if synchronized output is enabled
        if !delta.is_empty() {
            if let Ok(mut output) = self.output.lock() {
                if self.synchronized_output {
                    let _ = output.write_all(BSU);
                }
                let _ = output.write_all(&delta);
                if self.synchronized_output {
                    let _ = output.write_all(ESU);
                }
                let _ = output.flush();
            }
        }

        // Save the current frame as the previous frame for next render
        self.prev_frame = self.global_emulator.grid().clone();
    }

    fn sync_host_mouse_mode(&mut self) {
        let mode = self.active_tab().root.focused_mouse_mode();
        if mode == self.host_mouse_mode {
            return;
        }

        let sequence = host_mouse_sequence(mode);
        if let Ok(mut output) = self.output.lock() {
            let _ = output.write_all(&sequence);
            let _ = output.flush();
        }
        self.host_mouse_mode = mode;
    }

    /// Render the status bar at the bottom of the terminal.
    ///
    /// The status bar contains:
    /// - Left side: tabs with numbers and names
    /// - Right side: current date/time
    fn render_status_bar(&mut self) {
        let (cols, rows) = self.global_emulator.dimensions();
        if rows < STATUS_BAR_HEIGHT {
            return;
        }

        let status_bar_y = rows - STATUS_BAR_HEIGHT;

        // Create attributes for the status bar background
        let mut bar_attrs = emulator::CellAttributes::default();
        bar_attrs.bg_color = Some(emulator::Color::Green);
        bar_attrs.fg_color = Some(emulator::Color::Black);

        // Create attributes for the active tab
        let mut active_tab_attrs = emulator::CellAttributes::default();
        active_tab_attrs.bg_color = Some(emulator::Color::Black);
        active_tab_attrs.fg_color = Some(emulator::Color::Green);
        active_tab_attrs.bold = true;

        // Distinct colors for remote-owned tabs (blue vs the green of local tabs).
        let mut remote_tab_attrs = emulator::CellAttributes::default();
        remote_tab_attrs.bg_color = Some(emulator::Color::Blue);
        remote_tab_attrs.fg_color = Some(emulator::Color::BrightWhite);
        let mut active_remote_tab_attrs = remote_tab_attrs.clone();
        active_remote_tab_attrs.bg_color = Some(emulator::Color::BrightBlue);
        active_remote_tab_attrs.bold = true;

        // Fill the status bar rows with the background color
        for y in status_bar_y..rows {
            for x in 0..cols {
                self.global_emulator.grid_mut().set_cell(
                    x,
                    y,
                    emulator::Cell::new(' ', bar_attrs.clone()),
                );
            }
        }

        // Render tabs on the left side of the first status bar row
        let mut x_pos = 0;
        for (i, tab) in self.tabs.iter().enumerate() {
            // Add 'Z' suffix if tab is zoomed
            let zoom_indicator = if tab.zoomed { "Z" } else { "" };
            // Remote-owned tabs are distinguished by color alone (see attrs
            // below); the host name is omitted to keep the tab label compact.
            let tab_text = format!(" {} {}{} ", i, tab.name, zoom_indicator);
            let attrs = match (tab.remote_host.is_some(), i == self.active_tab) {
                (true, true) => active_remote_tab_attrs.clone(),
                (true, false) => remote_tab_attrs.clone(),
                (false, true) => active_tab_attrs.clone(),
                (false, false) => bar_attrs.clone(),
            };

            x_pos = write_str_to_grid(
                self.global_emulator.grid_mut(),
                x_pos,
                status_bar_y,
                &tab_text,
                &attrs,
                cols,
            );
        }

        // Check if we're in search mode (sub-mode of scrollback) first
        if self.tabs[self.active_tab].root.is_in_search_mode() {
            // Create attributes for search indicator
            let mut search_attrs = emulator::CellAttributes::default();
            search_attrs.bg_color = Some(emulator::Color::Cyan);
            search_attrs.fg_color = Some(emulator::Color::Black);
            search_attrs.bold = true;

            if let Some((query, current_idx, total)) =
                self.tabs[self.active_tab].root.get_search_info()
            {
                // Show search query on the left after tabs (plus a trailing space)
                let search_text = format!(" / {} ", query);
                write_str_to_grid(
                    self.global_emulator.grid_mut(),
                    x_pos,
                    status_bar_y,
                    &search_text,
                    &search_attrs,
                    cols,
                );

                // Show match count on the right
                let match_text = if total == 0 {
                    " No matches ".to_string()
                } else if total >= 100 {
                    let current_display = current_idx.map(|i| i + 1).unwrap_or(0);
                    format!(" {}/100+ ", current_display)
                } else {
                    let current_display = current_idx.map(|i| i + 1).unwrap_or(0);
                    format!(" {}/{} ", current_display, total)
                };

                let text_start_x = cols.saturating_sub(str_display_width(&match_text));
                write_str_to_grid(
                    self.global_emulator.grid_mut(),
                    text_start_x,
                    status_bar_y,
                    &match_text,
                    &search_attrs,
                    cols,
                );
                return;
            }
            return;
        }

        // Check if we're in URL mode (sub-mode of scrollback)
        if self.tabs[self.active_tab].root.is_in_url_mode() {
            // Create attributes for URL indicator
            let mut url_attrs = emulator::CellAttributes::default();
            url_attrs.bg_color = Some(emulator::Color::Cyan);
            url_attrs.fg_color = Some(emulator::Color::Black);
            url_attrs.bold = true;

            if let Some((current_idx, _total)) = self.tabs[self.active_tab].root.get_url_info() {
                // Show URL mode label on the left after tabs
                x_pos = write_str_to_grid(
                    self.global_emulator.grid_mut(),
                    x_pos,
                    status_bar_y,
                    " URL ",
                    &url_attrs,
                    cols,
                );

                // Show current URL if selected (truncated if too long)
                if let Some(url) = self.tabs[self.active_tab].root.get_current_url() {
                    // Calculate available space for URL (leave room for hint on right)
                    let hint_text = " j/k:nav Enter:open ";
                    let hint_width = str_display_width(hint_text);
                    let url_max_x = cols.saturating_sub(hint_width);
                    let available = url_max_x.saturating_sub(x_pos);

                    // Truncate by display width on character boundaries (never panics
                    // on multi-byte URLs, unlike byte-slicing).
                    let display_url = truncate_to_width(&url, available);
                    write_str_to_grid(
                        self.global_emulator.grid_mut(),
                        x_pos,
                        status_bar_y,
                        &display_url,
                        &url_attrs,
                        url_max_x,
                    );

                    // Show hint text on the right (only if we have a URL selected)
                    if current_idx.is_some() {
                        let text_start_x = cols.saturating_sub(hint_width);
                        write_str_to_grid(
                            self.global_emulator.grid_mut(),
                            text_start_x,
                            status_bar_y,
                            hint_text,
                            &url_attrs,
                            cols,
                        );
                    }
                } else {
                    // No URL selected
                    write_str_to_grid(
                        self.global_emulator.grid_mut(),
                        x_pos,
                        status_bar_y,
                        " No URLs found ",
                        &url_attrs,
                        cols,
                    );
                }
                return;
            }
            return;
        }

        // Check if we're in scrollback mode and render scroll indicator instead of date/time
        let Some(right_text) = (if self.tabs[self.active_tab].root.is_in_scrollback_mode() {
            // Create attributes for scrollback indicator
            let mut scroll_attrs = emulator::CellAttributes::default();
            scroll_attrs.bg_color = Some(emulator::Color::Yellow);
            scroll_attrs.fg_color = Some(emulator::Color::Black);
            scroll_attrs.bold = true;

            if let Some((scroll_offset, scrollback_len)) =
                self.tabs[self.active_tab].root.get_scrollback_info()
            {
                let current_line = scrollback_len.saturating_sub(scroll_offset);
                let scroll_text = format!(" SCROLL {}/{} ", current_line, scrollback_len);

                let text_start_x = cols.saturating_sub(str_display_width(&scroll_text));
                write_str_to_grid(
                    self.global_emulator.grid_mut(),
                    text_start_x,
                    status_bar_y,
                    &scroll_text,
                    &scroll_attrs,
                    cols,
                );
                return;
            }
            return;
        } else {
            // Render the date/time on the right side of the first status bar row
            let datetime = (self.clock)();
            Some(datetime.format(" %Y-%m-%d %H:%M ").to_string())
        }) else {
            return;
        };

        let diagnostics = libshell::global_diagnostic_flags();
        let diagnostics_width = diagnostics.len() * 2;
        let time_start_x = cols.saturating_sub(diagnostics_width + str_display_width(&right_text));
        let mut right_x = time_start_x;

        for flag in diagnostics {
            if right_x < cols {
                self.global_emulator.grid_mut().set_cell(
                    right_x,
                    status_bar_y,
                    emulator::Cell::new(' ', bar_attrs.clone()),
                );
                right_x += 1;
            }

            let mut diagnostic_attrs = bar_attrs.clone();
            diagnostic_attrs.fg_color = Some(match flag {
                libshell::GlobalDiagnosticFlag::EnvironmentParseFailure => emulator::Color::Red,
            });
            diagnostic_attrs.bold = true;

            if right_x < cols {
                self.global_emulator.grid_mut().set_cell(
                    right_x,
                    status_bar_y,
                    emulator::Cell::new(
                        match flag {
                            libshell::GlobalDiagnosticFlag::EnvironmentParseFailure => 'E',
                        },
                        diagnostic_attrs,
                    ),
                );
                right_x += 1;
            }
        }

        write_str_to_grid(
            self.global_emulator.grid_mut(),
            right_x,
            status_bar_y,
            &right_text,
            &bar_attrs,
            cols,
        );
    }

    /// Get a reference to the root pane cell of the active tab.
    pub fn root(&self) -> &PaneCell {
        &self.tabs[self.active_tab].root
    }

    /// Get a mutable reference to the root pane cell of the active tab.
    pub fn root_mut(&mut self) -> &mut PaneCell {
        &mut self.tabs[self.active_tab].root
    }

    /// Get a mutable reference to the focused pane of the active tab.
    pub fn get_focused_pane_mut(&mut self) -> Option<&mut crate::pane::Pane> {
        self.tabs[self.active_tab].root.get_focused_pane_mut()
    }

    /// Get a reference to the focused pane of the active tab.
    pub fn get_focused_pane(&self) -> Option<&crate::pane::Pane> {
        self.tabs[self.active_tab].root.get_focused_pane()
    }

    /// Get the focused pane's authoritative cwd.
    pub fn focused_cwd(&self) -> Option<PathBuf> {
        self.get_focused_pane()
            .map(|pane| pane.shell.cwd().to_path_buf())
    }

    /// Set the focused pane's authoritative cwd.
    pub fn set_focused_cwd(&mut self, cwd: PathBuf) -> bool {
        self.tabs[self.active_tab].root.set_focused_cwd(cwd)
    }

    /// Whether the compositor should terminate because its final pane closed.
    pub fn should_exit(&self) -> bool {
        self.exit_requested
    }

    /// Get the wake file descriptor for external polling.
    ///
    /// This can be used to integrate the compositor into an external event loop.
    pub fn wake_fd(&self) -> RawFd {
        self.wake_read.as_raw_fd()
    }

    /// Get a reference to the global emulator (for testing).
    pub fn global_emulator(&self) -> &emulator::TerminalEmulator {
        &self.global_emulator
    }

    /// Capture the current composited screen as a serializable snapshot, for
    /// sending to a (re)attaching client as a `GridResync`.
    ///
    /// Reflects the most recent `render()`; call `render()` first if the screen
    /// may be stale (e.g. just after a resize).
    pub fn grid_snapshot(&self) -> protocol::GridSnapshot {
        self.global_emulator.grid().to_snapshot()
    }

    /// Perform a render cycle and return the rendered output.
    ///
    /// This is useful for testing - it composites all panes and returns
    /// the delta output that would be written to the terminal.
    pub fn render_to_vec(&mut self) -> Vec<u8> {
        // Clear the global emulator to prepare for compositing
        let (cols, rows) = self.global_emulator.dimensions();
        self.global_emulator = emulator::TerminalEmulator::new(cols, rows);

        // Composite the active tab's panes into the global emulator
        self.tabs[self.active_tab]
            .root
            .composite_into(&mut self.global_emulator);

        // Render the status bar (skipped in bare mode)
        if self.status_bar_visible {
            self.render_status_bar();
        }

        // Set the cursor position and visibility from the focused pane
        if let Some((cursor_x, cursor_y, cursor_visible)) =
            self.tabs[self.active_tab].root.get_focused_cursor_info()
        {
            let grid = self.global_emulator.grid_mut();
            grid.cursor_x = cursor_x;
            grid.cursor_y = cursor_y;
            grid.cursor_visible = cursor_visible;
        }

        // Compute the delta from a blank grid to get the full render output.
        // This allows the output to be replayed on a fresh emulator.
        let blank_grid = emulator::TerminalGrid::new(cols, rows);
        let delta = emulator::compute_delta(&blank_grid, self.global_emulator.grid());

        // Save the current frame as the previous frame for next render
        self.prev_frame = self.global_emulator.grid().clone();

        delta
    }

    /// Get the ASCII text content of the composited display.
    ///
    /// Returns a vector of strings, one per line.
    pub fn get_text_lines(&self) -> Vec<String> {
        let (_, rows) = self.global_emulator.dimensions();
        (0..rows)
            .map(|y| self.global_emulator.grid().get_line_text(y))
            .collect()
    }

    /// Resize the compositor to new dimensions.
    ///
    /// This recalculates the size of all panes, distributing space evenly
    /// within each split. All terminal emulators and PTYs are resized accordingly.
    ///
    /// After calling resize, you should call `force_render()` to redraw the screen,
    /// since the previous frame dimensions no longer match.
    pub fn resize(&mut self, width: usize, height: usize) {
        self.width = width;
        self.height = height;

        // Resize the global emulator and prev_frame to the new dimensions.
        // prev_frame is reset to a blank grid so the next render will be a full redraw.
        self.global_emulator = emulator::TerminalEmulator::new(width, height);
        self.prev_frame = emulator::TerminalGrid::new(width, height);

        // Calculate pane height (total height minus status bar)
        let pane_height = height.saturating_sub(self.status_bar_height());

        // Recursively resize all tabs
        for tab in &mut self.tabs {
            tab.resize(width, pane_height);
            // If this tab is zoomed, also resize the focused pane's PTY to fullscreen
            if tab.zoomed {
                tab.root.resize_focused_pty(width, pane_height);
            }
        }
    }

    /// Force a full render of the compositor.
    ///
    /// This should be called after resize or when you need to redraw the entire screen.
    /// It composites all panes and outputs the result to the terminal.
    pub fn force_render(&mut self) {
        self.render();
    }

    /// Force a complete screen clear and full redraw.
    ///
    /// This clears the terminal screen, resets the previous frame state, and
    /// performs a full render. Use this when the screen gets out of sync
    /// (e.g., after graphical glitches or escape code issues).
    ///
    /// Triggered by Ctrl+b r.
    pub fn force_full_redraw(&mut self) {
        let (cols, rows) = self.global_emulator.dimensions();

        // Clear the screen by sending clear escape sequence
        if let Ok(mut output) = self.output.lock() {
            // Clear screen and move cursor to home
            let _ = output.write_all(b"\x1b[2J\x1b[H");
            let _ = output.flush();
        }

        // Reset prev_frame to blank so next render outputs everything
        self.prev_frame = emulator::TerminalGrid::new(cols, rows);

        // Perform a full render
        self.render();
    }

    /// Enable or disable synchronized output mode.
    ///
    /// When enabled, the compositor wraps each render update with BSU (Begin Synchronized
    /// Update) and ESU (End Synchronized Update) escape sequences. This prevents screen
    /// tearing in terminals that support this feature.
    ///
    /// Use `detect_synchronized_output_support()` to check if the terminal supports this mode.
    pub fn set_synchronized_output(&mut self, enabled: bool) {
        self.synchronized_output = enabled;
    }

    /// Rows reserved for the status bar: `STATUS_BAR_HEIGHT` normally, 0 in bare
    /// mode.
    pub fn status_bar_height(&self) -> usize {
        if self.status_bar_visible {
            STATUS_BAR_HEIGHT
        } else {
            0
        }
    }

    /// Show or hide the status bar (bare mode). Re-lays out panes to the new
    /// usable height.
    pub fn set_status_bar_visible(&mut self, visible: bool) {
        if self.status_bar_visible == visible {
            return;
        }
        self.status_bar_visible = visible;
        let (w, h) = (self.width, self.height);
        self.resize(w, h);
    }

    /// Check if synchronized output mode is currently enabled.
    pub fn synchronized_output_enabled(&self) -> bool {
        self.synchronized_output
    }
}

/// Copy text to the system clipboard.
///
/// On macOS, this uses `pbcopy`.
/// On Linux, this tries `wl-copy`, `xclip`, or `xsel`.
/// On other platforms, this is a no-op.
fn copy_to_clipboard(text: &str) -> std::io::Result<()> {
    #[cfg(target_os = "macos")]
    {
        copy_to_clipboard_with_commands(text, &[ClipboardCommand::new("pbcopy", &[])])
    }

    #[cfg(target_os = "linux")]
    {
        let commands = linux_clipboard_commands();
        copy_to_clipboard_with_commands(text, &commands)
    }

    #[cfg(not(any(target_os = "macos", target_os = "linux")))]
    {
        // No clipboard support on other platforms
        let _ = text;
        Ok(())
    }
}

#[cfg(any(target_os = "macos", target_os = "linux"))]
#[derive(Debug, PartialEq, Eq)]
struct ClipboardCommand {
    program: &'static str,
    args: &'static [&'static str],
}

#[cfg(any(target_os = "macos", target_os = "linux"))]
impl ClipboardCommand {
    const fn new(program: &'static str, args: &'static [&'static str]) -> Self {
        Self { program, args }
    }
}

#[cfg(any(target_os = "macos", target_os = "linux"))]
fn copy_to_clipboard_with_commands(
    text: &str,
    commands: &[ClipboardCommand],
) -> std::io::Result<()> {
    use std::process::{Command, Stdio};

    let mut failures = Vec::new();

    for command in commands {
        let mut child = match Command::new(command.program)
            .args(command.args)
            .stdin(Stdio::piped())
            .spawn()
        {
            Ok(child) => child,
            Err(err) => {
                failures.push(format!("{}: {}", command.program, err));
                continue;
            }
        };

        if let Some(stdin) = child.stdin.as_mut() {
            if let Err(err) = stdin.write_all(text.as_bytes()) {
                failures.push(format!("{}: {}", command.program, err));
                let _ = child.wait();
                continue;
            }
        }

        match child.wait() {
            Ok(status) if status.success() => return Ok(()),
            Ok(status) => failures.push(format!("{} exited with {}", command.program, status)),
            Err(err) => failures.push(format!("{}: {}", command.program, err)),
        }
    }

    Err(std::io::Error::new(
        std::io::ErrorKind::NotFound,
        format!("no clipboard command succeeded ({})", failures.join("; ")),
    ))
}

#[cfg(target_os = "linux")]
fn linux_clipboard_commands() -> Vec<ClipboardCommand> {
    linux_clipboard_commands_for(std::env::var_os("WAYLAND_DISPLAY").as_deref())
}

#[cfg(target_os = "linux")]
fn linux_clipboard_commands_for(
    wayland_display: Option<&std::ffi::OsStr>,
) -> Vec<ClipboardCommand> {
    let mut commands = Vec::new();

    if wayland_display.is_some_and(|display| !display.is_empty()) {
        commands.push(ClipboardCommand::new("wl-copy", &[]));
    }

    commands.push(ClipboardCommand::new("xclip", &["-selection", "clipboard"]));
    commands.push(ClipboardCommand::new("xsel", &["--clipboard", "--input"]));

    commands
}

/// Open a URL in the default browser.
///
/// On macOS, this uses `open`.
/// On Linux, this uses `xdg-open`.
/// On other platforms, this is a no-op.
fn open_url(url: &str) -> std::io::Result<()> {
    #[cfg(target_os = "macos")]
    {
        use std::process::Command;
        Command::new("open").arg(url).spawn()?.wait()?;
        Ok(())
    }

    #[cfg(target_os = "linux")]
    {
        use std::process::Command;
        Command::new("xdg-open").arg(url).spawn()?.wait()?;
        Ok(())
    }

    #[cfg(not(any(target_os = "macos", target_os = "linux")))]
    {
        // No URL open support on other platforms
        let _ = url;
        Ok(())
    }
}
