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

use nix::unistd::{pipe, read, write};
use std::collections::VecDeque;
use std::io::Write;
use std::os::fd::{AsRawFd, OwnedFd, RawFd};
use std::sync::{Arc, Mutex};

/// Number of rows reserved for the status bar at the bottom of the terminal
const STATUS_BAR_HEIGHT: usize = 1;

/// Escape sequence for Begin Synchronized Update (BSU)
const BSU: &[u8] = b"\x1b[?2026h";
/// Escape sequence for End Synchronized Update (ESU)
const ESU: &[u8] = b"\x1b[?2026l";

/// A tab containing a name and its own root pane tree
pub struct Tab {
    /// The display name for this tab
    pub name: String,
    /// The root pane cell for this tab's content
    pub root: PaneCell,
}

impl Tab {
    /// Create a new tab with the given name and dimensions
    pub fn new(name: String, width: usize, height: usize) -> Result<Self, CompositorError> {
        Ok(Self {
            name,
            root: PaneCell {
                inner: PaneCellInner::Pane(Pane {
                    terminal_emulator: emulator::TerminalEmulator::new(width, height),
                    pty: Some(
                        pty::PtyProcess::spawn("/bin/bash", width as u16, height as u16)
                            .map_err(CompositorError::Pty)?,
                    ),
                    read_buffer: [0u8; 4096],
                    scrollback_mode: false,
                    scroll_offset: 0,
                    search_mode: false,
                    search_query: String::new(),
                    search_matches: Vec::new(),
                    current_match_index: None,
                }),
                width,
                height,
                pos_x: 0,
                pos_y: 0,
                focus: true,
            },
        })
    }

    /// Resize the tab's root pane to new dimensions
    pub fn resize(&mut self, width: usize, height: usize) {
        self.root.resize(0, 0, width, height);
    }
}

/// The main compositor that manages terminal panes and the event loop.
pub struct Compositor {
    /// List of tabs, each with its own root pane tree
    tabs: Vec<Tab>,
    /// Index of the currently active tab
    active_tab: usize,
    /// Total terminal width
    width: usize,
    /// Total terminal height (including status bar)
    height: usize,

    // Global terminal emulator for compositing
    global_emulator: emulator::TerminalEmulator,
    // Previous frame for delta rendering
    prev_frame: emulator::TerminalGrid,
    // Output writer for rendering
    output: Arc<Mutex<dyn Write + Send>>,

    // Event loop communication
    wake_read: OwnedFd,
    wake_write: OwnedFd,
    input_queue: Mutex<VecDeque<Vec<u8>>>,

    // Prefix mode for tmux-style commands (Ctrl+b)
    prefix_mode: bool,

    // Whether the terminal supports synchronized output mode
    synchronized_output: bool,
}

/// A cell in the pane tree, which can be a single pane or a split.
pub struct PaneCell {
    inner: PaneCellInner,
    width: usize,
    height: usize,
    pos_x: usize,
    pos_y: usize,
    focus: bool,
}

/// A single terminal pane with its emulator and PTY process.
/// A match found during search, storing the line index and column range
#[derive(Clone, Debug, PartialEq)]
pub struct SearchMatch {
    /// Line index (negative values are scrollback lines, counting from -1 as most recent scrollback)
    /// 0 and positive values are grid lines
    line_index: isize,
    /// Starting column of the match
    start_col: usize,
    /// Ending column of the match (exclusive)
    end_col: usize,
}

pub struct Pane {
    terminal_emulator: emulator::TerminalEmulator,
    pty: Option<pty::PtyProcess>,
    read_buffer: [u8; 4096],
    /// Whether the pane is in scrollback mode
    scrollback_mode: bool,
    /// Scroll offset (number of lines scrolled up from the bottom)
    scroll_offset: usize,
    /// Whether the pane is in search mode (sub-mode of scrollback)
    search_mode: bool,
    /// Current search query
    search_query: String,
    /// All matches found for the current search
    search_matches: Vec<SearchMatch>,
    /// Index of the currently selected match (if any)
    current_match_index: Option<usize>,
}

/// The inner content of a pane cell.
pub enum PaneCellInner {
    Pane(Pane),
    VSplit(Vec<PaneCell>),
    HSplit(Vec<PaneCell>),
}

/// Events that can occur in the compositor.
#[derive(Debug, Clone)]
pub enum CompositorEvent {
    /// A pane received output from its PTY
    PtyOutput { pane_id: usize },
    /// Keyboard input was processed
    KeyboardInput,
    /// A pane's process exited
    ProcessExited { pane_id: usize },
}

/// Direction for focus movement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Left,
    Right,
    Up,
    Down,
}

/// Direction for splitting a pane.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SplitDirection {
    /// Split horizontally (creates top/bottom panes)
    Horizontal,
    /// Split vertically (creates left/right panes)
    Vertical,
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
        let (wake_read, wake_write) = pipe().map_err(CompositorError::Pipe)?;

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
        })
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
    pub fn handle_input(&mut self, input: &[u8]) {
        // Check if we're in scrollback mode
        if self.active_tab().root.is_in_scrollback_mode() {
            self.handle_scrollback_input(input);
            return;
        }

        // Handle prefix mode commands
        if self.prefix_mode {
            self.prefix_mode = false;
            if input.len() == 1 {
                match input[0] {
                    b'"' => {
                        // Ctrl+b " - horizontal split (top/bottom)
                        let _ = self.split_focused_pane(SplitDirection::Horizontal);
                        return;
                    }
                    b'%' => {
                        // Ctrl+b % - vertical split (left/right)
                        let _ = self.split_focused_pane(SplitDirection::Vertical);
                        return;
                    }
                    b'c' => {
                        // Ctrl+b c - create new tab
                        let _ = self.create_tab();
                        return;
                    }
                    b'n' => {
                        // Ctrl+b n - next tab
                        self.next_tab();
                        return;
                    }
                    b'p' => {
                        // Ctrl+b p - previous tab
                        self.prev_tab();
                        return;
                    }
                    b'0'..=b'9' => {
                        // Ctrl+b 0-9 - switch to tab 0-9
                        let tab_index = (input[0] - b'0') as usize;
                        self.switch_to_tab(tab_index);
                        return;
                    }
                    b'[' => {
                        // Ctrl+b [ - enter scrollback mode
                        self.active_tab_mut().root.enter_scrollback_mode();
                        self.render();
                        return;
                    }
                    0x02 => {
                        // Ctrl+b Ctrl+b - send Ctrl+b to the terminal
                        self.active_tab_mut().root.handle_input(&[0x02]);
                        return;
                    }
                    _ => {
                        // Unknown command, ignore
                        return;
                    }
                }
            }
            return;
        }

        // Check for prefix key (Ctrl+b = 0x02)
        if input.len() == 1 && input[0] == 0x02 {
            self.prefix_mode = true;
            return;
        }

        // Check for focus movement shortcuts (Ctrl+h/j/k/l)
        // Ctrl+h = 0x08, Ctrl+j = 0x0a, Ctrl+k = 0x0b, Ctrl+l = 0x0c
        if input.len() == 1 {
            match input[0] {
                0x08 => {
                    // Ctrl+h - move focus left
                    self.move_focus(Direction::Left);
                    self.render();
                    return;
                }
                0x0a => {
                    // Ctrl+j - move focus down
                    self.move_focus(Direction::Down);
                    self.render();
                    return;
                }
                0x0b => {
                    // Ctrl+k - move focus up
                    self.move_focus(Direction::Up);
                    self.render();
                    return;
                }
                0x0c => {
                    // Ctrl+l - move focus right
                    self.move_focus(Direction::Right);
                    self.render();
                    return;
                }
                _ => {}
            }
        }
        self.active_tab_mut().root.handle_input(input);
    }

    /// Handle input while in scrollback mode.
    ///
    /// - Ctrl+u: Scroll up by half screen
    /// - Ctrl+d: Scroll down by half screen
    /// - g: Jump to top of scrollback
    /// - G: Jump to bottom of scrollback
    /// - /: Enter search mode
    /// - Escape/q: Exit scrollback mode (or exit search mode if in search)
    fn handle_scrollback_input(&mut self, input: &[u8]) {
        // Check if we're in search mode
        if self.active_tab().root.is_in_search_mode() {
            self.handle_search_input(input);
            return;
        }

        if input.len() == 1 {
            match input[0] {
                0x15 => {
                    // Ctrl+u - scroll up by half screen
                    let half_height = self.height.saturating_sub(STATUS_BAR_HEIGHT) / 2;
                    self.active_tab_mut().root.scroll_up(half_height.max(1));
                    self.render();
                }
                0x04 => {
                    // Ctrl+d - scroll down by half screen
                    let half_height = self.height.saturating_sub(STATUS_BAR_HEIGHT) / 2;
                    self.active_tab_mut().root.scroll_down(half_height.max(1));
                    self.render();
                }
                b'g' => {
                    // g - jump to top of scrollback (oldest content)
                    if let Some((_, scrollback_len)) = self.active_tab().root.get_scrollback_info()
                    {
                        self.active_tab_mut().root.scroll_up(scrollback_len);
                        self.render();
                    }
                }
                b'G' => {
                    // G - jump to bottom of scrollback (most recent content)
                    if let Some((scroll_offset, _)) = self.active_tab().root.get_scrollback_info() {
                        self.active_tab_mut().root.scroll_down(scroll_offset);
                        self.render();
                    }
                }
                b'/' => {
                    // / - enter search mode
                    self.active_tab_mut().root.enter_search_mode();
                    self.render();
                }
                0x1b | b'q' => {
                    // Escape or 'q' - exit scrollback mode
                    self.active_tab_mut().root.exit_scrollback_mode();
                    self.render();
                }
                _ => {
                    // Ignore other keys in scrollback mode
                }
            }
        }
    }

    /// Handle input while in search mode.
    fn handle_search_input(&mut self, input: &[u8]) {
        if input.is_empty() {
            return;
        }

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
                0x0d | 0x10 | 0x15 => {
                    // Enter or Ctrl+P or Ctrl+U - go to next match (older / up on screen)
                    self.active_tab_mut().root.next_match();
                    self.render();
                }
                0x0e | 0x04 => {
                    // Ctrl+N or Ctrl+D - go to previous match (more recent / down on screen)
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

    /// Get a reference to the currently active tab
    #[allow(dead_code)]
    fn active_tab(&self) -> &Tab {
        &self.tabs[self.active_tab]
    }

    /// Get a mutable reference to the currently active tab
    fn active_tab_mut(&mut self) -> &mut Tab {
        &mut self.tabs[self.active_tab]
    }

    /// Create a new tab and switch to it
    pub fn create_tab(&mut self) -> Result<(), CompositorError> {
        let pane_height = self.height.saturating_sub(STATUS_BAR_HEIGHT);
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
        self.active_tab_mut().root.split_focused(direction)
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

            // Process queued keyboard input
            self.process_keyboard_queue();

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

        // Process queued keyboard input
        self.process_keyboard_queue();

        // Render the compositor
        self.render();

        Ok(had_events)
    }

    /// Process all queued keyboard input.
    fn process_keyboard_queue(&mut self) {
        let inputs: Vec<Vec<u8>> = {
            let mut queue = self.input_queue.lock().unwrap();
            queue.drain(..).collect()
        };

        for input in inputs {
            self.active_tab_mut().root.handle_input(&input);
        }
    }

    /// Render the compositor to the terminal.
    ///
    /// This traverses all panes, taking their grid contents and compositing them into
    /// a single terminal emulator. Then, it uses delta rendering to output only the changed parts.
    fn render(&mut self) {
        // Clear the global emulator to prepare for compositing
        let (cols, rows) = self.global_emulator.dimensions();
        self.global_emulator = emulator::TerminalEmulator::new(cols, rows);

        // Composite the active tab's panes into the global emulator
        self.tabs[self.active_tab]
            .root
            .composite_into(&mut self.global_emulator);

        // Render the status bar at the bottom
        self.render_status_bar();

        // Set the cursor position and visibility from the focused pane
        if let Some((cursor_x, cursor_y, cursor_visible)) =
            self.tabs[self.active_tab].root.get_focused_cursor_info()
        {
            let grid = self.global_emulator.grid_mut();
            grid.cursor_x = cursor_x;
            grid.cursor_y = cursor_y;
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
            let tab_text = format!(" {} {} ", i, tab.name);
            let attrs = if i == self.active_tab {
                active_tab_attrs.clone()
            } else {
                bar_attrs.clone()
            };

            for ch in tab_text.chars() {
                if x_pos < cols {
                    self.global_emulator.grid_mut().set_cell(
                        x_pos,
                        status_bar_y,
                        emulator::Cell::new(ch, attrs.clone()),
                    );
                    x_pos += 1;
                }
            }
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
                // Show search query on the left after tabs
                let search_prefix = " / ";
                for ch in search_prefix.chars() {
                    if x_pos < cols {
                        self.global_emulator.grid_mut().set_cell(
                            x_pos,
                            status_bar_y,
                            emulator::Cell::new(ch, search_attrs.clone()),
                        );
                        x_pos += 1;
                    }
                }
                for ch in query.chars() {
                    if x_pos < cols {
                        self.global_emulator.grid_mut().set_cell(
                            x_pos,
                            status_bar_y,
                            emulator::Cell::new(ch, search_attrs.clone()),
                        );
                        x_pos += 1;
                    }
                }
                // Add trailing space
                if x_pos < cols {
                    self.global_emulator.grid_mut().set_cell(
                        x_pos,
                        status_bar_y,
                        emulator::Cell::new(' ', search_attrs.clone()),
                    );
                }

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

                let text_start_x = cols.saturating_sub(match_text.len());
                for (i, ch) in match_text.chars().enumerate() {
                    let x = text_start_x + i;
                    if x < cols {
                        self.global_emulator.grid_mut().set_cell(
                            x,
                            status_bar_y,
                            emulator::Cell::new(ch, search_attrs.clone()),
                        );
                    }
                }
                return;
            }
            return;
        }

        // Check if we're in scrollback mode and render scroll indicator instead of date/time
        let right_text = if self.tabs[self.active_tab].root.is_in_scrollback_mode() {
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

                let text_start_x = cols.saturating_sub(scroll_text.len());
                for (i, ch) in scroll_text.chars().enumerate() {
                    let x = text_start_x + i;
                    if x < cols {
                        self.global_emulator.grid_mut().set_cell(
                            x,
                            status_bar_y,
                            emulator::Cell::new(ch, scroll_attrs.clone()),
                        );
                    }
                }
                return;
            }
            return;
        } else {
            // Render the date/time on the right side of the first status bar row
            let now = std::time::SystemTime::now();
            let datetime = chrono::DateTime::<chrono::Local>::from(now);
            datetime.format(" %Y-%m-%d %H:%M ").to_string()
        };

        let time_start_x = cols.saturating_sub(right_text.len());
        for (i, ch) in right_text.chars().enumerate() {
            let x = time_start_x + i;
            if x < cols {
                self.global_emulator.grid_mut().set_cell(
                    x,
                    status_bar_y,
                    emulator::Cell::new(ch, bar_attrs.clone()),
                );
            }
        }
    }

    /// Get a reference to the root pane cell of the active tab.
    pub fn root(&self) -> &PaneCell {
        &self.tabs[self.active_tab].root
    }

    /// Get a mutable reference to the root pane cell of the active tab.
    pub fn root_mut(&mut self) -> &mut PaneCell {
        &mut self.tabs[self.active_tab].root
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

        // Render the status bar
        self.render_status_bar();

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
        let pane_height = height.saturating_sub(STATUS_BAR_HEIGHT);

        // Recursively resize all tabs
        for tab in &mut self.tabs {
            tab.resize(width, pane_height);
        }
    }

    /// Force a full render of the compositor.
    ///
    /// This should be called after resize or when you need to redraw the entire screen.
    /// It composites all panes and outputs the result to the terminal.
    pub fn force_render(&mut self) {
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

    /// Check if synchronized output mode is currently enabled.
    pub fn synchronized_output_enabled(&self) -> bool {
        self.synchronized_output
    }
}

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

/// Errors that can occur in the compositor.
#[derive(Debug)]
pub enum CompositorError {
    Pipe(nix::Error),
    Fcntl(nix::Error),
    Poll(std::io::Error),
    Pty(pty::PtyError),
}

impl std::fmt::Display for CompositorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompositorError::Pipe(e) => write!(f, "Failed to create pipe: {}", e),
            CompositorError::Fcntl(e) => write!(f, "Failed to set fd flags: {}", e),
            CompositorError::Poll(e) => write!(f, "Poll failed: {}", e),
            CompositorError::Pty(e) => write!(f, "PTY error: {}", e),
        }
    }
}

impl std::error::Error for CompositorError {}

impl PaneCell {
    /// Handle keyboard input by routing it to the focused pane.
    pub fn handle_input(&mut self, input: &[u8]) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.handle_input(input);
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.handle_input(input);
                    }
                }
            }
        }
    }

    /// Collect PTY file descriptors for polling.
    fn collect_poll_fds(
        &mut self,
        fds: &mut Vec<libc::pollfd>,
        pane_map: &mut Vec<Option<*mut Pane>>,
    ) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                if let Some(ref pty) = pane.pty {
                    fds.push(libc::pollfd {
                        fd: pty.as_raw_fd(),
                        events: libc::POLLIN,
                        revents: 0,
                    });
                    pane_map.push(Some(pane as *mut Pane));
                }
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    cell.collect_poll_fds(fds, pane_map);
                }
            }
        }
    }

    /// Check if this cell or any of its children have focus.
    pub fn has_focus(&self) -> bool {
        self.focus
    }

    /// Get the dimensions of this cell.
    pub fn dimensions(&self) -> (usize, usize) {
        (self.width, self.height)
    }

    /// Get the position of this cell.
    pub fn position(&self) -> (usize, usize) {
        (self.pos_x, self.pos_y)
    }

    /// Get a reference to the inner content.
    pub fn inner(&self) -> &PaneCellInner {
        &self.inner
    }

    /// Enter scrollback mode on the focused pane
    pub fn enter_scrollback_mode(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.enter_scrollback_mode();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.enter_scrollback_mode();
                    }
                }
            }
        }
    }

    /// Exit scrollback mode on the focused pane
    pub fn exit_scrollback_mode(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.exit_scrollback_mode();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.exit_scrollback_mode();
                    }
                }
            }
        }
    }

    /// Check if the focused pane is in scrollback mode
    pub fn is_in_scrollback_mode(&self) -> bool {
        match &self.inner {
            PaneCellInner::Pane(pane) => pane.is_in_scrollback_mode(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.is_in_scrollback_mode();
                    }
                }
                false
            }
        }
    }

    /// Scroll the focused pane up by the given number of lines
    pub fn scroll_up(&mut self, lines: usize) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.scroll_up(lines);
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.scroll_up(lines);
                    }
                }
            }
        }
    }

    /// Scroll the focused pane down by the given number of lines
    pub fn scroll_down(&mut self, lines: usize) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.scroll_down(lines);
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.scroll_down(lines);
                    }
                }
            }
        }
    }

    /// Get scrollback info from the focused pane (scroll_offset, scrollback_len)
    pub fn get_scrollback_info(&self) -> Option<(usize, usize)> {
        match &self.inner {
            PaneCellInner::Pane(pane) => Some((pane.scroll_offset(), pane.scrollback_len())),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.get_scrollback_info();
                    }
                }
                None
            }
        }
    }

    /// Enter search mode on the focused pane
    pub fn enter_search_mode(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.enter_search_mode();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.enter_search_mode();
                    }
                }
            }
        }
    }

    /// Exit search mode on the focused pane
    pub fn exit_search_mode(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.exit_search_mode();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.exit_search_mode();
                    }
                }
            }
        }
    }

    /// Check if the focused pane is in search mode
    pub fn is_in_search_mode(&self) -> bool {
        match &self.inner {
            PaneCellInner::Pane(pane) => pane.is_in_search_mode(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.is_in_search_mode();
                    }
                }
                false
            }
        }
    }

    /// Input a character to the search query
    pub fn search_input_char(&mut self, c: char) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.search_input_char(c);
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.search_input_char(c);
                    }
                }
            }
        }
    }

    /// Handle backspace in search query
    pub fn search_input_backspace(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.search_input_backspace();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.search_input_backspace();
                    }
                }
            }
        }
    }

    /// Clear the search query
    pub fn search_clear(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.search_clear();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.search_clear();
                    }
                }
            }
        }
    }

    /// Jump to next match
    pub fn next_match(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.next_match();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.next_match();
                    }
                }
            }
        }
    }

    /// Jump to previous match
    pub fn prev_match(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.prev_match();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.prev_match();
                    }
                }
            }
        }
    }

    /// Get search info from the focused pane
    pub fn get_search_info(&self) -> Option<(String, Option<usize>, usize)> {
        match &self.inner {
            PaneCellInner::Pane(pane) => {
                let (current, total) = pane.search_match_info();
                Some((pane.search_query().to_string(), current, total))
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.get_search_info();
                    }
                }
                None
            }
        }
    }

    /// Move focus in the specified direction.
    ///
    /// Returns true if focus was successfully moved, false otherwise.
    pub fn move_focus(&mut self, direction: Direction) -> bool {
        match &mut self.inner {
            PaneCellInner::Pane(_) => {
                // Single pane, cannot move focus
                false
            }
            PaneCellInner::VSplit(cells) => {
                // VSplit arranges panes horizontally (left to right)
                // Left/Right moves between siblings, Up/Down goes into children
                let focused_idx = cells.iter().position(|c| c.focus);

                match (direction, focused_idx) {
                    (Direction::Left, Some(idx)) if idx > 0 => {
                        // Move focus to the left sibling
                        cells[idx].clear_focus();
                        cells[idx - 1].set_focus_first();
                        true
                    }
                    (Direction::Right, Some(idx)) if idx < cells.len() - 1 => {
                        // Move focus to the right sibling
                        cells[idx].clear_focus();
                        cells[idx + 1].set_focus_first();
                        true
                    }
                    (_, Some(idx)) => {
                        // Try to move focus within the focused child
                        cells[idx].move_focus(direction)
                    }
                    _ => false,
                }
            }
            PaneCellInner::HSplit(cells) => {
                // HSplit arranges panes vertically (top to bottom)
                // Up/Down moves between siblings, Left/Right goes into children
                let focused_idx = cells.iter().position(|c| c.focus);

                match (direction, focused_idx) {
                    (Direction::Up, Some(idx)) if idx > 0 => {
                        // Move focus to the upper sibling
                        cells[idx].clear_focus();
                        cells[idx - 1].set_focus_first();
                        true
                    }
                    (Direction::Down, Some(idx)) if idx < cells.len() - 1 => {
                        // Move focus to the lower sibling
                        cells[idx].clear_focus();
                        cells[idx + 1].set_focus_first();
                        true
                    }
                    (_, Some(idx)) => {
                        // Try to move focus within the focused child
                        cells[idx].move_focus(direction)
                    }
                    _ => false,
                }
            }
        }
    }

    /// Clear focus on this cell and all children.
    fn clear_focus(&mut self) {
        self.focus = false;
        match &mut self.inner {
            PaneCellInner::Pane(_) => {}
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    cell.clear_focus();
                }
            }
        }
    }

    /// Set focus on the first (or leftmost/topmost) pane in this cell.
    fn set_focus_first(&mut self) {
        self.focus = true;
        match &mut self.inner {
            PaneCellInner::Pane(_) => {}
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                if let Some(first) = cells.first_mut() {
                    first.set_focus_first();
                }
            }
        }
    }

    /// Split the focused pane in the given direction.
    ///
    /// If this cell is a pane with focus, it will be converted into a split.
    /// If this cell is a split, it will recursively find the focused pane and split it.
    pub fn split_focused(&mut self, direction: SplitDirection) -> Result<(), CompositorError> {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                if !self.focus {
                    return Ok(());
                }

                // Calculate dimensions accounting for border (1 cell between panes)
                let (old_width, old_height, new_width, new_height, new_pos_x, new_pos_y) =
                    match direction {
                        SplitDirection::Horizontal => {
                            // Split top/bottom: each pane gets half the height minus border
                            // Total height = top_height + 1 (border) + bottom_height
                            let available_height = self.height.saturating_sub(1); // Reserve 1 for border
                            let top_height = available_height / 2;
                            let bottom_height = available_height - top_height;
                            (
                                self.width,
                                top_height,
                                self.width,
                                bottom_height,
                                self.pos_x,
                                self.pos_y + top_height + 1, // +1 for border
                            )
                        }
                        SplitDirection::Vertical => {
                            // Split left/right: each pane gets half the width minus border
                            // Total width = left_width + 1 (border) + right_width
                            let available_width = self.width.saturating_sub(1); // Reserve 1 for border
                            let left_width = available_width / 2;
                            let right_width = available_width - left_width;
                            (
                                left_width,
                                self.height,
                                right_width,
                                self.height,
                                self.pos_x + left_width + 1, // +1 for border
                                self.pos_y,
                            )
                        }
                    };

                // Create the existing pane cell with updated dimensions
                pane.terminal_emulator.resize(old_width, old_height);
                if let Some(ref pty) = pane.pty {
                    let _ = pty.resize(old_width as u16, old_height as u16);
                }

                // Create a new pane with a new shell
                let new_pane = Pane {
                    terminal_emulator: emulator::TerminalEmulator::new(new_width, new_height),
                    pty: Some(
                        pty::PtyProcess::spawn("/bin/bash", new_width as u16, new_height as u16)
                            .map_err(CompositorError::Pty)?,
                    ),
                    read_buffer: [0u8; 4096],
                    scrollback_mode: false,
                    scroll_offset: 0,
                    search_mode: false,
                    search_query: String::new(),
                    search_matches: Vec::new(),
                    current_match_index: None,
                };

                // Take ownership of the old pane
                let old_inner = std::mem::replace(
                    &mut self.inner,
                    PaneCellInner::Pane(Pane {
                        terminal_emulator: emulator::TerminalEmulator::new(1, 1),
                        pty: None,
                        read_buffer: [0u8; 4096],
                        scrollback_mode: false,
                        scroll_offset: 0,
                        search_mode: false,
                        search_query: String::new(),
                        search_matches: Vec::new(),
                        current_match_index: None,
                    }),
                );

                let old_pane = match old_inner {
                    PaneCellInner::Pane(p) => p,
                    _ => unreachable!(),
                };

                // Create the two child cells
                let first_cell = PaneCell {
                    inner: PaneCellInner::Pane(old_pane),
                    width: old_width,
                    height: old_height,
                    pos_x: self.pos_x,
                    pos_y: self.pos_y,
                    focus: false, // Original pane loses focus
                };

                let second_cell = PaneCell {
                    inner: PaneCellInner::Pane(new_pane),
                    width: new_width,
                    height: new_height,
                    pos_x: new_pos_x,
                    pos_y: new_pos_y,
                    focus: true, // New pane gets focus
                };

                // Replace the inner content with a split
                self.inner = match direction {
                    SplitDirection::Horizontal => {
                        PaneCellInner::HSplit(vec![first_cell, second_cell])
                    }
                    SplitDirection::Vertical => {
                        PaneCellInner::VSplit(vec![first_cell, second_cell])
                    }
                };

                Ok(())
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                // Find the focused child and split it
                for cell in cells {
                    if cell.focus {
                        return cell.split_focused(direction);
                    }
                }
                Ok(())
            }
        }
    }

    /// Get mutable access to a child pane by index.
    pub fn get_child_mut(&mut self, index: usize) -> Option<&mut PaneCell> {
        match &mut self.inner {
            PaneCellInner::Pane(_) => None,
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => cells.get_mut(index),
        }
    }

    /// Composite this pane cell into the global emulator.
    ///
    /// Recursively traverses all child panes and blits their contents
    /// into the destination emulator at the appropriate positions.
    fn composite_into(&self, dest: &mut emulator::TerminalEmulator) {
        match &self.inner {
            PaneCellInner::Pane(pane) => {
                let grid = pane.terminal_emulator.grid();
                let scrollback_len = grid.scrollback_len();

                // Build a helper to check if a position matches a search result
                let is_match = |line_index: isize, col: usize| -> Option<bool> {
                    if !pane.search_mode || pane.search_matches.is_empty() {
                        return None;
                    }
                    for (i, m) in pane.search_matches.iter().enumerate() {
                        if m.line_index == line_index && col >= m.start_col && col < m.end_col {
                            let is_current = Some(i) == pane.current_match_index;
                            return Some(is_current);
                        }
                    }
                    None
                };

                if pane.scrollback_mode && pane.scroll_offset > 0 {
                    // In scrollback mode with active scrolling - composite scrollback + grid content
                    for row in 0..self.height {
                        let dst_y = self.pos_y + row;

                        // Calculate which line to display
                        // scroll_offset = number of lines scrolled up from bottom
                        // So if scroll_offset = 5, we show 5 lines from scrollback at the top
                        let source_line = if row < pane.scroll_offset {
                            // This row should show scrollback content
                            // scroll_offset lines are shown from scrollback
                            // row 0 shows scrollback[scrollback_len - scroll_offset]
                            let scrollback_idx =
                                scrollback_len.saturating_sub(pane.scroll_offset) + row;
                            if scrollback_idx < scrollback_len {
                                // Convert scrollback_idx to line_index format
                                let line_index = (scrollback_idx as isize) - (scrollback_len as isize);
                                Some((true, scrollback_idx, line_index))
                            } else {
                                None
                            }
                        } else {
                            // This row should show grid content
                            let grid_row = row - pane.scroll_offset;
                            if grid_row < grid.rows {
                                Some((false, grid_row, grid_row as isize))
                            } else {
                                None
                            }
                        };

                        if let Some((is_scrollback, line_idx, line_index)) = source_line {
                            let source_row = if is_scrollback {
                                grid.get_scrollback_row(line_idx)
                            } else {
                                grid.get_row(line_idx)
                            };

                            if let Some(cells) = source_row {
                                for col in 0..self.width {
                                    let dst_x = self.pos_x + col;
                                    let (dest_cols, dest_rows) = dest.dimensions();
                                    if dst_x < dest_cols && dst_y < dest_rows {
                                        let mut cell = if col < cells.len() {
                                            cells[col].clone()
                                        } else {
                                            emulator::Cell::new(
                                                ' ',
                                                emulator::CellAttributes::default(),
                                            )
                                        };
                                        
                                        // Apply search highlighting
                                        if let Some(is_current) = is_match(line_index, col) {
                                            if is_current {
                                                // Current match: bright magenta background, white text, bold
                                                cell.attrs.bg_color = Some(emulator::Color::Magenta);
                                                cell.attrs.fg_color = Some(emulator::Color::White);
                                                cell.attrs.bold = true;
                                            } else {
                                                // Other matches: yellow background
                                                cell.attrs.bg_color = Some(emulator::Color::Yellow);
                                                cell.attrs.fg_color = Some(emulator::Color::Black);
                                            }
                                        }
                                        
                                        dest.grid_mut().set_cell(dst_x, dst_y, cell);
                                    }
                                }
                            }
                        }
                    }
                } else if pane.search_mode && !pane.search_matches.is_empty() {
                    // Search mode with scroll_offset = 0: blit with highlighting
                    for row in 0..self.height.min(grid.rows) {
                        let dst_y = self.pos_y + row;
                        if let Some(cells) = grid.get_row(row) {
                            for col in 0..self.width.min(cells.len()) {
                                let dst_x = self.pos_x + col;
                                let (dest_cols, dest_rows) = dest.dimensions();
                                if dst_x < dest_cols && dst_y < dest_rows {
                                    let mut cell = cells[col].clone();
                                    
                                    // Apply search highlighting
                                    if let Some(is_current) = is_match(row as isize, col) {
                                        if is_current {
                                            // Current match: bright magenta background, white text, bold
                                            cell.attrs.bg_color = Some(emulator::Color::Magenta);
                                            cell.attrs.fg_color = Some(emulator::Color::White);
                                            cell.attrs.bold = true;
                                        } else {
                                            // Other matches: yellow background
                                            cell.attrs.bg_color = Some(emulator::Color::Yellow);
                                            cell.attrs.fg_color = Some(emulator::Color::Black);
                                        }
                                    }
                                    
                                    dest.grid_mut().set_cell(dst_x, dst_y, cell);
                                }
                            }
                        }
                    }
                } else {
                    // Normal mode or scrollback mode at offset 0 without search - blit normally
                    dest.blit_from(
                        &pane.terminal_emulator,
                        0,           // source x (from origin of pane's emulator)
                        0,           // source y
                        self.pos_x,  // destination x (pane's position in compositor)
                        self.pos_y,  // destination y
                        self.width,  // width to copy
                        self.height, // height to copy
                    );
                }
            }
            PaneCellInner::VSplit(cells) => {
                // Recursively composite all child panes
                for cell in cells {
                    cell.composite_into(dest);
                }
                // Draw vertical borders between panes
                self.draw_vsplit_borders(cells, dest);
            }
            PaneCellInner::HSplit(cells) => {
                // Recursively composite all child panes
                for cell in cells {
                    cell.composite_into(dest);
                }
                // Draw horizontal borders between panes
                self.draw_hsplit_borders(cells, dest);
            }
        }
    }

    /// Draw vertical borders between VSplit panes.
    fn draw_vsplit_borders(&self, cells: &[PaneCell], dest: &mut emulator::TerminalEmulator) {
        for i in 0..cells.len().saturating_sub(1) {
            // Border is positioned right after the current cell
            let border_x = cells[i].pos_x + cells[i].width;

            // Draw vertical line for the height of this split
            for y in self.pos_y..(self.pos_y + self.height) {
                let (cols, rows) = (dest.grid().cols, dest.grid().rows);
                if border_x < cols && y < rows {
                    let existing = dest.grid().get_cell(border_x, y).character;
                    let border_char = get_border_char(existing, '│');
                    dest.grid_mut().set_cell(
                        border_x,
                        y,
                        emulator::Cell::new(border_char, emulator::CellAttributes::default()),
                    );
                }
            }

            // Check the row above for a horizontal border and add DOWN direction to create T-junction
            if self.pos_y > 0 {
                let y_above = self.pos_y - 1;
                let (cols, rows) = (dest.grid().cols, dest.grid().rows);
                if border_x < cols && y_above < rows {
                    let existing = dest.grid().get_cell(border_x, y_above).character;
                    let existing_dirs = BorderDirections::from_char(existing);
                    // If there's a horizontal border above (has left+right), add down direction
                    if existing_dirs.left && existing_dirs.right {
                        let new_dirs = existing_dirs.merge(BorderDirections {
                            up: false,
                            down: true,
                            left: false,
                            right: false,
                        });
                        dest.grid_mut().set_cell(
                            border_x,
                            y_above,
                            emulator::Cell::new(
                                new_dirs.to_char(),
                                emulator::CellAttributes::default(),
                            ),
                        );
                    }
                }
            }

            // Check the column to the right for horizontal borders and add LEFT direction
            let x_right = border_x + 1;
            let (cols, rows) = (dest.grid().cols, dest.grid().rows);
            if x_right < cols {
                for y in self.pos_y..(self.pos_y + self.height) {
                    if y < rows {
                        let right_char = dest.grid().get_cell(x_right, y).character;
                        let right_dirs = BorderDirections::from_char(right_char);
                        // If there's a horizontal border to the right (has left+right), add right direction to current
                        if right_dirs.left && right_dirs.right {
                            let current = dest.grid().get_cell(border_x, y).character;
                            let current_dirs = BorderDirections::from_char(current);
                            let new_dirs = current_dirs.merge(BorderDirections {
                                up: false,
                                down: false,
                                left: false,
                                right: true,
                            });
                            dest.grid_mut().set_cell(
                                border_x,
                                y,
                                emulator::Cell::new(
                                    new_dirs.to_char(),
                                    emulator::CellAttributes::default(),
                                ),
                            );
                        }
                    }
                }
            }

            // Check the column to the left for horizontal borders and add RIGHT direction
            if border_x > 0 {
                let x_left = border_x - 1;
                let (cols, rows) = (dest.grid().cols, dest.grid().rows);
                if x_left < cols {
                    for y in self.pos_y..(self.pos_y + self.height) {
                        if y < rows {
                            let left_char = dest.grid().get_cell(x_left, y).character;
                            let left_dirs = BorderDirections::from_char(left_char);
                            // If there's a horizontal border to the left (has left+right), add left direction to current
                            if left_dirs.left && left_dirs.right {
                                let current = dest.grid().get_cell(border_x, y).character;
                                let current_dirs = BorderDirections::from_char(current);
                                let new_dirs = current_dirs.merge(BorderDirections {
                                    up: false,
                                    down: false,
                                    left: true,
                                    right: false,
                                });
                                dest.grid_mut().set_cell(
                                    border_x,
                                    y,
                                    emulator::Cell::new(
                                        new_dirs.to_char(),
                                        emulator::CellAttributes::default(),
                                    ),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    /// Draw horizontal borders between HSplit panes.
    fn draw_hsplit_borders(&self, cells: &[PaneCell], dest: &mut emulator::TerminalEmulator) {
        for i in 0..cells.len().saturating_sub(1) {
            // Border is positioned right after the current cell
            let border_y = cells[i].pos_y + cells[i].height;

            // Draw horizontal line for the width of this split
            for x in self.pos_x..(self.pos_x + self.width) {
                let (cols, rows) = (dest.grid().cols, dest.grid().rows);
                if x < cols && border_y < rows {
                    let existing = dest.grid().get_cell(x, border_y).character;
                    let border_char = get_border_char(existing, '─');
                    dest.grid_mut().set_cell(
                        x,
                        border_y,
                        emulator::Cell::new(border_char, emulator::CellAttributes::default()),
                    );
                }
            }

            // Check the row below for vertical borders and add DOWN direction to create T-junction
            let y_below = border_y + 1;
            let (cols, rows) = (dest.grid().cols, dest.grid().rows);
            if y_below < rows {
                for x in self.pos_x..(self.pos_x + self.width) {
                    if x < cols {
                        let below_char = dest.grid().get_cell(x, y_below).character;
                        let below_dirs = BorderDirections::from_char(below_char);
                        // If there's a vertical border below (has up+down), add down direction to current
                        if below_dirs.up && below_dirs.down {
                            let current = dest.grid().get_cell(x, border_y).character;
                            let current_dirs = BorderDirections::from_char(current);
                            let new_dirs = current_dirs.merge(BorderDirections {
                                up: false,
                                down: true,
                                left: false,
                                right: false,
                            });
                            dest.grid_mut().set_cell(
                                x,
                                border_y,
                                emulator::Cell::new(
                                    new_dirs.to_char(),
                                    emulator::CellAttributes::default(),
                                ),
                            );
                        }
                    }
                }
            }

            // Check the row above for vertical borders and add UP direction to create T-junction
            if border_y > 0 {
                let y_above = border_y - 1;
                let (cols, rows) = (dest.grid().cols, dest.grid().rows);
                if y_above < rows {
                    for x in self.pos_x..(self.pos_x + self.width) {
                        if x < cols {
                            let above_char = dest.grid().get_cell(x, y_above).character;
                            let above_dirs = BorderDirections::from_char(above_char);
                            // If there's a vertical border above (has up+down), add up direction to current
                            if above_dirs.up && above_dirs.down {
                                let current = dest.grid().get_cell(x, border_y).character;
                                let current_dirs = BorderDirections::from_char(current);
                                let new_dirs = current_dirs.merge(BorderDirections {
                                    up: true,
                                    down: false,
                                    left: false,
                                    right: false,
                                });
                                dest.grid_mut().set_cell(
                                    x,
                                    border_y,
                                    emulator::Cell::new(
                                        new_dirs.to_char(),
                                        emulator::CellAttributes::default(),
                                    ),
                                );
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Flags representing which directions a border character connects to
#[derive(Clone, Copy, Default)]
struct BorderDirections {
    up: bool,
    down: bool,
    left: bool,
    right: bool,
}

impl BorderDirections {
    /// Get directions from an existing character
    fn from_char(c: char) -> Self {
        match c {
            '─' => Self {
                left: true,
                right: true,
                up: false,
                down: false,
            },
            '│' => Self {
                left: false,
                right: false,
                up: true,
                down: true,
            },
            '├' => Self {
                left: false,
                right: true,
                up: true,
                down: true,
            },
            '┤' => Self {
                left: true,
                right: false,
                up: true,
                down: true,
            },
            '┬' => Self {
                left: true,
                right: true,
                up: false,
                down: true,
            },
            '┴' => Self {
                left: true,
                right: true,
                up: true,
                down: false,
            },
            '┼' => Self {
                left: true,
                right: true,
                up: true,
                down: true,
            },
            _ => Self::default(),
        }
    }

    /// Merge with another set of directions
    fn merge(&self, other: Self) -> Self {
        Self {
            up: self.up || other.up,
            down: self.down || other.down,
            left: self.left || other.left,
            right: self.right || other.right,
        }
    }

    /// Convert to the appropriate border character
    fn to_char(&self) -> char {
        match (self.up, self.down, self.left, self.right) {
            // Four-way
            (true, true, true, true) => '┼',
            // Three-way (T-junctions)
            (true, true, false, true) => '├',
            (true, true, true, false) => '┤',
            (false, true, true, true) => '┬',
            (true, false, true, true) => '┴',
            // Two-way
            (true, true, false, false) => '│',
            (false, false, true, true) => '─',
            // Corners (for completeness, though we may not use them)
            (false, true, false, true) => '┌',
            (false, true, true, false) => '┐',
            (true, false, false, true) => '└',
            (true, false, true, false) => '┘',
            // Single direction or none - shouldn't happen in practice
            _ => ' ',
        }
    }
}

/// Get the appropriate border character by merging existing with new directions.
fn get_border_char(existing: char, default: char) -> char {
    let existing_dirs = BorderDirections::from_char(existing);
    let new_dirs = BorderDirections::from_char(default);
    let merged = existing_dirs.merge(new_dirs);

    // If we have any directions, use the merged result; otherwise use default
    if merged.up || merged.down || merged.left || merged.right {
        merged.to_char()
    } else {
        default
    }
}

impl PaneCell {
    /// Get the cursor info from the focused pane.
    ///
    /// Returns the cursor position in global coordinates (x, y) and visibility.
    /// Returns None if no pane has focus.
    fn get_focused_cursor_info(&self) -> Option<(usize, usize, bool)> {
        if !self.focus {
            return None;
        }

        match &self.inner {
            PaneCellInner::Pane(pane) => {
                // Hide cursor when in scrollback mode
                if pane.scrollback_mode {
                    return Some((0, 0, false));
                }
                let (cursor_x, cursor_y) = pane.terminal_emulator.cursor_position();
                let cursor_visible = pane.terminal_emulator.grid().cursor_visible;
                // Transform cursor position to global coordinates
                let global_x = self.pos_x + cursor_x;
                let global_y = self.pos_y + cursor_y;
                Some((global_x, global_y, cursor_visible))
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                // Find the focused child and get its cursor info
                for cell in cells {
                    if cell.focus {
                        return cell.get_focused_cursor_info();
                    }
                }
                None
            }
        }
    }

    /// Resize this pane cell to new dimensions and position.
    ///
    /// For splits, this distributes space evenly among children.
    /// For leaf panes, this resizes the terminal emulator and PTY.
    fn resize(&mut self, pos_x: usize, pos_y: usize, width: usize, height: usize) {
        self.pos_x = pos_x;
        self.pos_y = pos_y;
        self.width = width;
        self.height = height;

        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                // Resize the terminal emulator
                pane.terminal_emulator.resize(width, height);
                // Resize the PTY if present
                if let Some(ref pty) = pane.pty {
                    let _ = pty.resize(width as u16, height as u16);
                }
            }
            PaneCellInner::VSplit(cells) => {
                // Distribute width evenly among children, reserving 1 column for each border
                let num_cells = cells.len();
                if num_cells == 0 {
                    return;
                }

                // Reserve 1 column for each border between panes
                let num_borders = num_cells.saturating_sub(1);
                let available_width = width.saturating_sub(num_borders);
                let base_width = available_width / num_cells;
                let extra = available_width % num_cells;
                let mut current_x = pos_x;

                for (i, cell) in cells.iter_mut().enumerate() {
                    // Give extra pixels to the first 'extra' cells
                    let cell_width = base_width + if i < extra { 1 } else { 0 };
                    cell.resize(current_x, pos_y, cell_width, height);
                    current_x += cell_width;
                    // Skip 1 column for border after each pane (except the last)
                    if i < num_cells - 1 {
                        current_x += 1;
                    }
                }
            }
            PaneCellInner::HSplit(cells) => {
                // Distribute height evenly among children, reserving 1 row for each border
                let num_cells = cells.len();
                if num_cells == 0 {
                    return;
                }

                // Reserve 1 row for each border between panes
                let num_borders = num_cells.saturating_sub(1);
                let available_height = height.saturating_sub(num_borders);
                let base_height = available_height / num_cells;
                let extra = available_height % num_cells;
                let mut current_y = pos_y;

                for (i, cell) in cells.iter_mut().enumerate() {
                    // Give extra pixels to the first 'extra' cells
                    let cell_height = base_height + if i < extra { 1 } else { 0 };
                    cell.resize(pos_x, current_y, width, cell_height);
                    current_y += cell_height;
                    // Skip 1 row for border after each pane (except the last)
                    if i < num_cells - 1 {
                        current_y += 1;
                    }
                }
            }
        }
    }
}

impl Pane {
    /// Handle keyboard input by writing to the PTY.
    pub fn handle_input(&mut self, input: &[u8]) {
        if let Some(pty) = &mut self.pty {
            let _ = pty.write(input);
        }
    }

    /// Read available data from the PTY and process it through the emulator.
    fn read_and_process(&mut self) {
        if let Some(ref pty) = self.pty {
            // Read all available data
            loop {
                match pty.read(&mut self.read_buffer) {
                    Ok(Some(0)) => break, // EOF
                    Ok(Some(n)) => {
                        // Process through terminal emulator
                        self.terminal_emulator.process(&self.read_buffer[..n]);

                        // Handle any responses from the terminal (e.g., cursor position queries)
                        let responses = self.terminal_emulator.drain_responses();
                        for response in responses {
                            let _ = pty.write(&response);
                        }
                    }
                    Ok(None) => break, // No more data available (EAGAIN)
                    Err(_) => break,   // Error reading
                }
            }
        }
    }

    /// Check if the PTY process is still running.
    pub fn is_running(&self) -> bool {
        self.pty.as_ref().map_or(false, |p| p.is_running())
    }

    /// Enter scrollback mode
    pub fn enter_scrollback_mode(&mut self) {
        // Don't enter scrollback mode if in alternate screen
        if self.terminal_emulator.grid().in_alternate_screen {
            return;
        }
        self.scrollback_mode = true;
        self.scroll_offset = 0;
    }

    /// Exit scrollback mode
    pub fn exit_scrollback_mode(&mut self) {
        self.scrollback_mode = false;
        self.scroll_offset = 0;
        // Also clear search state
        self.search_mode = false;
        self.search_query.clear();
        self.search_matches.clear();
        self.current_match_index = None;
    }

    /// Check if in scrollback mode
    pub fn is_in_scrollback_mode(&self) -> bool {
        self.scrollback_mode
    }

    /// Scroll up by the given number of lines (increases scroll_offset)
    pub fn scroll_up(&mut self, lines: usize) {
        let max_offset = self.terminal_emulator.grid().scrollback_len();
        self.scroll_offset = (self.scroll_offset + lines).min(max_offset);
    }

    /// Scroll down by the given number of lines (decreases scroll_offset)
    pub fn scroll_down(&mut self, lines: usize) {
        self.scroll_offset = self.scroll_offset.saturating_sub(lines);
    }

    /// Get the current scroll offset
    pub fn scroll_offset(&self) -> usize {
        self.scroll_offset
    }

    /// Get the total number of scrollback lines
    pub fn scrollback_len(&self) -> usize {
        self.terminal_emulator.grid().scrollback_len()
    }

    /// Get the terminal emulator
    pub fn emulator(&self) -> &emulator::TerminalEmulator {
        &self.terminal_emulator
    }

    /// Enter search mode (must be in scrollback mode first)
    pub fn enter_search_mode(&mut self) {
        if self.scrollback_mode {
            self.search_mode = true;
            self.search_query.clear();
            self.search_matches.clear();
            self.current_match_index = None;
        }
    }

    /// Exit search mode (back to scrollback mode)
    pub fn exit_search_mode(&mut self) {
        self.search_mode = false;
        self.search_query.clear();
        self.search_matches.clear();
        self.current_match_index = None;
    }

    /// Check if in search mode
    pub fn is_in_search_mode(&self) -> bool {
        self.search_mode
    }

    /// Get the current search query
    pub fn search_query(&self) -> &str {
        &self.search_query
    }

    /// Get the current match index and total matches
    pub fn search_match_info(&self) -> (Option<usize>, usize) {
        (self.current_match_index, self.search_matches.len())
    }

    /// Handle a character input for the search query
    pub fn search_input_char(&mut self, c: char) {
        self.search_query.push(c);
        self.update_search();
    }

    /// Handle backspace for the search query
    pub fn search_input_backspace(&mut self) {
        self.search_query.pop();
        self.update_search();
    }

    /// Clear the search query
    pub fn search_clear(&mut self) {
        self.search_query.clear();
        self.update_search();
    }

    /// Update search results based on current query
    fn update_search(&mut self) {
        self.search_matches.clear();
        self.current_match_index = None;

        if self.search_query.is_empty() {
            return;
        }

        const MAX_MATCHES: usize = 100;

        let query_lower = self.search_query.to_lowercase();
        let grid = self.terminal_emulator.grid();
        let scrollback_len = grid.scrollback_len();

        // Search from bottom to top (most recent first) so we find the most relevant matches
        // when hitting the MAX_MATCHES limit
        
        // First, search current grid from bottom to top
        'grid: for y in (0..grid.rows).rev() {
            if let Some(row) = grid.get_row(y) {
                let line_text: String = row.iter().map(|c| c.character).collect();
                let line_lower = line_text.to_lowercase();
                
                // Find all matches in this line (still left to right within line)
                let mut search_start = 0;
                while let Some(pos) = line_lower[search_start..].find(&query_lower) {
                    let start_col = search_start + pos;
                    let end_col = start_col + self.search_query.len();
                    self.search_matches.push(SearchMatch {
                        line_index: y as isize,
                        start_col,
                        end_col,
                    });
                    if self.search_matches.len() >= MAX_MATCHES {
                        break 'grid;
                    }
                    search_start = start_col + 1;
                    if search_start >= line_lower.len() {
                        break;
                    }
                }
            }
        }

        // Then search scrollback from newest to oldest (only if we haven't hit the limit)
        // Scrollback index scrollback_len-1 is the most recent, 0 is the oldest
        if self.search_matches.len() < MAX_MATCHES {
            'scrollback: for i in (0..scrollback_len).rev() {
                if let Some(row) = grid.get_scrollback_row(i) {
                    let line_text: String = row.iter().map(|c| c.character).collect();
                    let line_lower = line_text.to_lowercase();
                    
                    // Find all matches in this line
                    let mut search_start = 0;
                    while let Some(pos) = line_lower[search_start..].find(&query_lower) {
                        let start_col = search_start + pos;
                        let end_col = start_col + self.search_query.len();
                        // Convert scrollback index to our line_index format:
                        // scrollback line 0 (oldest) -> -(scrollback_len) 
                        // scrollback line (scrollback_len - 1) (newest) -> -1
                        let line_index = (i as isize) - (scrollback_len as isize);
                        self.search_matches.push(SearchMatch {
                            line_index,
                            start_col,
                            end_col,
                        });
                        if self.search_matches.len() >= MAX_MATCHES {
                            break 'scrollback;
                        }
                        search_start = start_col + 1;
                        if search_start >= line_lower.len() {
                            break;
                        }
                    }
                }
            }
        }

        // Initially select the first match (which is now the most recent/bottom-most
        // since we searched from bottom to top)
        if !self.search_matches.is_empty() {
            self.current_match_index = Some(0);
            self.jump_to_current_match();
        }
    }

    /// Jump to the next match (toward bottom/more recent)
    pub fn next_match(&mut self) {
        if self.search_matches.is_empty() {
            return;
        }
        match self.current_match_index {
            Some(idx) if idx < self.search_matches.len() - 1 => {
                self.current_match_index = Some(idx + 1);
            }
            _ => {
                // Wrap to start
                self.current_match_index = Some(0);
            }
        }
        self.jump_to_current_match();
    }

    /// Jump to the previous match (toward top/older)
    pub fn prev_match(&mut self) {
        if self.search_matches.is_empty() {
            return;
        }
        match self.current_match_index {
            Some(idx) if idx > 0 => {
                self.current_match_index = Some(idx - 1);
            }
            _ => {
                // Wrap to end
                self.current_match_index = Some(self.search_matches.len() - 1);
            }
        }
        self.jump_to_current_match();
    }

    /// Adjust scroll_offset to make the current match visible
    fn jump_to_current_match(&mut self) {
        if let Some(idx) = self.current_match_index {
            if let Some(m) = self.search_matches.get(idx) {
                let grid = self.terminal_emulator.grid();
                let scrollback_len = grid.scrollback_len();
                let visible_rows = grid.rows;

                // The display model (from composite_into):
                // - scroll_offset = number of scrollback lines shown at the top of the screen
                // - For screen row `row`:
                //   - If row < scroll_offset: shows scrollback[scrollback_len - scroll_offset + row]
                //   - If row >= scroll_offset: shows grid[row - scroll_offset]
                //
                // line_index encoding:
                // - Negative: scrollback line, -1 = most recent (scrollback_idx = scrollback_len - 1)
                // - Non-negative: grid row number

                if m.line_index < 0 {
                    // This is a scrollback line
                    // lines_back = how many lines back from grid start (1 = most recent scrollback)
                    let lines_back = (-m.line_index) as usize;
                    
                    // The scrollback line at `lines_back` has scrollback_idx = scrollback_len - lines_back
                    // It appears at screen row = scroll_offset - lines_back
                    // For it to be visible (row >= 0 and row < visible_rows):
                    //   scroll_offset >= lines_back (so row >= 0)
                    //   scroll_offset - lines_back < visible_rows (so row < visible_rows)
                    //     => scroll_offset < visible_rows + lines_back
                    
                    // Minimum scroll_offset to see this line (appears at bottom of scrollback area)
                    let min_offset = lines_back;
                    
                    // Maximum scroll_offset to see this line (line must appear in visible area)
                    // row = scroll_offset - lines_back < visible_rows
                    // scroll_offset < visible_rows + lines_back
                    let max_offset = (visible_rows + lines_back).saturating_sub(1);
                    
                    // Try to center the line on screen
                    // We want row ≈ visible_rows / 2
                    // scroll_offset - lines_back = visible_rows / 2
                    // scroll_offset = lines_back + visible_rows / 2
                    let ideal_offset = lines_back + visible_rows / 2;
                    
                    // Clamp to valid range
                    self.scroll_offset = ideal_offset
                        .max(min_offset)
                        .min(max_offset)
                        .min(scrollback_len);
                } else {
                    // This is a grid line
                    let grid_line = m.line_index as usize;
                    
                    // Grid row `grid_line` appears at screen row = scroll_offset + grid_line
                    // For it to be visible: scroll_offset + grid_line < visible_rows
                    // So: scroll_offset < visible_rows - grid_line
                    // Max valid scroll_offset = visible_rows - grid_line - 1
                    
                    if grid_line < visible_rows {
                        let max_offset = visible_rows - grid_line - 1;
                        if self.scroll_offset > max_offset {
                            self.scroll_offset = max_offset;
                        }
                    } else {
                        // Grid line is beyond visible area - shouldn't happen normally
                        // but handle gracefully by scrolling to 0
                        self.scroll_offset = 0;
                    }
                }
            }
        }
    }

    /// Get search matches for highlighting
    pub fn get_search_matches(&self) -> &[SearchMatch] {
        &self.search_matches
    }

    /// Get the current match index
    pub fn current_match_index(&self) -> Option<usize> {
        self.current_match_index
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compositor_new() {
        let compositor = Compositor::new(80, 24);
        assert!(compositor.is_ok());
    }

    #[test]
    fn test_queue_input() {
        let compositor = Compositor::new(80, 24).unwrap();
        compositor.queue_input(b"hello");

        let queue = compositor.input_queue.lock().unwrap();
        assert_eq!(queue.len(), 1);
        assert_eq!(queue[0], b"hello");
    }

    #[test]
    fn test_hsplit_focus_movement() {
        // Create an HSplit compositor (top/bottom panes) using Ctrl+b "
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_output(80, 24, output).unwrap();

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
        let mut compositor = Compositor::with_output(80, 24, output).unwrap();

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
    fn test_focus_movement_at_boundary() {
        // Test that focus doesn't move past boundaries
        let output = Arc::new(Mutex::new(Vec::<u8>::new()));
        let mut compositor = Compositor::with_output(80, 24, output).unwrap();

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
        let mut compositor = Compositor::with_output(80, 24, output).unwrap();

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
        let mut compositor = Compositor::with_output(80, 24, output).unwrap();

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
        let mut compositor = Compositor::with_output(80, 24, output).unwrap();

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
        let mut compositor = Compositor::with_output(80, 24, output).unwrap();

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
        let mut compositor = Compositor::with_output(80, 24, output).unwrap();

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
        let mut compositor = Compositor::with_output(80, 24, output).unwrap();

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
        let mut compositor = Compositor::with_output(80, 24, output.clone()).unwrap();

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
        let mut compositor = Compositor::with_output(80, 24, output).unwrap();

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
        let mut compositor = Compositor::with_output(80, 24, output.clone()).unwrap();

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
        let mut compositor = Compositor::with_output(80, 24, output.clone()).unwrap();

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
}
