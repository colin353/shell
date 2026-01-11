use crate::error::CompositorError;
use crate::pane::Pane;
use crate::pane_cell::PaneCell;
use crate::tab::Tab;
use crate::types::{Direction, SplitDirection};
use crate::{BSU, ESU, STATUS_BAR_HEIGHT};
use nix::unistd::{read, write};
use std::collections::VecDeque;
use std::io::Write;
use std::os::fd::{AsRawFd, OwnedFd, RawFd};
use std::sync::{Arc, Mutex};

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
                    b'u' => {
                        // Ctrl+b u - enter URL mode (automatically enters scrollback mode first)
                        self.active_tab_mut().root.enter_scrollback_mode();
                        self.active_tab_mut().root.enter_url_mode();
                        self.render();
                        return;
                    }
                    0x02 => {
                        // Ctrl+b Ctrl+b - send Ctrl+b to the terminal
                        if self.active_tab_mut().root.handle_input(&[0x02]) {
                            self.render();
                        }
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
        if self.active_tab_mut().root.handle_input(input) {
            self.render();
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
                        // Exit visual mode after yanking
                        self.active_tab_mut().root.handle_vim_input(&[0x1b]);
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
    /// Returns `true` if any input caused terminal content to change.
    fn process_keyboard_queue(&mut self) -> bool {
        let inputs: Vec<Vec<u8>> = {
            let mut queue = self.input_queue.lock().unwrap();
            queue.drain(..).collect()
        };

        let mut needs_render = false;
        for input in inputs {
            if self.active_tab_mut().root.handle_input(&input) {
                needs_render = true;
            }
        }
        needs_render
    }

    /// Render the compositor to the terminal.
    ///
    /// This traverses all panes, taking their grid contents and compositing them into
    /// a single terminal emulator. Then, it uses delta rendering to output only the changed parts.
    pub fn render(&mut self) {
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

        // Check if we're in URL mode (sub-mode of scrollback)
        if self.tabs[self.active_tab].root.is_in_url_mode() {
            // Create attributes for URL indicator
            let mut url_attrs = emulator::CellAttributes::default();
            url_attrs.bg_color = Some(emulator::Color::Cyan);
            url_attrs.fg_color = Some(emulator::Color::Black);
            url_attrs.bold = true;

            if let Some((current_idx, _total)) = self.tabs[self.active_tab].root.get_url_info() {
                // Show URL mode label on the left after tabs
                let url_label = " URL ";
                for ch in url_label.chars() {
                    if x_pos < cols {
                        self.global_emulator.grid_mut().set_cell(
                            x_pos,
                            status_bar_y,
                            emulator::Cell::new(ch, url_attrs.clone()),
                        );
                        x_pos += 1;
                    }
                }

                // Show current URL if selected (truncated if too long)
                if let Some(url) = self.tabs[self.active_tab].root.get_current_url() {
                    // Calculate available space for URL (leave room for hint on right)
                    let hint_text = " j/k:nav Enter:open ";
                    let available = cols.saturating_sub(x_pos).saturating_sub(hint_text.len());

                    let display_url = if url.len() > available && available > 3 {
                        format!("{}...", &url[..available.saturating_sub(3)])
                    } else {
                        url.clone()
                    };

                    for ch in display_url.chars() {
                        if x_pos < cols.saturating_sub(hint_text.len()) {
                            self.global_emulator.grid_mut().set_cell(
                                x_pos,
                                status_bar_y,
                                emulator::Cell::new(ch, url_attrs.clone()),
                            );
                            x_pos += 1;
                        }
                    }

                    // Show hint text on the right (only if we have a URL selected)
                    if current_idx.is_some() {
                        let text_start_x = cols.saturating_sub(hint_text.len());
                        for (i, ch) in hint_text.chars().enumerate() {
                            let x = text_start_x + i;
                            if x < cols {
                                self.global_emulator.grid_mut().set_cell(
                                    x,
                                    status_bar_y,
                                    emulator::Cell::new(ch, url_attrs.clone()),
                                );
                            }
                        }
                    }
                } else {
                    // No URL selected
                    let no_url_text = " No URLs found ";
                    for ch in no_url_text.chars() {
                        if x_pos < cols {
                            self.global_emulator.grid_mut().set_cell(
                                x_pos,
                                status_bar_y,
                                emulator::Cell::new(ch, url_attrs.clone()),
                            );
                            x_pos += 1;
                        }
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

/// Copy text to the system clipboard.
///
/// On macOS, this uses `pbcopy`.
/// On Linux, this tries `xclip` or `xsel`.
/// On other platforms, this is a no-op.
fn copy_to_clipboard(text: &str) -> std::io::Result<()> {
    #[cfg(target_os = "macos")]
    {
        use std::process::{Command, Stdio};

        let mut child = Command::new("pbcopy").stdin(Stdio::piped()).spawn()?;

        if let Some(stdin) = child.stdin.as_mut() {
            use std::io::Write;
            stdin.write_all(text.as_bytes())?;
        }

        child.wait()?;
        Ok(())
    }

    #[cfg(target_os = "linux")]
    {
        use std::process::{Command, Stdio};

        // Try xclip first, then xsel
        let result = Command::new("xclip")
            .args(["-selection", "clipboard"])
            .stdin(Stdio::piped())
            .spawn();

        let mut child = match result {
            Ok(child) => child,
            Err(_) => {
                // Fall back to xsel
                Command::new("xsel")
                    .args(["--clipboard", "--input"])
                    .stdin(Stdio::piped())
                    .spawn()?
            }
        };

        if let Some(stdin) = child.stdin.as_mut() {
            use std::io::Write;
            stdin.write_all(text.as_bytes())?;
        }

        child.wait()?;
        Ok(())
    }

    #[cfg(not(any(target_os = "macos", target_os = "linux")))]
    {
        // No clipboard support on other platforms
        let _ = text;
        Ok(())
    }
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
