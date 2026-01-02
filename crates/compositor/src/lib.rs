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

/// The main compositor that manages terminal panes and the event loop.
pub struct Compositor {
    root: PaneCell,

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
pub struct Pane {
    terminal_emulator: emulator::TerminalEmulator,
    pty: Option<pty::PtyProcess>,
    read_buffer: [u8; 4096],
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

        Ok(Self {
            root: PaneCell {
                inner: PaneCellInner::Pane(Pane {
                    terminal_emulator: emulator::TerminalEmulator::new(width, height),
                    pty: Some(
                        pty::PtyProcess::spawn("/bin/bash", width as u16, height as u16)
                            .map_err(CompositorError::Pty)?,
                    ),
                    read_buffer: [0u8; 4096],
                }),
                width,
                height,
                pos_x: 0,
                pos_y: 0,
                focus: true,
            },
            global_emulator: emulator::TerminalEmulator::new(width, height),
            prev_frame: emulator::TerminalGrid::new(width, height),
            output,
            wake_read,
            wake_write,
            input_queue: Mutex::new(VecDeque::new()),
            prefix_mode: false,
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
    pub fn handle_input(&mut self, input: &[u8]) {
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
                    0x02 => {
                        // Ctrl+b Ctrl+b - send Ctrl+b to the terminal
                        self.root.handle_input(&[0x02]);
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
                    return;
                }
                0x0a => {
                    // Ctrl+j - move focus down
                    self.move_focus(Direction::Down);
                    return;
                }
                0x0b => {
                    // Ctrl+k - move focus up
                    self.move_focus(Direction::Up);
                    return;
                }
                0x0c => {
                    // Ctrl+l - move focus right
                    self.move_focus(Direction::Right);
                    return;
                }
                _ => {}
            }
        }
        self.root.handle_input(input);
    }

    /// Split the currently focused pane.
    ///
    /// Creates a new pane by splitting the focused pane either horizontally or vertically.
    pub fn split_focused_pane(&mut self, direction: SplitDirection) -> Result<(), CompositorError> {
        self.root.split_focused(direction)
    }

    /// Move focus in the specified direction.
    ///
    /// Uses vim-style navigation:
    /// - Left (h): Move to the pane on the left
    /// - Down (j): Move to the pane below
    /// - Up (k): Move to the pane above
    /// - Right (l): Move to the pane on the right
    pub fn move_focus(&mut self, direction: Direction) {
        self.root.move_focus(direction);
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

            // Collect PTY fds from all panes
            self.root.collect_poll_fds(&mut poll_fds, &mut fd_to_pane);

            // If no PTYs are left, we're done
            if poll_fds.len() == 1 {
                return Ok(());
            }

            // Poll with no timeout (block until something happens)
            let n =
                unsafe { libc::poll(poll_fds.as_mut_ptr(), poll_fds.len() as libc::nfds_t, -1) };

            if n < 0 {
                return Err(CompositorError::Poll(std::io::Error::last_os_error()));
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

        // Collect PTY fds from all panes
        self.root.collect_poll_fds(&mut poll_fds, &mut fd_to_pane);

        // Poll with timeout
        let n = unsafe {
            libc::poll(
                poll_fds.as_mut_ptr(),
                poll_fds.len() as libc::nfds_t,
                timeout_ms,
            )
        };

        if n < 0 {
            return Err(CompositorError::Poll(std::io::Error::last_os_error()));
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
            self.root.handle_input(&input);
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

        // Composite all panes into the global emulator
        self.root.composite_into(&mut self.global_emulator);

        // Compute the delta between the previous frame and current frame
        let delta = emulator::compute_delta(&self.prev_frame, self.global_emulator.grid());

        // Write the delta to the output
        if !delta.is_empty() {
            if let Ok(mut output) = self.output.lock() {
                let _ = output.write_all(&delta);
                let _ = output.flush();
            }
        }

        // Save the current frame as the previous frame for next render
        self.prev_frame = self.global_emulator.grid().clone();
    }

    /// Get a reference to the root pane cell.
    pub fn root(&self) -> &PaneCell {
        &self.root
    }

    /// Get a mutable reference to the root pane cell.
    pub fn root_mut(&mut self) -> &mut PaneCell {
        &mut self.root
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

        // Composite all panes into the global emulator
        self.root.composite_into(&mut self.global_emulator);

        // Compute the delta between the previous frame and current frame
        let delta = emulator::compute_delta(&self.prev_frame, self.global_emulator.grid());

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

                // Create a new pane to add to the split
                let (new_width, new_height, new_pos_x, new_pos_y) = match direction {
                    SplitDirection::Horizontal => {
                        // Split top/bottom: each pane gets half the height
                        let top_height = self.height / 2;
                        let bottom_height = self.height - top_height;
                        (
                            self.width,
                            bottom_height,
                            self.pos_x,
                            self.pos_y + top_height,
                        )
                    }
                    SplitDirection::Vertical => {
                        // Split left/right: each pane gets half the width
                        let left_width = self.width / 2;
                        let right_width = self.width - left_width;
                        (
                            right_width,
                            self.height,
                            self.pos_x + left_width,
                            self.pos_y,
                        )
                    }
                };

                // Resize the existing pane's emulator
                let (old_width, old_height) = match direction {
                    SplitDirection::Horizontal => (self.width, self.height / 2),
                    SplitDirection::Vertical => (self.width / 2, self.height),
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
                };

                // Take ownership of the old pane
                let old_inner = std::mem::replace(
                    &mut self.inner,
                    PaneCellInner::Pane(Pane {
                        terminal_emulator: emulator::TerminalEmulator::new(1, 1),
                        pty: None,
                        read_buffer: [0u8; 4096],
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
                    focus: true, // Keep focus on the original pane
                };

                let second_cell = PaneCell {
                    inner: PaneCellInner::Pane(new_pane),
                    width: new_width,
                    height: new_height,
                    pos_x: new_pos_x,
                    pos_y: new_pos_y,
                    focus: false,
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
                // Blit the pane's emulator content into the destination
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
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                // Recursively composite all child panes
                for cell in cells {
                    cell.composite_into(dest);
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

        // The top pane (index 0) should have focus
        if let PaneCellInner::HSplit(cells) = &compositor.root().inner {
            assert!(cells[0].has_focus());
            assert!(!cells[1].has_focus());
        }

        // Move focus down (Ctrl+j = 0x0a)
        compositor.handle_input(&[0x0a]);

        // Now the bottom pane should have focus
        if let PaneCellInner::HSplit(cells) = &compositor.root().inner {
            assert!(!cells[0].has_focus());
            assert!(cells[1].has_focus());
        }

        // Move focus up (Ctrl+k = 0x0b)
        compositor.handle_input(&[0x0b]);

        // Top pane should have focus again
        if let PaneCellInner::HSplit(cells) = &compositor.root().inner {
            assert!(cells[0].has_focus());
            assert!(!cells[1].has_focus());
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

        // The left pane (index 0) should have focus
        if let PaneCellInner::VSplit(cells) = &compositor.root().inner {
            assert!(cells[0].has_focus());
            assert!(!cells[1].has_focus());
        }

        // Move focus right (Ctrl+l = 0x0c)
        compositor.handle_input(&[0x0c]);

        // Now the right pane should have focus
        if let PaneCellInner::VSplit(cells) = &compositor.root().inner {
            assert!(!cells[0].has_focus());
            assert!(cells[1].has_focus());
        }

        // Move focus left (Ctrl+h = 0x08)
        compositor.handle_input(&[0x08]);

        // Left pane should have focus again
        if let PaneCellInner::VSplit(cells) = &compositor.root().inner {
            assert!(cells[0].has_focus());
            assert!(!cells[1].has_focus());
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

        // Try to move up when already at top - should stay at top
        compositor.handle_input(&[0x0b]); // Ctrl+k

        if let PaneCellInner::HSplit(cells) = &compositor.root().inner {
            assert!(cells[0].has_focus());
            assert!(!cells[1].has_focus());
        }

        // Move to bottom
        compositor.handle_input(&[0x0a]); // Ctrl+j

        // Try to move down when already at bottom - should stay at bottom
        compositor.handle_input(&[0x0a]); // Ctrl+j

        if let PaneCellInner::HSplit(cells) = &compositor.root().inner {
            assert!(!cells[0].has_focus());
            assert!(cells[1].has_focus());
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
            // First pane should have focus
            assert!(cells[0].has_focus());
            assert!(!cells[1].has_focus());
            // Check dimensions - should be split vertically
            assert_eq!(cells[0].dimensions(), (80, 12));
            assert_eq!(cells[1].dimensions(), (80, 12));
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
            // First pane should have focus
            assert!(cells[0].has_focus());
            assert!(!cells[1].has_focus());
            // Check dimensions - should be split horizontally
            assert_eq!(cells[0].dimensions(), (40, 24));
            assert_eq!(cells[1].dimensions(), (40, 24));
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
}
