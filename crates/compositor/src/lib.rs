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
use std::os::fd::{AsRawFd, OwnedFd, RawFd};
use std::sync::Mutex;

/// The main compositor that manages terminal panes and the event loop.
pub struct Compositor {
    root: PaneCell,

    // Event loop communication
    wake_read: OwnedFd,
    wake_write: OwnedFd,
    input_queue: Mutex<VecDeque<Vec<u8>>>,
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

impl Compositor {
    /// Create a new compositor with the given dimensions.
    ///
    /// This spawns a default shell in the root pane.
    pub fn new(width: usize, height: usize) -> Result<Self, CompositorError> {
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
            wake_read,
            wake_write,
            input_queue: Mutex::new(VecDeque::new()),
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
    pub fn handle_input(&mut self, input: &[u8]) {
        self.root.handle_input(input);
    }

    /// Run the event loop. This blocks and handles all events.
    ///
    /// The loop will:
    /// 1. Poll all PTY file descriptors and the wake pipe
    /// 2. Process any PTY output (feed to emulators)
    /// 3. Process any queued keyboard input
    /// 4. Call the render callback
    ///
    /// Returns when all panes have exited or an error occurs.
    pub fn run<F>(&mut self, mut on_update: F) -> Result<(), CompositorError>
    where
        F: FnMut(&mut Self),
    {
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

            // Call render callback
            on_update(self);
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
}
