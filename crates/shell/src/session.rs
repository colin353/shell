//! Session module - combines PTY process with terminal emulator
//!
//! A session represents a running command with its terminal emulator state.

use crate::pty::{PtyError, PtyProcess};
use emulator::{Line, TerminalEmulator};

/// A session combining a PTY process with a terminal emulator
pub struct Session {
    pty: PtyProcess,
    emulator: TerminalEmulator,
    /// Buffer for reading from PTY
    read_buffer: [u8; 4096],
}

impl Session {
    /// Spawn a new session with the given command
    pub fn spawn(command: &str, cols: u16, rows: u16) -> Result<Self, PtyError> {
        let pty = PtyProcess::spawn(command, cols, rows)?;
        let emulator = TerminalEmulator::new(cols as usize, rows as usize);

        Ok(Self {
            pty,
            emulator,
            read_buffer: [0u8; 4096],
        })
    }

    /// Poll for new output from the PTY and process it through the emulator.
    /// Returns true if any output was processed.
    pub fn poll(&mut self) -> Result<bool, PtyError> {
        let mut had_output = false;

        // Read all available output
        loop {
            match self.pty.read(&mut self.read_buffer)? {
                Some(0) => break, // EOF
                Some(n) => {
                    self.emulator.process(&self.read_buffer[..n]);
                    had_output = true;
                }
                None => break, // No more data available (EAGAIN)
            }
        }

        Ok(had_output)
    }

    /// Check if the process is still running
    pub fn is_running(&self) -> bool {
        self.pty.is_running()
    }

    /// Get the current terminal display as lines
    pub fn get_lines(&self) -> Vec<Line> {
        self.emulator.to_lines()
    }

    /// Write input to the process
    pub fn write(&self, data: &[u8]) -> Result<usize, PtyError> {
        self.pty.write(data)
    }

    /// Resize the session
    pub fn resize(&mut self, cols: u16, rows: u16) -> Result<(), PtyError> {
        self.pty.resize(cols, rows)?;
        self.emulator.resize(cols as usize, rows as usize);
        Ok(())
    }

    /// Get the raw file descriptor for polling
    pub fn raw_fd(&self) -> std::os::fd::RawFd {
        self.pty.raw_fd()
    }
}
