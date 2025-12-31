use nix::fcntl::{fcntl, FcntlArg, OFlag};
use nix::pty::{openpty, OpenptyResult, Winsize};
use nix::unistd::{dup2, execvp, fork, read, setsid, write, ForkResult, Pid};
use std::ffi::CString;
use std::os::fd::{AsRawFd, OwnedFd, RawFd};

/// Represents a running process attached to a PTY
pub struct PtyProcess {
    master_fd: OwnedFd,
    child_pid: Pid,
}

/// Error type for PTY operations
#[derive(Debug)]
pub enum PtyError {
    OpenPty(nix::Error),
    Fork(nix::Error),
    Exec(nix::Error),
    Read(nix::Error),
    Write(nix::Error),
    Fcntl(nix::Error),
    InvalidCommand,
}

impl std::fmt::Display for PtyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PtyError::OpenPty(e) => write!(f, "Failed to open PTY: {}", e),
            PtyError::Fork(e) => write!(f, "Failed to fork: {}", e),
            PtyError::Exec(e) => write!(f, "Failed to exec: {}", e),
            PtyError::Read(e) => write!(f, "Failed to read from PTY: {}", e),
            PtyError::Write(e) => write!(f, "Failed to write to PTY: {}", e),
            PtyError::Fcntl(e) => write!(f, "Failed to set fd flags: {}", e),
            PtyError::InvalidCommand => write!(f, "Invalid command"),
        }
    }
}

impl std::error::Error for PtyError {}

impl PtyProcess {
    /// Spawn a new process with the given command in a PTY
    ///
    /// # Arguments
    /// * `command` - The command to execute (e.g., "ls -la" or "/bin/bash")
    /// * `cols` - Terminal width in columns
    /// * `rows` - Terminal height in rows
    pub fn spawn(command: &str, cols: u16, rows: u16) -> Result<Self, PtyError> {
        let winsize = Winsize {
            ws_row: rows,
            ws_col: cols,
            ws_xpixel: 0,
            ws_ypixel: 0,
        };

        // Open a PTY pair
        let OpenptyResult { master, slave } = openpty(&winsize, None).map_err(PtyError::OpenPty)?;

        // Fork the process
        match unsafe { fork() }.map_err(PtyError::Fork)? {
            ForkResult::Parent { child } => {
                // Parent process - slave is dropped automatically (OwnedFd)
                drop(slave);

                // Set master to non-blocking mode
                let flags =
                    fcntl(master.as_raw_fd(), FcntlArg::F_GETFL).map_err(PtyError::Fcntl)?;
                let new_flags = OFlag::from_bits_truncate(flags) | OFlag::O_NONBLOCK;
                fcntl(master.as_raw_fd(), FcntlArg::F_SETFL(new_flags)).map_err(PtyError::Fcntl)?;

                Ok(PtyProcess {
                    master_fd: master,
                    child_pid: child,
                })
            }
            ForkResult::Child => {
                // Child process - set up the slave as stdin/stdout/stderr
                drop(master);

                // Create a new session and set the controlling terminal
                setsid().expect("setsid failed");

                // Set slave as stdin, stdout, stderr
                dup2(slave.as_raw_fd(), 0).expect("dup2 stdin failed");
                dup2(slave.as_raw_fd(), 1).expect("dup2 stdout failed");
                dup2(slave.as_raw_fd(), 2).expect("dup2 stderr failed");

                // Close the original slave fd if it's not stdin/stdout/stderr
                if slave.as_raw_fd() > 2 {
                    drop(slave);
                }

                // Parse command and execute
                Self::exec_command(command);
            }
        }
    }

    /// Execute the command in the child process (never returns on success)
    fn exec_command(command: &str) -> ! {
        // Set TERM so programs know they can use colors and escape sequences
        std::env::set_var("TERM", "xterm-256color");
        // Some programs also check COLORTERM
        std::env::set_var("COLORTERM", "truecolor");

        // Use shell to parse the command
        let shell = CString::new("/bin/sh").unwrap();
        let arg0 = CString::new("sh").unwrap();
        let arg1 = CString::new("-c").unwrap();
        let arg2 = CString::new(command).unwrap();

        let args = [arg0.as_c_str(), arg1.as_c_str(), arg2.as_c_str()];

        // This never returns on success
        execvp(&shell, &args).expect("execvp failed");
        unreachable!()
    }

    /// Read available output from the PTY (non-blocking)
    /// Returns Ok(None) if no data is available (EAGAIN/EWOULDBLOCK)
    pub fn read(&self, buf: &mut [u8]) -> Result<Option<usize>, PtyError> {
        match read(self.master_fd.as_raw_fd(), buf) {
            Ok(n) => Ok(Some(n)),
            Err(nix::Error::EAGAIN) => Ok(None),
            Err(e) => Err(PtyError::Read(e)),
        }
    }

    /// Write input to the PTY (sends to the subprocess's stdin)
    pub fn write(&self, data: &[u8]) -> Result<usize, PtyError> {
        write(&self.master_fd, data).map_err(PtyError::Write)
    }

    /// Get the raw file descriptor for use with polling/select
    pub fn raw_fd(&self) -> RawFd {
        self.master_fd.as_raw_fd()
    }

    /// Get the child process ID
    pub fn pid(&self) -> Pid {
        self.child_pid
    }

    /// Check if the child process is still running
    pub fn is_running(&self) -> bool {
        use nix::sys::wait::{waitpid, WaitPidFlag};
        match waitpid(self.child_pid, Some(WaitPidFlag::WNOHANG)) {
            Ok(nix::sys::wait::WaitStatus::StillAlive) => true,
            Ok(_) => false,  // Process has exited
            Err(_) => false, // Error checking, assume dead
        }
    }

    /// Resize the PTY
    pub fn resize(&self, cols: u16, rows: u16) -> Result<(), PtyError> {
        let winsize = Winsize {
            ws_row: rows,
            ws_col: cols,
            ws_xpixel: 0,
            ws_ypixel: 0,
        };

        // TIOCSWINSZ ioctl
        unsafe {
            let ret = libc::ioctl(self.master_fd.as_raw_fd(), libc::TIOCSWINSZ, &winsize);
            if ret < 0 {
                return Err(PtyError::Fcntl(nix::Error::last()));
            }
        }
        Ok(())
    }

    /// Send a signal to the child process
    pub fn signal(&self, signal: nix::sys::signal::Signal) -> Result<(), PtyError> {
        nix::sys::signal::kill(self.child_pid, signal).map_err(|e| PtyError::Fcntl(e))
    }
}

impl Drop for PtyProcess {
    fn drop(&mut self) {
        // master_fd is closed automatically by OwnedFd
        // Optionally kill the child process
        let _ = nix::sys::signal::kill(self.child_pid, nix::sys::signal::Signal::SIGTERM);
    }
}

impl AsRawFd for PtyProcess {
    fn as_raw_fd(&self) -> RawFd {
        self.master_fd.as_raw_fd()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;
    use std::time::Duration;

    #[test]
    fn test_spawn_and_read() {
        let pty = PtyProcess::spawn("echo hello", 80, 24).unwrap();

        // Give the process time to run
        thread::sleep(Duration::from_millis(100));

        let mut buf = [0u8; 1024];
        let mut output = Vec::new();

        // Read until we get something or timeout
        for _ in 0..10 {
            if let Ok(Some(n)) = pty.read(&mut buf) {
                output.extend_from_slice(&buf[..n]);
                break;
            }
            thread::sleep(Duration::from_millis(50));
        }

        let output_str = String::from_utf8_lossy(&output);
        assert!(
            output_str.contains("hello"),
            "Expected 'hello' in output: {}",
            output_str
        );
    }

    #[test]
    fn test_process_exit() {
        let pty = PtyProcess::spawn("exit 0", 80, 24).unwrap();
        thread::sleep(Duration::from_millis(100));
        assert!(!pty.is_running());
    }
}
