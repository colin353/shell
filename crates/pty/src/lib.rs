//! PTY (pseudo-terminal) handling for spawning and communicating with child processes.

use nix::fcntl::{fcntl, FcntlArg, OFlag};
use nix::pty::{openpty, OpenptyResult, Winsize};
use nix::unistd::{execvp, fork, read, write, ForkResult, Pid};
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

                // Use login_tty which properly:
                // 1. Creates a new session (setsid)
                // 2. Sets the controlling terminal (TIOCSCTTY)
                // 3. Dups the slave to stdin/stdout/stderr
                // 4. Closes the original slave fd
                // This is the standard way terminal emulators set up PTYs
                let slave_fd = slave.as_raw_fd();
                std::mem::forget(slave); // Don't let OwnedFd close it, login_tty will handle it

                unsafe {
                    if libc::login_tty(slave_fd) < 0 {
                        panic!("login_tty failed: {}", std::io::Error::last_os_error());
                    }
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
        // Some programs also check COLORTERM for true color support detection
        std::env::set_var("COLORTERM", "truecolor");

        // Parse the command into program and arguments
        // This is a simple parser that handles:
        // - Whitespace-separated arguments
        // - Single and double quoted strings
        let parts = Self::parse_command(command);

        if parts.is_empty() {
            eprintln!("Empty command");
            std::process::exit(1);
        }

        let program = CString::new(parts[0].as_str()).unwrap();
        let c_args: Vec<CString> = parts
            .iter()
            .map(|s| CString::new(s.as_str()).unwrap())
            .collect();
        let c_arg_refs: Vec<&std::ffi::CStr> = c_args.iter().map(|s| s.as_c_str()).collect();

        // Execute the program directly - this makes the program the direct child
        // of the PTY, so isatty() will work correctly
        execvp(&program, &c_arg_refs).expect("execvp failed");
        unreachable!()
    }

    /// Parse a command string into arguments, handling quotes
    fn parse_command(command: &str) -> Vec<String> {
        let mut args = Vec::new();
        let mut current = String::new();
        let mut in_single_quote = false;
        let mut in_double_quote = false;
        let mut escape_next = false;

        for c in command.chars() {
            if escape_next {
                current.push(c);
                escape_next = false;
                continue;
            }

            match c {
                '\\' if !in_single_quote => {
                    escape_next = true;
                }
                '\'' if !in_double_quote => {
                    in_single_quote = !in_single_quote;
                }
                '"' if !in_single_quote => {
                    in_double_quote = !in_double_quote;
                }
                ' ' | '\t' if !in_single_quote && !in_double_quote => {
                    if !current.is_empty() {
                        args.push(current.clone());
                        current.clear();
                    }
                }
                _ => {
                    current.push(c);
                }
            }
        }

        if !current.is_empty() {
            args.push(current);
        }

        args
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
        use nix::sys::wait::{waitpid, WaitPidFlag, WaitStatus};
        use std::thread;
        use std::time::Duration;

        // First, check if the process has already exited
        match waitpid(self.child_pid, Some(WaitPidFlag::WNOHANG)) {
            Ok(WaitStatus::StillAlive) => {
                // Process is still running
                // The child is a session leader (setsid in login_tty), so kill the whole process group
                // Using negative PID sends the signal to the entire process group
                let pgid = nix::unistd::Pid::from_raw(-self.child_pid.as_raw());
                
                // Send SIGHUP first - this is the proper signal for "terminal hung up"
                // Many interactive programs handle SIGHUP by exiting cleanly
                let _ = nix::sys::signal::kill(pgid, nix::sys::signal::Signal::SIGHUP);
                
                // Also send SIGTERM for programs that don't handle SIGHUP
                let _ = nix::sys::signal::kill(pgid, nix::sys::signal::Signal::SIGTERM);
                // Also signal the specific process in case the group signal failed
                let _ = nix::sys::signal::kill(self.child_pid, nix::sys::signal::Signal::SIGHUP);
                let _ = nix::sys::signal::kill(self.child_pid, nix::sys::signal::Signal::SIGTERM);

                // Give it a short time to exit gracefully (up to 100ms)
                for _ in 0..10 {
                    thread::sleep(Duration::from_millis(10));
                    match waitpid(self.child_pid, Some(WaitPidFlag::WNOHANG)) {
                        Ok(WaitStatus::StillAlive) => continue,
                        _ => return, // Process exited or error, we're done
                    }
                }

                // Process didn't exit gracefully, force kill the process group
                let _ = nix::sys::signal::kill(pgid, nix::sys::signal::Signal::SIGKILL);
                let _ = nix::sys::signal::kill(self.child_pid, nix::sys::signal::Signal::SIGKILL);

                // Reap the zombie with non-blocking wait (SIGKILL is delivered asynchronously)
                // Try a few times to give the kernel time to deliver the signal
                for _ in 0..5 {
                    match waitpid(self.child_pid, Some(WaitPidFlag::WNOHANG)) {
                        Ok(WaitStatus::StillAlive) => {
                            thread::sleep(Duration::from_millis(5));
                        }
                        _ => return,
                    }
                }
                // If still not reaped, give up - the process will be reaped eventually
                // when the parent process exits or by init/systemd
            }
            Ok(_) => {
                // Process already exited, zombie has been reaped
            }
            Err(_) => {
                // Error checking process status (e.g., not our child), nothing to do
            }
        }
        // master_fd is closed automatically by OwnedFd
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
