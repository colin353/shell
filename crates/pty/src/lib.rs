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

        if command.trim().is_empty() {
            eprintln!("Empty command");
            std::process::exit(1);
        }

        // Execute via bash to get shell features (variable expansion, pipes, etc.)
        let bash = CString::new("bash").unwrap();
        let c_flag = CString::new("-c").unwrap();
        let c_command = CString::new(command).unwrap();
        let args: Vec<&std::ffi::CStr> =
            vec![bash.as_c_str(), c_flag.as_c_str(), c_command.as_c_str()];

        execvp(&bash, &args).expect("execvp failed");
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

    /// Check whether the PTY is currently in canonical mode with local echo.
    ///
    /// This is the mode used by line-oriented commands like `sleep` and `cat`.
    /// Full-screen applications normally switch the PTY to raw/no-echo mode.
    pub fn is_canonical_echo_mode(&self) -> bool {
        let mut termios = std::mem::MaybeUninit::<libc::termios>::uninit();
        let ret = unsafe { libc::tcgetattr(self.master_fd.as_raw_fd(), termios.as_mut_ptr()) };
        if ret < 0 {
            return false;
        }

        let termios = unsafe { termios.assume_init() };
        let required = libc::ICANON | libc::ECHO;
        termios.c_lflag & required == required
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

    /// Check if the child process has exited and return its exit code.
    /// Returns `Some(exit_code)` if the process has exited, `None` if still running.
    /// The exit code is the actual exit status for normal exits, or 128+signal for signal termination.
    pub fn try_wait(&self) -> Option<i32> {
        use nix::sys::wait::{waitpid, WaitPidFlag, WaitStatus};
        match waitpid(self.child_pid, Some(WaitPidFlag::WNOHANG)) {
            Ok(WaitStatus::Exited(_, code)) => Some(code),
            Ok(WaitStatus::Signaled(_, signal, _)) => Some(128 + signal as i32),
            Ok(WaitStatus::StillAlive) => None,
            Ok(_) => Some(1),  // Other status, assume error
            Err(_) => Some(1), // Error checking, assume error
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

    #[test]
    fn test_env_var_expansion() {
        // Test that shell features like variable expansion work
        let pty = PtyProcess::spawn("echo $TERM", 80, 24).unwrap();

        thread::sleep(Duration::from_millis(100));

        let mut buf = [0u8; 1024];
        let mut output = Vec::new();

        for _ in 0..10 {
            if let Ok(Some(n)) = pty.read(&mut buf) {
                output.extend_from_slice(&buf[..n]);
                break;
            }
            thread::sleep(Duration::from_millis(50));
        }

        let output_str = String::from_utf8_lossy(&output);
        // TERM is set to xterm-256color in exec_command
        assert!(
            output_str.contains("xterm-256color"),
            "Expected 'xterm-256color' in output (env var should be expanded): {}",
            output_str
        );
    }
}
