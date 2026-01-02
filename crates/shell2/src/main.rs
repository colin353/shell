use std::io::{Read, Write};
use std::os::fd::AsRawFd;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use raw_tty::GuardMode;

// Global flag to indicate a resize event occurred
static RESIZE_PENDING: AtomicBool = AtomicBool::new(false);

extern "C" fn handle_sigwinch(_: libc::c_int) {
    RESIZE_PENDING.store(true, Ordering::SeqCst);
}

fn main() {
    // Get terminal size
    let (width, height) = get_terminal_size();

    // Set up raw mode TTY for input
    let tty = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/tty")
        .expect("Failed to open /dev/tty");

    // Clone the TTY for output - the compositor will write to this
    let tty_output: Arc<Mutex<dyn Write + Send>> = Arc::new(Mutex::new(
        tty.try_clone().expect("Failed to clone tty for output"),
    ));

    let mut tty_input = tty.try_clone().unwrap().guard_mode().unwrap();
    tty_input.set_raw_mode().expect("Failed to set raw mode");

    // Create the compositor with the terminal dimensions and TTY output
    let mut compositor = compositor::Compositor::with_output(width, height, tty_output.clone())
        .expect("Failed to create compositor");

    // Set tty_input to non-blocking
    let fd = tty_input.as_raw_fd();
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFL);
        libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
    }

    // Set up SIGWINCH handler for terminal resize events
    unsafe {
        libc::signal(libc::SIGWINCH, handle_sigwinch as libc::sighandler_t);
    }

    // Clear screen and move cursor to home position
    {
        let mut output = tty_output.lock().unwrap();
        let _ = output.write_all(b"\x1b[2J\x1b[H");
        let _ = output.flush();
    }

    // Main event loop
    let mut input_buf = [0u8; 1024];

    loop {
        // Check for resize events
        if RESIZE_PENDING.swap(false, Ordering::SeqCst) {
            let (new_width, new_height) = get_terminal_size();
            compositor.resize(new_width, new_height);
            // Clear screen and rerender after resize
            {
                let mut output = tty_output.lock().unwrap();
                let _ = output.write_all(b"\x1b[2J\x1b[H");
                let _ = output.flush();
            }
        }

        // Check for keyboard input
        match tty_input.read(&mut input_buf) {
            Ok(0) => {
                // EOF - terminal closed
                break;
            }
            Ok(n) => {
                let input = &input_buf[..n];

                // Check for Ctrl+C to exit (for now, as a safety measure)
                if input.len() == 1 && input[0] == 0x03 {
                    break;
                }

                // Send input to the compositor
                compositor.handle_input(input);
            }
            Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                // No input available, continue
            }
            Err(e) => {
                eprintln!("Error reading input: {}", e);
                break;
            }
        }

        // Poll the compositor for PTY events with a short timeout
        match compositor.poll_once(10) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("Compositor error: {}", e);
                break;
            }
        }
    }

    // Clean up: reset terminal
    print!("\x1b[?25h"); // Show cursor
    print!("\x1b[0m"); // Reset attributes
    print!("\x1b[2J\x1b[H"); // Clear screen
    std::io::stdout().flush().unwrap();
}

/// Get the terminal size using ioctl
fn get_terminal_size() -> (usize, usize) {
    let mut winsize = libc::winsize {
        ws_row: 0,
        ws_col: 0,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };

    unsafe {
        if libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &mut winsize) == 0 {
            (winsize.ws_col as usize, winsize.ws_row as usize)
        } else {
            // Fallback to default size
            (80, 24)
        }
    }
}
