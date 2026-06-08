//! Bits shared between the standalone, daemon, and client entry points.

use std::path::PathBuf;

/// Put the host terminal into the alternate screen and clear it. Child apps'
/// alternate-screen/mouse modes are consumed by the embedded emulator, so the
/// outer terminal must stay out of normal scrollback while shell runs.
pub const ENTER_APP_MODE: &[u8] = b"\x1b[?1049h\x1b[2J\x1b[H";

/// Restore the host terminal: disable mouse reporting, leave the alternate
/// screen, show the cursor, reset attributes.
pub const EXIT_APP_MODE: &[u8] =
    b"\x1b[?1000l\x1b[?1002l\x1b[?1003l\x1b[?1005l\x1b[?1006l\x1b[?1049l\x1b[?25h\x1b[0m";

/// Query the terminal size via `TIOCGWINSZ`, falling back to 80x24.
pub fn get_terminal_size(fd: std::os::fd::RawFd) -> (usize, usize) {
    let mut winsize = libc::winsize {
        ws_row: 0,
        ws_col: 0,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };

    unsafe {
        if libc::ioctl(fd, libc::TIOCGWINSZ, &mut winsize) == 0 && winsize.ws_col > 0 {
            (winsize.ws_col as usize, winsize.ws_row as usize)
        } else {
            (80, 24)
        }
    }
}

/// Default per-user daemon socket path: `$XDG_RUNTIME_DIR/shell-daemon-<uid>.sock`,
/// falling back to the system temp dir.
pub fn default_socket_path() -> PathBuf {
    let base = std::env::var_os("XDG_RUNTIME_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(std::env::temp_dir);
    let uid = unsafe { libc::getuid() };
    base.join(format!("shell-daemon-{uid}.sock"))
}
