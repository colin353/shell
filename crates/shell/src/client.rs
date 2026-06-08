//! Attach mode (`shell attach`).
//!
//! A "dumb" client: it owns the real terminal, ships keystrokes to the daemon as
//! `ClientMsg::Input`, and writes the daemon's `ServerMsg::Render` bytes straight
//! to the TTY. A single-threaded `poll()` over the TTY and the socket keeps both
//! directions responsive without nonblocking-clone hazards.

use std::io::{self, Read, Write};
use std::os::fd::{AsRawFd, RawFd};
use std::os::unix::net::UnixStream;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};

use raw_tty::GuardMode;

use protocol::codec::{self, FrameReader};
use protocol::{ClientMode, ClientMsg, Hello, ServerMsg, PROTOCOL_VERSION};

use crate::common::{get_terminal_size, ENTER_APP_MODE, EXIT_APP_MODE};

/// Client-side detach key (`Ctrl-\`). Intercepted locally — never forwarded —
/// so the client disconnects while the daemon keeps the session running. The
/// daemon's own prefix is `Ctrl-b`, which must reach it, so detach uses a
/// distinct, rarely-used key.
const DETACH_KEY: u8 = 0x1c;

static RESIZE_PENDING: AtomicBool = AtomicBool::new(false);

extern "C" fn handle_sigwinch(_: libc::c_int) {
    RESIZE_PENDING.store(true, Ordering::SeqCst);
}

pub fn run(socket_path: &Path) -> io::Result<()> {
    let mut stream = UnixStream::connect(socket_path).map_err(|e| {
        io::Error::new(
            e.kind(),
            format!("cannot connect to daemon at {}: {e}", socket_path.display()),
        )
    })?;

    let tty = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/tty")?;
    let (width, height) = get_terminal_size(tty.as_raw_fd());

    // Handshake: Hello -> Welcome.
    codec::write_frame(
        &mut stream,
        &ClientMsg::Hello(Hello {
            version: PROTOCOL_VERSION,
            mode: ClientMode::Dumb,
            size: (width as u16, height as u16),
        }),
    )?;
    match codec::read_frame::<_, ServerMsg>(&mut stream)? {
        Some(ServerMsg::Welcome(welcome)) if welcome.version == PROTOCOL_VERSION => {}
        Some(ServerMsg::Welcome(welcome)) => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "daemon protocol version {} != client {}",
                    welcome.version, PROTOCOL_VERSION
                ),
            ));
        }
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "expected Welcome from daemon",
            ))
        }
    }

    // Take over the terminal.
    let mut tty_output = std::fs::OpenOptions::new().write(true).open("/dev/tty")?;
    tty_output.write_all(ENTER_APP_MODE)?;
    tty_output.flush()?;

    let mut tty_input = tty
        .try_clone()?
        .guard_mode()
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("failed to guard tty: {e}")))?;
    tty_input.set_raw_mode().map_err(|e| {
        io::Error::new(io::ErrorKind::Other, format!("failed to set raw mode: {e}"))
    })?;

    unsafe {
        libc::signal(
            libc::SIGWINCH,
            handle_sigwinch as *const () as libc::sighandler_t,
        );
    }

    let tty_fd = tty_input.as_raw_fd();

    // Run the event loop, then always restore the terminal.
    let result = event_loop(&mut stream, &mut tty_input, &mut tty_output, tty_fd);

    let _ = tty_output.write_all(EXIT_APP_MODE);
    let _ = tty_output.flush();
    result
}

fn event_loop(
    stream: &mut UnixStream,
    tty_input: &mut impl Read,
    tty_output: &mut impl Write,
    tty_fd: RawFd,
) -> io::Result<()> {
    let sock_fd = stream.as_raw_fd();

    let mut frames = FrameReader::new();
    let mut tty_buf = [0u8; 4096];
    let mut sock_buf = [0u8; 65536];

    loop {
        if RESIZE_PENDING.swap(false, Ordering::SeqCst) {
            let (cols, rows) = get_terminal_size(tty_fd);
            codec::write_frame(
                &mut *stream,
                &ClientMsg::Resize {
                    cols: cols as u16,
                    rows: rows as u16,
                },
            )?;
        }

        let mut pfds = [
            libc::pollfd {
                fd: tty_fd,
                events: libc::POLLIN,
                revents: 0,
            },
            libc::pollfd {
                fd: sock_fd,
                events: libc::POLLIN,
                revents: 0,
            },
        ];
        let n = unsafe { libc::poll(pfds.as_mut_ptr(), 2, 1000) };
        if n < 0 {
            let e = io::Error::last_os_error();
            if e.kind() == io::ErrorKind::Interrupted {
                continue; // SIGWINCH; re-check the resize flag
            }
            return Err(e);
        }

        // Keyboard -> daemon.
        if pfds[0].revents & libc::POLLIN != 0 {
            match tty_input.read(&mut tty_buf) {
                Ok(0) => break, // TTY closed
                Ok(k) => {
                    let bytes = &tty_buf[..k];
                    if bytes.contains(&DETACH_KEY) {
                        break; // detach: leave the daemon running
                    }
                    codec::write_frame(
                        &mut *stream,
                        &ClientMsg::Input {
                            bytes: bytes.to_vec(),
                        },
                    )?;
                }
                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {}
                Err(e) => return Err(e),
            }
        }
        if pfds[0].revents & (libc::POLLHUP | libc::POLLERR) != 0 {
            break;
        }

        // Daemon renders -> terminal.
        if pfds[1].revents & libc::POLLIN != 0 {
            match stream.read(&mut sock_buf) {
                Ok(0) => break, // daemon closed (detach or exit)
                Ok(k) => {
                    frames.push(&sock_buf[..k]);
                    while let Some(msg) = frames.next_frame::<ServerMsg>()? {
                        if let ServerMsg::Render { bytes } = msg {
                            tty_output.write_all(&bytes)?;
                            tty_output.flush()?;
                        }
                    }
                }
                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {}
                Err(e) => return Err(e),
            }
        }
        if pfds[1].revents & (libc::POLLHUP | libc::POLLERR) != 0 {
            break;
        }
    }

    Ok(())
}
