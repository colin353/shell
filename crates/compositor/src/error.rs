use pty;

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
