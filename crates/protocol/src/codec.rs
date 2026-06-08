//! Length-prefixed framing for protocol messages.
//!
//! Each frame is a little-endian `u32` byte length followed by that many bytes
//! of payload. The payload is JSON for now: `serde_json` is already in the
//! dependency tree, round-trips every protocol type, and is trivially
//! inspectable while the wire format is still settling. Swapping to a compact
//! binary codec (e.g. `bincode`) later is a localized change to this module.

use serde::de::DeserializeOwned;
use serde::Serialize;
use std::io::{self, Read, Write};

/// Largest frame we will read, as a guard against corrupt/hostile length
/// prefixes. 64 MiB comfortably covers a full-screen [`crate::GridSnapshot`].
pub const MAX_FRAME_LEN: u32 = 64 * 1024 * 1024;

/// Encode a value as a length-prefixed frame.
pub fn to_frame<T: Serialize>(value: &T) -> Vec<u8> {
    let body = serde_json::to_vec(value).expect("protocol values are always serializable");
    let mut frame = Vec::with_capacity(4 + body.len());
    frame.extend_from_slice(&(body.len() as u32).to_le_bytes());
    frame.extend_from_slice(&body);
    frame
}

/// Decode a value from a single complete frame (length prefix included).
pub fn from_frame<T: DeserializeOwned>(frame: &[u8]) -> io::Result<T> {
    if frame.len() < 4 {
        return Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            "frame shorter than length prefix",
        ));
    }
    let len = u32::from_le_bytes([frame[0], frame[1], frame[2], frame[3]]) as usize;
    let body = &frame[4..];
    if body.len() < len {
        return Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            "frame body shorter than declared length",
        ));
    }
    serde_json::from_slice(&body[..len]).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

/// Write a value to a stream as a length-prefixed frame.
pub fn write_frame<W: Write, T: Serialize>(mut w: W, value: &T) -> io::Result<()> {
    let frame = to_frame(value);
    w.write_all(&frame)?;
    w.flush()
}

/// Read one length-prefixed frame from a stream and decode it.
///
/// Returns `Ok(None)` on a clean end-of-stream at a frame boundary (the peer
/// closed the connection), and an error on a truncated frame.
pub fn read_frame<R: Read, T: DeserializeOwned>(mut r: R) -> io::Result<Option<T>> {
    let mut len_buf = [0u8; 4];
    match r.read_exact(&mut len_buf) {
        Ok(()) => {}
        Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e),
    }
    let len = u32::from_le_bytes(len_buf);
    if len > MAX_FRAME_LEN {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("frame length {len} exceeds maximum {MAX_FRAME_LEN}"),
        ));
    }
    let mut body = vec![0u8; len as usize];
    r.read_exact(&mut body)?;
    serde_json::from_slice(&body).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ClientMsg, PaneId};

    #[test]
    fn stream_round_trip_multiple_frames() {
        let a = ClientMsg::Detach;
        let b = ClientMsg::Resize { cols: 80, rows: 24 };
        let c = ClientMsg::ProcessSignal {
            pane: PaneId(3),
            signal: 2,
        };

        let mut buf = Vec::new();
        write_frame(&mut buf, &a).unwrap();
        write_frame(&mut buf, &b).unwrap();
        write_frame(&mut buf, &c).unwrap();

        let mut cursor = io::Cursor::new(buf);
        let d1: ClientMsg = read_frame(&mut cursor).unwrap().unwrap();
        let d2: ClientMsg = read_frame(&mut cursor).unwrap().unwrap();
        let d3: ClientMsg = read_frame(&mut cursor).unwrap().unwrap();
        assert!(matches!(d1, ClientMsg::Detach));
        assert!(matches!(d2, ClientMsg::Resize { cols: 80, rows: 24 }));
        assert!(matches!(d3, ClientMsg::ProcessSignal { pane: PaneId(3), signal: 2 }));

        // Clean EOF at a frame boundary.
        let end: Option<ClientMsg> = read_frame(&mut cursor).unwrap();
        assert!(end.is_none());
    }

    #[test]
    fn truncated_body_is_an_error() {
        let mut buf = to_frame(&ClientMsg::Detach);
        buf.pop(); // chop a byte off the body
        let mut cursor = io::Cursor::new(buf);
        let r: io::Result<Option<ClientMsg>> = read_frame(&mut cursor);
        assert!(r.is_err());
    }
}
