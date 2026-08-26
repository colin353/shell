//! Typed control requests emitted by foreground programs through their PTY.
//!
//! The framing is a private OSC sequence:
//!
//! ```text
//! OSC 777;shell-control;1;<token>;<operation>;<base64url payload> ST
//! ```
//!
//! A per-process capability token prevents terminal escape sequences copied
//! from logs or untrusted files from mutating shell state accidentally.

use base64::engine::general_purpose::URL_SAFE_NO_PAD;
use base64::Engine;
use std::ffi::OsString;
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::PathBuf;

pub const CONTROL_TOKEN_ENV: &str = "SHELL_CONTROL_TOKEN";

const OSC_PREFIX: &[u8] = b"777;shell-control;1;";
const OSC_PREFIX_TEXT: &str = "777;shell-control;1;";
const MAX_OSC_BYTES: usize = 8192;
const MAX_PATH_BYTES: usize = 4096;
const MAX_TITLE_BYTES: usize = 512;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TerminalControl {
    SetCwd(PathBuf),
    RenameWindow(String),
}

impl TerminalControl {
    fn operation(&self) -> &'static str {
        match self {
            Self::SetCwd(_) => "set-cwd",
            Self::RenameWindow(_) => "rename-window",
        }
    }

    fn payload(&self) -> &[u8] {
        match self {
            Self::SetCwd(path) => path.as_os_str().as_bytes(),
            Self::RenameWindow(name) => name.as_bytes(),
        }
    }
}

/// Encode one request. The caller should write the result directly to the
/// controlling terminal rather than stdout, which might be redirected.
pub fn encode(token: &str, control: &TerminalControl) -> Vec<u8> {
    let payload = URL_SAFE_NO_PAD.encode(control.payload());
    format!(
        "\x1b]{}{};{};{}\x1b\\",
        OSC_PREFIX_TEXT,
        token,
        control.operation(),
        payload
    )
    .into_bytes()
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum ParseState {
    #[default]
    Ground,
    Escape,
    Osc,
    OscEscape,
    IgnoreOsc,
    IgnoreOscEscape,
}

/// Streaming parser for shell control OSCs. All unrelated terminal traffic is
/// ignored. Changing the expected token also resets any partial OSC so one
/// foreground process cannot finish a sequence begun by its predecessor.
#[derive(Debug, Default)]
pub struct TerminalControlParser {
    expected_token: Option<String>,
    state: ParseState,
    osc: Vec<u8>,
}

impl TerminalControlParser {
    pub fn set_token(&mut self, token: Option<String>) {
        self.expected_token = token;
        self.state = ParseState::Ground;
        self.osc.clear();
    }

    pub fn advance(&mut self, bytes: &[u8]) -> Vec<TerminalControl> {
        let mut controls = Vec::new();
        for &byte in bytes {
            match self.state {
                ParseState::Ground => {
                    if byte == 0x1b {
                        self.state = ParseState::Escape;
                    }
                }
                ParseState::Escape => match byte {
                    b']' => {
                        self.osc.clear();
                        self.state = ParseState::Osc;
                    }
                    0x1b => {}
                    _ => self.state = ParseState::Ground,
                },
                ParseState::Osc => match byte {
                    0x07 => self.finish_osc(&mut controls),
                    0x1b => self.state = ParseState::OscEscape,
                    _ if self.osc.len() < MAX_OSC_BYTES => self.osc.push(byte),
                    _ => {
                        self.osc.clear();
                        self.state = ParseState::IgnoreOsc;
                    }
                },
                ParseState::OscEscape => {
                    if byte == b'\\' {
                        self.finish_osc(&mut controls);
                    } else {
                        self.osc.clear();
                        self.state = if byte == b']' {
                            ParseState::Osc
                        } else if byte == 0x1b {
                            ParseState::Escape
                        } else {
                            ParseState::Ground
                        };
                    }
                }
                ParseState::IgnoreOsc => match byte {
                    0x07 => self.state = ParseState::Ground,
                    0x1b => self.state = ParseState::IgnoreOscEscape,
                    _ => {}
                },
                ParseState::IgnoreOscEscape => {
                    self.state = if byte == b'\\' {
                        ParseState::Ground
                    } else if byte == 0x1b {
                        ParseState::IgnoreOscEscape
                    } else {
                        ParseState::IgnoreOsc
                    };
                }
            }
        }
        controls
    }

    fn finish_osc(&mut self, controls: &mut Vec<TerminalControl>) {
        if let Some(control) = self.decode_current() {
            controls.push(control);
        }
        self.osc.clear();
        self.state = ParseState::Ground;
    }

    fn decode_current(&self) -> Option<TerminalControl> {
        let expected_token = self.expected_token.as_deref()?;
        let body = self.osc.strip_prefix(OSC_PREFIX)?;
        let mut fields = body.splitn(3, |byte| *byte == b';');
        let token = fields.next()?;
        let operation = fields.next()?;
        let encoded_payload = fields.next()?;
        if token != expected_token.as_bytes() {
            return None;
        }

        let payload = URL_SAFE_NO_PAD.decode(encoded_payload).ok()?;
        match operation {
            b"set-cwd" if !payload.is_empty() && payload.len() <= MAX_PATH_BYTES => Some(
                TerminalControl::SetCwd(PathBuf::from(OsString::from_vec(payload))),
            ),
            b"rename-window" if !payload.is_empty() && payload.len() <= MAX_TITLE_BYTES => {
                let title = String::from_utf8(payload).ok()?;
                if title.chars().any(char::is_control) {
                    None
                } else {
                    Some(TerminalControl::RenameWindow(title))
                }
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_st_and_bel_terminated_controls() {
        let token = "0123456789abcdef";
        let cwd = TerminalControl::SetCwd(PathBuf::from("/tmp/a;b"));
        let title = TerminalControl::RenameWindow("build; debug".to_string());
        let mut parser = TerminalControlParser::default();
        parser.set_token(Some(token.to_string()));

        assert_eq!(parser.advance(&encode(token, &cwd)), vec![cwd]);

        let mut bel = encode(token, &title);
        bel.truncate(bel.len() - 2);
        bel.push(0x07);
        assert_eq!(parser.advance(&bel), vec![title]);
    }

    #[test]
    fn parses_a_sequence_split_at_every_boundary() {
        let token = "capability";
        let control = TerminalControl::SetCwd(PathBuf::from("/tmp/project"));
        let encoded = encode(token, &control);

        for split in 0..=encoded.len() {
            let mut parser = TerminalControlParser::default();
            parser.set_token(Some(token.to_string()));
            let mut result = parser.advance(&encoded[..split]);
            result.extend(parser.advance(&encoded[split..]));
            assert_eq!(result, vec![control.clone()], "split at {split}");
        }
    }

    #[test]
    fn preserves_non_utf8_unix_path_bytes() {
        let token = "capability";
        let path = PathBuf::from(OsString::from_vec(vec![b'/', b't', b'm', b'p', b'/', 0xff]));
        let control = TerminalControl::SetCwd(path);
        let mut parser = TerminalControlParser::default();
        parser.set_token(Some(token.to_string()));

        assert_eq!(parser.advance(&encode(token, &control)), vec![control]);
    }

    #[test]
    fn rejects_wrong_token_and_resets_partial_input_between_processes() {
        let control = TerminalControl::RenameWindow("trusted".to_string());
        let encoded = encode("old", &control);
        let split = encoded.len() - 1;
        let mut parser = TerminalControlParser::default();
        parser.set_token(Some("old".to_string()));
        assert!(parser.advance(&encoded[..split]).is_empty());

        parser.set_token(Some("new".to_string()));
        assert!(parser.advance(&encoded[split..]).is_empty());
        assert!(parser.advance(&encode("old", &control)).is_empty());
        assert_eq!(parser.advance(&encode("new", &control)), vec![control]);
    }

    #[test]
    fn ignores_oversized_or_malformed_controls() {
        let mut parser = TerminalControlParser::default();
        parser.set_token(Some("token".to_string()));
        let mut oversized = b"\x1b]".to_vec();
        oversized.extend(std::iter::repeat_n(b'a', MAX_OSC_BYTES + 1));
        oversized.extend_from_slice(b"\x1b\\");
        oversized.extend_from_slice(&encode(
            "token",
            &TerminalControl::RenameWindow("after".to_string()),
        ));

        assert_eq!(
            parser.advance(&oversized),
            vec![TerminalControl::RenameWindow("after".to_string())]
        );
    }
}
