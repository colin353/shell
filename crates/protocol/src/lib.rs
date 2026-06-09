//! Wire protocol shared between the shell client, the local daemon, and remote
//! daemons.
//!
//! This crate is deliberately a leaf: it depends only on `serde` and defines its
//! own copies of the small enums it needs (e.g. [`CompletionKind`], [`Origin`]).
//! The richer crates (`libshell`, `shell-syntax`, `emulator`) convert between
//! their internal types and these wire types at the boundary, so the protocol
//! never drags terminal-emulation or tree-sitter dependencies into the network
//! layer.
//!
//! # Topology
//!
//! The same message set is spoken at every hop:
//!
//! ```text
//!   client  <->  local daemon  <->  remote daemon (per remote pane)
//! ```
//!
//! A "smart" client edits the prompt line locally and only round-trips at the
//! three natural pauses (Tab / Enter / Ctrl+R), plus streaming when a process
//! owns the tty. A "dumb" client ships every keystroke and renders fully
//! composited frames the daemon sends back.

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

pub mod codec;

/// Bumped whenever [`ClientMsg`]/[`ServerMsg`] change incompatibly. The
/// handshake compares versions and reports [`RemoteStatus::VersionMismatch`]
/// when a remote daemon binary is stale.
pub const PROTOCOL_VERSION: u32 = 1;

// ---------------------------------------------------------------------------
// IDs
// ---------------------------------------------------------------------------

macro_rules! id_type {
    ($(#[$m:meta])* $name:ident) => {
        $(#[$m])*
        #[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
        pub struct $name(pub u64);
    };
}

id_type!(/// Identifies a multiplexer session (one daemon may host several).
    SessionId);
id_type!(/// Identifies a pane within a session.
    PaneId);
id_type!(/// Correlates an async request with its response (completions, history).
    RequestId);

/// Stable identity of a machine, e.g. `"colin@build-box"` or a configured alias.
#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug)]
pub struct HostId(pub String);

/// Where a command actually executed.
///
/// `Local` is always relative to the log that contains it: in any machine's own
/// history, `Local` means "ran here." Entries are rewritten to `Remote(host)`
/// when mirrored across a hop, so each log stays internally consistent and
/// origin-aware ranking is a pure function of the current pane's host.
#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug, Default)]
pub enum Origin {
    #[default]
    Local,
    Remote(HostId),
}

// ---------------------------------------------------------------------------
// Handshake
// ---------------------------------------------------------------------------

/// Whether the client renders frames itself (`Dumb`) or edits locally (`Smart`).
#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Debug)]
pub enum ClientMode {
    /// Daemon owns the whole compositor and line editor; sends composited
    /// [`ServerMsg::Render`] frames. Simplest possible client.
    Dumb,
    /// Client edits the prompt line locally against a cached [`ContextSnapshot`]
    /// and uses the per-pane streams. Low-latency typing; remote brains.
    Smart,
}

/// First message a client sends after connecting.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Hello {
    pub version: u32,
    pub mode: ClientMode,
    /// Terminal size `(cols, rows)`.
    pub size: (u16, u16),
}

/// Daemon's reply to [`Hello`].
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Welcome {
    pub version: u32,
    pub session: SessionId,
    pub host: HostId,
}

// ---------------------------------------------------------------------------
// Client -> Daemon
// ---------------------------------------------------------------------------

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum ClientMsg {
    Hello(Hello),
    Detach,
    Resize {
        cols: u16,
        rows: u16,
    },
    /// Ask the daemon to repaint the authoritative screen from scratch (used by
    /// `Ctrl-b r` when a pane is remote, to recover from any local drift).
    RequestResync,
    /// Ask the daemon to terminate this session (used by `sessions kill`). Sent
    /// as the first frame on a fresh connection instead of `Hello`.
    Shutdown,

    /// Dumb-mode: a raw keystroke stream (daemon owns line editing).
    Input {
        bytes: Vec<u8>,
    },

    // -- Smart-mode: line editing is local; we round-trip at the three pauses. --
    /// Enter: a finished command line to execute on the authoritative host.
    Submit {
        pane: PaneId,
        line: String,
        cwd: PathBuf,
    },
    /// Tab: request completions that need the real filesystem/PATH.
    Completions {
        pane: PaneId,
        req: RequestId,
        line: String,
        cursor: usize,
        cwd: PathBuf,
    },
    /// Ctrl+R: search history with an origin scope.
    HistorySearch {
        req: RequestId,
        query: String,
        limit: usize,
        scope: HistoryScope,
    },

    // -- Smart-mode: while a process owns the tty, stream through. --
    ProcessStdin {
        pane: PaneId,
        bytes: Vec<u8>,
    },
    ProcessSignal {
        pane: PaneId,
        signal: i32,
    },

    // -- Environment & remoting. --
    /// The client's local env changed (`~/.shell_env` watcher fired). The
    /// daemon re-merges (its own layer winning) and emits a fresh
    /// [`ServerMsg::Context`].
    UpdateLocalEnv {
        vars: Vec<(String, String)>,
    },
    /// Connect a pane to a remote host. The daemon opens `ssh` to the target,
    /// ensures a remote daemon is running, and proxies this pane to it.
    ConnectRemote {
        pane: PaneId,
        target: RemoteTarget,
        forward_env: Vec<(String, String)>,
    },
    DisconnectRemote {
        pane: PaneId,
    },
}

// ---------------------------------------------------------------------------
// Daemon -> Client
// ---------------------------------------------------------------------------

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum ServerMsg {
    Welcome(Welcome),

    /// Dumb-mode: a fully composited, delta-rendered frame (the bytes today's
    /// renderer already writes to the tty).
    Render {
        bytes: Vec<u8>,
    },

    // -- Smart-mode per-pane streams. --
    /// Raw VT bytes for a pane's emulator: shell echo or process output.
    PaneOutput {
        pane: PaneId,
        bytes: Vec<u8>,
    },
    /// Full screen state for repaint on (re)attach.
    GridResync {
        pane: PaneId,
        grid: GridSnapshot,
    },
    /// Authoritative cwd/env/PATH for a pane, so the client can highlight and
    /// prefix-complete locally without a round trip.
    Context {
        pane: PaneId,
        ctx: ContextSnapshot,
    },
    Completions {
        req: RequestId,
        items: Vec<CompletionItem>,
        replace_start: usize,
        replace_end: usize,
    },
    HistoryResults {
        req: RequestId,
        results: Vec<HistoryHit>,
    },

    // -- Lifecycle. --
    ProcessExited {
        pane: PaneId,
        code: i32,
    },
    RemoteStatus {
        pane: PaneId,
        status: RemoteStatus,
    },
    /// The remote session renamed its window (e.g. the `rename-window` builtin);
    /// the client should rename the local tab that owns this remote pane.
    RenameWindow {
        name: String,
    },
    /// The remote session ended deliberately (e.g. the shell exited). The client
    /// should NOT auto-reconnect — a dropped link sends no such message, which is
    /// how a recoverable disconnect is told apart from a clean exit.
    SessionEnded,

    /// Dual-write: mirror this entry into the local log (rewritten to
    /// `Remote(host)`), so closing a remote pane still leaves its commands in
    /// your local history.
    HistoryRecorded {
        entry: HistoryRecord,
    },
}

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Debug)]
pub enum RemoteStatus {
    Connecting,
    Connected,
    Disconnected,
    VersionMismatch,
}

// ---------------------------------------------------------------------------
// Payloads
// ---------------------------------------------------------------------------

/// Authoritative shell context for a pane. Small and slow-changing, so it is
/// pushed rather than polled; the client highlights against it locally.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct ContextSnapshot {
    pub cwd: PathBuf,
    /// Merged effective environment (the remote layer has already won).
    pub env: Vec<(String, String)>,
    /// Executable names on `PATH`, for local command completion + highlighting.
    pub path_executables: Vec<String>,
    pub origin: Origin,
}

/// One tab-completion candidate. Mirrors `shell_syntax::Completion`.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CompletionItem {
    pub display: String,
    pub text: String,
    pub kind: CompletionKind,
}

/// Kind of a completion candidate. Independent copy of
/// `shell_syntax::CompletionKind` to keep this crate dependency-free.
#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Debug)]
pub enum CompletionKind {
    Command,
    Builtin,
    Alias,
    Variable,
    File,
    Flag,
}

/// Target for [`ClientMsg::ConnectRemote`].
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct RemoteTarget {
    /// Passed verbatim to `ssh` (e.g. `"colin@build-box"`).
    pub ssh_destination: String,
    /// Friendly [`HostId`] for tagging history; defaults to `ssh_destination`.
    pub alias: Option<String>,
}

impl RemoteTarget {
    /// The [`HostId`] used to tag commands run through this target.
    pub fn host_id(&self) -> HostId {
        HostId(self.alias.clone().unwrap_or_else(|| self.ssh_destination.clone()))
    }
}

/// How history search treats entries from other hosts.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum HistoryScope {
    /// No origin weighting.
    All,
    /// Only entries with this exact origin.
    Only(Origin),
    /// Include everything, but downrank entries whose origin differs.
    Prefer(Origin),
}

impl Default for HistoryScope {
    fn default() -> Self {
        HistoryScope::All
    }
}

/// A history search result on the wire. Mirrors the scored result produced by
/// `libshell`, minus matcher internals.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct HistoryHit {
    pub command: String,
    pub score: i64,
    /// Character indices that matched the query (for highlighting).
    pub match_indices: Vec<usize>,
    pub origin: Origin,
    pub exit_code: Option<i32>,
}

/// A recorded command, for mirroring across a hop.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct HistoryRecord {
    pub id: String,
    pub command: String,
    /// Seconds since the Unix epoch.
    pub executed_at: u64,
    pub exit_code: Option<i32>,
    pub cwd: Option<String>,
    pub origin: Origin,
}

/// Serialized emulator grid for repaint on reattach (tmux-style server-side
/// emulator). The cell encoding is owned by the `emulator` crate; this carries
/// it opaquely so `protocol` stays free of terminal-emulation types.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct GridSnapshot {
    pub cols: u16,
    pub rows: u16,
    /// Packed cell data produced/consumed by the `emulator` crate.
    pub cells: Vec<u8>,
    /// Whether the alternate screen is active (so reattach lands back inside a
    /// full-screen app rather than the primary buffer).
    pub alternate_screen: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn origin_defaults_to_local() {
        assert_eq!(Origin::default(), Origin::Local);
    }

    #[test]
    fn remote_target_host_id_prefers_alias() {
        let t = RemoteTarget {
            ssh_destination: "colin@1.2.3.4".into(),
            alias: Some("build".into()),
        };
        assert_eq!(t.host_id(), HostId("build".into()));

        let t = RemoteTarget {
            ssh_destination: "colin@1.2.3.4".into(),
            alias: None,
        };
        assert_eq!(t.host_id(), HostId("colin@1.2.3.4".into()));
    }

    #[test]
    fn client_msg_round_trips() {
        let msg = ClientMsg::Submit {
            pane: PaneId(7),
            line: "cargo build --release".into(),
            cwd: PathBuf::from("/home/colin/code"),
        };
        let bytes = codec::to_frame(&msg);
        let decoded: ClientMsg = codec::from_frame(&bytes).unwrap();
        assert!(matches!(decoded, ClientMsg::Submit { pane: PaneId(7), .. }));
    }

    #[test]
    fn server_msg_round_trips() {
        let msg = ServerMsg::Context {
            pane: PaneId(1),
            ctx: ContextSnapshot {
                cwd: PathBuf::from("/tmp"),
                env: vec![("EDITOR".into(), "nvim".into())],
                path_executables: vec!["cargo".into(), "git".into()],
                origin: Origin::Remote(HostId("build".into())),
            },
        };
        let bytes = codec::to_frame(&msg);
        let decoded: ServerMsg = codec::from_frame(&bytes).unwrap();
        match decoded {
            ServerMsg::Context { ctx, .. } => {
                assert_eq!(ctx.origin, Origin::Remote(HostId("build".into())));
                assert_eq!(ctx.path_executables.len(), 2);
            }
            other => panic!("unexpected: {other:?}"),
        }
    }
}
