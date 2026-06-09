//! The shell "brain" behind a line editor, abstracted so it can run either
//! in-process or on a remote host.
//!
//! Today the [`Shell`](crate::Shell) line editor owns its syntax highlighter and
//! history directly. Splitting those operations behind [`ShellBackend`] is the
//! seam the daemon/remote work is built on: [`LocalBackend`] resolves everything
//! synchronously in-process (today's behavior, unchanged), while a future
//! `RemoteBackend` will proxy the same calls over the `protocol` connection so a
//! pane's completions, highlighting, history, and execution happen on another
//! machine.
//!
//! Phase 0 keeps the surface deliberately close to the existing method
//! signatures (cwd is still passed in by the line editor) so the extraction is
//! behavior-preserving.

use crate::history::{CommandSource, EntryId, HistorySearchResult};
use crate::syntax::{CompletionKind, SyntaxHandler};
use crate::ShellCore;
use protocol::{HistoryRecord, HistoryScope, HostId};
use std::path::PathBuf;
use std::sync::Arc;

/// A tab-completion candidate: `(display, replacement_text, kind)`.
pub type Completion = (String, String, CompletionKind);

/// The authoritative shell state and intelligence for one pane.
///
/// The line editor (local, latency-sensitive) drives this; the implementation
/// decides whether the work happens here or on a remote host.
pub trait ShellBackend {
    // --- Syntax highlighting & completion ---

    /// Return `input` with ANSI color codes applied for the given working dir.
    fn highlight(&mut self, input: &str, cwd: &PathBuf) -> String;

    /// All completions at the cursor, with their shared replace range.
    /// Returns `None` when there are no candidates.
    fn completions_full(
        &mut self,
        input: &str,
        cursor_pos: usize,
        cwd: &PathBuf,
    ) -> Option<(Vec<Completion>, usize, usize)>;

    /// The word under the cursor and its byte boundaries `(word, start, end)`.
    fn word_at_cursor(&mut self, input: &str, cursor_pos: usize) -> Option<(String, usize, usize)>;

    // --- History (read) ---

    /// The commands of the most recent `n` entries, most-recent first.
    fn recent_commands(&self, n: usize) -> Vec<String>;

    /// Fuzzy history search with an origin scope (see [`HistoryScope`]).
    fn history_search(
        &self,
        query: &str,
        limit: usize,
        scope: &HistoryScope,
    ) -> Vec<HistorySearchResult>;

    // --- History (write) ---

    /// Record a command execution, returning its id (or `None` on failure,
    /// which is logged). The execution's origin is the backend's host.
    fn record_command(
        &self,
        command: String,
        source: CommandSource,
        cwd: Option<String>,
    ) -> Option<EntryId>;

    /// Record a command's exit status.
    fn record_exit(&self, id: &EntryId, exit_code: i32, duration_ms: u64);

    /// Mark a command as killed (e.g. Ctrl+C).
    fn mark_killed(&self, id: &EntryId);

    /// Mirror a command that ran on a remote host into this backend's history,
    /// tagged `Remote(host)` (history dual-write). No-op by default.
    fn mirror_remote_record(&self, record: &HistoryRecord, host: &HostId) {
        let _ = (record, host);
    }
}

/// In-process backend: today's behavior, wrapping the existing
/// [`SyntaxHandler`] and shared [`ShellCore`] history. Records commands as
/// [`Origin::Local`](protocol::Origin::Local).
pub struct LocalBackend {
    core: Arc<ShellCore>,
    syntax: SyntaxHandler,
}

impl LocalBackend {
    /// Create a backend over a shared [`ShellCore`] (shared so multiple panes
    /// see one unified history).
    pub fn new(core: Arc<ShellCore>) -> Self {
        LocalBackend {
            core,
            syntax: SyntaxHandler::new(),
        }
    }

    /// The shared core, for callers that need history sharing across panes.
    pub fn core(&self) -> &Arc<ShellCore> {
        &self.core
    }
}

impl ShellBackend for LocalBackend {
    fn highlight(&mut self, input: &str, cwd: &PathBuf) -> String {
        self.syntax.highlight(input, cwd)
    }

    fn completions_full(
        &mut self,
        input: &str,
        cursor_pos: usize,
        cwd: &PathBuf,
    ) -> Option<(Vec<Completion>, usize, usize)> {
        self.syntax.completions_full(input, cursor_pos, cwd)
    }

    fn word_at_cursor(&mut self, input: &str, cursor_pos: usize) -> Option<(String, usize, usize)> {
        self.syntax.word_at_cursor(input, cursor_pos)
    }

    fn recent_commands(&self, n: usize) -> Vec<String> {
        self.core
            .history()
            .recent(n)
            .into_iter()
            .map(|e| e.command)
            .collect()
    }

    fn history_search(
        &self,
        query: &str,
        limit: usize,
        scope: &HistoryScope,
    ) -> Vec<HistorySearchResult> {
        self.core.history().search_with_indices_scoped(query, limit, scope)
    }

    fn record_command(
        &self,
        command: String,
        source: CommandSource,
        cwd: Option<String>,
    ) -> Option<EntryId> {
        match self.core.record_command(command, source, cwd) {
            Ok(id) => Some(id),
            Err(e) => {
                eprintln!("Warning: failed to record command in history: {}", e);
                None
            }
        }
    }

    fn record_exit(&self, id: &EntryId, exit_code: i32, duration_ms: u64) {
        if let Err(e) = self.core.record_exit(id, exit_code, duration_ms) {
            eprintln!("Warning: failed to record command exit in history: {}", e);
        }
    }

    fn mark_killed(&self, id: &EntryId) {
        if let Err(e) = self.core.mark_killed(id) {
            eprintln!("Warning: failed to mark command killed in history: {}", e);
        }
    }

    fn mirror_remote_record(&self, record: &HistoryRecord, host: &HostId) {
        if let Err(e) = self.core.history().record_mirrored(record, host) {
            eprintln!("Warning: failed to mirror remote history: {}", e);
        }
    }
}
