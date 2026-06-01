//! Process-wide diagnostic flags for non-pane-specific shell state.

use std::collections::BTreeSet;
use std::sync::{LazyLock, Mutex};

static GLOBAL_DIAGNOSTICS: LazyLock<Mutex<BTreeSet<GlobalDiagnosticFlag>>> =
    LazyLock::new(|| Mutex::new(BTreeSet::new()));

/// A process-wide condition that should be surfaced outside an individual pane.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum GlobalDiagnosticFlag {
    /// The shell environment file could not be loaded.
    EnvironmentParseFailure,
}

/// Return the current process-wide diagnostic flags.
pub fn global_diagnostic_flags() -> Vec<GlobalDiagnosticFlag> {
    GLOBAL_DIAGNOSTICS.lock().unwrap().iter().copied().collect()
}

/// Set a process-wide diagnostic flag.
pub fn set_global_diagnostic_flag(flag: GlobalDiagnosticFlag) {
    GLOBAL_DIAGNOSTICS.lock().unwrap().insert(flag);
}

/// Clear a process-wide diagnostic flag.
pub fn clear_global_diagnostic_flag(flag: GlobalDiagnosticFlag) {
    GLOBAL_DIAGNOSTICS.lock().unwrap().remove(&flag);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tracks_global_diagnostic_flags() {
        clear_global_diagnostic_flag(GlobalDiagnosticFlag::EnvironmentParseFailure);
        assert!(global_diagnostic_flags().is_empty());

        set_global_diagnostic_flag(GlobalDiagnosticFlag::EnvironmentParseFailure);
        assert_eq!(
            global_diagnostic_flags(),
            vec![GlobalDiagnosticFlag::EnvironmentParseFailure]
        );

        clear_global_diagnostic_flag(GlobalDiagnosticFlag::EnvironmentParseFailure);
        assert!(global_diagnostic_flags().is_empty());
    }
}
