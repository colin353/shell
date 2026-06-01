//! Process-wide shell environment support.

use crate::{clear_global_diagnostic_flag, set_global_diagnostic_flag, GlobalDiagnosticFlag};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{LazyLock, Mutex, Once};
use std::time::{Duration, SystemTime};

const SHELL_ENV_FILE: &str = ".shell_env";
const WATCH_INTERVAL: Duration = Duration::from_millis(500);

static GLOBAL_ENV: LazyLock<ShellEnvironment> = LazyLock::new(|| {
    let path = shell_env_file_path();
    let (exports, last_modified) = match load_env_file(&path) {
        Ok(loaded) => {
            clear_global_diagnostic_flag(GlobalDiagnosticFlag::EnvironmentParseFailure);
            loaded
        }
        Err(_) => {
            set_global_diagnostic_flag(GlobalDiagnosticFlag::EnvironmentParseFailure);
            (HashMap::new(), None)
        }
    };

    ShellEnvironment {
        path,
        state: Mutex::new(ShellEnvironmentState {
            exports,
            seen_modified: last_modified,
        }),
        watcher_started: Once::new(),
    }
});

/// Return the default shell environment file path (`~/.shell_env`).
pub fn shell_env_file_path() -> PathBuf {
    std::env::var_os("HOME")
        .map(PathBuf::from)
        .unwrap_or_else(std::env::temp_dir)
        .join(SHELL_ENV_FILE)
}

/// Return the current process-wide shell environment overrides.
pub fn shell_env_snapshot() -> Vec<(String, String)> {
    GLOBAL_ENV.ensure_watcher();
    let _ = GLOBAL_ENV.reload_if_changed();
    GLOBAL_ENV.snapshot()
}

/// Force a reload of `~/.shell_env`.
pub fn reload_shell_env() -> Result<usize, ShellEnvError> {
    GLOBAL_ENV.ensure_watcher();
    GLOBAL_ENV.reload()
}

struct ShellEnvironment {
    path: PathBuf,
    state: Mutex<ShellEnvironmentState>,
    watcher_started: Once,
}

struct ShellEnvironmentState {
    exports: HashMap<String, String>,
    seen_modified: Option<SystemTime>,
}

impl ShellEnvironment {
    fn ensure_watcher(&'static self) {
        self.watcher_started.call_once(|| {
            std::thread::spawn(move || loop {
                std::thread::sleep(WATCH_INTERVAL);
                let _ = self.reload_if_changed();
            });
        });
    }

    fn snapshot(&self) -> Vec<(String, String)> {
        self.state
            .lock()
            .unwrap()
            .exports
            .iter()
            .map(|(key, value)| (key.clone(), value.clone()))
            .collect()
    }

    fn reload(&self) -> Result<usize, ShellEnvError> {
        let (exports, last_modified) = match load_env_file(&self.path) {
            Ok(loaded) => loaded,
            Err(err) => {
                set_global_diagnostic_flag(GlobalDiagnosticFlag::EnvironmentParseFailure);
                return Err(err);
            }
        };
        let count = exports.len();
        let mut state = self.state.lock().unwrap();
        state.exports = exports;
        state.seen_modified = last_modified;
        clear_global_diagnostic_flag(GlobalDiagnosticFlag::EnvironmentParseFailure);
        Ok(count)
    }

    fn reload_if_changed(&self) -> Result<(), ShellEnvError> {
        let current_modified = env_file_modified(&self.path)?;
        let should_reload = {
            let state = self.state.lock().unwrap();
            state.seen_modified != current_modified
        };

        if should_reload {
            if let Err(err) = self.reload() {
                self.state.lock().unwrap().seen_modified = current_modified;
                return Err(err);
            }
        }

        Ok(())
    }
}

fn load_env_file(
    path: &Path,
) -> Result<(HashMap<String, String>, Option<SystemTime>), ShellEnvError> {
    let content = match std::fs::read_to_string(path) {
        Ok(content) => content,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            return Ok((HashMap::new(), None));
        }
        Err(err) => {
            return Err(ShellEnvError::Io {
                path: path.to_path_buf(),
                source: err,
            });
        }
    };

    let inherited = std::env::vars().collect();
    let exports = parse_shell_env(&content, &inherited, path)?;
    Ok((exports, env_file_modified(path)?))
}

fn env_file_modified(path: &Path) -> Result<Option<SystemTime>, ShellEnvError> {
    match std::fs::metadata(path) {
        Ok(metadata) => Ok(metadata.modified().ok()),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(err) => Err(ShellEnvError::Io {
            path: path.to_path_buf(),
            source: err,
        }),
    }
}

fn parse_shell_env(
    content: &str,
    inherited: &HashMap<String, String>,
    path: &Path,
) -> Result<HashMap<String, String>, ShellEnvError> {
    let mut resolved = inherited.clone();
    let mut exports = HashMap::new();

    for (index, raw_line) in content.lines().enumerate() {
        let line = raw_line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        let Some(rest) = line.strip_prefix("export") else {
            return Err(parse_error(path, index, "expected `export NAME=value`"));
        };

        if !rest.starts_with(char::is_whitespace) {
            return Err(parse_error(
                path,
                index,
                "expected whitespace after `export`",
            ));
        }

        let Some((name, raw_value)) = rest.trim_start().split_once('=') else {
            return Err(parse_error(path, index, "expected `=` in export"));
        };

        let name = name.trim();
        if !is_valid_env_name(name) {
            return Err(parse_error(
                path,
                index,
                "invalid environment variable name",
            ));
        }

        let value = parse_value(raw_value.trim(), &resolved);
        resolved.insert(name.to_string(), value.clone());
        exports.insert(name.to_string(), value);
    }

    Ok(exports)
}

fn parse_value(raw_value: &str, env: &HashMap<String, String>) -> String {
    if let Some(value) = raw_value
        .strip_prefix('\'')
        .and_then(|value| value.strip_suffix('\''))
    {
        return value.to_string();
    }

    let value = raw_value
        .strip_prefix('"')
        .and_then(|value| value.strip_suffix('"'))
        .unwrap_or(raw_value);
    expand_env_vars(value, env)
}

fn expand_env_vars(value: &str, env: &HashMap<String, String>) -> String {
    let mut expanded = String::new();
    let mut chars = value.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch != '$' {
            expanded.push(ch);
            continue;
        }

        if chars.peek() == Some(&'{') {
            chars.next();
            let mut name = String::new();
            while let Some(&next) = chars.peek() {
                chars.next();
                if next == '}' {
                    break;
                }
                name.push(next);
            }
            expanded.push_str(env.get(&name).map(String::as_str).unwrap_or(""));
            continue;
        }

        let mut name = String::new();
        while let Some(&next) = chars.peek() {
            if next.is_ascii_alphanumeric() || next == '_' {
                name.push(next);
                chars.next();
            } else {
                break;
            }
        }

        if name.is_empty() {
            expanded.push('$');
        } else {
            expanded.push_str(env.get(&name).map(String::as_str).unwrap_or(""));
        }
    }

    expanded
}

fn is_valid_env_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };

    (first.is_ascii_alphabetic() || first == '_')
        && chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
}

fn parse_error(path: &Path, line: usize, message: &'static str) -> ShellEnvError {
    ShellEnvError::Parse {
        path: path.to_path_buf(),
        line: line + 1,
        message,
    }
}

/// Error returned when loading the shell environment file fails.
#[derive(Debug)]
pub enum ShellEnvError {
    Io {
        path: PathBuf,
        source: std::io::Error,
    },
    Parse {
        path: PathBuf,
        line: usize,
        message: &'static str,
    },
}

impl std::fmt::Display for ShellEnvError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ShellEnvError::Io { path, source } => write!(f, "{}: {}", path.display(), source),
            ShellEnvError::Parse {
                path,
                line,
                message,
            } => write!(f, "{}:{}: {}", path.display(), line, message),
        }
    }
}

impl std::error::Error for ShellEnvError {}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(content: &str) -> HashMap<String, String> {
        let mut inherited = HashMap::new();
        inherited.insert("PATH".to_string(), "/usr/bin".to_string());
        inherited.insert("HOME".to_string(), "/home/tester".to_string());
        parse_shell_env(content, &inherited, Path::new("/tmp/.shell_env")).unwrap()
    }

    #[test]
    fn parses_exports_and_interpolates_inherited_vars() {
        let exports = parse("export PATH=$PATH:$HOME/bin\nexport EDITOR=nvim\n");

        assert_eq!(exports.get("PATH").unwrap(), "/usr/bin:/home/tester/bin");
        assert_eq!(exports.get("EDITOR").unwrap(), "nvim");
    }

    #[test]
    fn later_exports_can_reference_earlier_exports() {
        let exports = parse("export BASE=/opt/tools\nexport PATH=$BASE/bin:$PATH\n");

        assert_eq!(exports.get("PATH").unwrap(), "/opt/tools/bin:/usr/bin");
    }

    #[test]
    fn single_quotes_disable_interpolation() {
        let exports = parse("export LITERAL='$HOME/bin'\n");

        assert_eq!(exports.get("LITERAL").unwrap(), "$HOME/bin");
    }

    #[test]
    fn rejects_invalid_lines() {
        let inherited = HashMap::new();
        let err =
            parse_shell_env("PATH=/bin\n", &inherited, Path::new("/tmp/.shell_env")).unwrap_err();

        assert_eq!(
            err.to_string(),
            "/tmp/.shell_env:1: expected `export NAME=value`"
        );
    }
}
