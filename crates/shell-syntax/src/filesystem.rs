//! Filesystem abstraction for testability.

use std::collections::{HashMap, HashSet};
use std::io;
use std::path::{Path, PathBuf};

/// A directory entry returned by `FileSystem::read_dir`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirEntry {
    /// The file or directory name (not the full path).
    pub name: String,
    /// Whether this entry is a directory.
    pub is_dir: bool,
}

/// Trait for filesystem operations, allowing fake implementations in tests.
pub trait FileSystem: Send + Sync {
    /// List entries in a directory (files and subdirectories).
    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>>;

    /// Check if a path exists and is executable.
    fn is_executable(&self, path: &Path) -> bool;

    /// Check if a path is a directory.
    fn is_dir(&self, path: &Path) -> bool;

    /// Check if a path exists.
    fn exists(&self, path: &Path) -> bool;
}

/// Real filesystem implementation.
#[derive(Debug, Clone, Copy, Default)]
pub struct RealFileSystem;

impl FileSystem for RealFileSystem {
    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        let entries = std::fs::read_dir(path)?
            .filter_map(|e| e.ok())
            .map(|e| DirEntry {
                name: e.file_name().to_string_lossy().into_owned(),
                is_dir: e.file_type().map(|t| t.is_dir()).unwrap_or(false),
            })
            .collect();
        Ok(entries)
    }

    fn is_executable(&self, path: &Path) -> bool {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            std::fs::metadata(path)
                .map(|m| m.is_file() && (m.permissions().mode() & 0o111 != 0))
                .unwrap_or(false)
        }
        #[cfg(not(unix))]
        {
            // On non-Unix, just check if file exists
            path.is_file()
        }
    }

    fn is_dir(&self, path: &Path) -> bool {
        path.is_dir()
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }
}

/// Fake filesystem for testing.
#[derive(Debug, Clone, Default)]
pub struct FakeFileSystem {
    /// Directory contents: path -> list of entries.
    pub entries: HashMap<PathBuf, Vec<DirEntry>>,
    /// Set of paths that are executable.
    pub executables: HashSet<PathBuf>,
}

impl FakeFileSystem {
    /// Create a new empty fake filesystem.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a directory with entries.
    pub fn add_dir(&mut self, path: impl Into<PathBuf>, entries: Vec<DirEntry>) {
        self.entries.insert(path.into(), entries);
    }

    /// Mark a path as executable.
    pub fn add_executable(&mut self, path: impl Into<PathBuf>) {
        self.executables.insert(path.into());
    }
}

impl FileSystem for FakeFileSystem {
    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        self.entries
            .get(path)
            .cloned()
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "directory not found"))
    }

    fn is_executable(&self, path: &Path) -> bool {
        self.executables.contains(path)
    }

    fn is_dir(&self, path: &Path) -> bool {
        self.entries.contains_key(path)
    }

    fn exists(&self, path: &Path) -> bool {
        self.entries.contains_key(path)
            || self.executables.contains(path)
            || self
                .entries
                .values()
                .flatten()
                .any(|e| path.ends_with(&e.name))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fake_filesystem() {
        let mut fs = FakeFileSystem::new();
        fs.add_dir(
            "/home/user",
            vec![
                DirEntry {
                    name: "file.txt".into(),
                    is_dir: false,
                },
                DirEntry {
                    name: "src".into(),
                    is_dir: true,
                },
            ],
        );
        fs.add_executable("/home/user/script.sh");

        let entries = fs.read_dir(Path::new("/home/user")).unwrap();
        assert_eq!(entries.len(), 2);

        assert!(fs.is_executable(Path::new("/home/user/script.sh")));
        assert!(!fs.is_executable(Path::new("/home/user/file.txt")));

        assert!(fs.is_dir(Path::new("/home/user")));
        assert!(!fs.is_dir(Path::new("/home/user/file.txt")));
    }
}
