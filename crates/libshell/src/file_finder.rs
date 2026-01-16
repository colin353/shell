//! Fast fuzzy file finder with .gitignore support.
//!
//! This module provides a file finder that:
//! - Uses the `ignore` crate for fast, parallel file walking
//! - Respects .gitignore patterns
//! - Provides fuzzy matching using `fuzzy-matcher`
//! - Returns results incrementally for responsiveness

use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use ignore::WalkBuilder;
use std::path::{Path, PathBuf};

/// Maximum number of files to collect before stopping the walk.
/// This prevents memory issues in very large directories.
const MAX_FILES: usize = 50_000;

/// Maximum number of results to return from a search.
pub const MAX_RESULTS: usize = 100;

/// A file match result with score and match indices.
#[derive(Debug, Clone)]
pub struct FileMatch {
    /// The file path (relative or absolute based on search context)
    pub path: String,
    /// The display path (for showing in picker)
    pub display_path: String,
    /// Fuzzy match score (higher is better)
    pub score: i64,
    /// Character indices that matched the query (for highlighting)
    pub match_indices: Vec<usize>,
    /// Whether this is a directory
    pub is_directory: bool,
}

/// Context for a file finder search.
#[derive(Debug, Clone)]
pub struct FileFinderContext {
    /// The base directory to search in
    pub base_dir: PathBuf,
    /// Whether the original input was an absolute path
    pub is_absolute: bool,
    /// Start position for replacement in the input buffer
    pub replace_start: usize,
    /// End position for replacement in the input buffer
    pub replace_end: usize,
    /// The original word that triggered the search
    pub original_word: String,
    /// Cached list of all files (for fast re-filtering)
    pub files: Vec<FileCacheEntry>,
}

/// A cached file entry for fast re-filtering.
#[derive(Debug, Clone)]
pub struct FileCacheEntry {
    /// The path to display/insert (relative or absolute)
    pub path: String,
    /// The display path (usually same as path, but may differ)
    pub display_path: String,
    /// Whether this is a directory
    pub is_directory: bool,
}

impl FileFinderContext {
    /// Create a new file finder context.
    ///
    /// # Arguments
    /// * `base_dir` - The directory to search in
    /// * `is_absolute` - Whether paths should be absolute
    /// * `replace_start` - Start of the word to replace in input buffer
    /// * `replace_end` - End of the word to replace in input buffer
    /// * `original_word` - The word that was under the cursor
    pub fn new(
        base_dir: PathBuf,
        is_absolute: bool,
        replace_start: usize,
        replace_end: usize,
        original_word: String,
    ) -> Self {
        FileFinderContext {
            base_dir,
            is_absolute,
            replace_start,
            replace_end,
            original_word,
            files: Vec::new(),
        }
    }

    /// Collect all files from the base directory.
    /// This populates the `files` cache for fast re-filtering.
    pub fn collect_files(&mut self) {
        self.files.clear();

        let walker = WalkBuilder::new(&self.base_dir)
            .hidden(false) // Include hidden files
            .git_ignore(true) // Respect .gitignore
            .git_global(true) // Respect global gitignore
            .git_exclude(true) // Respect .git/info/exclude
            .follow_links(false) // Don't follow symlinks to avoid cycles
            .max_depth(Some(20)) // Reasonable depth limit
            .build();

        for entry in walker.take(MAX_FILES).flatten() {
            let path = entry.path();

            // Skip the base directory itself
            if path == self.base_dir {
                continue;
            }

            let is_directory = path.is_dir();

            // Create the path string based on whether we want absolute or relative
            let path_str = if self.is_absolute {
                path.to_string_lossy().to_string()
            } else {
                path.strip_prefix(&self.base_dir)
                    .unwrap_or(path)
                    .to_string_lossy()
                    .to_string()
            };

            // Display path is the same for now
            let display_path = path_str.clone();

            self.files.push(FileCacheEntry {
                path: path_str,
                display_path,
                is_directory,
            });
        }
    }

    /// Search the cached files with a fuzzy query.
    ///
    /// # Arguments
    /// * `query` - The fuzzy search query
    ///
    /// # Returns
    /// A vector of matching files, sorted by score (best first)
    pub fn search(&self, query: &str) -> Vec<FileMatch> {
        let matcher = SkimMatcherV2::default();

        let mut matches: Vec<FileMatch> = self
            .files
            .iter()
            .filter_map(|entry| {
                if query.is_empty() {
                    // No query - return all files with score 0
                    Some(FileMatch {
                        path: entry.path.clone(),
                        display_path: entry.display_path.clone(),
                        score: 0,
                        match_indices: Vec::new(),
                        is_directory: entry.is_directory,
                    })
                } else {
                    // Fuzzy match against the path
                    matcher
                        .fuzzy_indices(&entry.path, query)
                        .map(|(score, indices)| FileMatch {
                            path: entry.path.clone(),
                            display_path: entry.display_path.clone(),
                            score,
                            match_indices: indices,
                            is_directory: entry.is_directory,
                        })
                }
            })
            .collect();

        // Sort by score (descending) then by path length (shorter first for ties)
        matches.sort_by(|a, b| {
            b.score
                .cmp(&a.score)
                .then_with(|| a.path.len().cmp(&b.path.len()))
        });

        // Limit results
        matches.truncate(MAX_RESULTS);
        matches
    }
}

/// Parse a word to extract the base directory and initial query.
///
/// # Arguments
/// * `word` - The word under the cursor
/// * `cwd` - The current working directory
///
/// # Returns
/// (base_dir, initial_query, is_absolute)
pub fn parse_word_for_search(word: &str, cwd: &Path) -> (PathBuf, String, bool) {
    if word.is_empty() {
        return (cwd.to_path_buf(), String::new(), false);
    }

    let is_absolute = word.starts_with('/');

    if is_absolute {
        // Find the last '/' to split into directory and query
        if let Some(last_slash) = word.rfind('/') {
            let dir_part = &word[..=last_slash];
            let query_part = &word[last_slash + 1..];

            let base_dir = PathBuf::from(dir_part);
            if base_dir.is_dir() {
                return (base_dir, query_part.to_string(), true);
            } else {
                // Directory doesn't exist, try parent
                let parent = PathBuf::from(&word[..last_slash]);
                if parent.is_dir() || last_slash == 0 {
                    let search_base = if last_slash == 0 {
                        PathBuf::from("/")
                    } else {
                        parent
                    };
                    return (search_base, query_part.to_string(), true);
                }
            }
        }
        // Fallback: search from root with the word as query
        (PathBuf::from("/"), word[1..].to_string(), true)
    } else {
        // Relative path - check if it contains a directory component
        if let Some(last_slash) = word.rfind('/') {
            let dir_part = &word[..last_slash];
            let query_part = &word[last_slash + 1..];

            let base_dir = cwd.join(dir_part);
            if base_dir.is_dir() {
                return (base_dir, query_part.to_string(), false);
            }
        }
        // No directory component or directory doesn't exist - search from cwd
        (cwd.to_path_buf(), word.to_string(), false)
    }
}

/// Quote a path if it contains special characters that need quoting.
///
/// Uses double quotes and escapes internal double quotes and backslashes.
pub fn quote_path_if_needed(path: &str) -> String {
    // Characters that require quoting
    let needs_quoting = path.chars().any(|c| {
        matches!(
            c,
            ' ' | '\t'
                | '"'
                | '\''
                | '\\'
                | '$'
                | '`'
                | '!'
                | '*'
                | '?'
                | '['
                | ']'
                | '{'
                | '}'
                | '('
                | ')'
                | '<'
                | '>'
                | '&'
                | '|'
                | ';'
                | '#'
        )
    });

    if !needs_quoting {
        return path.to_string();
    }

    // Use double quotes and escape special characters
    let mut quoted = String::with_capacity(path.len() + 10);
    quoted.push('"');
    for c in path.chars() {
        match c {
            '"' | '\\' | '$' | '`' => {
                quoted.push('\\');
                quoted.push(c);
            }
            _ => quoted.push(c),
        }
    }
    quoted.push('"');
    quoted
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quote_path_simple() {
        assert_eq!(quote_path_if_needed("simple.txt"), "simple.txt");
        assert_eq!(quote_path_if_needed("path/to/file.rs"), "path/to/file.rs");
    }

    #[test]
    fn test_quote_path_with_spaces() {
        assert_eq!(
            quote_path_if_needed("my file.txt"),
            "\"my file.txt\""
        );
        assert_eq!(
            quote_path_if_needed("path/to/my file.txt"),
            "\"path/to/my file.txt\""
        );
    }

    #[test]
    fn test_quote_path_with_special_chars() {
        assert_eq!(quote_path_if_needed("file$var.txt"), "\"file\\$var.txt\"");
        assert_eq!(
            quote_path_if_needed("file\"quote.txt"),
            "\"file\\\"quote.txt\""
        );
    }

    #[test]
    fn test_parse_word_absolute() {
        let cwd = PathBuf::from("/home/user");
        
        let (base, query, is_abs) = parse_word_for_search("/usr/bin/ca", &cwd);
        assert!(is_abs);
        assert_eq!(query, "ca");
        // base should be /usr/bin if it exists, otherwise /usr
    }

    #[test]
    fn test_parse_word_relative() {
        let cwd = PathBuf::from("/home/user");
        
        // When `src` dir doesn't exist, falls back to cwd with full word as query
        let (base, query, is_abs) = parse_word_for_search("src/main", &cwd);
        assert!(!is_abs);
        // Since /home/user/src doesn't exist, it falls back to cwd with full query
        assert_eq!(base, cwd);
        assert_eq!(query, "src/main");
    }

    #[test]
    fn test_parse_word_empty() {
        let cwd = PathBuf::from("/home/user");
        
        let (base, query, is_abs) = parse_word_for_search("", &cwd);
        assert!(!is_abs);
        assert_eq!(base, cwd);
        assert_eq!(query, "");
    }
}
