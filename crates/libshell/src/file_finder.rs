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
    /// The base directory to search in (expanded path)
    pub base_dir: PathBuf,
    /// Whether the original input was an absolute path (or started with ~/$VAR)
    pub is_absolute: bool,
    /// Start position for replacement in the input buffer
    pub replace_start: usize,
    /// End position for replacement in the input buffer
    pub replace_end: usize,
    /// The original word that triggered the search
    pub original_word: String,
    /// Cached list of all files (for fast re-filtering)
    pub files: Vec<FileCacheEntry>,
    /// The original prefix to preserve (e.g., "~" or "$HOME")
    pub original_prefix: Option<String>,
    /// The expanded prefix (e.g., "/Users/name")
    pub expanded_prefix: Option<String>,
}

/// A cached file entry for fast re-filtering.
#[derive(Debug, Clone)]
pub struct FileCacheEntry {
    /// The path to display/insert (may use ~ instead of $HOME)
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
            original_prefix: None,
            expanded_prefix: None,
        }
    }

    /// Create a new file finder context with prefix information.
    ///
    /// # Arguments
    /// * `parsed` - The parsed search path with prefix info
    /// * `replace_start` - Start of the word to replace in input buffer
    /// * `replace_end` - End of the word to replace in input buffer
    /// * `original_word` - The word that was under the cursor
    pub fn from_parsed(
        parsed: ParsedSearchPath,
        replace_start: usize,
        replace_end: usize,
        original_word: String,
    ) -> Self {
        FileFinderContext {
            base_dir: parsed.base_dir,
            is_absolute: parsed.is_absolute,
            replace_start,
            replace_end,
            original_word,
            files: Vec::new(),
            original_prefix: parsed.original_prefix,
            expanded_prefix: parsed.expanded_prefix,
        }
    }

    /// Convert an expanded path back to display form using the original prefix.
    fn to_display_path(&self, expanded_path: &str) -> String {
        if let (Some(orig), Some(exp)) = (&self.original_prefix, &self.expanded_prefix) {
            if expanded_path.starts_with(exp) {
                return format!("{}{}", orig, &expanded_path[exp.len()..]);
            }
        }
        expanded_path.to_string()
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
            let expanded_path_str = if self.is_absolute {
                path.to_string_lossy().to_string()
            } else {
                path.strip_prefix(&self.base_dir)
                    .unwrap_or(path)
                    .to_string_lossy()
                    .to_string()
            };

            // Convert to display form (replaces $HOME with ~ if applicable)
            let path_str = self.to_display_path(&expanded_path_str);
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

/// Result of parsing a word for file search.
#[derive(Debug, Clone)]
pub struct ParsedSearchPath {
    /// The base directory to search in (expanded)
    pub base_dir: PathBuf,
    /// The query string (filename portion to match)
    pub query: String,
    /// Whether the original path was absolute (or started with ~)
    pub is_absolute: bool,
    /// The original prefix to preserve when constructing completion paths.
    /// For "~/Doc" this would be "~", for "$HOME/Doc" this would be "$HOME".
    /// None for regular relative/absolute paths.
    pub original_prefix: Option<String>,
    /// The expanded prefix (e.g., "/Users/name" for ~)
    pub expanded_prefix: Option<String>,
}

impl ParsedSearchPath {
    /// Convert an expanded path back to display form using the original prefix.
    ///
    /// For example, if original_prefix is "~" and expanded_prefix is "/Users/name",
    /// then "/Users/name/Documents" becomes "~/Documents".
    pub fn to_display_path(&self, expanded_path: &str) -> String {
        if let (Some(orig), Some(exp)) = (&self.original_prefix, &self.expanded_prefix) {
            if expanded_path.starts_with(exp) {
                return format!("{}{}", orig, &expanded_path[exp.len()..]);
            }
        }
        expanded_path.to_string()
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
    let parsed = parse_word_for_search_full(word, cwd);
    (parsed.base_dir, parsed.query, parsed.is_absolute)
}

/// Parse a word to extract the base directory, query, and prefix information.
///
/// This version preserves the original prefix (like ~) for path reconstruction.
///
/// # Arguments
/// * `word` - The word under the cursor
/// * `cwd` - The current working directory
///
/// # Returns
/// A ParsedSearchPath with all the information needed for completion
pub fn parse_word_for_search_full(word: &str, cwd: &Path) -> ParsedSearchPath {
    use crate::expand_path;

    if word.is_empty() {
        return ParsedSearchPath {
            base_dir: cwd.to_path_buf(),
            query: String::new(),
            is_absolute: false,
            original_prefix: None,
            expanded_prefix: None,
        };
    }

    // Check for tilde or environment variable at the start
    let (expanded_word, original_prefix, expanded_prefix) = if word.starts_with('~') {
        let expanded = expand_path(word);
        let home = std::env::var("HOME").unwrap_or_default();
        (
            expanded.to_string_lossy().to_string(),
            Some("~".to_string()),
            Some(home),
        )
    } else if word.starts_with('$') {
        let expanded = expand_path(word);
        let expanded_str = expanded.to_string_lossy().to_string();
        // Extract the variable name for the prefix
        let var_end = word[1..]
            .chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '{' || *c == '}')
            .count()
            + 1;
        let var_part = &word[..var_end];
        // Get the expanded value of just the variable
        let var_expanded = expand_path(var_part);
        (
            expanded_str,
            Some(var_part.to_string()),
            Some(var_expanded.to_string_lossy().to_string()),
        )
    } else {
        (word.to_string(), None, None)
    };

    let is_absolute = expanded_word.starts_with('/');

    if is_absolute {
        // Find the last '/' to split into directory and query
        if let Some(last_slash) = expanded_word.rfind('/') {
            let dir_part = &expanded_word[..=last_slash];
            let query_part = &expanded_word[last_slash + 1..];

            let base_dir = PathBuf::from(dir_part);
            if base_dir.is_dir() {
                return ParsedSearchPath {
                    base_dir,
                    query: query_part.to_string(),
                    is_absolute: true,
                    original_prefix,
                    expanded_prefix,
                };
            } else {
                // Directory doesn't exist, try parent
                let parent = PathBuf::from(&expanded_word[..last_slash]);
                if parent.is_dir() || last_slash == 0 {
                    let search_base = if last_slash == 0 {
                        PathBuf::from("/")
                    } else {
                        parent
                    };
                    return ParsedSearchPath {
                        base_dir: search_base,
                        query: query_part.to_string(),
                        is_absolute: true,
                        original_prefix,
                        expanded_prefix,
                    };
                }
            }
        }
        // Fallback: search from root with the word as query
        ParsedSearchPath {
            base_dir: PathBuf::from("/"),
            query: expanded_word[1..].to_string(),
            is_absolute: true,
            original_prefix,
            expanded_prefix,
        }
    } else {
        // Relative path - check if it contains a directory component
        if let Some(last_slash) = expanded_word.rfind('/') {
            let dir_part = &expanded_word[..last_slash];
            let query_part = &expanded_word[last_slash + 1..];

            let base_dir = cwd.join(dir_part);
            if base_dir.is_dir() {
                return ParsedSearchPath {
                    base_dir,
                    query: query_part.to_string(),
                    is_absolute: false,
                    original_prefix,
                    expanded_prefix,
                };
            }
        }
        // No directory component or directory doesn't exist - search from cwd
        ParsedSearchPath {
            base_dir: cwd.to_path_buf(),
            query: expanded_word,
            is_absolute: false,
            original_prefix,
            expanded_prefix,
        }
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
        assert_eq!(quote_path_if_needed("my file.txt"), "\"my file.txt\"");
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
