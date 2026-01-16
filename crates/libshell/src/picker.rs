//! Reusable picker UI component for modal selection interfaces.
//!
//! This module provides a generic picker that can be used for:
//! - CTRL+R history search
//! - Tab completions (when multiple ambiguous completions exist)
//! - Path completions
//! - And other selection interfaces
//!
//! The picker renders a list of items above the prompt, supports fuzzy filtering
//! via the input buffer, and handles keyboard navigation (up/down arrows, enter to select).

use crate::history::HistorySearchResult;

/// Maximum number of items to display in the picker
pub const MAX_VISIBLE_ITEMS: usize = 20;

/// The mode/type of picker currently active
#[derive(Debug, Clone, PartialEq)]
pub enum PickerMode {
    /// CTRL+R history search
    HistorySearch,
    /// Tab completion (when multiple ambiguous completions exist)
    TabCompletion,
}

/// An item that can be displayed in the picker
#[derive(Debug, Clone)]
pub enum PickerItem {
    /// A history search result
    History(HistorySearchResult),
    /// A tab completion option
    TabCompletion(TabCompletionItem),
}

/// A tab completion item
#[derive(Debug, Clone)]
pub struct TabCompletionItem {
    /// The text to display
    pub display_text: String,
    /// The text to insert when selected
    pub completion_text: String,
    /// Character indices that matched the query (for highlighting)
    pub match_indices: Vec<usize>,
}

/// Context for tab completion (stored when picker is in TabCompletion mode)
#[derive(Debug, Clone)]
pub struct TabCompletionContext {
    /// Start position for replacement in the input buffer
    pub replace_start: usize,
    /// End position for replacement in the input buffer
    pub replace_end: usize,
    /// The original prefix being completed (for filtering)
    pub prefix: String,
    /// All available completions (unfiltered)
    pub all_completions: Vec<TabCompletionItem>,
}

impl PickerItem {
    /// Get the display text for this item
    pub fn display_text(&self) -> &str {
        match self {
            PickerItem::History(result) => &result.entry.command,
            PickerItem::TabCompletion(item) => &item.display_text,
        }
    }

    /// Get the text to insert when this item is selected
    pub fn completion_text(&self) -> &str {
        match self {
            PickerItem::History(result) => &result.entry.command,
            PickerItem::TabCompletion(item) => &item.completion_text,
        }
    }

    /// Get the match indices for highlighting
    pub fn match_indices(&self) -> &[usize] {
        match self {
            PickerItem::History(result) => &result.match_indices,
            PickerItem::TabCompletion(item) => &item.match_indices,
        }
    }

    /// Get the status indicator (icon, is_success) if applicable
    /// Returns None if no status should be shown
    pub fn status_indicator(&self) -> Option<(char, bool)> {
        match self {
            PickerItem::History(result) => {
                result.entry.exit_code.map(|code| {
                    if code == 0 {
                        ('✓', true)
                    } else {
                        ('✗', false)
                    }
                })
            }
            PickerItem::TabCompletion(_) => None,
        }
    }
}

/// Configuration for the picker UI
#[derive(Debug, Clone)]
pub struct PickerConfig {
    /// Title displayed in the picker (e.g., "(reverse-i-search)" or "(completions)")
    pub title: String,
    /// Message shown when no items match
    pub empty_message: String,
    /// Whether to show the counter (e.g., "[1/10]")
    pub show_counter: bool,
}

impl PickerConfig {
    /// Create config for history search mode
    pub fn history_search() -> Self {
        PickerConfig {
            title: "(reverse-i-search)".to_string(),
            empty_message: "[no matches]".to_string(),
            show_counter: true,
        }
    }

    /// Create config for tab completion mode
    pub fn tab_completion() -> Self {
        PickerConfig {
            title: "(completions)".to_string(),
            empty_message: "[no completions]".to_string(),
            show_counter: true,
        }
    }

    /// Get config for a given picker mode
    pub fn for_mode(mode: &PickerMode) -> Self {
        match mode {
            PickerMode::HistorySearch => Self::history_search(),
            PickerMode::TabCompletion => Self::tab_completion(),
        }
    }
}

/// State for the picker UI
#[derive(Debug, Clone)]
pub struct PickerState {
    /// The current picker mode
    pub mode: PickerMode,
    /// Items to display
    pub items: Vec<PickerItem>,
    /// Currently selected item index (0 = first/best match)
    pub selected: usize,
    /// Number of UI lines drawn in the last render (for cleanup)
    pub ui_lines: usize,
    /// Context for tab completion (only set when mode is TabCompletion)
    pub tab_completion_ctx: Option<TabCompletionContext>,
}

impl PickerState {
    /// Create a new picker state
    pub fn new(mode: PickerMode) -> Self {
        PickerState {
            mode,
            items: Vec::new(),
            selected: 0,
            ui_lines: 0,
            tab_completion_ctx: None,
        }
    }

    /// Create a new picker state for tab completion with context
    pub fn new_tab_completion(ctx: TabCompletionContext) -> Self {
        let items: Vec<PickerItem> = ctx
            .all_completions
            .iter()
            .cloned()
            .map(PickerItem::TabCompletion)
            .collect();
        PickerState {
            mode: PickerMode::TabCompletion,
            items,
            selected: 0,
            ui_lines: 0,
            tab_completion_ctx: Some(ctx),
        }
    }

    /// Get the configuration for this picker
    pub fn config(&self) -> PickerConfig {
        PickerConfig::for_mode(&self.mode)
    }

    /// Get the currently selected item, if any
    pub fn selected_item(&self) -> Option<&PickerItem> {
        self.items.get(self.selected)
    }

    /// Move selection up (visually - higher index since items display bottom-to-top)
    pub fn move_up(&mut self) {
        if !self.items.is_empty() {
            self.selected = (self.selected + 1).min(self.items.len() - 1);
        }
    }

    /// Move selection down (visually - lower index, towards best match near prompt)
    pub fn move_down(&mut self) {
        if !self.items.is_empty() && self.selected > 0 {
            self.selected -= 1;
        }
    }

    /// Cycle to next item (wraps around)
    pub fn cycle_next(&mut self) {
        if !self.items.is_empty() {
            self.selected = (self.selected + 1) % self.items.len();
        }
    }

    /// Update items and reset selection
    pub fn set_items(&mut self, items: Vec<PickerItem>) {
        self.items = items;
        self.selected = 0;
    }
}

/// Render the picker UI above the input line
///
/// # Arguments
/// * `state` - The current picker state (will be mutated to update ui_lines)
/// * `input_buffer` - The current input/filter text
/// * `cursor_pos` - Cursor position within the input buffer
/// * `prompt` - The shell prompt string
/// * `cols` - Terminal width in columns
///
/// # Returns
/// Terminal escape sequences to render the UI
pub fn render_picker_ui(
    state: &mut PickerState,
    input_buffer: &str,
    cursor_pos: usize,
    prompt: &str,
    cols: u16,
) -> Vec<u8> {
    let mut output = Vec::new();
    let config = state.config();

    let num_results = state.items.len();
    let display_count = num_results.min(MAX_VISIBLE_ITEMS);

    // Always use fixed height for the UI to prevent prompt from jumping
    // This is MAX_VISIBLE_ITEMS (for results) + 1 (for the indicator line)
    let fixed_ui_lines = MAX_VISIBLE_ITEMS + 1;
    let prev_ui_lines = state.ui_lines;

    // Update the UI lines count immediately so subsequent renders in the same
    // batch will have the correct value
    state.ui_lines = fixed_ui_lines;

    let prompt_len = prompt.len();

    // Move cursor to column 0
    output.extend(b"\r");

    // If we previously rendered UI lines, move up past them first to get to the top
    if prev_ui_lines > 0 {
        for _ in 0..prev_ui_lines {
            output.extend(b"\x1b[A"); // Move up
        }
    }

    // Now we're at the top of the previous UI (or at the prompt if first render)
    // If this is the first render, we need to create space by printing newlines
    if prev_ui_lines == 0 && fixed_ui_lines > 0 {
        // First time: create space by printing newlines (scrolls terminal if needed)
        for _ in 0..fixed_ui_lines {
            output.extend(b"\n");
        }
        // Move back up to where UI should start
        for _ in 0..fixed_ui_lines {
            output.extend(b"\x1b[A");
        }
    }

    // Clear from cursor to end of screen
    output.extend(b"\x1b[J");

    // Calculate how many empty lines we need at the top for bottom-alignment
    let empty_lines = MAX_VISIBLE_ITEMS - display_count;

    // Draw empty lines at the top (for bottom-alignment)
    for _ in 0..empty_lines {
        output.extend(b"\x1b[K\r\n"); // Clear line and move to next
    }

    // Now draw each result line (from top to bottom)
    // Best matches should be at the bottom (closest to prompt), so display in reverse
    for i in (0..display_count).rev() {
        let item = &state.items[i];
        let is_selected = i == state.selected;

        // Selection indicator
        if is_selected {
            // Cyan background for selected line
            output.extend(b"\x1b[46m\x1b[30m"); // Cyan bg, black fg
            output.extend(b"> ");
        } else {
            output.extend(b"  ");
        }

        // Add status indicator if available (before the text)
        if let Some((icon, is_success)) = item.status_indicator() {
            if is_success {
                // Green for success
                if is_selected {
                    output.extend(format!("\x1b[32m{}\x1b[30m ", icon).as_bytes());
                } else {
                    output.extend(format!("\x1b[32m{}\x1b[0m ", icon).as_bytes());
                }
            } else {
                // Red for failure
                if is_selected {
                    output.extend(format!("\x1b[31m{}\x1b[30m ", icon).as_bytes());
                } else {
                    output.extend(format!("\x1b[31m{}\x1b[0m ", icon).as_bytes());
                }
            }
        } else {
            output.extend(b"  "); // Padding when no status
        }

        // Render the text with match highlighting
        output.extend(render_highlighted_text(
            item.display_text(),
            item.match_indices(),
            is_selected,
            cols,
        ));

        // Reset colors and clear to end of line
        output.extend(b"\x1b[0m\x1b[K\r\n");
    }

    // Draw the search indicator line
    output.extend(b"\x1b[36m"); // Cyan text
    output.extend(format!("{}`{}'", config.title, input_buffer).as_bytes());
    if num_results == 0 && !input_buffer.is_empty() {
        output.extend(format!(" {}", config.empty_message).as_bytes());
    } else if num_results > 0 && config.show_counter {
        output.extend(format!(" [{}/{}]", state.selected + 1, num_results).as_bytes());
    }
    output.extend(b"\x1b[0m\x1b[K\r\n");

    // Now redraw the prompt line
    output.extend(prompt.as_bytes());
    output.extend(input_buffer.as_bytes());
    output.extend(b"\x1b[K"); // Clear to end of line

    // Position cursor correctly
    let target_col = prompt_len + cursor_pos;
    let current_pos = prompt_len + input_buffer.len();
    if current_pos > target_col {
        output.extend(format!("\x1b[{}D", current_pos - target_col).as_bytes());
    }

    output
}

/// Render text with matched characters highlighted
fn render_highlighted_text(
    text: &str,
    match_indices: &[usize],
    is_selected: bool,
    cols: u16,
) -> Vec<u8> {
    let mut output = Vec::new();

    // Replace newlines and other control characters with visible representations
    let chars: Vec<char> = text.chars().collect();

    // Account for: 2 chars for selection indicator "  " or "> ", 2 chars for status "✓ " or "✗ ", 2 chars for ".."
    let max_display_width = (cols as usize).saturating_sub(6);
    let mut display_width = 0;
    let needs_truncation = chars.len() > max_display_width;

    for (i, &ch) in chars.iter().enumerate() {
        // Check if we need to truncate - leave room for ".."
        if needs_truncation && display_width >= max_display_width {
            output.extend(b"..");
            break;
        }

        // Determine what to display for this character
        let display_char = match ch {
            '\n' | '\r' => '\\',
            '\t' => ' ',
            c if c.is_control() => ' ',
            c => c,
        };

        if match_indices.contains(&i) {
            // Highlighted match - bold yellow
            if is_selected {
                output.extend(b"\x1b[1;33m"); // Bold yellow on cyan bg
            } else {
                output.extend(b"\x1b[1;33m"); // Bold yellow
            }
            output.extend(display_char.to_string().as_bytes());
            if is_selected {
                output.extend(b"\x1b[22;30m"); // Reset bold, back to black fg
            } else {
                output.extend(b"\x1b[22;39m"); // Reset bold and color
            }
        } else {
            output.extend(display_char.to_string().as_bytes());
        }

        display_width += 1;
    }

    output
}

/// Clear the picker UI and return escape sequences to redraw the prompt
///
/// # Arguments
/// * `ui_lines` - Number of UI lines to clear
/// * `prompt` - The shell prompt string
/// * `input_buffer` - The current input text
/// * `cursor_pos` - Cursor position within the input buffer
/// * `highlighted_input` - Pre-highlighted version of the input (for syntax highlighting)
///
/// # Returns
/// Terminal escape sequences to clear UI and redraw prompt
pub fn clear_picker_ui(
    ui_lines: usize,
    prompt: &str,
    highlighted_input: &str,
    input_buffer_len: usize,
    cursor_pos: usize,
) -> Vec<u8> {
    let mut output = Vec::new();

    // Move cursor to column 0
    output.extend(b"\r");

    // Move up to the first UI line
    for _ in 0..ui_lines {
        output.extend(b"\x1b[A");
    }

    // Clear from cursor to end of screen
    output.extend(b"\x1b[J");

    // Redraw prompt with highlighted input
    output.extend(prompt.as_bytes());
    output.extend(highlighted_input.as_bytes());

    // Position cursor correctly
    let move_back = input_buffer_len - cursor_pos;
    if move_back > 0 {
        output.extend(format!("\x1b[{}D", move_back).as_bytes());
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_picker_state_navigation() {
        let mut state = PickerState::new(PickerMode::HistorySearch);
        
        // Empty state - navigation should be safe
        state.move_up();
        state.move_down();
        state.cycle_next();
        assert_eq!(state.selected, 0);

        // Add some items
        state.set_items(vec![
            PickerItem::TabCompletion(TabCompletionItem {
                display_text: "item1".to_string(),
                completion_text: "item1".to_string(),
                match_indices: vec![],
            }),
            PickerItem::TabCompletion(TabCompletionItem {
                display_text: "item2".to_string(),
                completion_text: "item2".to_string(),
                match_indices: vec![],
            }),
            PickerItem::TabCompletion(TabCompletionItem {
                display_text: "item3".to_string(),
                completion_text: "item3".to_string(),
                match_indices: vec![],
            }),
        ]);

        assert_eq!(state.selected, 0);
        
        // Move up
        state.move_up();
        assert_eq!(state.selected, 1);
        state.move_up();
        assert_eq!(state.selected, 2);
        state.move_up(); // Should clamp at max
        assert_eq!(state.selected, 2);

        // Move down
        state.move_down();
        assert_eq!(state.selected, 1);
        state.move_down();
        assert_eq!(state.selected, 0);
        state.move_down(); // Should clamp at 0
        assert_eq!(state.selected, 0);

        // Cycle
        state.cycle_next();
        assert_eq!(state.selected, 1);
        state.cycle_next();
        assert_eq!(state.selected, 2);
        state.cycle_next(); // Should wrap
        assert_eq!(state.selected, 0);
    }

    #[test]
    fn test_picker_config() {
        let history_config = PickerConfig::history_search();
        assert_eq!(history_config.title, "(reverse-i-search)");

        let tab_config = PickerConfig::tab_completion();
        assert_eq!(tab_config.title, "(completions)");
    }

    #[test]
    fn test_tab_completion_item() {
        let item = PickerItem::TabCompletion(TabCompletionItem {
            display_text: "my_function".to_string(),
            completion_text: "my_function()".to_string(),
            match_indices: vec![0, 1, 2],
        });

        assert_eq!(item.display_text(), "my_function");
        assert_eq!(item.completion_text(), "my_function()");
        assert_eq!(item.match_indices(), &[0, 1, 2]);
        assert!(item.status_indicator().is_none());
    }

    #[test]
    fn test_new_tab_completion() {
        let completions = vec![
            TabCompletionItem {
                display_text: "cargo".to_string(),
                completion_text: "cargo".to_string(),
                match_indices: vec![],
            },
            TabCompletionItem {
                display_text: "cargo-clippy".to_string(),
                completion_text: "cargo-clippy".to_string(),
                match_indices: vec![],
            },
        ];

        let ctx = TabCompletionContext {
            replace_start: 0,
            replace_end: 4,
            prefix: "carg".to_string(),
            all_completions: completions,
        };

        let state = PickerState::new_tab_completion(ctx);
        
        assert_eq!(state.mode, PickerMode::TabCompletion);
        assert_eq!(state.items.len(), 2);
        assert_eq!(state.selected, 0);
        assert!(state.tab_completion_ctx.is_some());
        
        let ctx = state.tab_completion_ctx.as_ref().unwrap();
        assert_eq!(ctx.prefix, "carg");
        assert_eq!(ctx.replace_start, 0);
        assert_eq!(ctx.replace_end, 4);
    }
}
