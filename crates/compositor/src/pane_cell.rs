use crate::border::{get_border_char, BorderDirections};
use crate::error::CompositorError;
use crate::pane::{CtrlCResult, Pane, PaneInputResult};
use crate::types::{Direction, SplitDirection};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MouseTarget {
    pub local_x: usize,
    pub local_y: usize,
    pub mode: emulator::MouseMode,
}

/// A cell in the pane tree, which can be a single pane or a split.
pub struct PaneCell {
    pub inner: PaneCellInner,
    pub width: usize,
    pub height: usize,
    pub pos_x: usize,
    pub pos_y: usize,
    pub focus: bool,
}

/// The inner content of a pane cell.
pub enum PaneCellInner {
    Pane(Pane),
    VSplit(Vec<PaneCell>),
    HSplit(Vec<PaneCell>),
}

impl PaneCell {
    fn contains_position(&self, x: usize, y: usize) -> bool {
        x >= self.pos_x
            && x < self.pos_x.saturating_add(self.width)
            && y >= self.pos_y
            && y < self.pos_y.saturating_add(self.height)
    }

    /// Handle keyboard input by routing it to the focused pane.
    /// Returns a `PaneInputResult` indicating what action should be taken.
    pub fn handle_input(&mut self, input: &[u8]) -> PaneInputResult {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => pane.handle_input(input),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.handle_input(input);
                    }
                }
                PaneInputResult::None
            }
        }
    }

    /// Collect PTY file descriptors for polling.
    /// Only collects fds for panes that have active subprocesses.
    pub fn collect_poll_fds(
        &mut self,
        fds: &mut Vec<libc::pollfd>,
        pane_map: &mut Vec<Option<*mut Pane>>,
    ) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                // Only poll if there's an active subprocess
                if let Some(fd) = pane.subprocess_fd() {
                    fds.push(libc::pollfd {
                        fd,
                        events: libc::POLLIN,
                        revents: 0,
                    });
                    pane_map.push(Some(pane as *mut Pane));
                }
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    cell.collect_poll_fds(fds, pane_map);
                }
            }
        }
    }

    /// Check if this cell or any of its children have focus.
    pub fn has_focus(&self) -> bool {
        self.focus
    }

    /// Get the dimensions of this cell.
    pub fn dimensions(&self) -> (usize, usize) {
        (self.width, self.height)
    }

    /// Get the position of this cell.
    pub fn position(&self) -> (usize, usize) {
        (self.pos_x, self.pos_y)
    }

    /// Get a reference to the inner content.
    pub fn inner(&self) -> &PaneCellInner {
        &self.inner
    }

    /// Get a mutable reference to the focused pane.
    pub fn get_focused_pane_mut(&mut self) -> Option<&mut Pane> {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => Some(pane),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.get_focused_pane_mut();
                    }
                }
                None
            }
        }
    }

    pub fn focused_mouse_mode(&self) -> emulator::MouseMode {
        if !self.focus {
            return emulator::MouseMode::default();
        }

        match &self.inner {
            PaneCellInner::Pane(pane) => pane.mouse_mode(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => cells
                .iter()
                .find(|cell| cell.focus)
                .map(|cell| cell.focused_mouse_mode())
                .unwrap_or_default(),
        }
    }

    pub fn mouse_target_info(&self, x: usize, y: usize) -> Option<MouseTarget> {
        if !self.contains_position(x, y) {
            return None;
        }

        match &self.inner {
            PaneCellInner::Pane(pane) => Some(MouseTarget {
                local_x: x - self.pos_x,
                local_y: y - self.pos_y,
                mode: pane.mouse_mode(),
            }),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                cells.iter().find_map(|cell| cell.mouse_target_info(x, y))
            }
        }
    }

    pub fn focus_pane_at(&mut self, x: usize, y: usize) -> bool {
        if self.mouse_target_info(x, y).is_none() {
            return false;
        }

        self.clear_focus();
        self.set_focus_path_at(x, y)
    }

    fn set_focus_path_at(&mut self, x: usize, y: usize) -> bool {
        if !self.contains_position(x, y) {
            return false;
        }

        match &mut self.inner {
            PaneCellInner::Pane(_) => {
                self.focus = true;
                true
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                if let Some(cell) = cells.iter_mut().find(|cell| cell.contains_position(x, y)) {
                    if cell.set_focus_path_at(x, y) {
                        self.focus = true;
                        return true;
                    }
                }
                false
            }
        }
    }

    /// Get a mutable reference to the focused PaneCell (the leaf cell containing the focused pane).
    pub fn get_focused_cell_mut(&mut self) -> Option<&mut PaneCell> {
        if !self.focus {
            return None;
        }
        // Check if this is a leaf pane - if so, return self
        // Otherwise, recurse into children
        let is_pane = matches!(&self.inner, PaneCellInner::Pane(_));
        if is_pane {
            return Some(self);
        }

        match &mut self.inner {
            PaneCellInner::Pane(_) => unreachable!(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.get_focused_cell_mut();
                    }
                }
                None
            }
        }
    }

    /// Handle CTRL+C on the focused pane.
    ///
    /// Returns the result of the CTRL+C action. The caller should handle
    /// `CtrlCResult::ClosePane` by calling `close_focused_pane`.
    pub fn handle_ctrl_c(&mut self) -> CtrlCResult {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => pane.handle_ctrl_c(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.handle_ctrl_c();
                    }
                }
                // No focused cell found, shouldn't happen but treat as close
                CtrlCResult::ClosePane
            }
        }
    }

    /// Handle CTRL+D on the focused pane (same as CTRL+C but sends EOF instead of SIGINT).
    pub fn handle_ctrl_d(&mut self) -> CtrlCResult {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => pane.handle_ctrl_d(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.handle_ctrl_d();
                    }
                }
                CtrlCResult::ClosePane
            }
        }
    }

    /// Close the focused pane, returning true if there are still panes remaining.
    ///
    /// If this cell is a single pane with focus, returns false (caller should remove this cell).
    /// If this cell is a split, removes the focused child and restructures if needed.
    /// Returns true if the cell still has content after closing, false if the entire cell should be removed.
    pub fn close_focused_pane(&mut self) -> bool {
        match &mut self.inner {
            PaneCellInner::Pane(_) => {
                // This is a leaf pane with focus - it needs to be removed by the parent
                false
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                // Find the focused cell
                let focused_idx = cells.iter().position(|c| c.focus);

                if let Some(idx) = focused_idx {
                    // Check if the focused child is a leaf or a split
                    let is_leaf = matches!(cells[idx].inner, PaneCellInner::Pane(_));

                    if is_leaf {
                        // Remove the focused pane
                        cells.remove(idx);

                        if cells.is_empty() {
                            // No more children - this cell should be removed
                            return false;
                        }

                        if cells.len() == 1 {
                            // Only one child left - collapse the split
                            let remaining = cells.remove(0);
                            self.inner = remaining.inner;
                            self.focus = true;
                            // Set focus on the first pane in the remaining structure
                            self.set_focus_first();
                            // Recalculate dimensions
                            self.recalculate_layout();
                        } else {
                            // Multiple children remain - give focus to the next pane
                            let new_focus_idx = if idx >= cells.len() {
                                cells.len() - 1
                            } else {
                                idx
                            };
                            cells[new_focus_idx].focus = true;
                            cells[new_focus_idx].set_focus_first();
                            // Recalculate dimensions
                            self.recalculate_layout();
                        }
                        true
                    } else {
                        // Focused cell is a split - recursively close
                        if !cells[idx].close_focused_pane() {
                            // Child wants to be removed
                            cells.remove(idx);

                            if cells.is_empty() {
                                return false;
                            }

                            if cells.len() == 1 {
                                // Only one child left - collapse the split
                                let remaining = cells.remove(0);
                                self.inner = remaining.inner;
                                self.focus = true;
                                self.set_focus_first();
                                self.recalculate_layout();
                            } else {
                                let new_focus_idx = if idx >= cells.len() {
                                    cells.len() - 1
                                } else {
                                    idx
                                };
                                cells[new_focus_idx].focus = true;
                                cells[new_focus_idx].set_focus_first();
                                self.recalculate_layout();
                            }
                        }
                        true
                    }
                } else {
                    // No focused cell found
                    true
                }
            }
        }
    }

    /// Count the total number of panes in this cell tree.
    pub fn pane_count(&self) -> usize {
        match &self.inner {
            PaneCellInner::Pane(_) => 1,
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                cells.iter().map(|c| c.pane_count()).sum()
            }
        }
    }

    /// Recalculate layout after a pane is closed.
    fn recalculate_layout(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                // Resize the terminal emulator to match the cell dimensions
                pane.terminal_emulator.resize(self.width, self.height);
                pane.shell.resize(self.width as u16, self.height as u16);
                if let Some(ref proc) = pane.subprocess {
                    let _ = proc.resize(self.width as u16, self.height as u16);
                }
                if let Some(remote) = pane.remote.as_mut() {
                    let _ = remote.resize(self.width as u16, self.height as u16);
                }
            }
            PaneCellInner::VSplit(cells) => {
                // Vertical split - divide width evenly, accounting for borders
                let num_cells = cells.len();
                if num_cells == 0 {
                    return;
                }

                // Total borders needed = num_cells - 1
                let total_border_width = num_cells.saturating_sub(1);
                let available_width = self.width.saturating_sub(total_border_width);
                let base_width = available_width / num_cells;
                let mut extra = available_width % num_cells;

                let mut current_x = self.pos_x;
                for cell in cells.iter_mut() {
                    let cell_width = base_width
                        + if extra > 0 {
                            extra -= 1;
                            1
                        } else {
                            0
                        };
                    cell.pos_x = current_x;
                    cell.pos_y = self.pos_y;
                    cell.width = cell_width;
                    cell.height = self.height;
                    cell.recalculate_layout();
                    current_x += cell_width + 1; // +1 for border
                }
            }
            PaneCellInner::HSplit(cells) => {
                // Horizontal split - divide height evenly, accounting for borders
                let num_cells = cells.len();
                if num_cells == 0 {
                    return;
                }

                // Total borders needed = num_cells - 1
                let total_border_height = num_cells.saturating_sub(1);
                let available_height = self.height.saturating_sub(total_border_height);
                let base_height = available_height / num_cells;
                let mut extra = available_height % num_cells;

                let mut current_y = self.pos_y;
                for cell in cells.iter_mut() {
                    let cell_height = base_height
                        + if extra > 0 {
                            extra -= 1;
                            1
                        } else {
                            0
                        };
                    cell.pos_x = self.pos_x;
                    cell.pos_y = current_y;
                    cell.width = self.width;
                    cell.height = cell_height;
                    cell.recalculate_layout();
                    current_y += cell_height + 1; // +1 for border
                }
            }
        }
    }

    /// Enter scrollback mode on the focused pane
    pub fn enter_scrollback_mode(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.enter_scrollback_mode();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.enter_scrollback_mode();
                    }
                }
            }
        }
    }

    /// Exit scrollback mode on the focused pane
    pub fn exit_scrollback_mode(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.exit_scrollback_mode();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.exit_scrollback_mode();
                    }
                }
            }
        }
    }

    /// Check if the focused pane is in scrollback mode
    pub fn is_in_scrollback_mode(&self) -> bool {
        match &self.inner {
            PaneCellInner::Pane(pane) => pane.is_in_scrollback_mode(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.is_in_scrollback_mode();
                    }
                }
                false
            }
        }
    }

    /// Scroll the focused pane up by the given number of lines
    pub fn scroll_up(&mut self, lines: usize) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.scroll_up(lines);
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.scroll_up(lines);
                    }
                }
            }
        }
    }

    /// Scroll the focused pane down by the given number of lines
    pub fn scroll_down(&mut self, lines: usize) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.scroll_down(lines);
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.scroll_down(lines);
                    }
                }
            }
        }
    }

    /// Get scrollback info from the focused pane (scroll_offset, scrollback_len)
    pub fn get_scrollback_info(&self) -> Option<(usize, usize)> {
        match &self.inner {
            PaneCellInner::Pane(pane) => Some((pane.scroll_offset(), pane.scrollback_len())),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.get_scrollback_info();
                    }
                }
                None
            }
        }
    }

    /// Handle vim input on the focused pane (for scrollback mode navigation)
    pub fn handle_vim_input(&mut self, input: &[u8]) -> bool {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => pane.handle_vim_input(input),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.handle_vim_input(input);
                    }
                }
                false
            }
        }
    }

    /// Get vim cursor info from the focused pane
    pub fn get_vim_cursor_info(&self) -> Option<(usize, usize, bool)> {
        match &self.inner {
            PaneCellInner::Pane(pane) => pane.get_vim_cursor_info(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.get_vim_cursor_info();
                    }
                }
                None
            }
        }
    }

    /// Get vim selection info from the focused pane
    pub fn get_vim_selection_info(
        &self,
    ) -> Option<(libvim::Position, libvim::Position, libvim::Mode)> {
        match &self.inner {
            PaneCellInner::Pane(pane) => pane.get_vim_selection_info(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.get_vim_selection_info();
                    }
                }
                None
            }
        }
    }

    /// Get vim mode from the focused pane
    pub fn get_vim_mode(&self) -> libvim::Mode {
        match &self.inner {
            PaneCellInner::Pane(pane) => pane.get_vim_mode(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.get_vim_mode();
                    }
                }
                libvim::Mode::Normal
            }
        }
    }

    /// Get the selected text from the focused pane (if in visual mode)
    pub fn get_selected_text(&self) -> Option<String> {
        match &self.inner {
            PaneCellInner::Pane(pane) => pane.get_selected_text(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.get_selected_text();
                    }
                }
                None
            }
        }
    }

    /// Enter search mode on the focused pane
    pub fn enter_search_mode(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.enter_search_mode();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.enter_search_mode();
                    }
                }
            }
        }
    }

    /// Exit search mode on the focused pane
    pub fn exit_search_mode(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.exit_search_mode();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.exit_search_mode();
                    }
                }
            }
        }
    }

    /// Check if the focused pane is in search mode
    pub fn is_in_search_mode(&self) -> bool {
        match &self.inner {
            PaneCellInner::Pane(pane) => pane.is_in_search_mode(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.is_in_search_mode();
                    }
                }
                false
            }
        }
    }

    /// Check if the focused pane's search input is focused
    pub fn is_search_input_focused(&self) -> bool {
        match &self.inner {
            PaneCellInner::Pane(pane) => pane.is_search_input_focused(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.is_search_input_focused();
                    }
                }
                false
            }
        }
    }

    /// Focus the search input in the focused pane
    pub fn focus_search_input(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.focus_search_input();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.focus_search_input();
                    }
                }
            }
        }
    }

    /// Unfocus the search input in the focused pane
    pub fn unfocus_search_input(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.unfocus_search_input();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.unfocus_search_input();
                    }
                }
            }
        }
    }

    /// Input a character to the search query
    pub fn search_input_char(&mut self, c: char) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.search_input_char(c);
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.search_input_char(c);
                    }
                }
            }
        }
    }

    /// Handle backspace in search query
    pub fn search_input_backspace(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.search_input_backspace();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.search_input_backspace();
                    }
                }
            }
        }
    }

    /// Clear the search query
    pub fn search_clear(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.search_clear();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.search_clear();
                    }
                }
            }
        }
    }

    /// Jump to next match
    pub fn next_match(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.next_match();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.next_match();
                    }
                }
            }
        }
    }

    /// Jump to previous match
    pub fn prev_match(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.prev_match();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.prev_match();
                    }
                }
            }
        }
    }

    /// Get search info from the focused pane
    pub fn get_search_info(&self) -> Option<(String, Option<usize>, usize)> {
        match &self.inner {
            PaneCellInner::Pane(pane) => {
                let (current, total) = pane.search_match_info();
                Some((pane.search_query().to_string(), current, total))
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.get_search_info();
                    }
                }
                None
            }
        }
    }

    /// Enter URL mode on the focused pane
    pub fn enter_url_mode(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.enter_url_mode();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.enter_url_mode();
                    }
                }
            }
        }
    }

    /// Exit URL mode on the focused pane
    pub fn exit_url_mode(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.exit_url_mode();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.exit_url_mode();
                    }
                }
            }
        }
    }

    /// Check if the focused pane is in URL mode
    pub fn is_in_url_mode(&self) -> bool {
        match &self.inner {
            PaneCellInner::Pane(pane) => pane.is_in_url_mode(),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.is_in_url_mode();
                    }
                }
                false
            }
        }
    }

    /// Jump to next URL
    pub fn next_url(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.next_url();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.next_url();
                    }
                }
            }
        }
    }

    /// Jump to previous URL
    pub fn prev_url(&mut self) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                pane.prev_url();
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.prev_url();
                    }
                }
            }
        }
    }

    /// Get URL info from the focused pane (current index, total count)
    pub fn get_url_info(&self) -> Option<(Option<usize>, usize)> {
        match &self.inner {
            PaneCellInner::Pane(pane) => {
                let (current, total) = pane.url_match_info();
                Some((current, total))
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.get_url_info();
                    }
                }
                None
            }
        }
    }

    /// Get the currently selected URL from the focused pane
    pub fn get_current_url(&self) -> Option<String> {
        match &self.inner {
            PaneCellInner::Pane(pane) => pane.get_current_url().map(|s| s.to_string()),
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        return cell.get_current_url();
                    }
                }
                None
            }
        }
    }

    /// Move focus in the specified direction.
    ///
    /// Returns true if focus was successfully moved, false otherwise.
    pub fn move_focus(&mut self, direction: Direction) -> bool {
        match &mut self.inner {
            PaneCellInner::Pane(_) => {
                // Single pane, cannot move focus
                false
            }
            PaneCellInner::VSplit(cells) => {
                // VSplit arranges panes horizontally (left to right)
                // Left/Right moves between siblings, Up/Down goes into children
                let focused_idx = cells.iter().position(|c| c.focus);

                match (direction, focused_idx) {
                    (Direction::Left, Some(idx)) if idx > 0 => {
                        // Move focus to the left sibling
                        cells[idx].clear_focus();
                        cells[idx - 1].set_focus_first();
                        true
                    }
                    (Direction::Right, Some(idx)) if idx < cells.len() - 1 => {
                        // Move focus to the right sibling
                        cells[idx].clear_focus();
                        cells[idx + 1].set_focus_first();
                        true
                    }
                    (_, Some(idx)) => {
                        // Try to move focus within the focused child
                        cells[idx].move_focus(direction)
                    }
                    _ => false,
                }
            }
            PaneCellInner::HSplit(cells) => {
                // HSplit arranges panes vertically (top to bottom)
                // Up/Down moves between siblings, Left/Right goes into children
                let focused_idx = cells.iter().position(|c| c.focus);

                match (direction, focused_idx) {
                    (Direction::Up, Some(idx)) if idx > 0 => {
                        // Move focus to the upper sibling
                        cells[idx].clear_focus();
                        cells[idx - 1].set_focus_first();
                        true
                    }
                    (Direction::Down, Some(idx)) if idx < cells.len() - 1 => {
                        // Move focus to the lower sibling
                        cells[idx].clear_focus();
                        cells[idx + 1].set_focus_first();
                        true
                    }
                    (_, Some(idx)) => {
                        // Try to move focus within the focused child
                        cells[idx].move_focus(direction)
                    }
                    _ => false,
                }
            }
        }
    }

    /// Clear focus on this cell and all children.
    pub fn clear_focus(&mut self) {
        self.focus = false;
        match &mut self.inner {
            PaneCellInner::Pane(_) => {}
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    cell.clear_focus();
                }
            }
        }
    }

    /// Set focus on the first (or leftmost/topmost) pane in this cell.
    pub fn set_focus_first(&mut self) {
        self.focus = true;
        match &mut self.inner {
            PaneCellInner::Pane(_) => {}
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                if let Some(first) = cells.first_mut() {
                    first.set_focus_first();
                }
            }
        }
    }

    /// Split the focused pane in the given direction.
    ///
    /// If this cell is a pane with focus, it will be converted into a split.
    /// If this cell is a split, it will recursively find the focused pane and split it.
    pub fn split_focused(&mut self, direction: SplitDirection) -> Result<(), CompositorError> {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                if !self.focus {
                    return Ok(());
                }

                // Calculate dimensions accounting for border (1 cell between panes)
                let (old_width, old_height, new_width, new_height, new_pos_x, new_pos_y) =
                    match direction {
                        SplitDirection::Horizontal => {
                            // Split top/bottom: each pane gets half the height minus border
                            // Total height = top_height + 1 (border) + bottom_height
                            let available_height = self.height.saturating_sub(1); // Reserve 1 for border
                            let top_height = available_height / 2;
                            let bottom_height = available_height - top_height;
                            (
                                self.width,
                                top_height,
                                self.width,
                                bottom_height,
                                self.pos_x,
                                self.pos_y + top_height + 1, // +1 for border
                            )
                        }
                        SplitDirection::Vertical => {
                            // Split left/right: each pane gets half the width minus border
                            // Total width = left_width + 1 (border) + right_width
                            let available_width = self.width.saturating_sub(1); // Reserve 1 for border
                            let left_width = available_width / 2;
                            let right_width = available_width - left_width;
                            (
                                left_width,
                                self.height,
                                right_width,
                                self.height,
                                self.pos_x + left_width + 1, // +1 for border
                                self.pos_y,
                            )
                        }
                    };

                // Create the existing pane cell with updated dimensions
                pane.terminal_emulator.resize(old_width, old_height);
                if let Some(ref proc) = pane.subprocess {
                    let _ = proc.resize(old_width as u16, old_height as u16);
                }
                if let Some(remote) = pane.remote.as_mut() {
                    let _ = remote.resize(old_width as u16, old_height as u16);
                }
                pane.shell.resize(old_width as u16, old_height as u16);

                // Create a new pane with a new shell, starting in the same
                // working directory as the pane being split.
                let split_cwd = pane.shell.cwd().to_path_buf();
                let new_pane = Pane::new_in(new_width, new_height, split_cwd);

                // Take ownership of the old pane - use a temporary placeholder
                let placeholder_pane = Pane::new(1, 1);
                let old_inner =
                    std::mem::replace(&mut self.inner, PaneCellInner::Pane(placeholder_pane));

                let old_pane = match old_inner {
                    PaneCellInner::Pane(p) => p,
                    _ => unreachable!(),
                };

                // Create the two child cells
                let first_cell = PaneCell {
                    inner: PaneCellInner::Pane(old_pane),
                    width: old_width,
                    height: old_height,
                    pos_x: self.pos_x,
                    pos_y: self.pos_y,
                    focus: false, // Original pane loses focus
                };

                let second_cell = PaneCell {
                    inner: PaneCellInner::Pane(new_pane),
                    width: new_width,
                    height: new_height,
                    pos_x: new_pos_x,
                    pos_y: new_pos_y,
                    focus: true, // New pane gets focus
                };

                // Replace the inner content with a split
                self.inner = match direction {
                    SplitDirection::Horizontal => {
                        PaneCellInner::HSplit(vec![first_cell, second_cell])
                    }
                    SplitDirection::Vertical => {
                        PaneCellInner::VSplit(vec![first_cell, second_cell])
                    }
                };

                Ok(())
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                // Find the focused child and split it
                for cell in cells {
                    if cell.focus {
                        return cell.split_focused(direction);
                    }
                }
                Ok(())
            }
        }
    }

    /// Get mutable access to a child pane by index.
    pub fn get_child_mut(&mut self, index: usize) -> Option<&mut PaneCell> {
        match &mut self.inner {
            PaneCellInner::Pane(_) => None,
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => cells.get_mut(index),
        }
    }

    /// Composite this pane cell into the global emulator.
    ///
    /// Recursively traverses all child panes and blits their contents
    /// into the destination emulator at the appropriate positions.
    pub fn composite_into(&self, dest: &mut emulator::TerminalEmulator) {
        match &self.inner {
            PaneCellInner::Pane(pane) => {
                let grid = pane.terminal_emulator.grid();
                let scrollback_len = grid.scrollback_len();

                // Build a helper to check if a position matches a search result
                let is_match = |line_index: isize, col: usize| -> Option<bool> {
                    if !pane.search_mode || pane.search_matches.is_empty() {
                        return None;
                    }
                    for (i, m) in pane.search_matches.iter().enumerate() {
                        if m.line_index == line_index && col >= m.start_col && col < m.end_col {
                            let is_current = Some(i) == pane.current_match_index;
                            return Some(is_current);
                        }
                    }
                    None
                };

                // Build a helper to check if a position matches a URL
                let is_url_match = |line_index: isize, col: usize| -> Option<bool> {
                    if !pane.url_mode || pane.url_matches.is_empty() {
                        return None;
                    }
                    for (i, m) in pane.url_matches.iter().enumerate() {
                        // `contains` handles URLs that span multiple wrapped rows.
                        if m.contains(line_index, col) {
                            let is_current = Some(i) == pane.current_url_index;
                            return Some(is_current);
                        }
                    }
                    None
                };

                // Build a helper to check if a position is in vim visual selection
                // Takes absolute line number (scrollback + grid)
                let is_in_selection = |abs_line: usize, col: usize| -> bool {
                    if !pane.scrollback_mode || pane.vim_engine.mode == libvim::Mode::Normal {
                        return false;
                    }
                    let start = pane.vim_engine.selection_start;
                    let end = pane.vim_engine.selection_end;

                    match pane.vim_engine.mode {
                        libvim::Mode::Visual => {
                            // Character-wise selection
                            if abs_line < start.row || abs_line > end.row {
                                return false;
                            }
                            if abs_line == start.row && abs_line == end.row {
                                // Same line: check column range
                                col >= start.col && col <= end.col
                            } else if abs_line == start.row {
                                // First line: from start.col to end
                                col >= start.col
                            } else if abs_line == end.row {
                                // Last line: from start to end.col
                                col <= end.col
                            } else {
                                // Middle lines: entire line
                                true
                            }
                        }
                        libvim::Mode::VisualLine => {
                            // Line-wise selection
                            abs_line >= start.row && abs_line <= end.row
                        }
                        libvim::Mode::Normal => false,
                    }
                };

                // Get vim cursor position for block cursor rendering
                let vim_cursor_viewport = pane.get_vim_cursor_info();

                if pane.scrollback_mode {
                    // In scrollback mode - composite with vim cursor and selection
                    for row in 0..self.height {
                        let dst_y = self.pos_y + row;

                        // Calculate which absolute line to display
                        // scroll_offset = number of scrollback lines shown at top
                        // first_visible = scrollback_len - scroll_offset
                        let first_visible_line = scrollback_len.saturating_sub(pane.scroll_offset);
                        let abs_line = first_visible_line + row;

                        // Determine source of this line
                        let source_line = if abs_line < scrollback_len {
                            // Scrollback line
                            let line_index = (abs_line as isize) - (scrollback_len as isize);
                            Some((true, abs_line, line_index))
                        } else {
                            // Grid line
                            let grid_row = abs_line - scrollback_len;
                            if grid_row < grid.rows {
                                Some((false, grid_row, grid_row as isize))
                            } else {
                                None
                            }
                        };

                        if let Some((is_scrollback, line_idx, line_index)) = source_line {
                            let source_row = if is_scrollback {
                                grid.get_scrollback_row(line_idx)
                            } else {
                                grid.get_row(line_idx)
                            };

                            if let Some(cells) = source_row {
                                for col in 0..self.width {
                                    let dst_x = self.pos_x + col;
                                    let (dest_cols, dest_rows) = dest.dimensions();
                                    if dst_x < dest_cols && dst_y < dest_rows {
                                        let mut cell = if col < cells.len() {
                                            cells[col].clone()
                                        } else {
                                            emulator::Cell::new(
                                                ' ',
                                                emulator::CellAttributes::default(),
                                            )
                                        };

                                        // Apply search highlighting first (takes priority)
                                        if let Some(is_current) = is_match(line_index, col) {
                                            if is_current {
                                                // Current match: bright magenta background, white text, bold
                                                cell.attrs.bg_color =
                                                    Some(emulator::Color::Magenta);
                                                cell.attrs.fg_color = Some(emulator::Color::White);
                                                cell.attrs.bold = true;
                                            } else {
                                                // Other matches: yellow background
                                                cell.attrs.bg_color = Some(emulator::Color::Yellow);
                                                cell.attrs.fg_color = Some(emulator::Color::Black);
                                            }
                                        } else if let Some(is_current) =
                                            is_url_match(line_index, col)
                                        {
                                            // URL highlighting
                                            if is_current {
                                                // Current URL: cyan background, black text, bold, underline
                                                cell.attrs.bg_color = Some(emulator::Color::Cyan);
                                                cell.attrs.fg_color = Some(emulator::Color::Black);
                                                cell.attrs.bold = true;
                                                cell.attrs.underline = true;
                                            } else {
                                                // Other URLs: blue text, underline
                                                cell.attrs.fg_color = Some(emulator::Color::Blue);
                                                cell.attrs.underline = true;
                                            }
                                        } else if is_in_selection(abs_line, col) {
                                            // Visual selection: inverse video
                                            cell.attrs.inverse = true;
                                        }

                                        // Apply block cursor (vim cursor in scrollback mode)
                                        if let Some((cursor_col, cursor_row, visible)) =
                                            vim_cursor_viewport
                                        {
                                            if visible && row == cursor_row && col == cursor_col {
                                                // Block cursor: inverse video
                                                cell.attrs.inverse = !cell.attrs.inverse;
                                            }
                                        }

                                        dest.grid_mut().set_cell(dst_x, dst_y, cell);
                                    }
                                }
                            }
                        }
                    }
                } else {
                    // Normal mode - blit normally
                    dest.blit_from(
                        &pane.terminal_emulator,
                        0,           // source x (from origin of pane's emulator)
                        0,           // source y
                        self.pos_x,  // destination x (pane's position in compositor)
                        self.pos_y,  // destination y
                        self.width,  // width to copy
                        self.height, // height to copy
                    );
                }
            }
            PaneCellInner::VSplit(cells) => {
                // Recursively composite all child panes
                for cell in cells {
                    cell.composite_into(dest);
                }
                // Draw vertical borders between panes
                self.draw_vsplit_borders(cells, dest);
            }
            PaneCellInner::HSplit(cells) => {
                // Recursively composite all child panes
                for cell in cells {
                    cell.composite_into(dest);
                }
                // Draw horizontal borders between panes
                self.draw_hsplit_borders(cells, dest);
            }
        }
    }

    /// Draw vertical borders between VSplit panes.
    fn draw_vsplit_borders(&self, cells: &[PaneCell], dest: &mut emulator::TerminalEmulator) {
        for i in 0..cells.len().saturating_sub(1) {
            // Border is positioned right after the current cell
            let border_x = cells[i].pos_x + cells[i].width;

            // Draw vertical line for the height of this split
            for y in self.pos_y..(self.pos_y + self.height) {
                let (cols, rows) = (dest.grid().cols, dest.grid().rows);
                if border_x < cols && y < rows {
                    let existing = dest.grid().get_cell(border_x, y).character;
                    let border_char = get_border_char(existing, '│');
                    dest.grid_mut().set_cell(
                        border_x,
                        y,
                        emulator::Cell::new(border_char, emulator::CellAttributes::default()),
                    );
                }
            }

            // Check the row above for a horizontal border and add DOWN direction to create T-junction
            if self.pos_y > 0 {
                let y_above = self.pos_y - 1;
                let (cols, rows) = (dest.grid().cols, dest.grid().rows);
                if border_x < cols && y_above < rows {
                    let existing = dest.grid().get_cell(border_x, y_above).character;
                    let existing_dirs = BorderDirections::from_char(existing);
                    // If there's a horizontal border above (has left+right), add down direction
                    if existing_dirs.left && existing_dirs.right {
                        let new_dirs = existing_dirs.merge(BorderDirections {
                            up: false,
                            down: true,
                            left: false,
                            right: false,
                        });
                        dest.grid_mut().set_cell(
                            border_x,
                            y_above,
                            emulator::Cell::new(
                                new_dirs.to_char(),
                                emulator::CellAttributes::default(),
                            ),
                        );
                    }
                }
            }

            // Check the column to the right for horizontal borders and add LEFT direction
            let x_right = border_x + 1;
            let (cols, rows) = (dest.grid().cols, dest.grid().rows);
            if x_right < cols {
                for y in self.pos_y..(self.pos_y + self.height) {
                    if y < rows {
                        let right_char = dest.grid().get_cell(x_right, y).character;
                        let right_dirs = BorderDirections::from_char(right_char);
                        // If there's a horizontal border to the right (has left+right), add right direction to current
                        if right_dirs.left && right_dirs.right {
                            let current = dest.grid().get_cell(border_x, y).character;
                            let current_dirs = BorderDirections::from_char(current);
                            let new_dirs = current_dirs.merge(BorderDirections {
                                up: false,
                                down: false,
                                left: false,
                                right: true,
                            });
                            dest.grid_mut().set_cell(
                                border_x,
                                y,
                                emulator::Cell::new(
                                    new_dirs.to_char(),
                                    emulator::CellAttributes::default(),
                                ),
                            );
                        }
                    }
                }
            }

            // Check the column to the left for horizontal borders and add RIGHT direction
            if border_x > 0 {
                let x_left = border_x - 1;
                let (cols, rows) = (dest.grid().cols, dest.grid().rows);
                if x_left < cols {
                    for y in self.pos_y..(self.pos_y + self.height) {
                        if y < rows {
                            let left_char = dest.grid().get_cell(x_left, y).character;
                            let left_dirs = BorderDirections::from_char(left_char);
                            // If there's a horizontal border to the left (has left+right), add left direction to current
                            if left_dirs.left && left_dirs.right {
                                let current = dest.grid().get_cell(border_x, y).character;
                                let current_dirs = BorderDirections::from_char(current);
                                let new_dirs = current_dirs.merge(BorderDirections {
                                    up: false,
                                    down: false,
                                    left: true,
                                    right: false,
                                });
                                dest.grid_mut().set_cell(
                                    border_x,
                                    y,
                                    emulator::Cell::new(
                                        new_dirs.to_char(),
                                        emulator::CellAttributes::default(),
                                    ),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    /// Draw horizontal borders between HSplit panes.
    fn draw_hsplit_borders(&self, cells: &[PaneCell], dest: &mut emulator::TerminalEmulator) {
        for i in 0..cells.len().saturating_sub(1) {
            // Border is positioned right after the current cell
            let border_y = cells[i].pos_y + cells[i].height;

            // Draw horizontal line for the width of this split
            for x in self.pos_x..(self.pos_x + self.width) {
                let (cols, rows) = (dest.grid().cols, dest.grid().rows);
                if x < cols && border_y < rows {
                    let existing = dest.grid().get_cell(x, border_y).character;
                    let border_char = get_border_char(existing, '─');
                    dest.grid_mut().set_cell(
                        x,
                        border_y,
                        emulator::Cell::new(border_char, emulator::CellAttributes::default()),
                    );
                }
            }

            // Check the row below for vertical borders and add DOWN direction to create T-junction
            let y_below = border_y + 1;
            let (cols, rows) = (dest.grid().cols, dest.grid().rows);
            if y_below < rows {
                for x in self.pos_x..(self.pos_x + self.width) {
                    if x < cols {
                        let below_char = dest.grid().get_cell(x, y_below).character;
                        let below_dirs = BorderDirections::from_char(below_char);
                        // If there's a vertical border below (has up+down), add down direction to current
                        if below_dirs.up && below_dirs.down {
                            let current = dest.grid().get_cell(x, border_y).character;
                            let current_dirs = BorderDirections::from_char(current);
                            let new_dirs = current_dirs.merge(BorderDirections {
                                up: false,
                                down: true,
                                left: false,
                                right: false,
                            });
                            dest.grid_mut().set_cell(
                                x,
                                border_y,
                                emulator::Cell::new(
                                    new_dirs.to_char(),
                                    emulator::CellAttributes::default(),
                                ),
                            );
                        }
                    }
                }
            }

            // Check the row above for vertical borders and add UP direction to create T-junction
            if border_y > 0 {
                let y_above = border_y - 1;
                let (cols, rows) = (dest.grid().cols, dest.grid().rows);
                if y_above < rows {
                    for x in self.pos_x..(self.pos_x + self.width) {
                        if x < cols {
                            let above_char = dest.grid().get_cell(x, y_above).character;
                            let above_dirs = BorderDirections::from_char(above_char);
                            // If there's a vertical border above (has up+down), add up direction to current
                            if above_dirs.up && above_dirs.down {
                                let current = dest.grid().get_cell(x, border_y).character;
                                let current_dirs = BorderDirections::from_char(current);
                                let new_dirs = current_dirs.merge(BorderDirections {
                                    up: true,
                                    down: false,
                                    left: false,
                                    right: false,
                                });
                                dest.grid_mut().set_cell(
                                    x,
                                    border_y,
                                    emulator::Cell::new(
                                        new_dirs.to_char(),
                                        emulator::CellAttributes::default(),
                                    ),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    /// Get the cursor info from the focused pane.
    ///
    /// Returns the cursor position in global coordinates (x, y) and visibility.
    /// Returns None if no pane has focus.
    pub fn get_focused_cursor_info(&self) -> Option<(usize, usize, bool)> {
        if !self.focus {
            return None;
        }

        match &self.inner {
            PaneCellInner::Pane(pane) => {
                // Hide cursor when in scrollback mode
                if pane.scrollback_mode {
                    return Some((0, 0, false));
                }
                let (cursor_x, cursor_y) = pane.terminal_emulator.cursor_position();
                let cursor_visible = pane.terminal_emulator.grid().cursor_visible;
                // Transform cursor position to global coordinates
                let global_x = self.pos_x + cursor_x;
                let global_y = self.pos_y + cursor_y;
                Some((global_x, global_y, cursor_visible))
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                // Find the focused child and get its cursor info
                for cell in cells {
                    if cell.focus {
                        return cell.get_focused_cursor_info();
                    }
                }
                None
            }
        }
    }

    /// Resize this pane cell to new dimensions and position.
    ///
    /// For splits, this distributes space evenly among children.
    /// For leaf panes, this resizes the terminal emulator and PTY.
    pub fn resize(&mut self, pos_x: usize, pos_y: usize, width: usize, height: usize) {
        self.pos_x = pos_x;
        self.pos_y = pos_y;
        self.width = width;
        self.height = height;

        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                // Resize the terminal emulator
                pane.terminal_emulator.resize(width, height);
                // Resize the shell
                pane.shell.resize(width as u16, height as u16);
                // Resize the subprocess PTY if present
                if let Some(ref proc) = pane.subprocess {
                    let _ = proc.resize(width as u16, height as u16);
                }
                if let Some(remote) = pane.remote.as_mut() {
                    let _ = remote.resize(width as u16, height as u16);
                }
            }
            PaneCellInner::VSplit(cells) => {
                // Distribute width evenly among children, reserving 1 column for each border
                let num_cells = cells.len();
                if num_cells == 0 {
                    return;
                }

                // Reserve 1 column for each border between panes
                let num_borders = num_cells.saturating_sub(1);
                let available_width = width.saturating_sub(num_borders);
                let base_width = available_width / num_cells;
                let extra = available_width % num_cells;
                let mut current_x = pos_x;

                for (i, cell) in cells.iter_mut().enumerate() {
                    // Give extra pixels to the first 'extra' cells
                    let cell_width = base_width + if i < extra { 1 } else { 0 };
                    cell.resize(current_x, pos_y, cell_width, height);
                    current_x += cell_width;
                    // Skip 1 column for border after each pane (except the last)
                    if i < num_cells - 1 {
                        current_x += 1;
                    }
                }
            }
            PaneCellInner::HSplit(cells) => {
                // Distribute height evenly among children, reserving 1 row for each border
                let num_cells = cells.len();
                if num_cells == 0 {
                    return;
                }

                // Reserve 1 row for each border between panes
                let num_borders = num_cells.saturating_sub(1);
                let available_height = height.saturating_sub(num_borders);
                let base_height = available_height / num_cells;
                let extra = available_height % num_cells;
                let mut current_y = pos_y;

                for (i, cell) in cells.iter_mut().enumerate() {
                    // Give extra pixels to the first 'extra' cells
                    let cell_height = base_height + if i < extra { 1 } else { 0 };
                    cell.resize(pos_x, current_y, width, cell_height);
                    current_y += cell_height;
                    // Skip 1 row for border after each pane (except the last)
                    if i < num_cells - 1 {
                        current_y += 1;
                    }
                }
            }
        }
    }

    /// Resize only the terminal emulator and PTY of the focused pane without changing
    /// the layout dimensions. Used for zoom mode to give the child process the correct
    /// terminal size while keeping the layout intact for unzooming.
    pub fn resize_focused_pty(&mut self, width: usize, height: usize) {
        match &mut self.inner {
            PaneCellInner::Pane(pane) => {
                if self.focus {
                    pane.terminal_emulator.resize(width, height);
                    pane.shell.resize(width as u16, height as u16);
                    if let Some(ref proc) = pane.subprocess {
                        let _ = proc.resize(width as u16, height as u16);
                    }
                    if let Some(remote) = pane.remote.as_mut() {
                        let _ = remote.resize(width as u16, height as u16);
                    }
                }
            }
            PaneCellInner::VSplit(cells) | PaneCellInner::HSplit(cells) => {
                for cell in cells {
                    if cell.focus {
                        cell.resize_focused_pty(width, height);
                        break;
                    }
                }
            }
        }
    }

    /// Composite this pane cell into the global emulator at a custom position and size.
    /// Used for zoom mode to render the focused pane at fullscreen position without
    /// changing the cell's stored dimensions.
    pub fn composite_into_at(
        &self,
        dest: &mut emulator::TerminalEmulator,
        dest_x: usize,
        dest_y: usize,
        render_width: usize,
        render_height: usize,
    ) {
        match &self.inner {
            PaneCellInner::Pane(pane) => {
                let grid = pane.terminal_emulator.grid();
                let scrollback_len = grid.scrollback_len();

                // Build a helper to check if a position matches a search result
                let is_match = |line_index: isize, col: usize| -> Option<bool> {
                    if !pane.search_mode || pane.search_matches.is_empty() {
                        return None;
                    }
                    for (i, m) in pane.search_matches.iter().enumerate() {
                        if m.line_index == line_index && col >= m.start_col && col < m.end_col {
                            let is_current = Some(i) == pane.current_match_index;
                            return Some(is_current);
                        }
                    }
                    None
                };

                // Build a helper to check if a position matches a URL
                let is_url_match = |line_index: isize, col: usize| -> Option<bool> {
                    if !pane.url_mode || pane.url_matches.is_empty() {
                        return None;
                    }
                    for (i, m) in pane.url_matches.iter().enumerate() {
                        // `contains` handles URLs that span multiple wrapped rows.
                        if m.contains(line_index, col) {
                            let is_current = Some(i) == pane.current_url_index;
                            return Some(is_current);
                        }
                    }
                    None
                };

                // Build a helper to check if a position is in vim visual selection
                let is_in_selection = |abs_line: usize, col: usize| -> bool {
                    if !pane.scrollback_mode || pane.vim_engine.mode == libvim::Mode::Normal {
                        return false;
                    }
                    let start = pane.vim_engine.selection_start;
                    let end = pane.vim_engine.selection_end;

                    match pane.vim_engine.mode {
                        libvim::Mode::Visual => {
                            if abs_line < start.row || abs_line > end.row {
                                return false;
                            }
                            if abs_line == start.row && abs_line == end.row {
                                col >= start.col && col <= end.col
                            } else if abs_line == start.row {
                                col >= start.col
                            } else if abs_line == end.row {
                                col <= end.col
                            } else {
                                true
                            }
                        }
                        libvim::Mode::VisualLine => abs_line >= start.row && abs_line <= end.row,
                        libvim::Mode::Normal => false,
                    }
                };

                let vim_cursor_viewport = pane.get_vim_cursor_info();

                if pane.scrollback_mode {
                    // In scrollback mode - composite with vim cursor and selection
                    for row in 0..render_height {
                        let dst_y_pos = dest_y + row;
                        let first_visible_line = scrollback_len.saturating_sub(pane.scroll_offset);
                        let abs_line = first_visible_line + row;

                        let source_line = if abs_line < scrollback_len {
                            let line_index = (abs_line as isize) - (scrollback_len as isize);
                            Some((true, abs_line, line_index))
                        } else {
                            let grid_row = abs_line - scrollback_len;
                            if grid_row < grid.rows {
                                Some((false, grid_row, grid_row as isize))
                            } else {
                                None
                            }
                        };

                        if let Some((is_scrollback, line_idx, line_index)) = source_line {
                            let source_row = if is_scrollback {
                                grid.get_scrollback_row(line_idx)
                            } else {
                                grid.get_row(line_idx)
                            };

                            if let Some(cells) = source_row {
                                for col in 0..render_width {
                                    let dst_x_pos = dest_x + col;
                                    let (dest_cols, dest_rows) = dest.dimensions();
                                    if dst_x_pos < dest_cols && dst_y_pos < dest_rows {
                                        let mut cell = if col < cells.len() {
                                            cells[col].clone()
                                        } else {
                                            emulator::Cell::new(
                                                ' ',
                                                emulator::CellAttributes::default(),
                                            )
                                        };

                                        // Apply search highlighting
                                        if let Some(is_current) = is_match(line_index, col) {
                                            if is_current {
                                                cell.attrs.bg_color =
                                                    Some(emulator::Color::Magenta);
                                                cell.attrs.fg_color = Some(emulator::Color::White);
                                                cell.attrs.bold = true;
                                            } else {
                                                cell.attrs.bg_color = Some(emulator::Color::Yellow);
                                                cell.attrs.fg_color = Some(emulator::Color::Black);
                                            }
                                        } else if let Some(is_current) =
                                            is_url_match(line_index, col)
                                        {
                                            if is_current {
                                                cell.attrs.underline = true;
                                                cell.attrs.fg_color = Some(emulator::Color::Cyan);
                                                cell.attrs.bold = true;
                                            } else {
                                                cell.attrs.underline = true;
                                                cell.attrs.fg_color = Some(emulator::Color::Blue);
                                            }
                                        }

                                        let abs_for_selection = if is_scrollback {
                                            line_idx
                                        } else {
                                            scrollback_len + line_idx
                                        };
                                        if is_in_selection(abs_for_selection, col) {
                                            cell.attrs.inverse = true;
                                        }

                                        if let Some((vim_col, vim_row, visible)) =
                                            vim_cursor_viewport
                                        {
                                            if visible && col == vim_col && row == vim_row {
                                                cell.attrs.inverse = !cell.attrs.inverse;
                                            }
                                        }

                                        dest.grid_mut().set_cell(dst_x_pos, dst_y_pos, cell);
                                    }
                                }
                            }
                        }
                    }
                } else {
                    // Normal mode - direct composite
                    for row in 0..render_height {
                        let dst_y_pos = dest_y + row;
                        let source_row = grid.get_row(row);

                        if let Some(cells) = source_row {
                            for col in 0..render_width {
                                let dst_x_pos = dest_x + col;
                                let (dest_cols, dest_rows) = dest.dimensions();
                                if dst_x_pos < dest_cols && dst_y_pos < dest_rows {
                                    let cell = if col < cells.len() {
                                        cells[col].clone()
                                    } else {
                                        emulator::Cell::new(
                                            ' ',
                                            emulator::CellAttributes::default(),
                                        )
                                    };
                                    dest.grid_mut().set_cell(dst_x_pos, dst_y_pos, cell);
                                }
                            }
                        }
                    }
                }
            }
            PaneCellInner::VSplit(_) | PaneCellInner::HSplit(_) => {
                // For splits, this should only be called on the focused cell
                // which will be a leaf pane, so this branch shouldn't be reached
            }
        }
    }
}
