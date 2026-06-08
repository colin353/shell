//! Oracle-based differential-rendering tests.
//!
//! Faithfully mirrors the compositor's `render()` loop:
//!  * a "model" emulator is driven by raw bytes to produce a sequence of frames;
//!  * a "real terminal" emulator starts blank and is advanced ONLY by applying
//!    `compute_delta(prev_model_frame, next_model_frame)` for each step — exactly
//!    what the compositor writes to the host terminal.
//!
//! After every step we assert the real terminal equals the model. A failure is a
//! genuine differential-rendering bug: the screen diverges from the model and only
//! `CTRL+B r` (full repaint from blank) would repair it.

use emulator::{compute_delta, TerminalEmulator, TerminalGrid};

/// A cell holding a control character (e.g. a literal TAB) renders identically to a
/// blank cell, so compare characters by their *display* form.
fn display_char(c: char) -> char {
    if c.is_control() {
        ' '
    } else {
        c
    }
}

fn grids_match(real: &TerminalGrid, model: &TerminalGrid) -> Result<(), String> {
    let cols = real.cols.min(model.cols);
    let rows = real.rows.min(model.rows);
    for y in 0..rows {
        for x in 0..cols {
            let cr = real.get_cell(x, y);
            let cm = model.get_cell(x, y);
            if display_char(cr.character) != display_char(cm.character) {
                return Err(format!(
                    "cell ({x},{y}) char mismatch: real={:?} model={:?}\n  real line:  {:?}\n  model line: {:?}",
                    cr.character, cm.character, real.get_line_text(y), model.get_line_text(y),
                ));
            }
            if cr.attrs != cm.attrs {
                return Err(format!(
                    "cell ({x},{y}) attr mismatch (char {:?}):\n  real ={:?}\n  model={:?}",
                    cr.character, cr.attrs, cm.attrs
                ));
            }
        }
    }
    if real.cursor_x != model.cursor_x || real.cursor_y != model.cursor_y {
        return Err(format!(
            "cursor mismatch: real=({},{}) model=({},{})",
            real.cursor_x, real.cursor_y, model.cursor_x, model.cursor_y
        ));
    }
    Ok(())
}

/// Build a compositor-style "global" frame: a fresh blank grid (all mode fields at
/// default, exactly like `TerminalEmulator::new` in `Compositor::render`) into which
/// the pane's cells + cursor + visibility are copied. This is what the compositor
/// actually diffs — the pane's own alt-screen/scroll-region/autowrap state never
/// reaches `compute_delta`.
fn composite(model: &TerminalEmulator, cols: usize, rows: usize) -> TerminalGrid {
    let mut g = TerminalGrid::new(cols, rows);
    let src = model.grid();
    for y in 0..rows {
        for x in 0..cols {
            g.set_cell(x, y, src.get_cell(x, y).clone());
        }
    }
    g.cursor_x = src.cursor_x;
    g.cursor_y = src.cursor_y;
    g.cursor_visible = src.cursor_visible;
    g
}

/// Drive a model emulator through `steps` (each a chunk of raw PTY bytes). After
/// each step, composite a fresh global frame (as the compositor does), advance a
/// blank "real terminal" by the delta, and assert equality — reproducing the
/// compositor render loop including its trailing pen reset.
fn check_frames(cols: usize, rows: usize, steps: &[&[u8]]) {
    let mut model = TerminalEmulator::new(cols, rows);
    let mut real = TerminalEmulator::new(cols, rows);
    let mut prev_frame = TerminalGrid::new(cols, rows);

    for (i, step) in steps.iter().enumerate() {
        model.process(step);
        let next_frame = composite(&model, cols, rows);
        let delta = compute_delta(&prev_frame, &next_frame);
        real.process(&delta);

        if let Err(e) = grids_match(real.grid(), &next_frame) {
            panic!(
                "differential divergence after step {i} (CTRL+B r would repair):\n{e}\n\n\
                 step bytes: {:?}\n delta ({} bytes): {:?}",
                String::from_utf8_lossy(step),
                delta.len(),
                String::from_utf8_lossy(&delta),
            );
        }
        prev_frame = next_frame;
    }
}

/// Convenience: single prefix frame then a change frame.
fn check(cols: usize, rows: usize, prefix: &[u8], suffix: &[u8]) {
    check_frames(cols, rows, &[prefix, suffix]);
}

// ---- Scenarios ----

#[test]
fn plain_text_change() {
    check(20, 5, b"hello world", b"\r\x1b[Kgoodbye");
}

#[test]
fn color_then_clear_to_default() {
    check(20, 5, b"\x1b[31mREDTEXT\x1b[0m", b"\rplain");
}

#[test]
fn erase_line_with_bg_color() {
    check(20, 5, b"\x1b[41mAAAAAAAA", b"\r\x1b[2K\x1b[0mB");
}

#[test]
fn clear_screen_after_content() {
    check(20, 5, b"line1\r\nline2\r\nline3", b"\x1b[2J\x1b[Hfresh");
}

#[test]
fn scroll_up_via_newlines() {
    let mut prefix = Vec::new();
    for i in 0..5 {
        prefix.extend_from_slice(format!("row{i}\r\n").as_bytes());
    }
    check(20, 5, &prefix, b"row5\r\nrow6\r\nrow7");
}

#[test]
fn wide_chars_then_narrow() {
    check(20, 3, "\u{4e2d}\u{6587}\u{6d4b}\u{8bd5}".as_bytes(), b"\rabcd");
}

#[test]
fn narrow_then_wide_chars() {
    check(20, 3, b"abcdefgh", "\r\u{4e2d}\u{6587}".as_bytes());
}

#[test]
fn attribute_toggle_midline() {
    check(30, 3, b"normal \x1b[1mbold\x1b[0m x", b"\r\x1b[4munderline now\x1b[0m");
}

#[test]
fn cursor_move_no_content_change() {
    check(20, 5, b"abcde\r\nfghij", b"\x1b[1;1H");
}

#[test]
fn partial_line_overwrite() {
    check(20, 3, b"AAAAAAAAAAAAAAAAAAAA", b"\r\x1b[5CXXX");
}

#[test]
fn color_change_same_position() {
    check(20, 3, b"\x1b[31mABC", b"\r\x1b[32mABC");
}

#[test]
fn bg_color_run_then_default_run() {
    check(30, 3, b"\x1b[44mblue\x1b[0mwhite\x1b[44mblue", b"\rX");
}

#[test]
fn many_color_transitions() {
    check(40, 3, b"\x1b[31ma\x1b[32mb\x1b[33mc\x1b[34md\x1b[0me", b"\rZZZZZ");
}

// ---- Multi-frame sequences (accumulative; closer to the "occasional" report) ----

#[test]
fn many_incremental_edits() {
    check_frames(
        20,
        5,
        &[
            b"\x1b[31mred\x1b[0m",
            b" \x1b[1mbold\x1b[0m",
            b"\r\x1b[K",
            b"\x1b[42mgreenbg\x1b[0m",
            b"\r\x1b[2Kdone",
        ],
    );
}

#[test]
fn alternate_screen_roundtrip() {
    check_frames(
        30,
        6,
        &[
            b"main screen text\r\nsecond line",
            b"\x1b[?1049h",          // enter alt screen
            b"\x1b[2J\x1b[Halt!",     // draw in alt
            b"\x1b[?1049l",          // leave alt screen -> restore main
        ],
    );
}

#[test]
fn scroll_region_vim_like() {
    check_frames(
        30,
        8,
        &[
            b"\x1b[2J\x1b[H",
            b"\x1b[2;6r",                          // set scroll region rows 2..6
            b"\x1b[2;1Hline a\r\nline b\r\nline c", // fill region
            b"\x1b[6;1H\n",                         // newline at bottom -> region scrolls
            b"\x1b[rmore",                          // reset region, write
        ],
    );
}

#[test]
fn repeated_bg_erase_cycles() {
    // Cycle a colored background erase several times; accumulates pen state errors
    // if the trailing reset / pen tracking is ever wrong.
    check_frames(
        24,
        4,
        &[
            b"\x1b[41mAAAAAAAAAA\x1b[0m",
            b"\r\x1b[2K\x1b[44mBBBB\x1b[0m",
            b"\r\x1b[2K\x1b[42mCCCCCC\x1b[0m",
            b"\r\x1b[2Kplain",
            b"\r\x1b[2K",
        ],
    );
}

// ---- Bottom-right / autowrap edge (this path runs every production frame) ----

#[test]
fn fill_entire_screen() {
    // Fill every cell including the bottom-right corner.
    let fill: Vec<u8> = std::iter::repeat(b'X').take(20 * 5).collect();
    check(20, 5, b"", &fill);
}

#[test]
fn bottom_right_corner_char() {
    // Write exactly to the last cell of the screen.
    check(10, 3, b"", b"\x1b[3;10HZ");
}

#[test]
fn fill_then_change_corner() {
    let fill: Vec<u8> = std::iter::repeat(b'X').take(10 * 3).collect();
    check_frames(10, 3, &[&fill, b"\x1b[3;10HQ"]);
}

#[test]
fn wide_char_at_right_edge() {
    // A wide char that would straddle the last column: terminal pushes it to next
    // line, leaving a spacer. Then change it.
    check_frames(10, 3, &["abcdefghi\u{4e2d}".as_bytes(), b"\rZZZZ"]);
}

#[test]
fn full_screen_color_fill() {
    // Fill the whole screen with colored cells, then reset corner.
    let mut fill = Vec::from(&b"\x1b[42m"[..]);
    fill.extend(std::iter::repeat(b'G').take(10 * 3));
    check(10, 3, b"", &fill);
}

#[test]
fn typing_at_line_end_wraps() {
    // Type past the right margin so autowrap kicks in across several frames.
    check_frames(
        8,
        3,
        &[b"123456", b"78", b"9", b"abc", b"def"],
    );
}

#[test]
fn wide_char_shift_cycles() {
    check_frames(
        20,
        3,
        &[
            "\u{4e2d}\u{6587}abc".as_bytes(),
            "\rx\u{4e2d}\u{6587}".as_bytes(),
            "\r\u{4e2d}x\u{6587}".as_bytes(),
            b"\rplain text",
        ],
    );
}
