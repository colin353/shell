//! Randomized differential-rendering fuzz test.
//!
//! Generates many random escape-sequence "frames", drives a model emulator, and
//! after each frame advances a blank "real terminal" by `compute_delta` exactly as
//! the compositor's `render()` does. Any divergence is a differential-rendering bug
//! (the screen drifts from the model; only `CTRL+B r` would repair it).

use emulator::{compute_delta, TerminalEmulator, TerminalGrid};

/// Tiny deterministic PRNG (xorshift64) so failures are reproducible by seed.
struct Rng(u64);
impl Rng {
    fn next(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.0 = x;
        x
    }
    fn below(&mut self, n: u64) -> u64 {
        self.next() % n
    }
}

/// Build one random "frame" of PTY bytes: a handful of random terminal operations.
fn random_frame(rng: &mut Rng, cols: usize, rows: usize) -> Vec<u8> {
    let mut out = Vec::new();
    let ops = 1 + rng.below(6);
    for _ in 0..ops {
        match rng.below(11) {
            // printable run
            0 | 1 | 2 => {
                let len = 1 + rng.below(12);
                for _ in 0..len {
                    let c = b'!' + (rng.below(0x5d) as u8); // printable ASCII
                    out.push(c);
                }
            }
            // SGR attribute / color
            3 | 4 => {
                match rng.below(12) {
                    8 => {
                        // 256-color fg/bg
                        let idx = rng.below(256);
                        let sel = if rng.below(2) == 0 { 38 } else { 48 };
                        out.extend_from_slice(format!("\x1b[{sel};5;{idx}m").as_bytes());
                    }
                    9 => {
                        // RGB fg/bg
                        let (r, g, b) = (rng.below(256), rng.below(256), rng.below(256));
                        let sel = if rng.below(2) == 0 { 38 } else { 48 };
                        out.extend_from_slice(format!("\x1b[{sel};2;{r};{g};{b}m").as_bytes());
                    }
                    other => {
                        let code = match other {
                            0 => 0,                  // reset
                            1 => 1,                  // bold
                            2 => 4,                  // underline
                            3 => 7,                  // inverse
                            4 => 30 + rng.below(8),  // fg basic
                            5 => 40 + rng.below(8),  // bg basic
                            6 => 90 + rng.below(8),  // fg bright
                            _ => 100 + rng.below(8), // bg bright
                        };
                        out.extend_from_slice(format!("\x1b[{code}m").as_bytes());
                    }
                }
            }
            // CUP cursor position
            5 => {
                let r = 1 + rng.below(rows as u64);
                let c = 1 + rng.below(cols as u64);
                out.extend_from_slice(format!("\x1b[{r};{c}H").as_bytes());
            }
            // CR / LF
            6 => out.extend_from_slice(b"\r"),
            7 => out.extend_from_slice(b"\n"),
            // erase in line / display
            8 => {
                let n = rng.below(3);
                out.extend_from_slice(format!("\x1b[{n}K").as_bytes());
            }
            9 => {
                let n = rng.below(3);
                out.extend_from_slice(format!("\x1b[{n}J").as_bytes());
            }
            // cursor visibility toggle + backspace/tab
            _ => match rng.below(4) {
                0 => out.extend_from_slice(b"\x1b[?25l"),
                1 => out.extend_from_slice(b"\x1b[?25h"),
                2 => out.push(0x08),
                _ => out.push(b'\t'),
            },
        }
    }
    out
}

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

/// A control character in a cell renders the same as a blank, so compare by display form.
fn display_char(c: char) -> char {
    if c.is_control() {
        ' '
    } else {
        c
    }
}

fn diff(real: &TerminalGrid, model: &TerminalGrid) -> Option<String> {
    let cols = real.cols.min(model.cols);
    let rows = real.rows.min(model.rows);
    for y in 0..rows {
        for x in 0..cols {
            let cr = real.get_cell(x, y);
            let cm = model.get_cell(x, y);
            if display_char(cr.character) != display_char(cm.character) {
                return Some(format!(
                    "cell ({x},{y}) char: real={:?} model={:?}\n  real line:  {:?}\n  model line: {:?}",
                    cr.character, cm.character, real.get_line_text(y), model.get_line_text(y)
                ));
            }
            if cr.attrs != cm.attrs {
                return Some(format!(
                    "cell ({x},{y}) attrs (char {:?}):\n  real ={:?}\n  model={:?}",
                    cr.character, cr.attrs, cm.attrs
                ));
            }
        }
    }
    if real.cursor_x != model.cursor_x || real.cursor_y != model.cursor_y {
        return Some(format!(
            "cursor: real=({},{}) model=({},{})",
            real.cursor_x, real.cursor_y, model.cursor_x, model.cursor_y
        ));
    }
    None
}

fn run_seed(seed: u64, cols: usize, rows: usize, frames: usize) -> Result<(), String> {
    let mut rng = Rng(seed);
    let mut model = TerminalEmulator::new(cols, rows);
    let mut real = TerminalEmulator::new(cols, rows);
    let mut prev_frame = TerminalGrid::new(cols, rows);
    let mut history: Vec<Vec<u8>> = Vec::new();

    for f in 0..frames {
        let step = random_frame(&mut rng, cols, rows);
        history.push(step.clone());
        model.process(&step);
        let next_frame = composite(&model, cols, rows);
        let delta = compute_delta(&prev_frame, &next_frame);
        real.process(&delta);

        if let Some(d) = diff(real.grid(), &next_frame) {
            let hist: Vec<String> = history
                .iter()
                .map(|h| String::from_utf8_lossy(h).into_owned())
                .collect();
            return Err(format!(
                "seed {seed}: divergence at frame {f}:\n{d}\n\nframe bytes: {:?}\n\
                 delta ({} bytes): {:?}\n\nfull history: {:?}",
                String::from_utf8_lossy(&step),
                delta.len(),
                String::from_utf8_lossy(&delta),
                hist,
            ));
        }
        prev_frame = next_frame;
    }
    Ok(())
}

#[test]
fn fuzz_differential_rendering() {
    let mut failures = Vec::new();
    for seed in 1..=3000u64 {
        if let Err(e) = run_seed(seed.wrapping_mul(0x9E3779B97F4A7C15), 24, 9, 16) {
            failures.push(e);
            if failures.len() >= 5 {
                break; // report a handful, don't spam
            }
        }
    }
    if !failures.is_empty() {
        panic!(
            "differential rendering diverged in {} seed(s):\n\n{}",
            failures.len(),
            failures.join("\n\n========================================\n\n")
        );
    }
}
