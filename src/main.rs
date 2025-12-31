use std::io::{self, Write};
use std::process::Command;

mod emulator;
mod input;
mod pane;
mod pty;
mod terminal;
mod tui;

use raw_tty::GuardMode;
use tui::Transition;

fn main() {
    let ctrl = pane::Pane::new();
    let term = tui::Terminal::new();
    let mut app = tui::App::start_with_terminal(Box::new(ctrl), term);
    let tty = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/tty")
        .unwrap();
    let mut tty_input = tty.try_clone().unwrap().guard_mode().unwrap();
    tty_input.set_raw_mode().unwrap();

    let stream = tui::KeyboardEventStream::new(tty_input);
    for event in stream {
        match app.handle_event(event) {
            Transition::Terminate(_) => return,
            Transition::Finished(_) => return,
            _ => continue,
        }
    }

    loop {
        // Print prompt
        print!("> ");
        io::stdout().flush().unwrap();

        // Read input
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => break, // EOF (Ctrl+D)
            Ok(_) => {}
            Err(e) => {
                eprintln!("Error reading input: {}", e);
                continue;
            }
        }

        let input = input.trim();

        // Skip empty lines
        if input.is_empty() {
            continue;
        }

        // Handle exit command
        if input == "exit" {
            break;
        }

        // Parse command and arguments
        let parts: Vec<&str> = input.split_whitespace().collect();
        let command = parts[0];
        let args = &parts[1..];

        // Execute command
        match Command::new(command).args(args).spawn() {
            Ok(mut child) => {
                // Wait for the command to complete
                let _ = child.wait();
            }
            Err(e) => {
                eprintln!("{}: {}", command, e);
            }
        }
    }
}
