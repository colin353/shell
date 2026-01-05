//! libshell library

use std::collections::HashMap;
use std::sync::RwLock;

mod history;

use history::ShellHistory;

struct ShellCore {
    env: RwLock<HashMap<String, String>>,
    history: ShellHistory,
}

impl ShellCore {
    fn new() -> Self {
        ShellCore {
            env: RwLock::new(HashMap::new()),
            history: ShellHistory {
                entries: Vec::new(),
                history: ShellHistory::default(),
            },
        }
    }
}

pub fn hello() {
    println!("Hello from libshell!");
}
