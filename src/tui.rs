use libc::{c_int, sigaction, sighandler_t, SIGWINCH};
use raw_tty::GuardMode;
use std::collections::HashMap;
use std::io::{Read, Write};
use std::rc::Rc;
use std::sync::Mutex;

lazy_static::lazy_static! {
    static ref DEBUG_MESSAGE: Mutex<String> = Mutex::new(String::new());
    static ref RAW_EVENT_SINK: Mutex<Option<std::sync::mpsc::SyncSender<RawEvent>>> = Mutex::new(None);
}

#[allow(dead_code)]
pub fn debug<S: Into<String>>(message: S) {
    *DEBUG_MESSAGE.lock().unwrap() = message.into();
}

#[derive(Clone)]
pub struct Terminal {
    pub width: usize,
    pub height: usize,
    pub offset_x: usize,
    pub offset_y: usize,
    pos_x: usize,
    pos_y: usize,
    pub wrap: bool,
    stdout: Rc<raw_tty::TtyWithGuard<std::io::Stderr>>,
    prefix: String,
    tree: Rc<Mutex<HashMap<String, usize>>>,
    focus: Rc<Mutex<Option<(usize, usize)>>>,
}

#[allow(dead_code)]
pub enum Color {
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    Black,
}

impl Color {
    fn as_code(&self) -> usize {
        match self {
            Self::Red => 31,
            Self::Green => 32,
            Self::Yellow => 33,
            Self::Blue => 34,
            Self::Magenta => 35,
            Self::Cyan => 36,
            Self::White => 37,
            Self::Black => 30,
        }
    }
}

impl Terminal {
    pub fn new() -> Self {
        let mut t = Terminal {
            width: 80,
            height: 80,
            offset_x: 0,
            offset_y: 0,
            pos_x: 0,
            pos_y: 0,
            wrap: true,
            stdout: Rc::new(std::io::stderr().guard_mode().unwrap()),
            prefix: String::new(),
            tree: Rc::new(Mutex::new(HashMap::new())),
            focus: Rc::new(Mutex::new(None)),
        };
        t.determine_terminal_size();
        t.disable_echo();
        t
    }

    pub fn derive(&self, prefix: String) -> Self {
        let mut t = self.clone();
        t.prefix += "::";
        t.prefix += &prefix;
        t
    }

    pub fn set_rendered_size(&self, size: usize) -> usize {
        self.tree.lock().unwrap().insert(self.prefix.clone(), size);
        size
    }

    pub fn get_rendered_size(&self) -> usize {
        *self.tree.lock().unwrap().get(&self.prefix).unwrap()
    }

    pub fn set_focus(&self, x: usize, y: usize) {
        *self.focus.lock().unwrap() = Some((x + self.offset_x, y + self.offset_y));
    }

    pub fn unset_focus(&self) {
        *self.focus.lock().unwrap() = None;
    }

    pub fn get_focus(&self) -> Option<(usize, usize)> {
        *self.focus.lock().unwrap()
    }

    pub fn disable_echo(&mut self) {
        Rc::get_mut(&mut self.stdout)
            .unwrap()
            .modify_mode(|mut ios| {
                ios.c_lflag &= !0000010;
                ios
            })
            .unwrap();
    }

    pub fn determine_terminal_size(&mut self) {
        unsafe {
            let mut winsize: libc::winsize = std::mem::zeroed();

            libc::ioctl(libc::STDERR_FILENO, libc::TIOCGWINSZ.into(), &mut winsize);
            if winsize.ws_row > 0 && winsize.ws_col > 0 {
                self.width = winsize.ws_col as usize;
                self.height = winsize.ws_row as usize;
            }
        }
    }

    pub fn clear_line(&self) {
        eprint!("\r\x1b[2K");
    }

    pub fn clear_screen(&self) {
        eprint!("\r\x1b[2J\r\x1b[H");
    }

    pub fn disable_wrap(&self) {
        eprint!("\x1b[7l");
    }

    pub fn enable_wrap(&self) {
        eprint!("\x1b[7h");
    }

    pub fn show_cursor(&self) {
        eprint!("\x1b[?25h");
    }

    pub fn hide_cursor(&self) {
        let esc = "\u{001B}";
        eprint!("{}[?25l", esc)
    }

    pub fn get_cursor_pos(&self) -> (usize, usize) {
        let mut tty = std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open("/dev/tty")
            .unwrap();
        let tty_input = tty.try_clone().unwrap();
        write!(tty, "\x1B[6n").unwrap();

        let buf: Vec<_> = tty_input
            .bytes()
            .map(|b| b.unwrap())
            .take_while(|b| *b != 0x52)
            .collect();

        let response = std::str::from_utf8(&buf[2..buf.len()]).unwrap();
        let mut iter = response.split(";");
        let y = iter.next().unwrap().parse().unwrap();
        let x = iter.next().unwrap().parse().unwrap();
        (x, y)
    }

    pub fn move_cursor_to(&mut self, x: usize, y: usize) {
        self.pos_x = x;
        self.pos_y = y;
        eprint!("\x1B[{};{}H", y + 1 + self.offset_y, x + 1 + self.offset_x)
    }

    pub fn set_bold(&self) {
        eprint!("\x1b[{}m", 1);
    }

    pub fn set_underline(&self) {
        eprint!("\x1b[{}m", 4);
    }

    pub fn set_grey(&self) {
        eprint!("\x1b[38;5;245m");
    }

    pub fn set_color(&self, color: Color) {
        eprint!("\x1b[{}m", color.as_code());
    }

    pub fn set_normal(&self) {
        eprint!("\x1b[{}m", 0);
    }

    pub fn set_inverted(&self) {
        eprint!("\x1b[{}m", 40);
        eprint!("\x1b[{}m", 37);
    }

    pub fn flush(&self) {
        std::io::stdout().flush().unwrap()
    }

    pub fn clean_up(&mut self) {
        self.set_normal();
        self.show_cursor();
    }

    pub fn print(&mut self, content: &str) {
        let mut has_printed = false;
        for line in content.lines() {
            if has_printed {
                self.move_cursor_to(0, self.pos_y + 1);
            }

            if self.pos_y > self.height {
                break;
            }

            let line_width = line.chars().count();
            let mut line_chars = line.chars();
            let space_left = self.width - self.pos_x;
            if line_width > space_left {
                eprint!(
                    "{}",
                    &line_chars.by_ref().take(space_left).collect::<String>()
                );
                self.move_cursor_to(0, self.pos_y + 1);
                has_printed = true;
            }

            if self.pos_y >= self.height {
                break;
            }

            if has_printed && !self.wrap {
                return;
            }

            let c: Vec<_> = line_chars.collect();
            for chunk in c.chunks(self.width) {
                eprint!("{}", chunk.iter().collect::<String>());
                self.pos_x += chunk.len();
                if self.wrap {
                    break;
                }
            }

            has_printed = true;
        }
    }

    /// Print cells with their attributes (colors, bold, etc.)
    pub fn print_cells(&mut self, cells: &[crate::emulator::Cell]) {
        use crate::emulator::Color as EmulatorColor;

        for cell in cells {
            // Apply attributes
            let attrs = &cell.attrs;

            // Build SGR sequence
            let mut sgr_parts: Vec<String> = Vec::new();

            if attrs.bold {
                sgr_parts.push("1".to_string());
            }
            if attrs.dim {
                sgr_parts.push("2".to_string());
            }
            if attrs.italic {
                sgr_parts.push("3".to_string());
            }
            if attrs.underline {
                sgr_parts.push("4".to_string());
            }
            if attrs.inverse {
                sgr_parts.push("7".to_string());
            }
            if attrs.hidden {
                sgr_parts.push("8".to_string());
            }
            if attrs.strikethrough {
                sgr_parts.push("9".to_string());
            }

            // Foreground color
            if let Some(ref fg) = attrs.fg_color {
                match fg {
                    EmulatorColor::Black => sgr_parts.push("30".to_string()),
                    EmulatorColor::Red => sgr_parts.push("31".to_string()),
                    EmulatorColor::Green => sgr_parts.push("32".to_string()),
                    EmulatorColor::Yellow => sgr_parts.push("33".to_string()),
                    EmulatorColor::Blue => sgr_parts.push("34".to_string()),
                    EmulatorColor::Magenta => sgr_parts.push("35".to_string()),
                    EmulatorColor::Cyan => sgr_parts.push("36".to_string()),
                    EmulatorColor::White => sgr_parts.push("37".to_string()),
                    EmulatorColor::BrightBlack => sgr_parts.push("90".to_string()),
                    EmulatorColor::BrightRed => sgr_parts.push("91".to_string()),
                    EmulatorColor::BrightGreen => sgr_parts.push("92".to_string()),
                    EmulatorColor::BrightYellow => sgr_parts.push("93".to_string()),
                    EmulatorColor::BrightBlue => sgr_parts.push("94".to_string()),
                    EmulatorColor::BrightMagenta => sgr_parts.push("95".to_string()),
                    EmulatorColor::BrightCyan => sgr_parts.push("96".to_string()),
                    EmulatorColor::BrightWhite => sgr_parts.push("97".to_string()),
                    EmulatorColor::Indexed(idx) => sgr_parts.push(format!("38;5;{}", idx)),
                    EmulatorColor::Rgb(r, g, b) => {
                        sgr_parts.push(format!("38;2;{};{};{}", r, g, b))
                    }
                }
            }

            // Background color
            if let Some(ref bg) = attrs.bg_color {
                match bg {
                    EmulatorColor::Black => sgr_parts.push("40".to_string()),
                    EmulatorColor::Red => sgr_parts.push("41".to_string()),
                    EmulatorColor::Green => sgr_parts.push("42".to_string()),
                    EmulatorColor::Yellow => sgr_parts.push("43".to_string()),
                    EmulatorColor::Blue => sgr_parts.push("44".to_string()),
                    EmulatorColor::Magenta => sgr_parts.push("45".to_string()),
                    EmulatorColor::Cyan => sgr_parts.push("46".to_string()),
                    EmulatorColor::White => sgr_parts.push("47".to_string()),
                    EmulatorColor::BrightBlack => sgr_parts.push("100".to_string()),
                    EmulatorColor::BrightRed => sgr_parts.push("101".to_string()),
                    EmulatorColor::BrightGreen => sgr_parts.push("102".to_string()),
                    EmulatorColor::BrightYellow => sgr_parts.push("103".to_string()),
                    EmulatorColor::BrightBlue => sgr_parts.push("104".to_string()),
                    EmulatorColor::BrightMagenta => sgr_parts.push("105".to_string()),
                    EmulatorColor::BrightCyan => sgr_parts.push("106".to_string()),
                    EmulatorColor::BrightWhite => sgr_parts.push("107".to_string()),
                    EmulatorColor::Indexed(idx) => sgr_parts.push(format!("48;5;{}", idx)),
                    EmulatorColor::Rgb(r, g, b) => {
                        sgr_parts.push(format!("48;2;{};{};{}", r, g, b))
                    }
                }
            }

            // Emit SGR sequence if we have any attributes
            if !sgr_parts.is_empty() {
                eprint!("\x1b[{}m", sgr_parts.join(";"));
            }

            // Print the character
            eprint!("{}", cell.character);

            // Reset after each character (simpler but slightly less efficient)
            if !sgr_parts.is_empty() {
                eprint!("\x1b[0m");
            }
        }
        self.pos_x += cells.len();
    }
}

pub trait Component<T> {
    fn render(&mut self, term: &mut Terminal, state: &T, prev_state: Option<&T>) -> usize;
}

pub struct Container<T> {
    components: Vec<Box<dyn Component<T>>>,
}

impl<T> Container<T> {
    pub fn new(components: Vec<Box<dyn Component<T>>>) -> Self {
        Self {
            components: components,
        }
    }
}

impl<T> Component<T> for Container<T>
where
    T: PartialEq,
{
    fn render(&mut self, term: &mut Terminal, state: &T, prev_state: Option<&T>) -> usize {
        if let Some(p) = prev_state {
            if state == p {
                return term.get_rendered_size();
            }
        }

        let mut size = 0;
        for (idx, component) in self.components.iter_mut().enumerate() {
            let mut t = term.derive(format!("{}", idx));
            t.offset_y += size;
            t.height -= size;
            let offset = component.render(&mut t, state, prev_state);
            size += offset;
        }

        term.set_rendered_size(size)
    }
}

pub struct ScrollContainer<GlobalState, ItemState, SelectFn, ItemStateFn> {
    selected: SelectFn,
    transformer: ItemStateFn,
    component: Box<dyn Component<(ItemState, GlobalState, bool)>>,
    view_position: usize,
    num_rendered_components: usize,
}

impl<GlobalState, ItemState, SelectFn, ItemStateFn>
    ScrollContainer<GlobalState, ItemState, SelectFn, ItemStateFn>
{
    pub fn new(
        component: Box<dyn Component<(ItemState, GlobalState, bool)>>,
        transformer: ItemStateFn,
        selected: SelectFn,
    ) -> Self {
        ScrollContainer {
            selected: selected,
            transformer: transformer,
            component: component,
            view_position: 0,
            num_rendered_components: 0,
        }
    }
}

impl<GlobalState, ItemState, SelectFn, ItemStateFn> Component<GlobalState>
    for ScrollContainer<GlobalState, ItemState, SelectFn, ItemStateFn>
where
    ItemState: Clone + PartialEq,
    GlobalState: Clone + PartialEq,
    SelectFn: Fn(&GlobalState) -> Option<usize>,
    ItemStateFn: Fn(&GlobalState) -> &[ItemState],
{
    fn render(
        &mut self,
        term: &mut Terminal,
        state: &GlobalState,
        prev_state: Option<&GlobalState>,
    ) -> usize {
        let (selected_index, has_selected_index) = match (self.selected)(state) {
            Some(idx) => (idx, true),
            None => (0, false),
        };
        let component_state = (self.transformer)(state);

        let prev_component_state = match prev_state {
            Some(x) => Some((self.transformer)(x)),
            None => None,
        };
        let prev_selected_index = match prev_state {
            Some(x) => Some((self.selected)(x).unwrap_or(0)),
            None => None,
        };

        if selected_index < self.view_position {
            self.view_position = selected_index;
        }

        if selected_index > self.view_position + self.num_rendered_components {
            self.view_position = selected_index - self.num_rendered_components;
        }

        let mut size = 0;
        let mut fully_rendered_components = 0;
        for (index, s_i) in component_state.iter().enumerate().skip(self.view_position) {
            let mut t = term.derive(format!("{}", index));
            t.offset_y += size;

            let prev_item: Option<(ItemState, GlobalState, bool)> = match prev_selected_index {
                Some(idx) if has_selected_index && idx == selected_index => prev_component_state
                    .as_ref()
                    .map(|s| s.get(idx))
                    .flatten()
                    .map(|s| {
                        (
                            (*s).clone(),
                            (**prev_state.as_ref().unwrap()).clone(),
                            index == idx,
                        )
                    }),
                _ => None,
            };
            let offset = self.component.render(
                &mut t,
                &(
                    s_i.clone(),
                    state.clone(),
                    has_selected_index && selected_index == index,
                ),
                prev_item.as_ref(),
            );
            t.offset_y += offset;
            size += offset;
            if t.offset_y <= t.height {
                fully_rendered_components += 1;
            } else {
                break;
            }
        }

        for i in size..term.height {
            term.move_cursor_to(0, i);
            term.clear_line();
        }

        self.num_rendered_components = fully_rendered_components;

        term.height
    }
}

pub struct VecContainer<T> {
    component: Box<dyn Component<T>>,
}

impl<T> VecContainer<T> {
    pub fn new(component: Box<dyn Component<T>>) -> Self {
        Self {
            component: component,
        }
    }
}

impl<T> Component<Vec<T>> for VecContainer<T>
where
    T: PartialEq,
{
    fn render(
        &mut self,
        term: &mut Terminal,
        state: &Vec<T>,
        prev_state: Option<&Vec<T>>,
    ) -> usize {
        if let Some(prev) = prev_state {
            if state == prev {
                return term.get_rendered_size();
            }
        }

        let mut size = 0;
        for (index, s_i) in state.iter().enumerate() {
            let mut t = term.derive(format!("{}", index));
            t.offset_y += size;
            let prev_item = prev_state.as_ref().map(|s| s.get(index)).flatten();
            let offset = self.component.render(&mut t, s_i, prev_item);
            t.offset_y += offset;
            size += offset;
        }

        term.set_rendered_size(size)
    }
}

pub struct Transformer<F, T2> {
    transformer: F,
    component: Box<dyn Component<T2>>,
}

impl<F, T2> Transformer<F, T2> {
    pub fn new(component: Box<dyn Component<T2>>, transformer: F) -> Self {
        Self {
            component: component,
            transformer: transformer,
        }
    }
}

impl<F, T1, T2> Component<T1> for Transformer<F, T2>
where
    F: Fn(&T1) -> &T2,
{
    fn render(&mut self, term: &mut Terminal, state: &T1, prev_state: Option<&T1>) -> usize {
        let transformed = (self.transformer)(state);
        self.component
            .render(term, transformed, prev_state.map(|s| (self.transformer)(s)))
    }
}

pub trait AppController<S> {
    fn render(&mut self, term: &mut Terminal, state: &S, prev_state: Option<&S>);
    fn initial_state(&self) -> S;
    fn transition(&mut self, state: &S, event: KeyboardEvent) -> Transition<S>;

    fn get_terminal_size(&self) -> (usize, usize) {
        (0, 0)
    }

    fn debug(&self, t: &mut Terminal) {
        let msg = DEBUG_MESSAGE.lock().unwrap();
        if !msg.is_empty() {
            t.move_cursor_to(0, 0);
            t.set_inverted();
            t.print("╔═");
            for _ in 0..msg.len() {
                t.print("═");
            }
            t.print("═╗");
            t.move_cursor_to(0, 1);
            t.print("║ ");
            t.print(&msg);
            t.print(" ║");
            t.move_cursor_to(0, 2);
            t.print("╚═");
            for _ in 0..msg.len() {
                t.print("═");
            }
            t.print("═╝");
            t.set_normal();
        }
    }

    fn clean_up(&self, _: &mut Terminal) {}
}

pub struct App<S> {
    terminal: Terminal,
    state: S,
    controller: Box<dyn AppController<S>>,
}

pub enum Transition<S> {
    Updated(S),
    // Terminate the program with the provided exit code
    Terminate(S),
    // Program is finished, clean up and quit
    Finished(S),
    // No state update
    Nothing,
}

impl<S> App<S>
where
    S: Clone,
{
    pub fn start(controller: Box<dyn AppController<S>>) -> Self {
        let term = Terminal::new();
        term.clear_screen();
        Self::start_with_terminal(controller, term)
    }

    pub fn start_with_terminal(controller: Box<dyn AppController<S>>, term: Terminal) -> Self {
        let mut app = Self {
            terminal: term,
            state: controller.initial_state(),
            controller: controller,
        };

        let terminal_size_override = app.controller.get_terminal_size();
        if terminal_size_override != (0, 0) {
            app.terminal.width = terminal_size_override.0;
            app.terminal.height = terminal_size_override.1;
        }
        app.controller.render(&mut app.terminal, &app.state, None);
        let focus = *app.terminal.focus.lock().unwrap();
        if let Some((x, y)) = focus {
            app.terminal.move_cursor_to(x, y);
            app.terminal.show_cursor();
        } else {
            app.terminal.hide_cursor();
        }
        app
    }

    pub fn handle_event(&mut self, event: KeyboardEvent) -> Transition<S> {
        if event == KeyboardEvent::TerminalResizeEvent {
            // Reload the terminal size, since we think it changed.
            self.terminal.determine_terminal_size();

            // Allow controller to potentially handle the resize transition
            let t = self.controller.transition(&self.state, event);

            // Force re-render of everything by setting prev state to None
            self.controller
                .render(&mut self.terminal, &self.state, None);
            return t;
        }

        let t = self.controller.transition(&self.state, event);
        match t {
            Transition::Updated(ref new_state) => {
                self.terminal.hide_cursor();
                self.controller
                    .render(&mut self.terminal, &new_state, Some(&self.state));
                let focus = *self.terminal.focus.lock().unwrap();
                if let Some((x, y)) = focus {
                    self.terminal.move_cursor_to(x, y);
                    self.terminal.show_cursor();
                } else {
                    self.terminal.hide_cursor();
                }
                self.state = new_state.clone();
            }
            Transition::Terminate(_) => {
                self.controller.clean_up(&mut self.terminal);
            }
            Transition::Nothing => (),
            Transition::Finished(_) => {
                self.controller.clean_up(&mut self.terminal);
            }
        };

        self.controller.debug(&mut self.terminal);

        t
    }
}

impl<S> Drop for App<S> {
    fn drop(&mut self) {
        self.controller.clean_up(&mut self.terminal)
    }
}

#[derive(PartialEq)]
pub enum KeyboardEvent {
    Character(char),
    Enter,
    CtrlC,
    CtrlD,
    Backspace,
    CtrlA,
    CtrlE,
    CtrlW,
    CtrlZ,
    CtrlX,
    CtrlT,
    CtrlP,
    CtrlF,
    AltF,
    AltB,
    UpArrow,
    DownArrow,
    LeftArrow,
    RightArrow,
    Escape,
    UnknownControl(char),
    TerminalResizeEvent,
}

enum RawEvent {
    Byte(u8),
    TerminalResizeEvent,
    Terminate,
}

pub struct KeyboardEventStream {
    rx: std::sync::mpsc::Receiver<RawEvent>,
}

impl KeyboardEventStream {
    pub fn new<R: std::io::Read + Send + 'static>(reader: R) -> Self {
        let (tx, rx) = std::sync::mpsc::sync_channel(16);

        unsafe {
            let mut sa: sigaction = std::mem::zeroed();
            sa.sa_flags = 0;
            sa.sa_sigaction = handle_sigwinch as sighandler_t;

            // Register the signal handler
            libc::sigaction(SIGWINCH, &sa, std::ptr::null_mut());
        }

        // NOTE: the C signal handlers can preempt any thread, which means that
        // the signal handler may write to a channel that it is waiting on. If this
        // happens, it won't wake itself up, and the message will be lost. So we need
        // to have a separate thread to bounce the message between, so we can ensure
        // somebody wakes up somebody else.
        let evt_tx = tx.clone();
        let (raw_tx, raw_rx) = std::sync::mpsc::sync_channel(16);
        *RAW_EVENT_SINK.lock().unwrap() = Some(raw_tx.clone());
        std::thread::spawn(move || loop {
            match raw_rx.recv() {
                Ok(m) => {
                    evt_tx.send(m);
                }
                Err(_) => break,
            }
        });

        std::thread::spawn(move || {
            for byte in reader.bytes() {
                match byte {
                    Ok(b) => tx.send(RawEvent::Byte(b)).unwrap(),
                    _ => {
                        tx.send(RawEvent::Terminate);
                        return;
                    }
                }
            }
        });

        Self { rx }
    }
}

impl KeyboardEventStream {
    /// Try to get the next event without blocking.
    /// Returns None if no event is available.
    pub fn try_next(&mut self, timeout: std::time::Duration) -> Option<KeyboardEvent> {
        KeyboardEvent::try_from_stream(&mut self.rx, timeout)
    }
}

impl Iterator for KeyboardEventStream {
    type Item = KeyboardEvent;
    fn next(&mut self) -> Option<Self::Item> {
        KeyboardEvent::from_stream(&mut self.rx)
    }
}

impl KeyboardEvent {
    fn try_from_stream(
        stream: &mut std::sync::mpsc::Receiver<RawEvent>,
        timeout: std::time::Duration,
    ) -> Option<Self> {
        let ch: char = match stream.recv_timeout(timeout) {
            Ok(RawEvent::Byte(b)) => b.into(),
            Ok(RawEvent::TerminalResizeEvent) => return Some(KeyboardEvent::TerminalResizeEvent),
            Ok(RawEvent::Terminate) => return None,
            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => return None,
            Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => return None,
        };

        // For escape sequences, we need to check for more bytes quickly
        Some(Self::parse_char(ch, stream))
    }

    fn parse_char(ch: char, stream: &mut std::sync::mpsc::Receiver<RawEvent>) -> Self {
        match ch {
            '\n' | '\x0d' => Self::Enter,
            '\x03' => Self::CtrlC,
            '\x04' => Self::CtrlD,
            '\x7f' => Self::Backspace,
            '\x01' => Self::CtrlA,
            '\x05' => Self::CtrlE,
            '\x06' => Self::CtrlF,
            '\x10' => Self::CtrlP,
            '\x14' => Self::CtrlT,
            '\x17' => Self::CtrlW,
            '\x18' => Self::CtrlX,
            '\x1a' => Self::CtrlZ,
            '\x1b' => {
                // Control sequence - wait briefly for next byte
                let ch: char = match stream.recv_timeout(std::time::Duration::from_millis(50)) {
                    Ok(RawEvent::Byte(b)) => b.into(),
                    Ok(RawEvent::TerminalResizeEvent) => return KeyboardEvent::TerminalResizeEvent,
                    Ok(RawEvent::Terminate) => return Self::Escape,
                    Err(_) => return Self::Escape,
                };

                match ch {
                    'f' => Self::AltF,
                    'b' => Self::AltB,
                    '[' => {
                        let ch: char =
                            match stream.recv_timeout(std::time::Duration::from_millis(50)) {
                                Ok(RawEvent::Byte(b)) => b.into(),
                                Ok(RawEvent::TerminalResizeEvent) => {
                                    return KeyboardEvent::TerminalResizeEvent
                                }
                                Ok(RawEvent::Terminate) => return Self::UnknownControl('['),
                                Err(_) => return Self::UnknownControl('['),
                            };

                        match ch {
                            'C' => Self::RightArrow,
                            'D' => Self::LeftArrow,
                            'A' => Self::UpArrow,
                            'B' => Self::DownArrow,
                            _ => Self::UnknownControl('['),
                        }
                    }
                    _ => Self::Escape,
                }
            }
            x if x.is_ascii_control() => Self::UnknownControl(x),
            x => Self::Character(x),
        }
    }

    fn from_stream(stream: &mut std::sync::mpsc::Receiver<RawEvent>) -> Option<Self> {
        let ch: char = match stream.recv().unwrap() {
            RawEvent::Byte(b) => b.into(),
            RawEvent::TerminalResizeEvent => return Some(KeyboardEvent::TerminalResizeEvent),
            RawEvent::Terminate => return None,
        };

        Some(match ch {
            '\n' | '\x0d' => Self::Enter, // Enter or Carriage Return
            '\x03' => Self::CtrlC,        // Ctrl+C
            '\x04' => Self::CtrlD,        // Ctrl+D
            '\x7f' => Self::Backspace,    // Backspace
            '\x01' => Self::CtrlA,        // Ctrl+A
            '\x05' => Self::CtrlE,        // Ctrl+E
            '\x06' => Self::CtrlF,        // Ctrl+F
            '\x10' => Self::CtrlP,        // Ctrl+P
            '\x14' => Self::CtrlT,        // Ctrl+T
            '\x17' => Self::CtrlW,        // Ctrl+W
            '\x18' => Self::CtrlX,        // Ctrl+X
            '\x1a' => Self::CtrlZ,        // Ctrl+Z
            '\x1b' => {
                // Control sequence
                let ch: char = match stream.recv().unwrap() {
                    RawEvent::Byte(b) => b.into(),
                    RawEvent::TerminalResizeEvent => {
                        return Some(KeyboardEvent::TerminalResizeEvent)
                    }
                    RawEvent::Terminate => return None,
                };

                match ch {
                    'f' => Self::AltF, // Alt+F
                    'b' => Self::AltB, // Alt+B
                    '[' => {
                        let ch: char = match stream.recv().unwrap() {
                            RawEvent::Byte(b) => b.into(),
                            RawEvent::TerminalResizeEvent => {
                                return Some(KeyboardEvent::TerminalResizeEvent)
                            }
                            RawEvent::Terminate => return None,
                        };

                        match ch {
                            'C' => Self::RightArrow, // Right arrow
                            'D' => Self::LeftArrow,  // Left arrow
                            'A' => Self::UpArrow,    // Up arrow
                            'B' => Self::DownArrow,  // Down arrow
                            _ => Self::UnknownControl('['),
                        }
                    }
                    _ => Self::Escape,
                }
            }
            x if x.is_ascii_control() => Self::UnknownControl(x),
            x => Self::Character(x), // Printable characters
        })
    }
}

// Resize signal handlinng
extern "C" fn handle_sigwinch(_signal: c_int) {
    let _lock = RAW_EVENT_SINK.lock().unwrap();
    let sink = match _lock.as_ref() {
        Some(s) => s,
        None => return,
    };
    sink.send(RawEvent::TerminalResizeEvent);
}
