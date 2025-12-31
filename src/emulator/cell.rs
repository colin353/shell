//! Cell types for the terminal grid

/// A color for text or background
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    BrightBlack,
    BrightRed,
    BrightGreen,
    BrightYellow,
    BrightBlue,
    BrightMagenta,
    BrightCyan,
    BrightWhite,
    /// 256-color palette index
    Indexed(u8),
    /// True color RGB
    Rgb(u8, u8, u8),
}

impl Color {
    /// Convert from SGR color code (30-37 for fg, 40-47 for bg)
    pub fn from_sgr_basic(code: u16) -> Option<Self> {
        match code % 10 {
            0 => Some(Color::Black),
            1 => Some(Color::Red),
            2 => Some(Color::Green),
            3 => Some(Color::Yellow),
            4 => Some(Color::Blue),
            5 => Some(Color::Magenta),
            6 => Some(Color::Cyan),
            7 => Some(Color::White),
            _ => None,
        }
    }

    /// Convert from SGR bright color code (90-97 for fg, 100-107 for bg)
    pub fn from_sgr_bright(code: u16) -> Option<Self> {
        match code % 10 {
            0 => Some(Color::BrightBlack),
            1 => Some(Color::BrightRed),
            2 => Some(Color::BrightGreen),
            3 => Some(Color::BrightYellow),
            4 => Some(Color::BrightBlue),
            5 => Some(Color::BrightMagenta),
            6 => Some(Color::BrightCyan),
            7 => Some(Color::BrightWhite),
            _ => None,
        }
    }
}

/// Text attributes for a cell
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CellAttributes {
    pub fg_color: Option<Color>,
    pub bg_color: Option<Color>,
    pub bold: bool,
    pub italic: bool,
    pub underline: bool,
    pub strikethrough: bool,
    pub dim: bool,
    pub inverse: bool,
    pub hidden: bool,
}

impl CellAttributes {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn reset(&mut self) {
        *self = Self::default();
    }
}

/// A single cell in the terminal grid
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cell {
    pub character: char,
    pub attrs: CellAttributes,
}

impl Cell {
    pub fn new(character: char, attrs: CellAttributes) -> Self {
        Self { character, attrs }
    }

    pub fn empty() -> Self {
        Self {
            character: ' ',
            attrs: CellAttributes::default(),
        }
    }

    pub fn with_char(character: char) -> Self {
        Self {
            character,
            attrs: CellAttributes::default(),
        }
    }
}

impl Default for Cell {
    fn default() -> Self {
        Self::empty()
    }
}
