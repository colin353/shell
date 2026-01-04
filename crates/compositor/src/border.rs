/// Flags representing which directions a border character connects to
#[derive(Clone, Copy, Default)]
pub struct BorderDirections {
    pub up: bool,
    pub down: bool,
    pub left: bool,
    pub right: bool,
}

impl BorderDirections {
    /// Get directions from an existing character
    pub fn from_char(c: char) -> Self {
        match c {
            '─' => Self {
                left: true,
                right: true,
                up: false,
                down: false,
            },
            '│' => Self {
                left: false,
                right: false,
                up: true,
                down: true,
            },
            '├' => Self {
                left: false,
                right: true,
                up: true,
                down: true,
            },
            '┤' => Self {
                left: true,
                right: false,
                up: true,
                down: true,
            },
            '┬' => Self {
                left: true,
                right: true,
                up: false,
                down: true,
            },
            '┴' => Self {
                left: true,
                right: true,
                up: true,
                down: false,
            },
            '┼' => Self {
                left: true,
                right: true,
                up: true,
                down: true,
            },
            _ => Self::default(),
        }
    }

    /// Merge with another set of directions
    pub fn merge(&self, other: Self) -> Self {
        Self {
            up: self.up || other.up,
            down: self.down || other.down,
            left: self.left || other.left,
            right: self.right || other.right,
        }
    }

    /// Convert to the appropriate border character
    pub fn to_char(&self) -> char {
        match (self.up, self.down, self.left, self.right) {
            // Four-way
            (true, true, true, true) => '┼',
            // Three-way (T-junctions)
            (true, true, false, true) => '├',
            (true, true, true, false) => '┤',
            (false, true, true, true) => '┬',
            (true, false, true, true) => '┴',
            // Two-way
            (true, true, false, false) => '│',
            (false, false, true, true) => '─',
            // Corners (for completeness, though we may not use them)
            (false, true, false, true) => '┌',
            (false, true, true, false) => '┐',
            (true, false, false, true) => '└',
            (true, false, true, false) => '┘',
            // Single direction or none - shouldn't happen in practice
            _ => ' ',
        }
    }
}

/// Get the appropriate border character by merging existing with new directions.
pub fn get_border_char(existing: char, default: char) -> char {
    let existing_dirs = BorderDirections::from_char(existing);
    let new_dirs = BorderDirections::from_char(default);
    let merged = existing_dirs.merge(new_dirs);

    // If we have any directions, use the merged result; otherwise use default
    if merged.up || merged.down || merged.left || merged.right {
        merged.to_char()
    } else {
        default
    }
}
