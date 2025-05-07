//! Components related to reading and storing source files.

use crate::{Result, scanner::ScanError};
use std::{cmp::Ordering, fmt::{self, Display, Formatter}, fs::File, io::Read, sync::Arc};

/// Represents a UTF-8 source code file that has been read.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Source {
  /// The shared name of the source file.
  ///
  /// This is not guaranteed here to be unique across all sources.
  pub name: Arc<str>,

  /// The shared contents of the source file.
  pub contents: Arc<[char]>,
}

impl Source {
  /// Constructs a `Source` with the name "<empty>" and no contents.
  pub fn empty() -> Self {
    Self {
      name: Arc::from("<empty>"),
      contents: Arc::new([]),
    }
  }

  /// Constructs a `Source` by reading from a reader.
  pub fn read(name: &str, mut reader: impl Read) -> Result<Self> {
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;
    Ok(Self::from_str(name, &contents))
  }

  /// Constructs a `Source` by reading from the given path.
  /// The name of the source will be the path.
  pub fn read_path(path: &str) -> Result<Self> {
    let file = File::open(path)?;
    Self::read(path, file)
  }

  /// Constructs a `Source` by parsing a UTF-8 string.
  pub fn from_str(name: &str, string: &str) -> Self {
    let contents = string
      .chars()
      .collect::<Vec<char>>()
      .into_boxed_slice();

    Self {
      name: Arc::from(name),
      contents: Arc::from(contents),
    }
  }
}

/// Represents an absolute position in a source file.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Point {
  /// The absolute byte offset from the beginning of the source.
  pub byte_offset: usize,

  /// The absolute UTF-8 character index.
  pub char_offset: usize,

  /// The column in the source file.
  pub column: usize,

  /// The line in the source file.
  pub line: usize,

  /// A reference to the source file.
  pub source: Source,
}

impl Point {
  /// Advances the point by a single character.
  pub fn advance(&mut self) {
    if self.char_offset >= self.source.contents.len() {
      return;
    }

    let Some(c) = self.char_at() else {
      return;
    };

    // Update line and column numbers.
    match c {
      // Newlines (line feeds) create a new line and reset the column.
      //
      // Windows uses the "\r\n" sequence to generate newlines. This does
      // generate an extra whitespace character to scan, but it does not affect
      // the logic here because carriage returns are ignored.
      '\n' => {
        self.column = 1;
        self.line += 1;
      },

      // Control characters that should be ignored.
      '\r' => {},

      // Otherwise, increment the column number.
      _ => {
        self.column += 1;
      },
    }

    self.byte_offset += c.len_utf8();
    self.char_offset += 1;
  }

  /// Rolls back the point by a single character.
  pub fn back(&mut self) {
    if self.char_offset == 0 {
      return;
    }

    self.char_offset -= 1;

    // Get the previous character.
    //
    // The `else` statement here should not be possible to execute based on the
    // bounds checking for `advance`.
    let Some(c) = self.char_at() else {
      return;
    };

    // Update line and column numbers.
    match c {
      '\n' => {
        // TODO: Replace this linear algorithm with something constant.
        // A table of line lengths is likely necessary in the source.
        self.column = self.source.contents[0..self.char_offset]
          .iter()
          .rev()
          .position(|&c| c == '\n')
          .unwrap_or(0) + 1;

        self.line -= 1;
      },

      // Ignore the same characters as in `advance`.
      '\r' => {},

      // Otherwise, decrement the column number.
      _ => {
        self.column -= 1;
      },
    };

    self.byte_offset -= c.len_utf8();
  }

  /// Gets the character exactly at the point.
  pub fn char_at(&self) -> Option<char> {
    self.source.contents
      .get(self.char_offset)
      .copied()
  }

  /// Constructs a new `Point` at the start of a source file.
  pub fn start(source: Source) -> Self {
    Self {
      byte_offset: 0,
      char_offset: 0,
      column: 1,
      line: 1,
      source,
    }
  }
}

impl Display for Point {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "line {}, col {}", self.line, self.column)
  }
}

impl Ord for Point {
  fn cmp(&self, other: &Self) -> Ordering {
    self.byte_offset.cmp(&other.byte_offset)
  }
}

impl PartialOrd for Point {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

/// An iterator that wraps a `Chars` iterator and counts the line number,
/// column number, character offset, and byte offset.
#[derive(Clone, Debug)]
pub struct SourceIterator<'s> {
  point: Point,
  source: &'s Source,
}

impl<'s> SourceIterator<'s> {
  /// Constructs a new `SourceIterator` at the start of a source file.
  pub fn new(source: &'s Source) -> Self {
    Self {
      point: Point::start(source.clone()),
      source,
    }
  }

  /// Rolls back the iterator by a single character.
  /// If the iterator is at the beginning, nothing will change.
  pub fn back(&mut self) {
    self.point.back();
  }

  /// Peeks the character at the front of the iterator.
  pub fn peek(&self, lookahead: usize) -> Option<char> {
    self.source.contents
      .get(self.point.char_offset + lookahead)
      .copied()
  }

  /// Gets the point at the front of the iterator.
  pub fn point(&self) -> Point {
    self.point.clone()
  }

  /// Gives a `ScanError` position information and returns it as a
  /// `weave::Result`.
  pub fn locate<T>(&self, error: ScanError) -> Result<T> {
    Err((error, self.point.clone()).into())
  }
}

impl Iterator for SourceIterator<'_> {
  type Item = char;

  fn next(&mut self) -> Option<Self::Item> {
    let c = self.source.contents
      .get(self.point.char_offset)
      .copied();

    self.point.advance();

    c
  }
}
