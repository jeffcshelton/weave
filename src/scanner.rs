use crate::Result;
use std::{cmp::Ordering, io::Read, iter::Peekable, str::Chars};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ScanError {
  EmptyToken,
  NumberCharacter(char),
  TokenInvalid(char),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ProgramPoint {
  pub line: usize,
  pub column: usize,
}

impl PartialOrd for ProgramPoint {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for ProgramPoint {
  fn cmp(&self, other: &Self) -> Ordering {
    self.line.cmp(&other.line)
  }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
  /// Multiplication symbol, *.
  Asterisk,

  /// Left brace, '{'
  BraceLeft,

  /// Right brace, '}'
  BraceRight,

  /// Left bracket, '['
  BracketLeft,

  /// Right bracket, ']'
  BracketRight,

  /// Colon, ':'
  Colon,

  // Comma, ','
  Comma,

  Def,

  /// Dot, '.'
  Dot,

  /// End of file.
  EOF,

  Else,

  /// Keyword "extern".
  Extern,

  Float {
    /// Whether the number is negative.
    negative: bool,

    /// The whole (integer) part of the number.
    whole: u64,

    /// The fractional part of the number (after the decimal).
    fractional: u64,
  },

  /// >
  GreaterThan,

  /// >=
  GreaterThanOrEqualTo,

  If,

  Integer {
    /// Whether the number is negative.
    negative: bool,

    /// The integer value of the number.
    value: u64,
  },

  /// An arbitrary identifier name.
  Identifier(Box<str>),

  /// <
  LessThan,

  /// <=
  LessThanOrEqualTo,

  /// -
  Minus,

  /// Left parenthesis, '('
  ParenthesisLeft,

  /// Right parenthesis, ')'
  ParenthesisRight,

  Plus,

  /// Semicolon, ';'
  Semicolon,

  Slash,
}

#[derive(Clone, Debug)]
pub struct Scanner {
  /// Contents of the source file read to create the scanner.
  contents: Box<str>,
}

pub struct TokenStream<'s> {
  empty: bool,
  position: ProgramPoint,
  stream: Peekable<Chars<'s>>,
}

impl<'s> TokenStream<'s> {
  /// Advances the internal iterator past any whitespace at the front.
  /// Additionally updates the program point, accounting for newlines.
  fn skip_whitespace(&mut self) {
    while let Some(&c) = self.stream.peek() {
      if !c.is_whitespace() {
        break;
      }

      self.stream.next();

      if c == '\n' {
        self.position.line += 1;
        self.position.column = 0;
      } else {
        self.position.column += 1;
      }
    }
  }

  /// Scans a keyword or identifier token.
  ///
  /// Must be called with at least one non-whitespace character at the start of
  /// the internal iterator. Otherwise, will produce an empty identifier.
  fn scan_word(&mut self) -> Result<Token> {
    let mut word = String::new();

    /// Determines whether the peeked character indicates that the word is done.
    fn is_stop(c: char) -> bool {
      c.is_whitespace() || "{}[]:,.();".contains(c)
    }

    // Push characters to the word string until the stream is empty or reaches a
    // stop character.
    //
    // `peek` must be called here instead of `next` because the stop character
    // may not be whitespace, it which case it will be scanned as the next
    // token.
    while let Some(&c) = self.stream.peek() {
      if is_stop(c) {
        break;
      }

      self.stream.next();
      self.position.column += 1;
      word.push(c);
    }

    // Empty tokens are an internal error. This should never run.
    if word.len() == 0 {
      return Err((ScanError::EmptyToken, self.position).into());
    }

    match word.as_str() {
      // Check for keyword matches.
      "def" => Ok(Token::Def),
      "extern" => Ok(Token::Extern),
      "else" => Ok(Token::Else),
      "if" => Ok(Token::If),

      // If the word matches no keywords, it must be an identifier.
      _ => Ok(Token::Identifier(word.into_boxed_str())),
    }
  }

  /// Scans an integer or floating point number token.
  ///
  /// Must be called with at least one numeral character or dash at the start of
  /// the internal iterator. Otherwise, it will produce an integer zero.
  fn scan_number(&mut self, negative: bool) -> Result<Token> {
    // Whole (integer) and fractional parts of the number.
    let mut whole = 0;
    let mut fractional = 0;

    /// Determines whether the peeked character indicates that the number is
    /// done.
    fn is_stop(c: char) -> bool {
      c.is_whitespace() || "{}[]:,();".contains(c)
    }

    // Loop variables required to decode the number.
    let mut i = 0;
    let mut base = 10;
    let mut past_dot = false;

    // Push digits to the number until the stream is empty or reaches a stop
    // character.
    while let Some(&c) = self.stream.peek() {
      if is_stop(c) {
        break;
      }

      if let Some(digit) = c.to_digit(base) {
        // Append the new digit to either the whole or fractional part depending
        // on whether there was previously a dot.
        if past_dot {
          fractional = fractional * 10 + digit as u64;
        } else {
          whole = whole * 10 + digit as u64;
        }
      } else if c == '.' && !past_dot {
        // If one dot occurs in the number, then record the fractional part.
        past_dot = true;
      } else if i == 1 && whole == 0 {
        // Change base if number starts with 0b, 0o, or 0x.
        match c {
          'b' => base = 2,
          'o' => base = 8,
          'x' => base = 16,
          _ => {},
        }
      } else {
        return Err((ScanError::NumberCharacter(c), self.position).into());
      }

      // Advance to the next character.
      self.stream.next();
      self.position.column += 1;
      i += 1;
    }

    // Empty tokens are an internal error. This should never run.
    if i == 0 {
      return Err((ScanError::EmptyToken, self.position).into());
    }

    // Pass back either an integer for float depending on whether a dot was
    // present.
    if past_dot {
      Ok(Token::Float { negative, whole, fractional })
    } else {
      Ok(Token::Integer { negative, value: whole })
    }
  }

  fn scan_delimiter(&mut self) -> Result<Token> {
    let Some(c) = self.stream.next() else {
      // Empty tokens are an internal error. This should never run.
      return Err((ScanError::EmptyToken, self.position).into());
    };

    self.position.column += 1;

    // Match every delimiter character.
    match c {
      '*' => Ok(Token::Asterisk),
      '{' => Ok(Token::BraceLeft),
      '}' => Ok(Token::BraceRight),
      '[' => Ok(Token::BracketLeft),
      ']' => Ok(Token::BracketRight),
      ':' => Ok(Token::Colon),
      ',' => Ok(Token::Comma),
      '.' => Ok(Token::Dot),
      '>' => {
        if self.stream.peek() == Some(&'=') {
          self.stream.next();
          self.position.column += 1;

          Ok(Token::GreaterThanOrEqualTo)
        } else {
          Ok(Token::GreaterThan)
        }
      },
      '<' => {
        if self.stream.peek() == Some(&'=') {
          self.stream.next();
          self.position.column += 1;

          Ok(Token::LessThanOrEqualTo)
        } else {
          Ok(Token::LessThan)
        }
      },
      '(' => Ok(Token::ParenthesisLeft),
      ')' => Ok(Token::ParenthesisRight),
      '+' => Ok(Token::Plus),
      ';' => Ok(Token::Semicolon),
      '/' => Ok(Token::Slash),
      c => Err((ScanError::TokenInvalid(c), self.position).into()),
    }
  }
}

impl<'s> Iterator for TokenStream<'s> {
  type Item = Result<Token>;

  fn next(&mut self) -> Option<Self::Item> {
    // Indicate that no more tokens are left after EOF.
    if self.empty {
      return None;
    }

    self.skip_whitespace();

    // The first character can be used to determine what broad class the token
    // falls into:
    //
    // 1. Alpha - Either a keyword or an arbitrary identifier.
    // 2. Number - An integer or float.
    // 3. Delimiter - A single-character symbol token.
    //
    // Notably, this does not account for floating point numbers that start with
    // a '.', which this scanner does not support.
    let Some(&first) = self.stream.peek() else {
      self.empty = true;
      return Some(Ok(Token::EOF));
    };

    let token = match first {
      'a'..='z' | 'A'..='Z' | '_' => self.scan_word(),
      '0'..='9' => self.scan_number(false),
      '-' => {
        // Advance past the dash.
        self.position.column += 1;
        self.stream.next();

        // Choose between deserializing as a number or a minus.
        let is_number = self.stream
          .peek()
          .map(|&c| c.is_ascii_digit())
          .unwrap_or(false);

        if is_number {
          self.scan_number(true)
        } else {
          Ok(Token::Minus)
        }
      },
      _ => self.scan_delimiter(),
    };

    Some(token)
  }
}

impl Scanner {
  /// Instantiates a new `Scanner` by reading a source file.
  pub fn new(mut reader: impl Read) -> Result<Self> {
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;

    Ok(Self { contents: contents.into() })
  }

  /// Loads a new source file into the scanner.
  pub fn load(&mut self, mut reader: impl Read) -> Result<()> {
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;
    self.contents = contents.into_boxed_str();

    Ok(())
  }

  /// Returns a `TokenStream` that lazily yields tokens.
  pub fn stream(&mut self) -> TokenStream {
    // Create an iterator over UTF-8 characters (not bytes), starting at the
    // first non-whitespace character.
    //
    // This operation does not loop over the enture string; it yields the UTF-8
    // characters lazily.
    let stream = self.contents
      .chars()
      .peekable();

    TokenStream {
      empty: false,
      position: ProgramPoint { line: 1, column: 0 },
      stream,
    }
  }
}
