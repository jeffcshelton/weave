use crate::Result;
use std::{cmp::Ordering, io::Read};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ScanError {
  CharacterEmpty,
  EmptyToken,
  EscapeInvalid(char),
  LiteralNotClosed,
  NumberCharacter(char),
  TokenInvalid,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SourcePosition {
  /// The number of bytes.
  pub byte_offset: usize,

  /// The index, in terms of UTF-8 characters, of the position.
  pub char_offset: usize,

  /// The line number, starting at 1.
  pub line: usize,

  pub column: usize,
}

impl Default for SourcePosition {
  fn default() -> Self {
    Self {
      byte_offset: 0,
      char_offset: 0,
      column: 0,
      line: 1,
    }
  }
}

impl PartialOrd for SourcePosition {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for SourcePosition {
  fn cmp(&self, other: &Self) -> Ordering {
    self.byte_offset.cmp(&other.byte_offset)
  }
}

/// An iterator that wraps a `Chars` iterator and counts the line number,
/// column number, character offset, and byte offset.
#[derive(Clone, Debug)]
struct SourceIterator<'a> {
  offset: usize,
  source: &'a [char],
}

impl<'a> SourceIterator<'a> {
  pub fn new(source: &'a [char]) -> Self {
    Self {
      offset: 0,
      source,
    }
  }
}

impl SourceIterator<'_> {
  pub fn back(&mut self) {
    self.offset -= 1;
  }

  pub fn peek(&self, lookahead: usize) -> Option<char> {
    self.source
      .get(self.offset + lookahead)
      .copied()
  }

  pub fn position(&self) -> SourcePosition {
    SourcePosition::default()
  }

  pub fn wrap<T>(&self, error: ScanError) -> Result<T> {
    Err((error, self.position()).into())
  }
}

impl Iterator for SourceIterator<'_> {
  type Item = char;

  fn next(&mut self) -> Option<Self::Item> {
    let c = self.source.get(self.offset).copied();

    if c.is_some() {
      self.offset += 1;
    }

    c
  }
}

#[deny(unused)]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Token {
  /// `&`
  Ampersand,

  /// `&=`
  AmpersandEquals,

  /// `<`
  AngleLeft,

  /// `<=`
  AngleLeftEquals,

  /// `>`
  AngleRight,

  /// `>=`
  AngleRightEquals,

  /// `*`
  Asterisk,

  /// `*=`
  AsteriskEquals,

  /// `{`
  BraceLeft,

  /// `}`
  BraceRight,

  /// `[`
  BracketLeft,

  /// `]`
  BracketRight,

  /// `^`
  Caret,

  /// `^=`
  CaretEquals,

  /// A character literal, beginning and ending with single quotes.
  Character(char),

  /// `class`
  Class,

  /// `:`
  Colon,

  /// `const`
  Const,

  /// `,`
  Comma,

  /// `-`
  Dash,

  /// `-=`
  DashEquals,

  /// `&&`
  DoubleAmpersand,

  /// `<<`
  DoubleAngleLeft,

  /// `<<=`
  DoubleAngleLeftEquals,

  /// `>>`
  DoubleAngleRight,

  /// `>>=`
  DoubleAngleRightEquals,

  /// `==`
  DoubleEquals,

  /// `||`
  DoublePipe,

  /// `.`
  Dot,

  /// End of file
  EOF,

  /// `else`
  Else,

  /// `=`
  Equals,

  /// `!`
  Exclamation,

  /// `!=`
  ExclamationEquals,

  /// `extern`
  Extern,

  Float {
    /// Whether the number is negative.
    negative: bool,

    /// The whole (integer) part of the number.
    whole: u64,

    /// The fractional part of the number (after the decimal).
    fractional: u64,
  },

  /// `function`
  Function,

  /// `if`
  If,

  /// `import`
  Import,

  Integer {
    /// Whether the number is negative.
    negative: bool,

    /// The integer value of the number.
    value: u64,
  },

  /// An arbitrary identifier name.
  ///
  /// Must begin with [A-Za-z_].
  Identifier(Box<str>),

  /// `(`
  ParenthesisLeft,

  /// `)`
  ParenthesisRight,

  /// `%`
  Percent,

  /// `%=`
  PercentEquals,

  /// `|`
  Pipe,

  /// `|=`
  PipeEquals,

  /// `+`
  Plus,

  /// `+=`
  PlusEquals,

  /// `;`
  Semicolon,

  /// `/`
  Slash,

  /// `/=`
  SlashEquals,

  /// A string literal, beginning and ending with double quotes.
  String(Box<str>),

  /// `~`
  Tilde,

  /// `var`
  Var,
}

macro_rules! consume {
  (priv $base:ident, $stream:expr, $default:ident;) => {
    $base::$default
  };

  (priv $base:ident, $stream:expr, $default:ident; { $($c:literal => $d2:ident $({ $($tail:tt)* })?,)* }) => {
    match $stream.peek(0) {
      $(Some($c) => {
        $stream.next();
        consume!(priv $base, $stream, $d2; $({ $($tail)* })?)
      },)*
      _ => consume!(priv $base, $stream, $default;),
    }
  };

  ($base:ident, $stream:expr, { $($c:literal => $default:ident $({ $($tail:tt)* })?,)* }) => {
    match $stream.peek(0) {
      $(Some($c) => {
        $stream.next();
        consume!(priv $base, $stream, $default; $({ $($tail)* })?)
      },)*
      _ => return Err((ScanError::TokenInvalid, $stream.position()).into()),
    }
  };
}

#[derive(Clone, Debug)]
pub struct TokenStream<'s> {
  stream: SourceIterator<'s>,
}

impl<'s> TokenStream<'s> {
  /// Advances the internal iterator past any whitespace at the front.
  /// Additionally updates the program point, accounting for newlines.
  fn skip_whitespace(&mut self) {
    while let Some(c) = self.stream.next() {
      if !c.is_whitespace() {
        self.stream.back();
        break;
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
    while let Some(c) = self.stream.next() {
      if is_stop(c) {
        self.stream.back();
        break;
      }

      word.push(c);
    }

    // Empty tokens are an internal error. This should never run.
    if word.len() == 0 {
      return Err((ScanError::EmptyToken, self.stream.position()).into());
    }

    match word.as_str() {
      // Check for keyword matches.
      "class" => Ok(Token::Class),
      "const" => Ok(Token::Const),
      "extern" => Ok(Token::Extern),
      "else" => Ok(Token::Else),
      "function" => Ok(Token::Function),
      "if" => Ok(Token::If),
      "import" => Ok(Token::Import),
      "var" => Ok(Token::Var),

      // If the word matches no keywords, it must be an identifier.
      _ => Ok(Token::Identifier(word.into_boxed_str())),
    }
  }

  /// Scans an integer or floating point number token.
  ///
  /// Must be called with at least one numeral character or dash at the start of
  /// the internal iterator. Otherwise, it will produce an integer zero.
  fn scan_number(&mut self) -> Result<Token> {
    let negative = self.stream.peek(0) == Some('-');
    if negative {
      self.stream.next();
    }

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
    while let Some(c) = self.stream.next() {
      if is_stop(c) {
        self.stream.back();
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
        return Err((ScanError::NumberCharacter(c), self.stream.position()).into());
      }

      // Advance to the next character.
      i += 1;
    }

    // Empty tokens are an internal error. This should never run.
    if i == 0 {
      return Err((ScanError::EmptyToken, self.stream.position()).into());
    }

    // Pass back either an integer for float depending on whether a dot was
    // present.
    if past_dot {
      Ok(Token::Float { negative, whole, fractional })
    } else {
      Ok(Token::Integer { negative, value: whole })
    }
  }

  /// Scans a single symbol token (a delimiter or operator).
  fn scan_symbol(&mut self) -> Result<Token> {
    let token = consume!(Token, self.stream, {
      '<' => AngleLeft {
        '<' => DoubleAngleLeft {
          '=' => DoubleAngleLeftEquals,
        },
      },
      '>' => AngleRight {
        '>' => DoubleAngleRight {
          '=' => DoubleAngleRightEquals,
        },
      },
      '&' => Ampersand {
        '=' => AmpersandEquals,
        '&' => DoubleAmpersand,
      },
      '*' => Asterisk {
        '=' => AsteriskEquals,
      },
      '{' => BraceLeft,
      '}' => BraceRight,
      '[' => BracketLeft,
      ']' => BracketRight,
      '^' => Caret {
        '=' => CaretEquals,
      },
      ':' => Colon,
      ',' => Comma,
      '-' => Dash {
        '=' => DashEquals,
      },
      '.' => Dot,
      '=' => Equals {
        '=' => DoubleEquals,
      },
      '!' => Exclamation {
        '=' => ExclamationEquals,
      },
      '(' => ParenthesisLeft,
      ')' => ParenthesisRight,
      '%' => Percent {
        '=' => PercentEquals,
      },
      '|' => Pipe {
        '|' => DoublePipe,
        '=' => PipeEquals,
      },
      '+' => Plus {
        '=' => PlusEquals,
      },
      ';' => Semicolon,
      '/' => Slash {
        '=' => SlashEquals,
      },
    });

    Ok(token)
  }

  fn scan_string(&mut self) -> Result<Token> {
    // Discard the starting double quote.
    self.stream.next();

    let mut content = String::new();
    let mut closed = false;

    while let Some(mut c) = self.stream.next() {
      c = match c {
        // Handle escape sequences led by backslashes.
        '\\' => self.scan_escape()?,

        // Handle string closures (ending double quote).
        '"' => {
          closed = true;
          break;
        },

        // Handle unexpected control characters (results in error).
        '\n' | '\r' => {
          break;
        },

        // Otherwise, append the character to the literal as is.
        c => c,
      };

      // Push the (potentially modified) character onto the literal.
      // Note that this may not run due to a break in the above match.
      content.push(c);
    }

    // Handle breaks that resulted from the string unexpectedly ending.
    if closed {
      Ok(Token::String(content.into_boxed_str()))
    } else {
      self.stream.wrap(ScanError::LiteralNotClosed)
    }
  }

  fn scan_character(&mut self) -> Result<Token> {
    // Discard the starting single quote.
    self.stream.next();

    let Some(c) = self.stream.next() else {
      return self.stream.wrap(ScanError::LiteralNotClosed);
    };

    let c = match c {
      '\\' => self.scan_escape()?,
      '\n' | '\r' => {
        return self.stream.wrap(ScanError::LiteralNotClosed);
      },
      '\'' => {
        return self.stream.wrap(ScanError::CharacterEmpty);
      },
      c => c,
    };

    if self.stream.next() != Some('\'') {
      return self.stream.wrap(ScanError::LiteralNotClosed);
    }

    Ok(Token::Character(c))
  }

  fn scan_escape(&mut self) -> Result<char> {
    let Some(escape) = self.stream.next() else {
      return Err((ScanError::LiteralNotClosed, self.stream.position()).into());
    };

    let c = match escape {
      // Null terminator
      '0' => '\0',

      // Alert (bell / beep)
      'a' => '\x07',

      // Backspace
      'b' => '\x08',

      // Formfeed page break
      'f' => '\x0C',

      // Newline (line feed)
      'n' => '\n',

      // Carriage return
      'r' => '\r',

      // Horizontal tab
      't' => '\t',

      // Vertical tab
      'v' => '\x0B',

      // Backslash
      '\\' => '\\',

      // Double quote
      '"' => '"',

      // Single quote
      '\'' => '\'',

      // Invalid escape sequence
      escape => {
        return Err((ScanError::EscapeInvalid(escape), self.stream.position()).into());
      },
    };

    Ok(c)
  }

  pub fn next(&mut self) -> Result<Token> {
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
    let Some(first) = self.stream.peek(0) else {
      return Ok(Token::EOF);
    };

    match first {
      'a'..='z' | 'A'..='Z' | '_' => self.scan_word(),
      '0'..='9' => self.scan_number(),
      '-' => {
        // Choose between deserializing as a number or a minus.
        let is_number = self.stream
          .peek(1)
          .map(|c| c.is_ascii_digit())
          .unwrap_or(false);

        if is_number {
          self.scan_number()
        } else {
          Ok(Token::Dash)
        }
      },
      '"' => self.scan_string(),
      '\'' => self.scan_character(),
      _ => self.scan_symbol(),
    }
  }
}

impl<'s> Iterator for TokenStream<'s> {
  type Item = Result<Token>;

  fn next(&mut self) -> Option<Self::Item> {
    match self.next() {
      Ok(Token::EOF) => None,
      token => Some(token),
    }
  }
}

#[derive(Clone, Debug)]
pub struct Scanner {
  /// Contents of the source file read to create the scanner.
  contents: Box<[char]>,
}

impl Scanner {
  /// Instantiates a new `Scanner` by reading a source file.
  pub fn new(mut reader: impl Read) -> Result<Self> {
    // Read the source file into a string.
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;

    // Decode the string as individual UTF-8 characters because this is what
    // the `SourceIterator` operates on. [Design 1.1]
    let contents = contents
      .chars()
      .collect::<Vec<char>>()
      .into_boxed_slice();

    Ok(Self { contents })
  }

  /// Loads a new source file into the scanner.
  pub fn load(&mut self, mut reader: impl Read) -> Result<()> {
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;

    self.contents = contents
      .chars()
      .collect::<Vec<char>>()
      .into_boxed_slice();

    Ok(())
  }

  /// Returns a `TokenStream` that lazily yields tokens.
  pub fn stream(&mut self) -> TokenStream {
    TokenStream {
      stream: SourceIterator::new(&self.contents),
    }
  }
}
