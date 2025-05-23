//! All components related to lexing a source file into Weave tokens.

pub mod token;

use crate::{source::{Point, Source, SourceIterator}, Result};
use num::{BigInt, BigRational, One};
use std::{collections::VecDeque, fmt::{self, Display, Formatter}, ops::Range, path::Path};
use token::Token;

macro_rules! consume {
  (priv $base:ident, $stream:expr, $default:ident;) => {
    $base::$default
  };

  (priv
   $base:ident,
   $stream:expr,
   $default:ident;
   { $($c:literal => $d2:ident $({ $($tail:tt)* })?,)* }
  ) => {
    match $stream.peek(0) {
      $(Some($c) => {
        $stream.next();
        consume!(priv $base, $stream, $d2; $({ $($tail)* })?)
      },)*
      _ => consume!(priv $base, $stream, $default;),
    }
  };

  (
    $base:ident,
    $stream:expr,
    { $($c:literal => $default:ident $({ $($tail:tt)* })?,)* }
  ) => {
    match $stream.peek(0) {
      $(Some($c) => {
        $stream.next();
        consume!(priv $base, $stream, $default; $({ $($tail)* })?)
      },)*
      _ => return $stream.locate(Error::TokenInvalid),
    }
  };
}

/// A peekable stream that produces tokens.
#[derive(Clone, Debug)]
pub struct TokenStream<'s> {
  stream: SourceIterator<'s>,
  peeked: VecDeque<(Token, Range<Point>)>,

  last_range: Range<Point>,
}

impl<'s> TokenStream<'s> {
  /// Constructs a new `TokenStream` from a reference to a source file.
  pub fn new(source: &'s Source) -> Self {
    let stream = SourceIterator::new(source);
    let start = stream.point();

    Self {
      stream,
      peeked: VecDeque::new(),
      last_range: start.clone()..start,
    }
  }

  /// Advances the internal iterator past any whitespace at the front.
  /// Additionally updates the program point, accounting for newlines.
  fn skip_whitespace(&mut self) {
    while let Some(c) = self.stream.peek(0) {
      if !c.is_whitespace() {
        break;
      }

      self.stream.next();
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
      c.is_whitespace() || "<>&*{}[]^:,-.=!()%|+;/".contains(c)
    }

    // Push characters to the word string until the stream is empty or reaches a
    // stop character.
    //
    // `peek` must be called here instead of `next` because the stop character
    // may not be whitespace, it which case it will be scanned as the next
    // token.
    while let Some(c) = self.stream.peek(0) {
      if is_stop(c) {
        break;
      }

      self.stream.next();
      word.push(c);
    }

    // Empty tokens are an internal error. This should never run.
    if word.len() == 0 {
      return self.stream.locate(Error::TokenEmpty);
    }

    // Check for keyword matches.
    //
    // If not a keyword, then the word is a token.
    let token = match word.as_str() {
      "break" => Token::Break,
      "class" => Token::Class,
      "const" => Token::Const,
      "continue" => Token::Continue,
      "else" => Token::Else,
      "enum" => Token::Enum,
      "extern" => Token::Extern,
      "false" => Token::False,
      "for" => Token::For,
      "function" => Token::Function,
      "if" => Token::If,
      "import" => Token::Import,
      "in" => Token::In,
      "return" => Token::Return,
      "self" => Token::Self_,
      "struct" => Token::Struct,
      "true" => Token::True,
      "union" => Token::Union,
      "var" => Token::Var,
      "while" => Token::While,

      // If the word matches no keywords, it must be an identifier.
      _ => Token::Identifier(word.into_boxed_str()),
    };

    Ok(token)
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
    let mut whole = BigInt::ZERO;
    let mut divisor = BigInt::one();

    /// Determines whether the peeked character indicates that the number is
    /// done.
    fn is_stop(c: char) -> bool {
      c.is_whitespace() || "<>&*{}[]^:,-=!()%|+;/".contains(c)
    }

    // Loop variables required to decode the number.
    let mut i = 0;
    let mut base = 10;
    let mut past_dot = false;

    // Push digits to the number until the stream is empty or reaches a stop
    // character.
    while let Some(c) = self.stream.peek(0) {
      if is_stop(c) {
        break;
      }

      _ = self.stream.next();

      if let Some(digit) = c.to_digit(base) {
        // Append the new digit to the whole part.
        whole = whole * 10 + digit;

        // Update the divisor if this is a floating point number to account
        // for the base-10 shifting of the digit.
        if past_dot {
          divisor *= 10;
        }
      } else if c == '.' && !past_dot && base == 10 {
        // If a dot is found in the middle of a base-10 number, then it must be
        // a floating point number.
        past_dot = true;
      } else if i == 1 && whole == BigInt::ZERO {
        base = match c {
          'b' => 2,
          'o' => 8,
          'x' => 16,
          c => return self.stream.locate(Error::BaseUnrecognized(c))
        };
      } else {
        return self.stream.locate(Error::NumberCharacter(c));
      }

      // Increment the loop counter for the base specifier.
      // TODO: Consider peeking the base specifier at the start instead.
      i += 1;
    }

    // Empty tokens are an internal error. This should never run.
    if i == 0 {
      return self.stream.locate(Error::TokenEmpty);
    }

    // Pass back either an integer for float depending on whether a dot was
    // present.
    if past_dot {
      Ok(Token::Float(BigRational::new(whole, divisor)))
    } else {
      Ok(Token::Integer(whole))
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
      '@' => At,
      '{' => BraceLeft,
      '}' => BraceRight,
      '[' => BracketLeft,
      ']' => BracketRight,
      '^' => Caret {
        '=' => CaretEquals,
      },
      ':' => Colon {
        ':' => DoubleColon,
      },
      ',' => Comma,
      '-' => Dash {
        '>' => Arrow,
        '=' => DashEquals,
        '-' => MinusMinus,
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
        '+' => PlusPlus,
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
      self.stream.locate(Error::LiteralNotClosed)
    }
  }

  fn scan_character(&mut self) -> Result<Token> {
    // Discard the starting single quote.
    self.stream.next();

    let Some(c) = self.stream.next() else {
      return self.stream.locate(Error::LiteralNotClosed);
    };

    let c = match c {
      '\\' => self.scan_escape()?,
      '\n' | '\r' => {
        return self.stream.locate(Error::LiteralNotClosed);
      },
      '\'' => {
        return self.stream.locate(Error::CharacterEmpty);
      },
      c => c,
    };

    if self.stream.next() != Some('\'') {
      return self.stream.locate(Error::LiteralNotClosed);
    }

    Ok(Token::Character(c))
  }

  fn scan_escape(&mut self) -> Result<char> {
    let Some(escape) = self.stream.next() else {
      return self.stream.locate(Error::LiteralNotClosed);
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
        return self.stream.locate(Error::EscapeInvalid(escape));
      },
    };

    Ok(c)
  }

  fn scan_next(&mut self) -> Result<(Token, Range<Point>)> {
    self.skip_whitespace();

    // The starting point of the token.
    let start = self.stream.point();

    // The first character can be used to determine what broad class the token
    // falls into:
    //
    // 1. Alpha - Either a keyword or an arbitrary identifier.
    // 2. Number - An integer or float.
    // 3. Delimiter - A single-character symbol token.
    //
    // Notably, this does not account for floating point numbers that start with
    // a '.', which this lexer does not support.
    let Some(first) = self.stream.peek(0) else {
      return Ok((Token::EOF, start.clone()..start));
    };

    let token = match first {
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
          self.scan_symbol()
        }
      },
      '"' => self.scan_string(),
      '\'' => self.scan_character(),
      _ => self.scan_symbol(),
    }?;

    let mut end = self.stream.point();
    end.back();

    Ok((token, start..end))
  }

  /// Scan the next token and advance the iterator.
  pub fn next(&mut self) -> Result<Token> {
    // Immediately return the first peeked item if it exists.
    // Otherwise, scan the next item.
    let (token, range) = match self.peeked.pop_front() {
      Some(peeked) => peeked,
      None => self.scan_next()?,
    };

    self.last_range = range;
    Ok(token)
  }

  /// Peek the next token without advancing the iterator.
  pub fn peek(&mut self, index: usize) -> Result<Token> {
    for _ in self.peeked.len()..=index {
      let token = self.scan_next()?;
      self.peeked.push_back(token);
    }

    // TODO: Change to reference.
    Ok(self.peeked[index].0.clone())
  }

  /// Gets the source point range of the last scanned element.
  pub fn last_range(&self) -> Range<Point> {
    self.last_range.clone()
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

/// A lexer that produces tokens by scanning source files.
#[derive(Clone, Debug)]
pub struct Lexer {
  /// Contents of the source file read to create the lexer.
  source: Source,
}

impl Lexer {
  /// Constructs a new `Lexer` by reading from a file path.
  pub fn from_path(path: impl AsRef<Path>) -> Result<Self> {
    let mut lexer = Self::empty();
    lexer.load(path)?;
    Ok(lexer)
  }

  /// Constructs an empty lexer that only yields EOF.
  pub fn empty() -> Self {
    Self { source: Source::empty() }
  }

  /// Loads a new source file into the lexer.
  pub fn load(&mut self, path: impl AsRef<Path>) -> Result<()> {
    self.source = Source::read_path(path)?;
    Ok(())
  }

  /// Returns a `TokenStream` that lazily yields tokens.
  pub fn stream(&mut self) -> TokenStream {
    TokenStream::new(&self.source)
  }
}

/// An error that can occur while lexing tokens.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Error {
  /// The base specifier of a number is unrecognized (`0b`, `0o`, or `0x`).
  BaseUnrecognized(char),

  /// A character declaration is empty. (`''`)
  CharacterEmpty,

  /// An escape sequence is invalid.
  EscapeInvalid(char),

  /// A string or character literal is not closed.
  LiteralNotClosed,

  /// An invalid character appears in a number literal.
  NumberCharacter(char),

  /// A token is found to be empty.
  ///
  /// This is an internal error that should not be possible.
  TokenEmpty,

  /// A token is invalid.
  TokenInvalid,
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::BaseUnrecognized(c) => {
        write!(f, "unrecognized base specifier '{c}'")
      },
      Self::CharacterEmpty => {
        write!(f, "character empty")
      },
      Self::EscapeInvalid(escape) => {
        write!(f, "invalid escape sequence: \\{escape}")
      },
      Self::LiteralNotClosed => {
        write!(f, "literal not closed")
      },
      Self::NumberCharacter(c) => {
        write!(f, "number character invalid: {c}")
      },
      Self::TokenEmpty => {
        write!(f, "empty token")
      },
      Self::TokenInvalid => {
        write!(f, "invalid token")
      },
    }
  }
}

impl std::error::Error for Error {}
