//! All components related to scanning a source file into Weave tokens.

use crate::{source::{Source, SourceIterator}, Result};
use std::collections::VecDeque;

/// An error that can occur while scanning tokens.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ScanError {
  /// A character declaration is empty. (`''`)
  CharacterEmpty,

  /// A token is found to be empty.
  ///
  /// This is an internal error that should not be possible.
  EmptyToken,

  /// An escape sequence is invalid.
  EscapeInvalid(char),

  /// A string or character literal is not closed.
  LiteralNotClosed,

  /// An invalid character appears in a number literal.
  NumberCharacter(char),

  /// A token is invalid.
  TokenInvalid,
}

/// A single scanner token.
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

  /// A floating point number.
  Float {
    /// Whether the number is negative.
    negative: bool,

    /// The whole (integer) part of the number.
    whole: u64,

    /// The fractional part of the number (after the decimal).
    fractional: u64,
  },

  /// `for`
  For,

  /// `function`
  Function,

  /// `if`
  If,

  /// `import`
  Import,

  /// `in`
  In,

  /// An integer number.
  Integer {
    /// Whether the number is negative.
    negative: bool,

    /// The integer value of the number.
    absolute: u64,
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

  /// `return`
  Return,

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

  /// `while`
  While,
}

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
      _ => return $stream.locate(ScanError::TokenInvalid),
    }
  };
}

/// A peekable stream that produces tokens.
#[derive(Clone, Debug)]
pub struct TokenStream<'s> {
  stream: SourceIterator<'s>,
  peeked: VecDeque<Result<Token>>,
}

impl<'s> TokenStream<'s> {
  /// Constructs a new `TokenStream` from a reference to a source file.
  pub fn new(source: &'s Source) -> Self {
    Self {
      stream: SourceIterator::new(source),
      peeked: VecDeque::new(),
    }
  }

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
      c.is_whitespace() || "<>&*{}[]^:,-.=!()%|+;/".contains(c)
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
      return self.stream.locate(ScanError::EmptyToken);
    }

    let token = match word.as_str() {
      // Check for keyword matches.
      "class" => Token::Class,
      "const" => Token::Const,
      "extern" => Token::Extern,
      "else" => Token::Else,
      "for" => Token::For,
      "function" => Token::Function,
      "if" => Token::If,
      "import" => Token::Import,
      "in" => Token::In,
      "return" => Token::Return,
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
    let mut whole = 0;
    let mut fractional = 0;

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
        return self.stream.locate(ScanError::NumberCharacter(c));
      }

      // Advance to the next character.
      i += 1;
    }

    // Empty tokens are an internal error. This should never run.
    if i == 0 {
      return self.stream.locate(ScanError::EmptyToken);
    }

    // Pass back either an integer for float depending on whether a dot was
    // present.
    if past_dot {
      Ok(Token::Float { negative, whole, fractional })
    } else {
      Ok(Token::Integer { negative, absolute: whole })
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
      self.stream.locate(ScanError::LiteralNotClosed)
    }
  }

  fn scan_character(&mut self) -> Result<Token> {
    // Discard the starting single quote.
    self.stream.next();

    let Some(c) = self.stream.next() else {
      return self.stream.locate(ScanError::LiteralNotClosed);
    };

    let c = match c {
      '\\' => self.scan_escape()?,
      '\n' | '\r' => {
        return self.stream.locate(ScanError::LiteralNotClosed);
      },
      '\'' => {
        return self.stream.locate(ScanError::CharacterEmpty);
      },
      c => c,
    };

    if self.stream.next() != Some('\'') {
      return self.stream.locate(ScanError::LiteralNotClosed);
    }

    Ok(Token::Character(c))
  }

  fn scan_escape(&mut self) -> Result<char> {
    let Some(escape) = self.stream.next() else {
      return self.stream.locate(ScanError::LiteralNotClosed);
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
        return self.stream.locate(ScanError::EscapeInvalid(escape));
      },
    };

    Ok(c)
  }

  fn scan_next(&mut self) -> Result<Token> {
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
          self.stream.next();
          Ok(Token::Dash)
        }
      },
      '"' => self.scan_string(),
      '\'' => self.scan_character(),
      _ => self.scan_symbol(),
    }
  }

  /// Scan the next token and advance the iterator.
  pub fn next(&mut self) -> Result<Token> {
    // Immediately return the first peeked item if it exists.
    if let Some(peeked) = self.peeked.pop_front() {
      return peeked;
    }

    self.scan_next()
  }

  /// Peek the next token without advancing the iterator.
  pub fn peek(&mut self, index: usize) -> Result<Token> {
    for _ in self.peeked.len()..=index {
      let token = self.scan_next();
      self.peeked.push_back(token);
    }

    // TODO: Change to reference.
    self.peeked[index].clone()
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

/// A token scanner that operates on source files.
#[derive(Clone, Debug)]
pub struct Scanner {
  /// Contents of the source file read to create the scanner.
  source: Source,
}

impl Scanner {
  /// Constructs a new `Scanner` by reading from a file path.
  pub fn from_path(path: &str) -> Result<Self> {
    let mut scanner = Scanner::empty();
    scanner.load(path)?;
    Ok(scanner)
  }

  /// Constructs an empty scanner that only yields EOF.
  pub fn empty() -> Self {
    Self { source: Source::empty() }
  }

  /// Loads a new source file into the scanner.
  pub fn load(&mut self, path: &str) -> Result<()> {
    self.source = Source::read_path(path)?;
    Ok(())
  }

  /// Returns a `TokenStream` that lazily yields tokens.
  pub fn stream(&mut self) -> TokenStream {
    TokenStream::new(&self.source)
  }
}
