//! All components related to lexing a source file into Weave tokens.

use crate::{source::{Point, Source, SourceIterator}, Result};
use num::{BigInt, BigRational, One};
use std::{collections::VecDeque, fmt::{self, Display, Formatter}, ops::Range};

/// A single lexer token.
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

  /// `->`
  Arrow,

  /// `*`
  Asterisk,

  /// `*=`
  AsteriskEquals,

  /// `@`
  At,

  /// `{`
  BraceLeft,

  /// `}`
  BraceRight,

  /// `[`
  BracketLeft,

  /// `]`
  BracketRight,

  /// `break`
  Break,

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

  /// `,`
  Comma,

  /// `const`
  Const,

  /// `continue`
  Continue,

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

  /// `::`
  DoubleColon,

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

  /// `false`
  False,

  /// A floating-point literal number of arbitrary size and precision.
  Float(BigRational),

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

  /// An integer literal number of arbitrary size.
  Integer(BigInt),

  /// An arbitrary identifier name.
  ///
  /// Must begin with [A-Za-z_].
  Identifier(Box<str>),

  /// `--`
  MinusMinus,

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

  /// `++`
  PlusPlus,

  /// `return`
  Return,

  /// `self`
  ///
  /// Unfortunately, the identifier `Self` is a reserved keyword in Rust, so it
  /// cannot be used here. The alternative `Self_` was chosen as a stand-in.
  Self_,

  /// `;`
  Semicolon,

  /// `/`
  Slash,

  /// `/=`
  SlashEquals,

  /// A string literal, beginning and ending with double quotes.
  String(Box<str>),

  /// `struct`
  Struct,

  /// `~`
  Tilde,

  /// `true`
  True,

  /// `var`
  Var,

  /// `while`
  While,
}

impl Display for Token {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Ampersand => write!(f, "&"),
      Self::AmpersandEquals => write!(f, "&="),
      Self::AngleLeft => write!(f, "<"),
      Self::AngleLeftEquals => write!(f, "<="),
      Self::AngleRight => write!(f, ">"),
      Self::AngleRightEquals => write!(f, ">="),
      Self::Arrow => write!(f, "->"),
      Self::Asterisk => write!(f, "*"),
      Self::AsteriskEquals => write!(f, "*="),
      Self::At => write!(f, "@"),
      Self::BraceLeft => write!(f, "{{"),
      Self::BraceRight => write!(f, "}}"),
      Self::BracketLeft => write!(f, "["),
      Self::BracketRight => write!(f, "]"),
      Self::Break => write!(f, "break"),
      Self::Caret => write!(f, "^"),
      Self::CaretEquals => write!(f, "^="),
      Self::Character(c) => write!(f, "'{c}'"),
      Self::Class => write!(f, "class"),
      Self::Colon => write!(f, ":"),
      Self::Comma => write!(f, ","),
      Self::Const => write!(f, "const"),
      Self::Continue => write!(f, "continue"),
      Self::Dash => write!(f, "-"),
      Self::DashEquals => write!(f, "-="),
      Self::DoubleAmpersand => write!(f, "&&"),
      Self::DoubleAngleLeft => write!(f, "<<"),
      Self::DoubleAngleLeftEquals => write!(f, "<<="),
      Self::DoubleAngleRight => write!(f, ">>"),
      Self::DoubleAngleRightEquals => write!(f, ">>="),
      Self::DoubleColon => write!(f, "::"),
      Self::DoubleEquals => write!(f, "=="),
      Self::DoublePipe => write!(f, "||"),
      Self::Dot => write!(f, "."),
      Self::EOF => write!(f, "<eof>"),
      Self::Else => write!(f, "else"),
      Self::Equals => write!(f, "="),
      Self::Exclamation => write!(f, "!"),
      Self::ExclamationEquals => write!(f, "!="),
      Self::Extern => write!(f, "extern"),
      Self::False => write!(f, "false"),
      Self::Float(float) => write!(f, "{float}"),
      Self::For => write!(f, "for"),
      Self::Function => write!(f, "function"),
      Self::If => write!(f, "if"),
      Self::Import => write!(f, "import"),
      Self::In => write!(f, "in"),
      Self::Integer(integer) => write!(f, "{integer}"),
      Self::Identifier(identifier) => write!(f, "{identifier}"),
      Self::MinusMinus => write!(f, "--"),
      Self::ParenthesisLeft => write!(f, "("),
      Self::ParenthesisRight => write!(f, ")"),
      Self::Percent => write!(f, "%"),
      Self::PercentEquals => write!(f, "%="),
      Self::Pipe => write!(f, "|"),
      Self::PipeEquals => write!(f, "|="),
      Self::Plus => write!(f, "+"),
      Self::PlusEquals => write!(f, "+="),
      Self::PlusPlus => write!(f, "++"),
      Self::Return => write!(f, "return"),
      Self::Self_ => write!(f, "self"),
      Self::Semicolon => write!(f, ";"),
      Self::Slash => write!(f, "/"),
      Self::SlashEquals => write!(f, "/="),
      Self::String(string) => write!(f, "\"{string}\""),
      Self::Struct => write!(f, "struct"),
      Self::Tilde => write!(f, "~"),
      Self::True => write!(f, "true"),
      Self::Var => write!(f, "var"),
      Self::While => write!(f, "while"),
    }
  }
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
      _ => return $stream.locate(LexError::TokenInvalid),
    }
  };
}

/// Implemented on objects onto which tokens can be written.
pub trait TokenWriter: Sized {
  /// Writes a single token to the `TokenWriter`.
  fn write_one(&mut self, token: Token) -> Result<()>;

  /// Writes all tokens of an object implementing `Tokenize` to the
  /// `TokenWriter`.
  fn write(&mut self, item: impl Tokenize) -> Result<()> {
    item.tokenize(self)
  }

  /// Tokenizes an iterator of objects and writes their tokens to the
  /// `TokenWriter`, joined with a delimiter token.
  fn join<'t, I, T>(&mut self, items: I, inter: Token) -> Result<()>
  where
    I: IntoIterator<Item = &'t T>,
    T: Tokenize + 't,
  {
    let mut iter = items.into_iter();

    if let Some(first) = iter.next() {
      self.write(first)?;
    }

    while let Some(item) = iter.next() {
      self.write_one(inter.clone())?;
      self.write(item)?;
    }

    Ok(())
  }
}

impl TokenWriter for Vec<Token> {
  fn write_one(&mut self, token: Token) -> Result<()> {
    self.push(token);
    Ok(())
  }
}

/// Implemented on objects which can be converted into a series of tokens.
pub trait Tokenize {
  /// Converts the object into a series of tokens and writes them, in order, to
  /// a `TokenWriter`.
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()>;
}

impl<T> Tokenize for &T where T: Tokenize {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    (*self).tokenize(writer)
  }
}

impl<T> Tokenize for Box<T> where T: Tokenize {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    (&**self).tokenize(writer)
  }
}

impl<T> Tokenize for Option<T> where T: Tokenize {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    if let Some(inner) = self {
      inner.tokenize(writer)?;
    }

    Ok(())
  }
}

impl Tokenize for Box<str> {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(Token::String(self.clone()))
  }
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
      return self.stream.locate(LexError::TokenEmpty);
    }

    // Check for keyword matches.
    //
    // If not a keyword, then the word is a token.
    let token = match word.as_str() {
      "break" => Token::Break,
      "class" => Token::Class,
      "const" => Token::Const,
      "continue" => Token::Continue,
      "extern" => Token::Extern,
      "else" => Token::Else,
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
          c => return self.stream.locate(LexError::BaseUnrecognized(c))
        };
      } else {
        return self.stream.locate(LexError::NumberCharacter(c));
      }

      // Increment the loop counter for the base specifier.
      // TODO: Consider peeking the base specifier at the start instead.
      i += 1;
    }

    // Empty tokens are an internal error. This should never run.
    if i == 0 {
      return self.stream.locate(LexError::TokenEmpty);
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
      self.stream.locate(LexError::LiteralNotClosed)
    }
  }

  fn scan_character(&mut self) -> Result<Token> {
    // Discard the starting single quote.
    self.stream.next();

    let Some(c) = self.stream.next() else {
      return self.stream.locate(LexError::LiteralNotClosed);
    };

    let c = match c {
      '\\' => self.scan_escape()?,
      '\n' | '\r' => {
        return self.stream.locate(LexError::LiteralNotClosed);
      },
      '\'' => {
        return self.stream.locate(LexError::CharacterEmpty);
      },
      c => c,
    };

    if self.stream.next() != Some('\'') {
      return self.stream.locate(LexError::LiteralNotClosed);
    }

    Ok(Token::Character(c))
  }

  fn scan_escape(&mut self) -> Result<char> {
    let Some(escape) = self.stream.next() else {
      return self.stream.locate(LexError::LiteralNotClosed);
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
        return self.stream.locate(LexError::EscapeInvalid(escape));
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
  pub fn from_path(path: &str) -> Result<Self> {
    let mut lexer = Self::empty();
    lexer.load(path)?;
    Ok(lexer)
  }

  /// Constructs an empty lexer that only yields EOF.
  pub fn empty() -> Self {
    Self { source: Source::empty() }
  }

  /// Loads a new source file into the lexer.
  pub fn load(&mut self, path: &str) -> Result<()> {
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
pub enum LexError {
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

impl Display for LexError {
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

impl std::error::Error for LexError {}
