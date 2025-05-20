//! All components and definitions related directly to tokens.

use crate::Result;
use num::{BigInt, BigRational};
use std::fmt::{self, Display, Formatter};

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

  /// `enum`
  Enum,

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

  /// `union`
  Union,

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
      Self::Enum => write!(f, "enum"),
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
      Self::Union => write!(f, "union"),
      Self::Var => write!(f, "var"),
      Self::While => write!(f, "while"),
    }
  }
}

/// Implemented on objects onto which tokens can be written.
pub trait TokenWriter: Sized {
  /// Writes a single token to the `TokenWriter`.
  fn write_one(&mut self, token: Token) -> Result<()>;

  /// Writes all tokens of an object implementing `Tokenize` to the
  /// `TokenWriter`.
  fn write<T: Tokenize + ?Sized>(&mut self, item: &T) -> Result<()> {
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

/// A printable iterator over tokens for a parsed syntax object.
pub struct Tokens<'t, T: Tokenize + ?Sized>(&'t T);

impl<T: Tokenize + ?Sized> Display for Tokens<'_, T> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    f.write(self.0).unwrap();
    Ok(())
  }
}

/// Implemented on objects which can be converted into a series of tokens.
pub trait Tokenize {
  /// Converts the object into a series of tokens and writes them, in order, to
  /// a `TokenWriter`.
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()>;

  /// Constructs an iterator over the tokens representing the object.
  fn tokens<'t>(&'t self) -> Tokens<'t, Self> {
    Tokens(self)
  }
}

impl Tokenize for Token {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(self.clone())
  }
}

impl TokenWriter for Formatter<'_> {
  fn write_one(&mut self, token: Token) -> Result<()> {
    // Determine the left and right padding of each token.
    let (left, right) = match token {
      // Tokens that require padding on both the left and right, including:
      // - Binary and assignment operators.
      // - Arrows.
      // - Mid-statement keywords (else, in, etc.)
      Token::Ampersand
      | Token::AmpersandEquals
      | Token::AngleLeft
      | Token::AngleLeftEquals
      | Token::AngleRight
      | Token::AngleRightEquals
      | Token::Arrow
      | Token::Asterisk
      | Token::AsteriskEquals
      | Token::At
      | Token::Caret
      | Token::CaretEquals
      | Token::Dash
      | Token::DashEquals
      | Token::DoubleAmpersand
      | Token::DoubleAngleLeft
      | Token::DoubleAngleLeftEquals
      | Token::DoubleAngleRight
      | Token::DoubleAngleRightEquals
      | Token::DoubleEquals
      | Token::DoublePipe
      | Token::Equals
      | Token::Else
      | Token::ExclamationEquals
      | Token::In
      | Token::Percent
      | Token::PercentEquals
      | Token::Pipe
      | Token::PipeEquals
      | Token::Plus
      | Token::PlusEquals
      | Token::Slash
      | Token::SlashEquals => (" ", " "),

      // Tokens that should have only right padding, including:
      // - Left braces (starting blocks).
      // - Keywords expecting an expression or identifier after.
      // - Delimiters (commas, colons).
      // - Semicolons (ending statements).
      Token::BraceLeft
      | Token::Class
      | Token::Colon
      | Token::Comma
      | Token::Const
      | Token::Enum
      | Token::Extern
      | Token::For
      | Token::Function
      | Token::If
      | Token::Import
      | Token::Return
      | Token::Semicolon
      | Token::Struct
      | Token::Union
      | Token::Var
      | Token::While => ("", " "),

      // Tokens that should have only left padding, including:
      // - EOF indicators.
      Token::EOF => (" ", ""),

      // Tokens that should not have padding, including:
      // - Array and tuple delimiting brackets.
      // - Keywords that are immediately followed by a semicolon.
      // - Unary operators.
      // - Scope and path operators.
      // - Literals.
      // - Identifiers.
      Token::BraceRight
      | Token::BracketLeft
      | Token::BracketRight
      | Token::Break
      | Token::Character(_)
      | Token::Continue
      | Token::DoubleColon
      | Token::Dot
      | Token::Exclamation
      | Token::False
      | Token::Float(_)
      | Token::Integer(_)
      | Token::Identifier(_)
      | Token::MinusMinus
      | Token::ParenthesisLeft
      | Token::ParenthesisRight
      | Token::PlusPlus
      | Token::Self_
      | Token::String(_)
      | Token::Tilde
      | Token::True => ("", ""),
    };

    write!(self, "{left}{token}{right}")?;
    Ok(())
  }
}
