//! Components related to parsing a Weave abstract syntax tree (AST).

pub mod expression;
pub mod function;
pub mod import;
pub mod operator;
pub mod statement;
pub mod types;
pub mod unit;

pub use expression::*;
pub use function::*;
pub use import::*;
pub use operator::*;
pub use statement::*;
pub use types::*;
pub use unit::*;

use crate::{Result, lexer::{Token, TokenStream, TokenWriter, Tokenize}};
use num::BigInt;
use std::fmt::{self, Display, Formatter};

/// A parser that produces an abstract syntax tree (AST).
#[derive(Clone, Debug)]
pub struct Parser<'s> {
  stream: TokenStream<'s>,
}

impl<'s> Parser<'s> {
  /// Constructs a `Parser` by reading from a token stream.
  pub fn new(stream: TokenStream<'s>) -> Self {
    Self { stream }
  }

  /// Expect a specific token to be next. Throw an error if not.
  fn expect(&mut self, token: Token) -> Result<()> {
    let candidate = self.stream.next()?;

    if candidate == token {
      Ok(())
    } else {
      self.unexpected(candidate)
    }
  }

  fn unexpected<T>(&self, token: Token) -> Result<T> {
    self.locate(ParseError::TokenUnexpected(token))
  }

  fn locate<T>(&self, error: ParseError) -> Result<T> {
    Err((error, self.stream.last_range()).into())
  }

  /// Parse a single parsable structure.
  pub fn parse<T: Parse>(&mut self) -> Result<T> {
    T::parse(self)
  }
}

/// Implements the ability to parse a structure.
pub trait Parse where Self: Sized {
  /// Parse an instance of the type using a pre-constructed parser.
  fn parse(parser: &mut Parser) -> Result<Self>;
}

// Implement `Parse` for `Box<T>` where it's already implemented for `T`.
//
// This is just for convenience, as often parsed nonterminals must be wrapped
// in `Box` to avoid cyclic dependencies.
impl<T: Parse> Parse for Box<T> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    Ok(Box::new(parser.parse::<T>()?))
  }
}

/// An identifier naming a variable or function.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Identifier(Box<str>);

impl Parse for Identifier {
  fn parse(parser: &mut Parser) -> Result<Self> {
    match parser.stream.next()? {
      Token::Identifier(name) => Ok(Self(name)),
      token => parser.unexpected(token),
    }
  }
}

impl Tokenize for Identifier {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(Token::Identifier(self.0.clone()))
  }
}

/// An error that can occur while parsing the AST.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParseError {
  /// A static array length is negative.
  ArrayLengthNegative(BigInt),

  /// An ordering mismatch has occurred between different types of brackets.
  BracketMismatch,

  /// A comma is trailing where it should not be.
  CommaTrailing,

  /// A token that appears is unexpected (does not fit the grammar).
  TokenUnexpected(Token),
}

impl Display for ParseError {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::ArrayLengthNegative(length) => {
        write!(f, "negative array length: {length}")
      },
      Self::BracketMismatch => {
        write!(f, "mismatching brackets")
      },
      Self::CommaTrailing => {
        write!(f, "trailing comma unexpected")
      },
      Self::TokenUnexpected(token) => {
        write!(f, "unexpected token: {token}")
      },
    }
  }
}

impl std::error::Error for ParseError {}
