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

use crate::{
  Result,
  Token,
  lexer::{TokenStream, token::{TokenWriter, Tokenize}},
};
use num::BigInt;
use std::{any::{Any, TypeId}, fmt::{self, Display, Formatter}};

/// A parser that produces an abstract syntax tree (AST).
pub struct Parser<'s> {
  stream: TokenStream<'s>,
  peeked: Option<Box<dyn Parse>>,
}

impl<'s> Parser<'s> {
  /// Parse and consume a single parsable structure.
  pub fn consume<T: Parse>(&mut self) -> Result<T> {
    if let Some(peeked) = self.peeked.take() {
      // Coerce the peeked item into an instance of `Any` so that it can be
      // downcasted below.
      let any = peeked as Box<dyn Any>;

      // Attempt to downcast the stored peeked item.
      //
      // Failure is an internal error resulting from a peek operation, followed
      // by a parse operation that requires a different type from the peek.
      let peeked = match any.downcast::<T>() {
        Ok(inner) => *inner,
        Err(actual) => {
          return self.locate(Error::TypeMismatch {
            actual: actual.type_id(),
            expected: TypeId::of::<T>(),
          });
        },
      };

      Ok(peeked)
    } else {
      T::parse(self)
    }
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

  /// Wraps a `parser::Error` in a `weave::Result` with location context.
  fn locate<T>(&self, error: Error) -> Result<T> {
    Err((error, self.stream.last_range()).into())
  }

  /// Constructs a `Parser` by reading from a token stream.
  pub fn new(stream: TokenStream<'s>) -> Self {
    Self {
      stream,
      peeked: None,
    }
  }

  /// Peeks the next item that can be parsed without consuming it.
  ///
  /// NOTE: While this method does not consume the parsed item, it does modify
  /// the underlying `stream` object to advance past it.
  pub fn peek<T: Parse>(&mut self) -> Result<&T> {
    if self.peeked.is_none() {
      let item = Box::new(T::parse(self)?);
      self.peeked = Some(item as Box<dyn Parse>);
    }

    // Fundamentally, since this function returns a _reference_ to the peeked
    // item, it must do this redundant type casting. This is due to the fact
    // that the reference must be pointing to the contents of the instance of
    // `Parser`, because the item is moved into `self`. However, when moved into
    // `self.peeked`, the underlying type is erased, so it must be downcast.

    // Cast the currently peeked item into a &dyn Any for downcasting.
    //
    // SAFETY: This unwrap is safe because if `peeked` was `None` before, it is
    // populated above.
    let any = self.peeked
      .as_ref()
      .map(|peeked| &**peeked)
      .unwrap() as &dyn Any;

    // Attempt to downcast the peeked item.
    // Failures are identical to those described in `parse`.
    match any.downcast_ref() {
      Some(inner) => Ok(inner),
      None => {
        return self.locate(Error::TypeMismatch {
          actual: any.type_id(),
          expected: TypeId::of::<T>(),
        })
      },
    }
  }

  fn unexpected<T>(&self, token: Token) -> Result<T> {
    self.locate(Error::TokenUnexpected(token))
  }
}

/// Implements the ability to parse a structure.
pub trait Parse: Any {
  /// Parse an instance of the type using a pre-constructed parser.
  fn parse(parser: &mut Parser) -> Result<Self> where Self: Sized;
}

// Implement `Parse` for `Box<T>` where it's already implemented for `T`.
//
// This is just for convenience, as often parsed nonterminals must be wrapped
// in `Box` to avoid cyclic dependencies.
impl<T: Parse> Parse for Box<T> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    Ok(Box::new(parser.consume::<T>()?))
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

impl Display for Identifier {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

/// An error that can occur while parsing the AST.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Error {
  /// A static array length is negative.
  ArrayLengthNegative(BigInt),

  /// An ordering mismatch has occurred between different types of brackets.
  BracketMismatch,

  /// A comma is trailing where it should not be.
  CommaTrailing,

  /// A token that appears is unexpected (does not fit the grammar).
  TokenUnexpected(Token),

  /// The expected type does not match the type of the last peeked item
  /// (internal).
  TypeMismatch {
    /// The ID of the actual type received after downcasting.
    actual: TypeId,

    /// The ID of the expected type in the generic.
    expected: TypeId,
  },
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::ArrayLengthNegative(length) => {
        write!(f, "negative array length: {length}")
      },
      Self::BracketMismatch => write!(f, "mismatching brackets"),
      Self::CommaTrailing => write!(f, "trailing comma unexpected"),
      Self::TokenUnexpected(token) => write!(f, "unexpected token: {token}"),
      Self::TypeMismatch { actual, expected } => {
        write!(f, "expected type {expected:?}, got {actual:?} (internal)")
      },
    }
  }
}

impl std::error::Error for Error {}
