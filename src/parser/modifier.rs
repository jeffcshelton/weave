//! Modifiers, such as visibility.

use crate::{lexer::token::{TokenWriter, Tokenize}, Result, Token};
use super::{Parse, Parser};

/// A modifier indicating whether a declared item is constant or mutable.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Mutability {
  /// The declared item is constant. It may not be mutated.
  Constant,

  /// The declared item is mutable. It may be mutated.
  Mutable,
}

impl Parse for Mutability {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mutability = match parser.stream.next()? {
      Token::Const => Self::Constant,
      Token::Var => Self::Mutable,
      token => return parser.unexpected(token),
    };

    Ok(mutability)
  }
}

impl Tokenize for Mutability {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    let token = match self {
      Self::Constant => Token::Const,
      Self::Mutable => Token::Var,
    };

    writer.write_one(token)
  }
}

/// A modifier specifying access in layers outside of a definition.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Visibility {
  /// Private (restricted) access.
  Private,

  /// Public (unrestricted) access.
  Public,
}

impl Parse for Visibility {
  fn parse(parser: &mut Parser) -> Result<Self> where Self: Sized {
    let visibility = match parser.stream.next()? {
      Token::Private => Self::Private,
      Token::Public => Self::Public,
      token => return parser.unexpected(token),
    };

    Ok(visibility)
  }
}

impl Tokenize for Visibility {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    let token = match self {
      Self::Private => Token::Private,
      Self::Public => Token::Public,
    };

    writer.write_one(token)
  }
}
