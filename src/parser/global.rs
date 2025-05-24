//! Global variable definitions and parsing.

use crate::{lexer::token::{TokenWriter, Tokenize}, Result, Token};
use super::{Declaration, Parse, Parser, Visibility};

/// A declaration of a global variable.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Global {
  /// The visibility of the declared variable to outside access.
  pub visibility: Visibility,

  /// The underlying variable declaration of the global variable.
  pub declaration: Declaration,
}

impl Parse for Global {
  fn parse(parser: &mut Parser) -> Result<Self> where Self: Sized {
    let visibility = parser.consume::<Visibility>()?;
    let declaration = parser.consume::<Declaration>()?;
    parser.expect(Token::Semicolon)?;

    Ok(Self {
      visibility,
      declaration,
    })
  }
}

impl Tokenize for Global {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.visibility)?;
    writer.write(&self.declaration)?;
    writer.write_one(Token::Semicolon)?;

    Ok(())
  }
}

impl Tokenize for [Global] {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    for global in self {
      writer.write(global)?;
    }

    Ok(())
  }
}
