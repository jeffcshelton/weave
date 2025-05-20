//! Compilation unit components of the AST.

use crate::{Result, Token, lexer::token::{TokenWriter, Tokenize}};
use super::{Function, Import, Parse, Parser};

/// A single compilation unit, corresponding to one source file.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Unit {
  /// The imports at the top of the source file.
  pub imports: Box<[Import]>,

  /// The functions defined throughout the source file.
  pub functions: Box<[Function]>,
}

impl Parse for Unit {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let imports = parser.consume::<Box<[Import]>>()?;
    let functions = parser.consume::<Box<[Function]>>()?;

    parser.expect(Token::EOF)?;

    Ok(Unit {
      imports,
      functions,
    })
  }
}

impl Tokenize for Unit {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&*self.imports)?;
    writer.write(&self.functions)?;
    Ok(())
  }
}

