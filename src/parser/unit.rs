//! Compilation unit components of the AST.

use crate::{Result, Token, lexer::token::{TokenWriter, Tokenize}};
use super::{Declaration, Import, Parse, Parser};

/// A single compilation unit, corresponding to one source file.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Unit {
  /// The imports at the top of the source file.
  pub imports: Box<[Import]>,

  /// All top-level type and variable declarations.
  pub declarations: Box<[Declaration]>,
}

impl Parse for Unit {
  fn parse(parser: &mut Parser) -> Result<Self> {
    // Imports must be at the top of the file.
    let imports = parser.consume::<Box<[Import]>>()?;
    let declarations = parser.consume::<Box<[Declaration]>>()?;
    parser.expect(Token::EOF)?;

    Ok(Self { declarations, imports })
  }
}

impl Tokenize for Unit {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&*self.imports)?;
    writer.write(&*self.declarations)?;
    Ok(())
  }
}
