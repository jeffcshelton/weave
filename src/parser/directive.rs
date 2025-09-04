//! Compiler directives parsing.

use crate::{lexer::token::{TokenWriter, Tokenize}, Result, Token};
use super::{Expression, Identifier, Parse, Parser};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DirectiveArgument {
  pub param: Identifier,
  pub value: Expression,
}

impl Parse for DirectiveArgument {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let param = parser.consume::<Identifier>()?;
    parser.expect(Token::Colon)?;
    let value = parser.consume::<Expression>()?;

    Ok(Self { param, value })
  }
}

impl Tokenize for DirectiveArgument {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.param)?;
    writer.write_one(Token::Colon)?;
    writer.write(&self.value)?;

    Ok(())
  }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Directive {
  pub arguments: Box<[DirectiveArgument]>,
  pub target: Identifier,
}

impl Parse for Directive {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::At)?;
    let target = parser.consume::<Identifier>()?;
    parser.expect(Token::ParenthesisLeft)?;
    let arguments = parser.joined(Token::Comma, Token::ParenthesisRight)?;
    parser.expect(Token::ParenthesisRight)?;

    Ok(Self { arguments, target })
  }
}

impl Tokenize for Directive {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(Token::At)?;
    writer.write(&self.target)?;
    writer.write_one(Token::ParenthesisLeft)?;
    writer.join(&self.arguments, Token::Comma)?;
    writer.write_one(Token::ParenthesisRight)?;

    Ok(())
  }
}

impl Parse for Option<Directive> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    if parser.stream.peek(0)? == Token::At {
      Ok(Some(parser.consume::<Directive>()?))
    } else {
      Ok(None)
    }
  }
}

impl Tokenize for Option<Directive> {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    if let Some(directive) = self {
      writer.write(directive)?;
    }

    Ok(())
  }
}
