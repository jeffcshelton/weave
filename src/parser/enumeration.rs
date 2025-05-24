//! Enum definitions and parsing.

use crate::{lexer::token::{TokenWriter, Tokenize}, Result, Token};
use super::{Identifier, Parse, Parser, Type, Visibility};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct EnumVariantMember {
  pub identifier: Identifier,
  pub typ: Type,
}

impl Parse for EnumVariantMember {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let identifier = parser.consume::<Identifier>()?;
    parser.expect(Token::Colon)?;
    let typ = parser.consume::<Type>()?;

    Ok(Self {
      identifier,
      typ,
    })
  }
}

impl Tokenize for EnumVariantMember {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.identifier)?;
    writer.write_one(Token::Colon)?;
    writer.write(&self.typ)?;

    Ok(())
  }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum EnumVariantData {
  Structured {
    members: Box<[EnumVariantMember]>,
  },
  Tuple {
    subtypes: Box<[Type]>,
  },
}

impl EnumVariantData {
  fn parse_structured(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::BraceLeft)?;

    let members = parser.joined::<EnumVariantMember>(
      Token::Comma,
      Token::BraceRight,
    )?;

    parser.expect(Token::BraceRight)?;

    Ok(Self::Structured { members })
  }

  fn parse_tuple(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::ParenthesisLeft)?;

    let subtypes = parser.joined::<Type>(
      Token::Comma,
      Token::ParenthesisRight,
    )?;

    parser.expect(Token::ParenthesisRight)?;

    Ok(Self::Tuple { subtypes })
  }
}

impl Parse for EnumVariantData {
  fn parse(parser: &mut Parser) -> Result<Self> {
    match parser.stream.peek(0)? {
      Token::BraceLeft => Self::parse_structured(parser),
      Token::ParenthesisLeft => Self::parse_tuple(parser),
      token => parser.unexpected(token),
    }
  }
}

impl Tokenize for EnumVariantData {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    match self {
      Self::Structured { members } => {
        writer.write_one(Token::BraceLeft)?;
        writer.join(&**members, Token::Comma)?;
        writer.write_one(Token::BraceRight)?;
      },
      Self::Tuple { subtypes } => {
        writer.write_one(Token::ParenthesisLeft)?;
        writer.join(&**subtypes, Token::Comma)?;
        writer.write_one(Token::ParenthesisRight)?;
      },
    }

    Ok(())
  }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct EnumVariant {
  pub identifier: Identifier,
  pub data: Option<EnumVariantData>,
}

impl Parse for EnumVariant {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let identifier = parser.consume::<Identifier>()?;

    let data = match parser.stream.peek(0)? {
      Token::BraceLeft | Token::ParenthesisLeft => {
        Some(parser.consume::<EnumVariantData>()?)
      },
      Token::BraceRight  | Token::Comma => None,
      token => return parser.unexpected(token),
    };

    Ok(Self {
      identifier,
      data,
    })
  }
}

impl Tokenize for EnumVariant {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.identifier)?;

    if let Some(data) = &self.data {
      writer.write(data)?;
    }

    Ok(())
  }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Enum {
  pub visibility: Visibility,
  pub identifier: Identifier,
  pub variants: Box<[EnumVariant]>,
}

impl Parse for Enum {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let visibility = parser.consume::<Visibility>()?;
    parser.expect(Token::Enum)?;

    let identifier = parser.consume::<Identifier>()?;

    parser.expect(Token::BraceLeft)?;

    let variants = parser.joined::<EnumVariant>(
      Token::Comma,
      Token::BraceRight,
    )?;

    parser.expect(Token::BraceRight)?;

    Ok(Self {
      visibility,
      identifier,
      variants,
    })
  }
}

impl Tokenize for Enum {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.visibility)?;
    writer.write_one(Token::Enum)?;
    writer.write(&self.identifier)?;
    writer.write_one(Token::BraceLeft)?;
    writer.join(&self.variants, Token::Comma)?;
    writer.write_one(Token::BraceRight)?;

    Ok(())
  }
}
