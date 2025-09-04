use crate::{lexer::token::{TokenWriter, Tokenize}, Result, Token};
use super::{
  Class,
  Enum,
  Extension,
  Function,
  Parse,
  Parser,
  Struct,
  Variable,
  Visibility,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum DeclarationInner {
  Class(Class),
  Enum(Enum),
  Extension(Extension),
  Function(Function),
  Struct(Struct),
  Variable(Variable),
}

impl Parse for DeclarationInner {
  fn parse(parser: &mut Parser) -> Result<Self> {
    Ok(match parser.stream.peek(0)? {
      Token::Class => Self::Class(parser.consume::<Class>()?),
      Token::Enum => Self::Enum(parser.consume::<Enum>()?),
      Token::Extension => Self::Extension(parser.consume::<Extension>()?),
      Token::Function => Self::Function(parser.consume::<Function>()?),
      Token::Struct => Self::Struct(parser.consume::<Struct>()?),
      Token::Const
      | Token::Var => Self::Variable(parser.consume::<Variable>()?),
      token => return parser.unexpected(token),
    })
  }
}

impl Tokenize for DeclarationInner {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    match self {
      Self::Class(class) => writer.write(class),
      Self::Enum(enum_) => writer.write(enum_),
      Self::Extension(ext) => writer.write(ext),
      Self::Function(func) => writer.write(func),
      Self::Struct(struct_) => writer.write(struct_),
      Self::Variable(var) => writer.write(var),
    }
  }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Declaration {
  pub visibility: Visibility,
  pub inner: DeclarationInner,
}

impl Parse for Declaration {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let visibility = parser.consume::<Visibility>()?;
    let inner = parser.consume::<DeclarationInner>()?;
    Ok(Self { visibility, inner })
  }
}

impl Tokenize for Declaration {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.visibility)?;
    writer.write(&self.inner)?;
    Ok(())
  }
}

impl Parse for Box<[Declaration]> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mut declarations = Vec::new();

    while matches!(parser.stream.peek(0)?, Token::Public | Token::Private) {
      declarations.push(parser.consume::<Declaration>()?);
    }

    Ok(declarations.into_boxed_slice())
  }
}

impl Tokenize for [Declaration] {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    for decl in self {
      writer.write(decl)?;
    }

    Ok(())
  }
}
