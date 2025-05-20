//! Function components of the AST.

use crate::{Result, Token, lexer::token::{TokenWriter, Tokenize}};
use super::{Block, Expression, Identifier, Parse, Parser, Type};

/// A concrete argument passed to a function or closure.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Argument {
  /// The optional label of the argument, if required by the function.
  pub label: Option<Identifier>,

  /// The expression evaluating to the value of the argument.
  pub value: Expression,
}

impl Parse for Argument {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let label = match parser.stream.peek(1)? {
      Token::Colon => {
        let label = parser.consume::<Identifier>()?;
        _ = parser.stream.next();
        Some(label)
      },
      _ => None,
    };

    let value = parser.consume::<Expression>()?;

    Ok(Self {
      label,
      value,
    })
  }
}

impl Tokenize for Argument {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    if let Some(label) = &self.label {
      writer.write(label)?;
      writer.write_one(Token::Colon)?;
    }

    writer.write(&self.value)?;
    Ok(())
  }
}

impl Parse for Box<[Argument]> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    // Check for an empty argument list as a special case.
    if parser.stream.peek(0)? == Token::ParenthesisRight {
      return Ok(Box::new([]));
    }

    let mut arguments = Vec::new();

    loop {
      // Parse the next argument.
      arguments.push(parser.consume::<Argument>()?);

      // Parse next argument if a comma is reached.
      // Stop parsing upon reaching a right parenthesis.
      match parser.stream.peek(0)? {
        Token::Comma => _ = parser.stream.next(),
        Token::ParenthesisRight => break,
        token => return parser.unexpected(token),
      }
    }

    Ok(arguments.into_boxed_slice())
  }
}

impl Tokenize for Box<[Argument]> {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.join(self, Token::Comma)
  }
}

/// A function parameter.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionParameter {
  /// The variable identifier of the parameter.
  pub identifier: Identifier,

  /// The type identifier associated with the variable.
  pub typ: Type,
}

impl Parse for FunctionParameter {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let identifier = parser.consume::<Identifier>()?;
    parser.expect(Token::Colon)?;

    let typ = parser.consume::<Type>()?;

    Ok(FunctionParameter {
      identifier,
      typ,
    })
  }
}

impl Tokenize for FunctionParameter {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.identifier)?;
    writer.write_one(Token::Colon)?;
    writer.write(&self.typ)?;

    Ok(())
  }
}

impl Parse for Box<[FunctionParameter]> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mut parameters = Vec::new();

    // Check for an empty parameter list as a special case.
    if parser.stream.peek(0)? == Token::ParenthesisRight {
      return Ok(Box::new([]));
    }

    loop {
      // Parse the next parameter.
      parameters.push(parser.consume::<FunctionParameter>()?);

      // Parse next parameter if a comma is reached.
      // Stop parsing upon reaching a right parenthesis.
      match parser.stream.peek(0)? {
        Token::Comma => _ = parser.stream.next(),
        Token::ParenthesisRight => break,
        token => return parser.unexpected(token),
      }
    }

    Ok(parameters.into_boxed_slice())
  }
}

impl Tokenize for Box<[FunctionParameter]> {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.join(self, Token::Comma)
  }
}

/// A function declaration.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Function {
  /// The identifier by which the function may be called.
  pub identifier: Identifier,

  /// The parameter list of the function.
  pub parameters: Box<[FunctionParameter]>,

  /// The optional return type identifier.
  pub return_type: Option<Type>,

  /// The block executed when the function is called.
  pub block: Block,
}

impl Parse for Function {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::Function)?;

    let identifier = parser.consume::<Identifier>()?;

    parser.expect(Token::ParenthesisLeft)?;
    let parameters = parser.consume::<Box<[FunctionParameter]>>()?;
    parser.expect(Token::ParenthesisRight)?;

    let return_type = match parser.stream.peek(0)? {
      Token::Arrow => {
        _ = parser.stream.next();
        Some(parser.consume::<Type>()?)
      },
      _ => None,
    };

    let block = parser.consume::<Block>()?;

    Ok(Function {
      identifier,
      parameters,
      return_type,
      block,
    })
  }
}

impl Tokenize for Function {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(Token::Function)?;
    writer.write(&self.identifier)?;
    writer.write_one(Token::ParenthesisLeft)?;
    writer.write(&self.parameters)?;
    writer.write_one(Token::ParenthesisRight)?;

    if let Some(return_type) = &self.return_type {
      writer.write_one(Token::Arrow)?;
      writer.write(return_type)?;
    }

    writer.write(&self.block)?;
    Ok(())
  }
}

impl Parse for Box<[Function]> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mut functions = Vec::new();

    while parser.stream.peek(0)? == Token::Function {
      let function = parser.consume::<Function>()?;
      functions.push(function);
    }

    Ok(functions.into_boxed_slice())
  }
}

impl Tokenize for Box<[Function]> {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    for function in self {
      writer.write(function)?;
    }

    Ok(())
  }
}
