//! Class and struct definitions and parsing.

use crate::{lexer::token::{TokenWriter, Tokenize}, Result, Token};
use super::{
  Block,
  FunctionParameter,
  Identifier,
  Mutability,
  Parse,
  Parser,
  Type,
  Visibility,
};

/// A definition of a member variable of a class or struct.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Member {
  /// The visibility of the member variable to outside access.
  pub visibility: Visibility,

  /// Whether the member is mutable after construction.
  pub mutability: Mutability,

  /// The identifier, or variable name, of the member.
  pub identifier: Identifier,

  /// The type of the member variable.
  pub typ: Type,
}

impl Parse for Member {
  fn parse(parser: &mut Parser) -> Result<Self> where Self: Sized {
    let visibility = parser.consume::<Visibility>()?;
    let mutability = parser.consume::<Mutability>()?;
    let identifier = parser.consume::<Identifier>()?;
    parser.expect(Token::Colon)?;
    let typ = parser.consume::<Type>()?;
    parser.expect(Token::Semicolon)?;

    Ok(Self {
      visibility,
      mutability,
      identifier,
      typ,
    })
  }
}

impl Tokenize for Member {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.visibility)?;
    writer.write(&self.mutability)?;
    writer.write(&self.identifier)?;
    writer.write_one(Token::Colon)?;
    writer.write(&self.typ)?;

    Ok(())
  }
}

impl Parse for Box<[Member]> {
  fn parse(parser: &mut Parser) -> Result<Self> where Self: Sized {
    let mut members = Vec::new();

    while parser.stream.peek(3)? == Token::Colon {
      members.push(parser.consume::<Member>()?);
    }

    Ok(members.into_boxed_slice())
  }
}

impl Tokenize for [Member] {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    for member in self {
      writer.write(member)?;
    }

    Ok(())
  }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SelfParameter {
  pub reference: bool,
}

impl Parse for SelfParameter {
  fn parse(parser: &mut Parser) -> Result<Self> where Self: Sized {
    let reference = match parser.stream.peek(0)? {
      Token::Ampersand => {
        parser.stream.advance(1);
        true
      },
      _ => false,
    };

    parser.expect(Token::Self_)?;

    Ok(Self { reference })
  }
}

impl Tokenize for SelfParameter {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    if self.reference {
      writer.write_one(Token::Ampersand)?;
    }

    writer.write_one(Token::Self_)?;
    Ok(())
  }
}

impl Parse for Option<SelfParameter> {
  fn parse(parser: &mut Parser) -> Result<Self> where Self: Sized {
    Ok(match parser.stream.peek(0)? {
      Token::Ampersand
      | Token::Self_ => Some(parser.consume::<SelfParameter>()?),

      _ => None,
    })
  }
}

impl Tokenize for Option<SelfParameter> {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    if let Some(param) = self {
      writer.write(param)?;
    }

    Ok(())
  }
}

/// A method associated with a class.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Method {
  /// The visibility of the method to outside callers.
  pub visibility: Visibility,

  /// Whether the method can be evaluated at compile time.
  pub is_const: bool,

  /// The identifier, or name, of the method.
  pub identifier: Identifier,

  /// The type of the instance parameter, identified by `self`.
  /// If this is not populated, then the method is static.
  ///
  /// The better identifier here is `self`, but that is a reserved keyword.
  pub instance: Option<SelfParameter>,

  /// The additional parameters of the method besides `self`.
  pub parameters: Box<[FunctionParameter]>,

  /// The optional return type of the method.
  /// If the return type is not supplied, it is implied to be void.
  pub return_type: Option<Type>,

  /// The block of statements executed when the method is called.
  pub block: Block,
}

impl Method {
  pub fn is_static(&self) -> bool {
    self.instance.is_none()
  }
}

impl Parse for Method {
  fn parse(parser: &mut Parser) -> Result<Self> where Self: Sized {
    let visibility = parser.consume::<Visibility>()?;

    let is_const = match parser.stream.peek(0)? {
      Token::Const => {
        parser.stream.advance(1);
        true
      },
      _ => false,
    };

    parser.expect(Token::Function)?;
    let identifier = parser.consume::<Identifier>()?;
    parser.expect(Token::ParenthesisLeft)?;
    let instance = parser.consume::<Option<SelfParameter>>()?;

    // If the `self` parameter exists, then it's necessary to consume the
    // leading comma before parsing the remaining parameters. If there is no
    // comma after `self`, then it must be the end of the parameter list.
    //
    // However, if there is no `self` parameter, then the remaining parameters
    // can be parsed as normal, because no extra comma will exist.
    let parameters = if instance.is_some() {
      match parser.stream.peek(0)? {
        Token::Comma => {
          parser.stream.advance(1);
          let parameters = parser.consume::<Box<[FunctionParameter]>>()?;

          // Check that the parameter list has at least one element to avoid the
          // edge case of accepting a trailing comma after the `self`.
          if parameters.len() == 0 {
            let next = parser.stream.peek(0)?;
            return parser.unexpected(next);
          }

          parameters
        },
        Token::ParenthesisRight => Box::new([]),
        token => return parser.unexpected(token),
      }
    } else {
      parser.consume::<Box<[FunctionParameter]>>()?
    };

    // Expect that a right parenthesis closes the parameter list.
    parser.expect(Token::ParenthesisRight)?;

    // Optionally parse the return type.
    // If there is no return type, it is implied to be void.
    let return_type = match parser.stream.peek(0)? {
      Token::Arrow => {
        parser.stream.advance(1);
        Some(parser.consume::<Type>()?)
      },
      _ => None,
    };

    // Parse the body of the method.
    let block = parser.consume::<Block>()?;

    Ok(Self {
      visibility,
      is_const,
      identifier,
      instance,
      parameters,
      return_type,
      block,
    })
  }
}

impl Tokenize for Method {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.visibility)?;

    if self.is_const {
      writer.write_one(Token::Const)?;
    }

    writer.write_one(Token::Function)?;
    writer.write(&self.identifier)?;
    writer.write_one(Token::ParenthesisLeft)?;
    writer.write(&self.instance)?;

    if self.instance.is_some() && !self.parameters.is_empty() {
      writer.write_one(Token::Comma)?;
    }

    writer.write(&*self.parameters)?;
    writer.write_one(Token::ParenthesisRight)?;

    if let Some(return_type) = &self.return_type {
      writer.write_one(Token::Arrow)?;
      writer.write(return_type)?;
    }

    writer.write(&self.block)?;

    Ok(())
  }
}

impl Parse for Box<[Method]> {
  fn parse(parser: &mut Parser) -> Result<Self> where Self: Sized {
    let mut methods = Vec::new();

    while matches!(parser.stream.peek(0)?, Token::Private | Token::Public) {
      methods.push(parser.consume::<Method>()?);
    }

    Ok(methods.into_boxed_slice())
  }
}

impl Tokenize for [Method] {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    for method in self {
      writer.write(method)?;
    }

    Ok(())
  }
}

/// A definition of a class.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Class {
  /// The visibility of the class to outside instantiation.
  pub visibility: Visibility,

  /// The identifier, or type name, of the class.
  pub identifier: Identifier,

  /// The parent class and any interfaces which the class must implement.
  pub parents: Box<[Type]>,

  /// The set of all member variables of the class.
  pub members: Box<[Member]>,

  /// The set of all methods of the class.
  pub methods: Box<[Method]>,
}

impl Parse for Class {
  fn parse(parser: &mut Parser) -> Result<Self> where Self: Sized {
    let visibility = parser.consume::<Visibility>()?;
    parser.expect(Token::Class)?;

    let identifier = parser.consume::<Identifier>()?;

    // Check for a parent class or interfaces.
    let parents = match parser.stream.peek(0)? {
      Token::Colon => {
        parser.stream.advance(1);

        let first = parser.consume::<Type>()?;
        let mut parents = vec![first];

        while parser.stream.peek(0)? == Token::Comma {
          parser.stream.advance(1);
          parents.push(parser.consume::<Type>()?);
        }

        parents.into_boxed_slice()
      },
      _ => Box::new([]),
    };

    // Parse the body of the class.
    parser.expect(Token::BraceLeft)?;
    let members = parser.consume::<Box<[Member]>>()?;
    let methods = parser.consume::<Box<[Method]>>()?;
    parser.expect(Token::BraceRight)?;

    Ok(Self {
      visibility,
      identifier,
      parents,
      members,
      methods,
    })
  }
}

impl Tokenize for Class {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.visibility)?;
    writer.write_one(Token::Class)?;
    writer.write(&self.identifier)?;
    writer.write_one(Token::BraceLeft)?;
    writer.write(&*self.members)?;
    writer.write(&*self.methods)?;
    writer.write_one(Token::BraceRight)?;

    Ok(())
  }
}

impl Tokenize for [Class] {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    for class in self {
      writer.write(class)?;
    }

    Ok(())
  }
}

/// A definition of a struct.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Struct {
  /// The visibility of the struct to outside instantiation.
  pub visibility: Visibility,

  /// The identifier, or type name, of the struct.
  pub identifier: Identifier,

  /// The set of all members variables of the struct.
  pub members: Box<[Member]>,
}

impl Parse for Struct {
  fn parse(parser: &mut Parser) -> Result<Self> where Self: Sized {
    let visibility = parser.consume::<Visibility>()?;
    parser.expect(Token::Struct)?;
    let identifier = parser.consume::<Identifier>()?;
    parser.expect(Token::BraceLeft)?;
    let members = parser.consume::<Box<[Member]>>()?;
    parser.expect(Token::BraceRight)?;

    Ok(Self {
      visibility,
      identifier,
      members,
    })
  }
}

impl Tokenize for Struct {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.visibility)?;
    writer.write_one(Token::Struct)?;
    writer.write(&self.identifier)?;
    writer.write_one(Token::BraceLeft)?;
    writer.write(&*self.members)?;
    writer.write_one(Token::BraceRight)?;

    Ok(())
  }
}

impl Tokenize for [Struct] {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    for struct_ in self {
      writer.write(struct_)?;
    }

    Ok(())
  }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Extension {
  pub visibility: Visibility,
  pub class: Identifier,
  pub interfaces: Box<[Type]>,
  pub methods: Box<[Method]>,
}

impl Parse for Extension {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let visibility = parser.consume::<Visibility>()?;
    parser.expect(Token::Extension)?;

    let class = parser.consume::<Identifier>()?;

    let interfaces = match parser.stream.peek(0)? {
      Token::Colon => {
        parser.stream.advance(1);

        let interfaces = parser.joined(Token::Comma, Token::BraceLeft)?;

        // If the colon is present, at least one interface must be specified.
        // This call to parse a `Type` will intentionally fail.
        //
        // TODO: Potentially replace this with its own error.
        if interfaces.is_empty() {
          parser.consume::<Type>()?;
        }

        interfaces
      },

      // If there is no colon, there must be no interfaces specified.
      _ => Box::new([]),
    };

    parser.expect(Token::BraceLeft)?;
    let methods = parser.consume::<Box<[Method]>>()?;
    parser.expect(Token::BraceRight)?;

    Ok(Self {
      visibility,
      class,
      interfaces,
      methods,
    })
  }
}

impl Tokenize for Extension {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.visibility)?;
    writer.write_one(Token::Extension)?;
    writer.write(&self.class)?;
    writer.write(&*self.methods)?;

    Ok(())
  }
}
