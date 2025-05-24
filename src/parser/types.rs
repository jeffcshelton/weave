//! Type components of the AST.

use crate::{Result, Token, lexer::token::{TokenWriter, Tokenize}};
use num::{BigInt, Signed};
use super::{Error, Expression, Identifier, Parse, Parser};

/// An expression producing a type when evaluated.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
  /// An array type.
  ///
  /// Example: `u32[10]`
  Array {
    /// The type of an individual element in the array.
    base: Box<Type>,

    /// The dimensions of the array, from left to right.
    dimensions: Box<[Option<BigInt>]>,
  },

  Enclosed(Box<Type>),

  Pointer {
    base: Box<Type>,
    is_const: bool,
  },

  Reference {
    base: Box<Type>,
    is_const: bool,
  },

  Scoped(Box<[Identifier]>),

  /// A tuple type.
  ///
  /// Example: `(u32, i32)`
  Tuple(Box<[Type]>),
}

impl Type {
  /// Parses the trailing array dimensions of a type expression.
  fn parse_dimensions(
    parser: &mut Parser,
  ) -> Result<Option<Box<[Option<BigInt>]>>> {
    let mut dimensions = Vec::new();

    // Handle array types, which are trailing.
    while parser.stream.peek(0)? == Token::BracketLeft {
      parser.stream.advance(1);

      let length = match parser.stream.peek(0)? {
        Token::Integer(int) => {
          // Check that the dimension isn't negative or zero.
          if !int.is_positive() {
            return parser.locate(Error::ArrayLengthInvalid(int));
          }

          parser.stream.advance(1);
          Some(int)
        },
        _ => None,
      };

      dimensions.push(length);
      parser.expect(Token::BracketRight)?;
    }

    if dimensions.len() > 0 {
      Ok(Some(dimensions.into_boxed_slice()))
    } else {
      Ok(None)
    }
  }

  fn parse_scoped(parser: &mut Parser) -> Result<Self> {
    // There must be a base identifier in the scoped type.
    //
    // In many cases, the base identifier is the only one. The `Scoped` variant
    // is made responsible for the lone identifier in this way.
    let base = parser.consume::<Identifier>()?;
    let mut idents = vec![base];

    // Keep parsing scope path components while there are `::` tokens.
    while parser.stream.peek(0)? == Token::DoubleColon {
      parser.stream.advance(1);
      idents.push(parser.consume::<Identifier>()?);
    }

    Ok(Self::Scoped(idents.into_boxed_slice()))
  }

  /// Parses a tuple or enclosed expression.
  fn parse_parenthesized(parser: &mut Parser) -> Result<Self> {
    parser.stream.advance(1);

    // Special case for an empty tuple type expression.
    if parser.stream.peek(0)? == Token::ParenthesisRight {
      return Ok(Self::Tuple(Box::new([])));
    }

    // Parse the first element, which is special because it could still be
    // either a tuple or an enclosed type expression.
    let first = parser.consume::<Type>()?;

    // Check if there is a comma following the first type expression.
    // If so, it must be a tuple. If not, it's an enclosed type.
    match parser.stream.next()? {
      Token::Comma => {},
      Token::ParenthesisRight => {
        return Ok(Self::Enclosed(Box::new(first)));
      },
      token => return parser.unexpected(token),
    }

    // If the code reaches here, the type must be a tuple.
    let mut subtypes = vec![first];

    // Special case for a 1-element tuple.
    // This is required because all other tuples do not have a trailing comma.
    if parser.stream.peek(0)? == Token::ParenthesisRight {
      return Ok(Self::Tuple(subtypes.into_boxed_slice()));
    }

    // Parse all the subtypes of the tuple in order.
    loop {
      subtypes.push(parser.consume::<Type>()?);

      // If the next token is a comma, then the tuple must continue.
      // If the next token is a right parenthesis, then it must end.
      // Any other token is unexpected in this context.
      match parser.stream.next()? {
        Token::Comma => {},
        Token::ParenthesisRight => break,
        token => return parser.unexpected(token),
      }
    }

    Ok(Self::Tuple(subtypes.into_boxed_slice()))
  }

  fn parse_pointer(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::Asterisk)?;

    let is_const = match parser.stream.peek(0)? {
      Token::Const => {
        parser.stream.advance(1);
        true
      },
      _ => false,
    };

    let base = parser.consume::<Type>()?;

    Ok(Self::Pointer {
      base: Box::new(base),
      is_const,
    })
  }

  fn parse_reference(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::Ampersand)?;

    let is_const = match parser.stream.peek(0)? {
      Token::Const => {
        parser.stream.advance(1);
        true
      },
      _ => false,
    };

    let base = parser.consume::<Type>()?;

    Ok(Self::Reference {
      base: Box::new(base),
      is_const,
    })
  }
}

impl Parse for Type {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mut typ = match parser.stream.peek(0)? {
      Token::Asterisk => Self::parse_pointer(parser)?,
      Token::Identifier(_) => Self::parse_scoped(parser)?,
      Token::ParenthesisLeft => Self::parse_parenthesized(parser)?,
      token => return parser.unexpected(token),
    };

    // Only wrap the type in an array type if there were trailing dimensions.
    if let Some(dimensions) = Self::parse_dimensions(parser)? {
      typ = Type::Array {
        base: Box::new(typ),
        dimensions,
      };
    }

    Ok(typ)
  }
}

impl Tokenize for Type {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    match self {
      Self::Array { base, dimensions } => {
        writer.write(&**base)?;

        for dim in dimensions {
          writer.write_one(Token::BracketLeft)?;

          if let Some(dim) = dim {
            writer.write_one(Token::Integer(dim.clone()))?;
          }

          writer.write_one(Token::BracketRight)?;
        }
      },
      Self::Enclosed(inner) => {
        writer.write_one(Token::ParenthesisLeft)?;
        writer.write(&**inner)?;
        writer.write_one(Token::ParenthesisRight)?;
      },
      Self::Pointer { base, is_const } => {
        writer.write_one(Token::Asterisk)?;

        if *is_const {
          writer.write_one(Token::Const)?;
        }

        writer.write(&**base)?;
      },
      Self::Reference { base, is_const } => {
        writer.write_one(Token::Ampersand)?;

        if *is_const {
          writer.write_one(Token::Const)?;
        }

        writer.write(&**base)?;
      },
      Self::Scoped(idents) => {
        if let Some(first) = idents.first() {
          writer.write(first)?;

          for ident in &idents[1..] {
            writer.write_one(Token::Colon)?;
            writer.write(ident)?;
          }
        }
      },
      Self::Tuple(subtypes) => {
        writer.write_one(Token::ParenthesisLeft)?;
        writer.join(subtypes, Token::Comma)?;

        if subtypes.len() == 1 {
          writer.write_one(Token::Comma)?;
        }

        writer.write_one(Token::ParenthesisRight)?
      },
    }

    Ok(())
  }
}
