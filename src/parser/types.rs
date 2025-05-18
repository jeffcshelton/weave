//! Type components of the AST.

use crate::{Result, Token, lexer::{TokenWriter, Tokenize}};
use num::{BigInt, Signed};
use super::{Expression, Parse, ParseError, Parser};

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
    size: Box<[Option<BigInt>]>,
  },

  /// An expression that evaluates to a type.
  /// TODO: Consider changing this to `TypeExpression`.
  Expression(Expression),

  /// A tuple type.
  ///
  /// Example: `(u32, i32)`
  Tuple(Box<[Type]>),
}

impl Parse for Type {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let typ = match parser.stream.peek(0)? {
      Token::Identifier(_) => {
        let expression = parser.parse::<Expression>()?;
        let mut size = Vec::new();

        // Handle array types.
        while parser.stream.peek(0)? == Token::BracketLeft {
          _ = parser.stream.next();

          let length = match parser.stream.peek(0)? {
            Token::Integer(int) => {
              if int.is_negative() {
                return parser.locate(ParseError::ArrayLengthNegative(int));
              }

              _ = parser.stream.next();
              Some(int)
            }
            _ => None,
          };

          size.push(length);
          parser.expect(Token::BracketRight)?;
        }

        let base = Type::Expression(expression);

        if size.len() > 0 {
          Type::Array {
            base: Box::new(base),
            size: size.into_boxed_slice(),
          }
        } else {
          base
        }
      },
      Token::ParenthesisLeft => {
        _ = parser.stream.next();

        let mut subtypes = Vec::new();

        loop {
          let subtype = parser.parse::<Type>()?;
          subtypes.push(subtype);

          match parser.stream.next()? {
            Token::Comma => {},
            Token::ParenthesisRight => break,
            token => return parser.unexpected(token),
          }
        }

        Type::Tuple(subtypes.into_boxed_slice())
      },
      token => return parser.unexpected(token),
    };

    Ok(typ)
  }
}

impl Tokenize for Type {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    match self {
      Self::Array { base, size } => {
        writer.write(base)?;

        for dimension in size {
          writer.write_one(Token::BracketLeft)?;

          if let Some(dimension) = dimension {
            writer.write_one(Token::Integer(dimension.clone()))?;
          }

          writer.write_one(Token::BracketRight)?;
        }
      },
      Self::Expression(expression) => writer.write(expression)?,
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
