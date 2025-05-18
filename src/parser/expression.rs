//! Expression components of the AST.

use crate::{Result, Token, lexer::{TokenWriter, Tokenize}};
use num::{BigInt, BigRational};
use super::{
  BinaryOperator,
  Block,
  Identifier,
  Parse,
  Parser,
  PostfixOperator,
  PrefixOperator,
  Type,
};

/// An array containing zero or more elements.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Array {
  /// The expressions which produce the values of the array when evaluated.
  pub elements: Box<[Expression]>,
}

impl Parse for Array {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::BracketLeft)?;

    let mut elements = Vec::new();

    // Check for an empty array as a special case.
    if parser.stream.peek(0)? == Token::BracketRight {
      _ = parser.stream.next();

      return Ok(Array {
        elements: Box::new([]),
      });
    }

    loop {
      elements.push(parser.parse::<Expression>()?);

      match parser.stream.next()? {
        Token::BracketRight => break,
        Token::Comma => {},
        token => return parser.unexpected(token),
      }
    }

    Ok(Array {
      elements: elements.into_boxed_slice(),
    })
  }
}

impl Tokenize for Array {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(Token::BracketLeft)?;
    writer.join(&self.elements, Token::Comma)?;
    writer.write_one(Token::BracketRight)?;

    Ok(())
  }
}

/// An ordered, finite list of elements enclosed in parentheses.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Tuple {
  /// The expressions producing the values containing in the tuple.
  pub elements: Box<[Expression]>,
}

impl Parse for Tuple {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::ParenthesisLeft)?;

    let mut elements = Vec::new();

    loop {
      // Handle the edge cases of 0 or 1 element(s).
      //
      // 0. The zero case is empty: `()`.
      // 1. The one case requires a trailing comma: `(1,)`. This differentiates
      //    a tuple from an expression enclosed with parentheses.
      if {
        parser.stream.peek(0)? == Token::ParenthesisRight
        && elements.len() <= 1
      } {
        break;
      }

      // Parse the element expression.
      let element = parser.parse::<Expression>()?;
      elements.push(element);

      match parser.stream.next()? {
        Token::Comma => {},
        Token::ParenthesisRight => break,
        token => return parser.unexpected(token),
      }
    }

    Ok(Self {
      elements: elements.into_boxed_slice(),
    })
  }
}

impl Tokenize for Tuple {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(Token::ParenthesisLeft)?;
    writer.join(&self.elements, Token::Comma)?;

    if self.elements.len() == 1 {
      writer.write_one(Token::Comma)?;
    }

    writer.write_one(Token::ParenthesisRight)?;
    Ok(())
  }
}

/// A single, optionally typed parameter of a closure.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ClosureParameter {
  /// Tge variable identifier of the parameter.
  pub identifier: Identifier,

  /// The optional type associated with the variable.
  /// If no type is specified, it will be inferred.
  pub typ: Option<Type>,
}

impl Parse for ClosureParameter {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let identifier = parser.parse::<Identifier>()?;

    let typ = if parser.stream.peek(0)? == Token::Colon {
      _ = parser.stream.next();
      Some(parser.parse::<Type>()?)
    } else {
      None
    };

    Ok(Self {
      identifier,
      typ,
    })
  }
}

impl Tokenize for ClosureParameter {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.identifier)?;

    if let Some(typ) = &self.typ {
      writer.write_one(Token::Colon)?;
      writer.write(typ)?;
    }

    Ok(())
  }
}

impl Parse for Box<[ClosureParameter]> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mut parameters = Vec::new();

    // Check for an empty parameter list as a special case.
    if parser.stream.peek(0)? == Token::ParenthesisRight {
      return Ok(Box::new([]));
    }

    loop {
      // Parse the next parameter.
      parameters.push(parser.parse::<ClosureParameter>()?);

      // Parse the next parameter if a comma is reached.
      // Stop upon reaching a right parenthesis.
      match parser.stream.peek(0)? {
        Token::Comma => _ = parser.stream.next(),
        Token::ParenthesisRight => break,
        token => return parser.unexpected(token),
      }
    }

    Ok(parameters.into_boxed_slice())
  }
}

impl Tokenize for Box<[ClosureParameter]> {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.join(self, Token::Comma)
  }
}

/// The body of a closure function.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ClosureBody {
  /// A closure body containing multiple statements and a return.
  Block(Block),

  /// A closure body that evaluates a single expression and implicitly returns.
  ///
  /// This variant must be boxed because otherwise it creates a cyclic
  /// dependency with `Expression`.
  Expression(Box<Expression>),
}

impl Parse for ClosureBody {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let body = match parser.stream.peek(0)? {
      Token::BraceLeft => Self::Block(parser.parse::<Block>()?),

      // TODO: Consider matching against the FIRST set of `Expression` instead
      // of redirecting all other tokens there, for better error messages.
      _ => Self::Expression(Box::new(parser.parse::<Expression>()?)),
    };

    Ok(body)
  }
}

impl Tokenize for ClosureBody {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    match self {
      Self::Block(block) => writer.write(block),
      Self::Expression(expression) => writer.write(expression),
    }
  }
}

/// An inline function that closes over variables in the local scope.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Closure {
  /// THe parameter list of the closure function.
  pub parameters: Box<[ClosureParameter]>,

  /// The expression evaluated as a result of calling the closure.
  pub body: ClosureBody,
}

impl Parse for Closure {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::ParenthesisLeft)?;
    let parameters = parser.parse::<Box<[ClosureParameter]>>()?;
    parser.expect(Token::ParenthesisRight)?;
    parser.expect(Token::Arrow)?;

    let body = parser.parse::<ClosureBody>()?;

    Ok(Self {
      parameters,
      body,
    })
  }
}

impl Tokenize for Closure {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(Token::ParenthesisLeft)?;
    writer.write(&self.parameters)?;
    writer.write_one(Token::ParenthesisRight)?;
    writer.write_one(Token::Arrow)?;
    writer.write(&self.body)?;

    Ok(())
  }
}

/// A literal expression (character, string, or number).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Literal {
  /// A single character.
  Character(char),

  /// A floating point number.
  Float(BigRational),

  /// An integer number.
  Integer(BigInt),

  /// A string.
  String(Box<str>),
}

impl Parse for Literal {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let literal = match parser.stream.next()? {
      Token::Character(c) => Literal::Character(c),
      Token::Float(float) => Literal::Float(float),
      Token::Integer(int) => Literal::Integer(int),
      Token::String(string) => Literal::String(string),
      token => return parser.unexpected(token),
    };

    Ok(literal)
  }
}

impl Tokenize for Literal {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    let token = match self {
      Self::Character(c) => Token::Character(*c),
      Self::Float(float) => Token::Float(float.clone()),
      Self::Integer(int) => Token::Integer(int.clone()),
      Self::String(string) => Token::String(string.clone()),
    };

    writer.write_one(token)?;
    Ok(())
  }
}

/// An expression that can be evaluated.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expression {
  /// Elements of an array, surrounded by brackets and joined by commas.
  Array(Array),

  /// Two sub-expressions joined by a binary operation.
  Binary {
    /// The expression on the left-hand side.
    left: Box<Expression>,

    /// The operator defining the binary operation.
    operator: BinaryOperator,

    /// The expression on the right-hand side.
    right: Box<Expression>,
  },

  /// A closure function.
  Closure(Closure),

  /// A single identifier.
  Identifier(Identifier),

  /// A single literal.
  Literal(Literal),

  /// Elements of a tuple, surrounded by parentheses and joined by commas.
  Tuple(Tuple),

  /// A postfix operation applied to a sub-expression.
  Postfix {
    /// The expression on which the operation is applied.
    inner: Box<Expression>,

    /// The operator defining the postfix operation.
    operator: PostfixOperator,
  },

  /// A prefix operation applied to a sub-expression.
  Prefix {
    /// The operator defining the prefix operation.
    operator: PrefixOperator,

    /// The expression on which the operation is applied.
    inner: Box<Expression>,
  },
}

impl Parse for Expression {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mut expression = match parser.stream.peek(0)? {
      // Arrays.
      Token::BracketLeft => {
        Expression::Array(parser.parse::<Array>()?)
      },

      // Identifiers.
      Token::Identifier(_) => {
        Expression::Identifier(parser.parse::<Identifier>()?)
      },

      // Enclosed expression, closure, or tuple.
      Token::ParenthesisLeft => {
        // Parsing an expression with a left parenthesis at the start is
        // challenging because it can be:
        // - An enclosed expression, .
        // - A closure.
        // - A tuple.
        //
        // This overlap in syntax forces the parser to peek arbitrarily many
        // tokens ahead to determine which of these three the expression is.
        // Picking between a tuple and a closure is especially tricky, because
        // the parameter list of a closure is a valid tuple. It is only when the
        // parser reaches the arrow that it can be sure the expression is a
        // closure.

        // Loop state variables.
        // TODO: Find a clever way to optimize this.
        let mut t = 1;
        let mut layer = 1;
        let mut comma_separated = false;

        // This loop simultaneously:
        // 1. Advances the lookahead index to the end of the parentheses. This
        //    is so that the next token can be checked for an arrow.
        // 2. Determines whether there is at least one comma separator in the
        //    outer loop of parentheses. This indicates that the expression must
        //    be either a closure or a tuple.
        loop {
          match parser.stream.peek(t)? {
            // If a comma appears in the outer parenthesis layer, then the
            // expression must be either a closure or a tuple.
            Token::Comma => {
              if layer == 1 {
                comma_separated = true;
              }
            },

            // Keep track of brackets layers.
            //
            // Here, the parser assumes that brackets are correctly grouped in
            // the right order for efficiency. If this turns out not to be
            // true, the parsing of the sub-expressions will reveal that.
            Token::BraceLeft
            | Token::BracketLeft
            | Token::ParenthesisLeft => layer += 1,

            Token::BraceRight
            | Token::BracketRight
            | Token::ParenthesisRight => layer -= 1,

            // Otherwise, ignore the token. It could be part of an inner
            // expression, a closure, or a tuple.
            _ => {},
          }

          // Increment the token lookahead index.
          t += 1;

          // If the layer is 0, then we have escaped the outer parentheses.
          // If the next token is an arrow, then this must be a closure.
          if layer == 0 {
            break;
          }
        }

        // If the next token is an arrow, the expression must be a closure.
        // `t` is pre-incremented, so it's the index of the next token.
        if parser.stream.peek(t)? == Token::Arrow {
          Self::Closure(parser.parse::<Closure>()?)
        } else if comma_separated || t == 2 {
          // If there are comma separators or it's empty, then it's a tuple.
          Self::Tuple(parser.parse::<Tuple>()?)
        } else {
          // Otherwise, it must be an inner expression.

          // Consume the left parenthesis.
          _ = parser.stream.next();

          // Parse the inner expression.
          let inner = parser.parse::<Expression>()?;

          // Check that it's capped by a right parenthesis.
          //
          // Despite checking the layers in the earlier loop, this is still
          // necessary. The parsed inner expression may have terminated before
          // the ending parenthesis, which should be a syntax error.
          parser.expect(Token::ParenthesisRight)?;

          inner
        }
      },

      // Literals.
      Token::Character(_)
      | Token::Float(_)
      | Token::Integer(_)
      | Token::String(_) => {
        Expression::Literal(parser.parse::<Literal>()?)
      },

      // Prefix operators.
      Token::Ampersand
      | Token::Asterisk
      | Token::Dash
      | Token::Exclamation
      | Token::MinusMinus
      | Token::Plus
      | Token::PlusPlus
      | Token::Tilde => {
        let operator = parser.parse::<PrefixOperator>()?;
        let inner = parser.parse::<Expression>()?;

        Expression::Prefix {
          operator,
          inner: Box::new(inner),
        }
      },

      // Unexpected token.
      token => return parser.unexpected(token),
    };

    // Match the expression tail.
    match parser.stream.peek(0)? {
      // Binary operators.
      Token::Ampersand
      | Token::AngleLeft
      | Token::AngleLeftEquals
      | Token::AngleRight
      | Token::AngleRightEquals
      | Token::Asterisk
      | Token::Caret
      | Token::Dash
      | Token::DoubleAmpersand
      | Token::DoubleAngleLeft
      | Token::DoubleAngleRight
      | Token::DoubleEquals
      | Token::DoublePipe
      | Token::ExclamationEquals
      | Token::Percent
      | Token::Pipe
      | Token::Plus
      | Token::Slash => {
        let operator = parser.parse::<BinaryOperator>()?;
        let right = parser.parse::<Box<Expression>>()?;

        expression = Expression::Binary {
          left: Box::new(expression),
          operator,
          right,
        };
      },

      // Postfix operators.
      Token::BracketLeft
      | Token::Dot
      | Token::DoubleColon
      | Token::MinusMinus
      | Token::ParenthesisLeft
      | Token::PlusPlus => {
        let operator = parser.parse::<PostfixOperator>()?;

        expression = Expression::Postfix {
          inner: Box::new(expression),
          operator,
        };
      },

      // Ignore all other tokens.
      // They must not be part of the expression.
      _ => {},
    }

    Ok(expression)
  }
}

impl Tokenize for Expression {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    match self {
      Self::Array(array) => {
        writer.write(array)?;
      },
      Self::Binary { left, operator, right } => {
        writer.write(left)?;
        writer.write(operator)?;
        writer.write(right)?;
      },
      Self::Closure(closure) => {
        writer.write(closure)?;
      },
      Self::Identifier(identifier) => {
        writer.write(identifier)?;
      },
      Self::Literal(literal) => {
        writer.write(literal)?;
      },
      Self::Tuple(tuple) => {
        writer.write(tuple)?;
      },
      Self::Postfix { inner, operator } => {
        writer.write(inner)?;
        writer.write(operator)?;
      },
      Self::Prefix { operator, inner } => {
        writer.write(operator)?;
        writer.write(inner)?;
      },
    }

    Ok(())
  }
}

