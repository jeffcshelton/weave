//! Components related to parsing a Weave abstract syntax tree (AST).

use crate::{lexer::{Token, TokenStream, TokenWriter, Tokenize}, Result};
use num::{BigInt, BigRational, Signed};
use std::fmt::{self, Display, Formatter};

/// A parser that produces an abstract syntax tree (AST).
#[derive(Clone, Debug)]
pub struct Parser<'s> {
  stream: TokenStream<'s>,
}

impl<'s> Parser<'s> {
  /// Constructs a `Parser` by reading from a token stream.
  pub fn new(stream: TokenStream<'s>) -> Self {
    Self { stream }
  }

  /// Expect a specific token to be next. Throw an error if not.
  fn expect(&mut self, token: Token) -> Result<()> {
    let candidate = self.stream.next()?;

    if candidate == token {
      Ok(())
    } else {
      self.unexpected(candidate)
    }
  }

  fn unexpected<T>(&self, token: Token) -> Result<T> {
    self.locate(ParseError::TokenUnexpected(token))
  }

  fn locate<T>(&self, error: ParseError) -> Result<T> {
    Err((error, self.stream.last_range()).into())
  }

  /// Parse a single parsable structure.
  pub fn parse<T: Parse>(&mut self) -> Result<T> {
    T::parse(self)
  }
}

/// Implements the ability to parse a structure.
pub trait Parse where Self: Sized {
  /// Parse an instance of the type using a pre-constructed parser.
  fn parse(parser: &mut Parser) -> Result<Self>;
}

// Implement `Parse` for `Box<T>` where it's already implemented for `T`.
//
// This is just for convenience, as often parsed nonterminals must be wrapped
// in `Box` to avoid cyclic dependencies.
impl<T: Parse> Parse for Box<T> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    Ok(Box::new(parser.parse::<T>()?))
  }
}

/// An identifier naming a variable or function.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Identifier(Box<str>);

impl Parse for Identifier {
  fn parse(parser: &mut Parser) -> Result<Self> {
    match parser.stream.next()? {
      Token::Identifier(name) => Ok(Self(name)),
      token => parser.unexpected(token),
    }
  }
}

impl Tokenize for Identifier {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(Token::Identifier(self.0.clone()))
  }
}

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

/// A block of statements that can be executed.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Block {
  /// The statements which are executed in order when the block is executed.
  pub statements: Box<[Statement]>,
}

impl Parse for Block {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::BraceLeft)?;
    let statements = parser.parse::<Box<[Statement]>>()?;
    parser.expect(Token::BraceRight)?;

    Ok(Self { statements })
  }
}

impl Tokenize for Block {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(Token::BraceLeft)?;
    writer.write(&self.statements)?;
    writer.write_one(Token::BraceRight)?;
    Ok(())
  }
}

/// An operator that requires left and right expressions as arguments.
#[deny(unused)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BinaryOperator {
  /// Bitwise AND.
  BitwiseAnd,

  /// Bitwise OR.
  BitwiseOr,

  /// Integer or float division.
  Divide,

  /// Boolean equality check.
  Equals,

  /// Boolean greater than check.
  GreaterThan,

  /// Boolean greater than or equal to check.
  GreaterThanOrEqualTo,

  /// Boolean less than check.
  LessThan,

  /// Boolean less than or equal to check.
  LessThanOrEqualTo,

  /// Logical (boolean) AND.
  LogicalAnd,

  /// Logical (boolean) OR.
  LogicalOr,

  /// Subtraction.
  Minus,

  /// Integer remainder.
  Modulo,

  /// Multiplication.
  Multiply,

  /// Boolean negated equality test.
  NotEquals,

  /// Addition.
  Plus,

  /// Bitwise shift left (big endian).
  ShiftLeft,

  /// Bitwise shift right (big endian).
  ShiftRight,

  /// Bitwise exclusive OR.
  Xor,
}

impl Parse for BinaryOperator {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let operator = match parser.stream.next()? {
      Token::Ampersand => Self::BitwiseAnd,
      Token::AngleLeft => Self::LessThan,
      Token::AngleLeftEquals => Self::LessThanOrEqualTo,
      Token::AngleRight => Self::GreaterThan,
      Token::AngleRightEquals => Self::GreaterThanOrEqualTo,
      Token::Asterisk => Self::Multiply,
      Token::Caret => Self::Xor,
      Token::Dash => Self::Minus,
      Token::DoubleAmpersand => Self::LogicalAnd,
      Token::DoubleAngleLeft => Self::ShiftLeft,
      Token::DoubleAngleRight => Self::ShiftRight,
      Token::DoubleEquals => Self::Equals,
      Token::DoublePipe => Self::LogicalOr,
      Token::ExclamationEquals => Self::NotEquals,
      Token::Percent => Self::Modulo,
      Token::Pipe => Self::BitwiseOr,
      Token::Plus => Self::Plus,
      Token::Slash => Self::Divide,
      token => return parser.unexpected(token),
    };

    Ok(operator)
  }
}

impl Tokenize for BinaryOperator {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    let token = match self {
      Self::BitwiseAnd => Token::Ampersand,
      Self::BitwiseOr => Token::Pipe,
      Self::Divide => Token::Slash,
      Self::Equals => Token::DoubleEquals,
      Self::GreaterThan => Token::AngleRight,
      Self::GreaterThanOrEqualTo => Token::AngleRightEquals,
      Self::LessThan => Token::AngleLeft,
      Self::LessThanOrEqualTo => Token::AngleLeftEquals,
      Self::LogicalAnd => Token::DoubleAmpersand,
      Self::LogicalOr => Token::DoublePipe,
      Self::Minus => Token::Dash,
      Self::Modulo => Token::Percent,
      Self::Multiply => Token::Asterisk,
      Self::NotEquals => Token::ExclamationEquals,
      Self::Plus => Token::Plus,
      Self::ShiftLeft => Token::DoubleAngleLeft,
      Self::ShiftRight => Token::DoubleAngleRight,
      Self::Xor => Token::Caret,
    };

    writer.write_one(token)
  }
}

/// A unary operator that is placed before the expression it operates upon.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum PrefixOperator {
  /// Bitwise NOT.
  BitwiseNot,

  /// Prefix decrement.
  Decrement,

  /// Dereference.
  Dereference,

  /// Prefix increment.
  Increment,

  /// Logical (boolean) NOT.
  LogicalNot,

  /// Negation.
  Negative,

  /// No-op.
  Positive,

  /// Take a reference to.
  Reference,
}

impl Parse for PrefixOperator {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let operator = match parser.stream.next()? {
      Token::Ampersand => Self::Reference,
      Token::Asterisk => Self::Dereference,
      Token::Dash => Self::Negative,
      Token::Exclamation => Self::LogicalNot,
      Token::MinusMinus => Self::Decrement,
      Token::Plus => Self::Positive,
      Token::PlusPlus => Self::Increment,
      Token::Tilde => Self::BitwiseNot,
      token => return parser.unexpected(token),
    };

    Ok(operator)
  }
}

impl Tokenize for PrefixOperator {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    let token = match self {
      Self::BitwiseNot => Token::Tilde,
      Self::Decrement => Token::MinusMinus,
      Self::Dereference => Token::Asterisk,
      Self::Increment => Token::PlusPlus,
      Self::LogicalNot => Token::Exclamation,
      Self::Negative => Token::Dash,
      Self::Positive => Token::Plus,
      Self::Reference => Token::Ampersand,
    };

    writer.write_one(token)
  }
}

/// A unary operator that is placed after the expression it operates upon.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PostfixOperator {
  /// Function or closure call.
  Call {
    /// The argument expressions to the call.
    arguments: Box<[Argument]>,
  },

  /// Postfix decrement.
  Decrement,

  /// Postfix increment.
  Increment,

  /// Array element indexing.
  Index {
    /// The expression producing the array index.
    expression: Box<Expression>,
  },

  /// Member access.
  ///
  /// This is not a binary operation because it does not combine two independent
  /// expressions to produce a new value. The right expression is fundamentally
  /// dependent on the value of the left expression. This is not true for binary
  /// operations.
  Member {
    /// The right-side expression.
    ///
    /// Not all expressions are valid here, but that will be caught later in
    /// analysis.
    right: Box<Expression>,
  },

  /// Scope resolution.
  ///
  /// This is not a binary operation because it does not combine two independent
  /// expressions to produce a new value. The right expression is fundamentally
  /// dependent on the value of the left expression. This is not true for binary
  /// operations.
  Scope {
    /// The right-side expression.
    ///
    /// Not all expressions are valid here, but that will be caught later in
    /// analysis.
    right: Box<Expression>,
  },
}

impl Parse for PostfixOperator {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let operator = match parser.stream.next()? {
      Token::BracketLeft => {
        let expression = parser.parse::<Box<Expression>>()?;
        parser.expect(Token::BracketRight)?;

        Self::Index { expression }
      },
      Token::Dot => {
        Self::Member {
          right: parser.parse::<Box<Expression>>()?,
        }
      },
      Token::DoubleColon => {
        Self::Scope {
          right: parser.parse::<Box<Expression>>()?,
        }
      },
      Token::MinusMinus => Self::Decrement,
      Token::ParenthesisLeft => {
        let arguments = parser.parse::<Box<[Argument]>>()?;
        parser.expect(Token::ParenthesisRight)?;

        Self::Call { arguments }
      },
      Token::PlusPlus => Self::Increment,
      token => return parser.unexpected(token),
    };

    Ok(operator)
  }
}

impl Tokenize for PostfixOperator {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    match self {
      Self::Call { arguments } => {
        writer.write_one(Token::ParenthesisLeft)?;
        writer.write(arguments)?;
        writer.write_one(Token::ParenthesisRight)?;
      },
      Self::Decrement => writer.write_one(Token::MinusMinus)?,
      Self::Increment => writer.write_one(Token::PlusPlus)?,
      Self::Index { expression } => {
        writer.write_one(Token::BracketLeft)?;
        writer.write(expression)?;
        writer.write_one(Token::BracketRight)?;
      },
      Self::Member { right } => {
        writer.write_one(Token::Dot)?;
        writer.write(right)?;
      },
      Self::Scope { right } => {
        writer.write_one(Token::DoubleColon)?;
        writer.write(right)?;
      },
    }

    Ok(())
  }
}

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
        let label = parser.parse::<Identifier>()?;
        _ = parser.stream.next();
        Some(label)
      },
      _ => None,
    };

    let value = parser.parse::<Expression>()?;

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
      arguments.push(parser.parse::<Argument>()?);

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
    let identifier = parser.parse::<Identifier>()?;
    parser.expect(Token::Colon)?;

    let typ = parser.parse::<Type>()?;

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
      parameters.push(parser.parse::<FunctionParameter>()?);

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

/// An operator that updates its left identifier with the expression on its
/// right.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum AssignmentOperator {
  /// Bitwise AND assignment.
  AndEquals,

  /// Division assignment.
  DivideEquals,

  /// Simple assignment.
  Equals,

  /// Subtraction assignment.
  MinusEquals,

  /// Remainder assignment.
  ModuloEquals,

  /// Multiplication assignment.
  MultiplyEquals,

  /// Bitwise OR assignment.
  OrEquals,

  /// Addition assignment.
  PlusEquals,

  /// Bitwise left shift assignment.
  ShiftLeftEquals,

  /// Bitwise right shift assignment.
  ShiftRightEquals,

  /// Bitwise XOR assignment.
  XorEquals,
}

impl Parse for AssignmentOperator {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let operator = match parser.stream.next()? {
      Token::AmpersandEquals => Self::AndEquals,
      Token::AsteriskEquals => Self::MultiplyEquals,
      Token::CaretEquals => Self::XorEquals,
      Token::DashEquals => Self::MinusEquals,
      Token::DoubleAngleLeftEquals => Self::ShiftLeftEquals,
      Token::DoubleAngleRightEquals => Self::ShiftRightEquals,
      Token::Equals => Self::Equals,
      Token::PercentEquals => Self::ModuloEquals,
      Token::PipeEquals => Self::OrEquals,
      Token::PlusEquals => Self::PlusEquals,
      Token::SlashEquals => Self::DivideEquals,
      token => return parser.unexpected(token),
    };

    Ok(operator)
  }
}

impl Tokenize for AssignmentOperator {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    let token = match self {
      Self::AndEquals => Token::AmpersandEquals,
      Self::DivideEquals => Token::SlashEquals,
      Self::Equals => Token::Equals,
      Self::MinusEquals => Token::DashEquals,
      Self::ModuloEquals => Token::PercentEquals,
      Self::MultiplyEquals => Token::AsteriskEquals,
      Self::OrEquals => Token::PipeEquals,
      Self::PlusEquals => Token::PlusEquals,
      Self::ShiftLeftEquals => Token::DoubleAngleLeftEquals,
      Self::ShiftRightEquals => Token::DoubleAngleRightEquals,
      Self::XorEquals => Token::CaretEquals,
    };

    writer.write_one(token)
  }
}

/// An assignment operation.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Assignment {
  /// The mutable lvalue expression to which the rvalue will be assigned.
  pub lvalue: Expression,

  /// The operator defining the particular update operation.
  pub operator: AssignmentOperator,

  /// The rvalue expression evaluated before assignment.
  pub rvalue: Expression,
}

impl Parse for Assignment {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let lvalue = parser.parse::<Expression>()?;
    let operator = parser.parse::<AssignmentOperator>()?;
    let rvalue = parser.parse::<Expression>()?;

    Ok(Assignment {
      lvalue,
      operator,
      rvalue,
    })
  }
}

impl Tokenize for Assignment {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.lvalue)?;
    writer.write(&self.operator)?;
    writer.write(&self.rvalue)?;
    Ok(())
  }
}

/// A declaration of a new variable, optionally with an initial assignment.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Declaration {
  /// Whether the variable being declared is mutable.
  pub mutable: bool,

  /// The identifier of the variable being declared.
  pub variable: Identifier,

  /// The type of the variable being declared.
  pub typ: Option<Type>,

  /// The expression to be initially assigned to the variable.
  pub expression: Option<Expression>,
}

impl Parse for Declaration {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mutable = match parser.stream.next()? {
      Token::Const => false,
      Token::Var => true,
      token => return parser.unexpected(token),
    };

    let variable = parser.parse::<Identifier>()?;

    let typ = match parser.stream.peek(0)? {
      Token::Colon => {
        _ = parser.stream.next();
        Some(parser.parse::<Type>()?)
      },
      _ => None,
    };

    let expression = match parser.stream.peek(0)? {
      Token::Equals => {
        _ = parser.stream.next();
        Some(parser.parse::<Expression>()?)
      },
      _ => None,
    };

    Ok(Declaration {
      mutable,
      variable,
      typ,
      expression,
    })
  }
}

impl Tokenize for Declaration {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    if self.mutable {
      writer.write_one(Token::Var)?;
    } else {
      writer.write_one(Token::Const)?;
    }

    writer.write(&self.variable)?;

    if let Some(typ) = &self.typ {
      writer.write_one(Token::Colon)?;
      writer.write(typ)?;
    }

    if let Some(expression) = &self.expression {
      writer.write_one(Token::Equals)?;
      writer.write(expression)?;
    }

    Ok(())
  }
}

/// An if statement, optionally with an else block.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct If {
  /// The condition that determines whether the if statements execute.
  pub condition: Expression,

  /// The block evaluated if the condition is true.
  pub block: Block,

  /// The block evaluated if the condition is false (else block).
  pub otherwise: Option<Block>,
}

impl Parse for If {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::If)?;

    let condition = parser.parse::<Expression>()?;
    let block = parser.parse::<Block>()?;

    let otherwise = match parser.stream.peek(0)? {
      Token::Else => {
        _ = parser.stream.next();
        Some(parser.parse::<Block>()?)
      },
      _ => None,
    };

    Ok(If {
      condition,
      block,
      otherwise,
    })
  }
}

impl Tokenize for If {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(Token::If)?;
    writer.write(&self.condition)?;
    writer.write(&self.block)?;

    if let Some(otherwise) = &self.otherwise {
      writer.write_one(Token::Else)?;
      writer.write(otherwise)?;
    }

    Ok(())
  }
}

/// A for loop.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct For {
  /// The variable defined as the loop iteration element.
  pub variable: Identifier,

  /// The range expression that yields the element at every loop iteration.
  pub range: Expression,

  /// The block executed every iteration of the loop.
  pub block: Block,
}

impl Parse for For {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::For)?;

    let variable = parser.parse::<Identifier>()?;
    parser.expect(Token::In)?;

    let range = parser.parse::<Expression>()?;
    let block = parser.parse::<Block>()?;

    Ok(For {
      variable,
      range,
      block,
    })
  }
}

impl Tokenize for For {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(Token::For)?;
    writer.write(&self.variable)?;
    writer.write_one(Token::In)?;
    writer.write(&self.range)?;
    writer.write(&self.block)?;

    Ok(())
  }
}

/// A while loop.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct While {
  /// The condition which breaks the for loop upon evaluating to false.
  pub condition: Expression,

  /// The block executed every iteration of the loop.
  pub block: Block,
}

impl Parse for While {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::While)?;

    let condition = parser.parse::<Expression>()?;
    let block = parser.parse::<Block>()?;

    Ok(While {
      condition,
      block,
    })
  }
}

impl Tokenize for While {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(Token::While)?;
    writer.write(&self.condition)?;
    writer.write(&self.block)?;

    Ok(())
  }
}

/// A single statement.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Statement {
  /// An assignment statement.
  Assignment(Assignment),

  /// A declaration statement creating a variable.
  Declaration(Declaration),

  /// An empty statement (no-op).
  Empty,

  /// A statement containing an expression to be evaluated.
  Expression(Expression),

  /// A for loop.
  For(For),

  /// An if statement.
  If(If),

  /// A return statement.
  Return(Option<Expression>),

  /// A while loop.
  While(While),
}

impl Parse for Statement {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let statement = match parser.stream.peek(0)? {
      // A declaration statement.
      //
      // Examples:
      // - "const foo = 1;"
      // - "var bar = 2;"
      // - "var bar;"
      Token::Const | Token::Var => {
        let declaration = parser.parse::<Declaration>()?;
        parser.expect(Token::Semicolon)?;

        Statement::Declaration(declaration)
      },

      // A completely empty statement.
      //
      // Example: ";"
      Token::Semicolon => {
        _ = parser.stream.next();
        Statement::Empty
      },

      // A for loop
      Token::For => {
        Statement::For(parser.parse::<For>()?)
      },

      // An if statement
      Token::If => {
        Statement::If(parser.parse::<If>()?)
      },

      // A return statement
      Token::Return => {
        // Advance past the "return" keyword.
        _ = parser.stream.next();

        let ret = match parser.stream.peek(0)? {
          Token::Semicolon => None,
          _ => Some(parser.parse::<Expression>()?),
        };

        parser.expect(Token::Semicolon)?;

        Statement::Return(ret)
      },

      // A while loop
      Token::While => {
        Statement::While(parser.parse::<While>()?)
      },

      // Otherwise, assume it must be an expression.
      _ => {
        let expression = parser.parse::<Expression>()?;

        let statement = match parser.stream.peek(0)? {
          // Check for an assignment operator.
          Token::AmpersandEquals
          | Token::AsteriskEquals
          | Token::CaretEquals
          | Token::DashEquals
          | Token::DoubleAngleLeftEquals
          | Token::DoubleAngleRightEquals
          | Token::Equals
          | Token::PercentEquals
          | Token::PipeEquals
          | Token::PlusEquals
          | Token::SlashEquals => {
            let operator = parser.parse::<AssignmentOperator>()?;
            let rvalue = parser.parse::<Expression>()?;

            Statement::Assignment(Assignment {
              lvalue: expression,
              operator,
              rvalue,
            })
          },

          // Otherwise, it must be a lone expression.
          _ => Statement::Expression(expression),
        };

        parser.expect(Token::Semicolon)?;
        statement
      },
    };

    Ok(statement)
  }
}

impl Tokenize for Statement {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    match self {
      Self::Assignment(assignment) => {
        writer.write(assignment)?;
        writer.write_one(Token::Semicolon)?;
      },
      Self::Declaration(declaration) => {
        writer.write(declaration)?;
        writer.write_one(Token::Semicolon)?;
      },
      Self::Empty => {
        writer.write_one(Token::Semicolon)?;
      },
      Self::Expression(expression) => {
        writer.write(expression)?;
        writer.write_one(Token::Semicolon)?;
      },
      Self::For(for_loop) => {
        writer.write(for_loop)?;
      },
      Self::If(if_statement) => {
        writer.write(if_statement)?;
      },
      Self::Return(expression) => {
        writer.write_one(Token::Return)?;
        
        if let Some(expression) = expression {
          writer.write(expression)?;
        }

        writer.write_one(Token::Semicolon)?;
      },
      Self::While(while_loop) => {
        writer.write(while_loop)?;
      },
    }

    Ok(())
  }
}

impl Parse for Box<[Statement]> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mut statements = Vec::new();

    while {
      matches!(
        parser.stream.peek(0)?,
        Token::Const
        | Token::Dash
        | Token::Exclamation
        | Token::For
        | Token::Identifier(_)
        | Token::If
        | Token::ParenthesisLeft
        | Token::Plus
        | Token::Return
        | Token::Semicolon
        | Token::Tilde
        | Token::Var
        | Token::While,
      )
    } {
      statements.push(parser.parse::<Statement>()?);
    }

    Ok(statements.into_boxed_slice())
  }
}

impl Tokenize for Box<[Statement]> {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    for statement in self {
      writer.write(statement)?;
    }

    Ok(())
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

    let identifier = parser.parse::<Identifier>()?;

    parser.expect(Token::ParenthesisLeft)?;
    let parameters = parser.parse::<Box<[FunctionParameter]>>()?;
    parser.expect(Token::ParenthesisRight)?;

    let return_type = match parser.stream.peek(0)? {
      Token::Arrow => {
        _ = parser.stream.next();
        Some(parser.parse::<Type>()?)
      },
      _ => None,
    };

    let block = parser.parse::<Block>()?;

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
      let function = parser.parse::<Function>()?;
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

/// An import of another source file.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Import {
  /// The list of files to be imported.
  pub files: Box<[Box<str>]>,
}

impl Parse for Import {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::Import)?;

    let mut files = Vec::new();

    match parser.stream.next()? {
      // import { "foo.c", "foo.h" };
      Token::BraceLeft => {
        loop {
          match parser.stream.next()? {
            Token::String(file) => files.push(file),
            token => return parser.unexpected(token),
          }

          match parser.stream.next()? {
            Token::BraceRight => break,
            Token::Comma => {},
            token => return parser.unexpected(token),
          }
        }
      },

      // import "foo.w";
      Token::String(string) => files.push(string),
      token => return parser.unexpected(token),
    }

    parser.expect(Token::Semicolon)?;

    Ok(Self {
      files: files.into_boxed_slice(),
    })
  }
}

impl Tokenize for Import {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write_one(Token::Import)?;

    if self.files.len() > 1 {
      writer.write_one(Token::BraceLeft)?;
    }

    writer.join(&self.files, Token::Comma)?;

    if self.files.len() > 1 {
      writer.write_one(Token::BraceRight)?;
    }

    writer.write_one(Token::Semicolon)?;
    Ok(())
  }
}

impl Parse for Box<[Import]> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mut imports = Vec::new();

    while parser.stream.peek(0)? == Token::Import {
      let import = parser.parse::<Import>()?;
      imports.push(import);
    }

    Ok(imports.into_boxed_slice())
  }
}

impl Tokenize for Box<[Import]> {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    for import in self {
      writer.write(import)?;
    }

    Ok(())
  }
}

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
    let imports = parser.parse::<Box<[Import]>>()?;
    let functions = parser.parse::<Box<[Function]>>()?;

    parser.expect(Token::EOF)?;

    Ok(Unit {
      imports,
      functions,
    })
  }
}

impl Tokenize for Unit {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&self.imports)?;
    writer.write(&self.functions)?;
    Ok(())
  }
}

/// An error that can occur while parsing the AST.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParseError {
  /// A static array length is negative.
  ArrayLengthNegative(BigInt),

  /// An ordering mismatch has occurred between different types of brackets.
  BracketMismatch,

  /// A comma is trailing where it should not be.
  CommaTrailing,

  /// A token that appears is unexpected (does not fit the grammar).
  TokenUnexpected(Token),
}

impl Display for ParseError {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::ArrayLengthNegative(length) => {
        write!(f, "negative array length: {length}")
      },
      Self::BracketMismatch => {
        write!(f, "mismatching brackets")
      },
      Self::CommaTrailing => {
        write!(f, "trailing comma unexpected")
      },
      Self::TokenUnexpected(token) => {
        write!(f, "unexpected token: {token}")
      },
    }
  }
}

impl std::error::Error for ParseError {}
