//! Operator components of the AST.

use crate::{Result, Token, lexer::{TokenWriter, Tokenize}};
use super::{Argument, Expression, Parse, Parser};

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

// impl BinaryOperator {
//   pub fn precedence(&self) -> u8 {
//     match self {
//       Self::BitwiseAnd => 7,
//       Self::BitwiseOr => 5,
//       Self::Divide => 10,
//       Self::Equals => 4,
//       Self::GreaterThan => 4,
//       Self::GreaterThanOrEqualTo => 4,
//       Self::LessThan => 4,
//       Self::LessThanOrEqualTo => 4,
//       Self::LogicalAnd => 3,
//       Self::LogicalOr => 2,
//       Self::Minus => 9,
//       Self::Modulo => 10,
//       Self::Multiply => 10,
//       Self::NotEquals => 4,
//       Self::Plus => 9,
//       Self::ShiftLeft => 8,
//       Self::ShiftRight => 8,
//       Self::Xor => 6,
//     }
//   }
// }

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
