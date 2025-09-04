//! Operator components of the AST.

use crate::{Result, Token, lexer::token::{TokenWriter, Tokenize}};
use super::{Argument, Expression, Identifier, Parse, Parser};

/// A precedence class that can be applied to an operator, with relative
/// ordering.
///
/// Farther down indicates higher precedence.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Precedence {
  /// The precedence class of all assignment operators.
  Assignment,

  /// The precedence class of all logical operators.
  Logical,

  /// The precedence class of equality operators, namely equality and
  /// non-equality.
  Equality,

  /// The precedence class of all comparison operators except equality and
  /// non-equality.
  Comparison,

  /// The precedence class of bitwise operators except the shifts.
  Bitwise,

  /// The precedence class of bitwise left and right shift operators.
  Shift,

  /// The precedence class of binary operators at the additive level, including
  /// addition and subtraction.
  Addition,

  /// The precedence class of binary operators at the multiplicative level,
  /// including multiplication, division, and modulus.
  Multiplication,

  /// The precedence class of all prefix operators.
  Prefix,

  /// The precedence class of all postfix operators.
  Postfix,
}

/// An associativity class determining in what order operations of identical
/// precedence should be performed.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Associativity {
  /// When both operators are of the same precedence, the operation on the left
  /// should be performed first.
  LeftToRight,

  /// When both operators are of the same precedence, the operation on the right
  /// should be performed first.
  RightToLeft,
}

/// Implemented by all operators which require a strict ordering of precedence.
pub trait Operator {
  /// Returns the associativity of the operator.
  fn associativity(&self) -> Associativity;

  /// Returns the relative precedence of the operator.
  fn precedence(&self) -> Precedence;

  /// Returns the binding power of the operator in the form (left, right).
  fn binding_power(&self) -> (u8, u8) {
    // The modified precedence is `p * 2 + 1` for two reasons:
    // 1. Multiplying by 2 is necessary because it must leave room for the
    //    in-between powers created by applying the associativity increment.
    // 2. Adding 1 is necessary because it avoids collision with the base-level
    //    precedence, which is 0.
    let precedence = (self.precedence() as u8) * 2 + 1;
    let mut power = (precedence, precedence);

    // Apply the associativity increment to the power corresponding to the
    // opposite of the preferred direction.
    //
    // For example, in the left-to-right case, a binary operator will see two
    // sides, left and right. In order to prefer left ordering, it must more
    // strongly bind to the right operand.
    match self.associativity() {
      Associativity::LeftToRight => power.1 += 1,
      Associativity::RightToLeft => power.0 += 1,
    }

    power
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
    let operator = match parser.stream.peek(0)? {
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

    _ = parser.stream.next();
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

impl Operator for AssignmentOperator {
  fn associativity(&self) -> Associativity {
    Associativity::RightToLeft
  }

  fn precedence(&self) -> Precedence {
    Precedence::Assignment
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
    let operator = match parser.stream.peek(0)? {
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

    _ = parser.stream.next();
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

impl Operator for BinaryOperator {
  fn associativity(&self) -> Associativity {
    Associativity::LeftToRight
  }

  fn precedence(&self) -> Precedence {
    match self {
      Self::BitwiseAnd => Precedence::Bitwise,
      Self::BitwiseOr => Precedence::Bitwise,
      Self::Divide => Precedence::Multiplication,
      Self::Equals => Precedence::Equality,
      Self::GreaterThan => Precedence::Comparison,
      Self::GreaterThanOrEqualTo => Precedence::Comparison,
      Self::LessThan => Precedence::Comparison,
      Self::LessThanOrEqualTo => Precedence::Comparison,
      Self::LogicalAnd => Precedence::Logical,
      Self::LogicalOr => Precedence::Logical,
      Self::Minus => Precedence::Addition,
      Self::Modulo => Precedence::Multiplication,
      Self::Multiply => Precedence::Multiplication,
      Self::NotEquals => Precedence::Equality,
      Self::Plus => Precedence::Addition,
      Self::ShiftLeft => Precedence::Shift,
      Self::ShiftRight => Precedence::Shift,
      Self::Xor => Precedence::Bitwise,
    }
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

  /// Path / member access.
  ///
  /// This is not a binary operation because it does not combine two independent
  /// expressions to produce a new value. The right expression is fundamentally
  /// dependent on the value of the left expression. This is not true for binary
  /// operations.
  Path {
    /// The chain of member identifiers on the right side.
    members: Box<[Identifier]>,
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
        writer.write(&**expression)?;
        writer.write_one(Token::BracketRight)?;
      },
      Self::Path { members } => {
        for member in members {
          writer.write_one(Token::Dot)?;
          writer.write(member)?;
        }
      },
      Self::Scope { right } => {
        writer.write_one(Token::DoubleColon)?;
        writer.write(&**right)?;
      },
    }

    Ok(())
  }
}

impl Parse for PostfixOperator {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let operator = match parser.stream.peek(0)? {
      Token::BracketLeft => {
        _ = parser.stream.next();

        let expression = parser.consume::<Box<Expression>>()?;
        parser.expect(Token::BracketRight)?;

        Self::Index { expression }
      },
      Token::Dot => {
        _ = parser.stream.next();

        let mut members = Vec::new();
        members.push(parser.consume::<Identifier>()?);

        while parser.stream.peek(0)? == Token::Dot {
          _ = parser.stream.next();
          members.push(parser.consume::<Identifier>()?);
        }

        Self::Path {
          members: members.into_boxed_slice(),
        }
      },
      Token::DoubleColon => {
        _ = parser.stream.next();

        Self::Scope {
          right: parser.consume::<Box<Expression>>()?,
        }
      },
      Token::MinusMinus => Self::Decrement,
      Token::ParenthesisLeft => {
        _ = parser.stream.next();

        let arguments = parser.consume::<Box<[Argument]>>()?;
        parser.expect(Token::ParenthesisRight)?;

        Self::Call { arguments }
      },
      Token::PlusPlus => {
        _ = parser.stream.next();

        Self::Increment
      },
      token => return parser.unexpected(token),
    };

    Ok(operator)
  }
}

impl Operator for PostfixOperator {
  fn associativity(&self) -> Associativity {
    Associativity::LeftToRight
  }

  fn precedence(&self) -> Precedence {
    Precedence::Postfix
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
    let operator = match parser.stream.peek(0)? {
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

    _ = parser.stream.next();
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

impl Operator for PrefixOperator {
  fn associativity(&self) -> Associativity {
    Associativity::RightToLeft
  }

  fn precedence(&self) -> Precedence {
    Precedence::Prefix
  }
}
