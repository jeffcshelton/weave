//! Components related to parsing a Weave abstract syntax tree (AST).

use crate::{Result, scanner::{Token, TokenStream}};
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

/// A scoped identifier, such as `foo::bar`.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Scoped {
  /// The outer scoped identifier.
  /// The `foo` in `foo::bar`.
  pub outer: Identifier,

  /// The inner scoped identifier.
  /// The `bar` in `foo::bar`.
  pub inner: Option<Box<Scoped>>,
}

impl Parse for Scoped {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let outer = parser.parse::<Identifier>()?;

    let inner = match parser.stream.peek(0)? {
      Token::DoubleColon => {
        _ = parser.stream.next();
        Some(Box::new(parser.parse::<Scoped>()?))
      },
      _ => None,
    };

    Ok(Scoped {
      outer,
      inner,
    })
  }
}

/// A datatype identifier.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Type {
  /// The identifier of the type.
  pub identifier: Scoped,

  /// Array.
  pub array: Vec<Option<BigInt>>,
}

impl Parse for Type {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let identifier = parser.parse::<Scoped>()?;
    let mut array = Vec::new();

    while parser.stream.peek(0)? == Token::BracketLeft {
      _ = parser.stream.next();

      let length = match parser.stream.peek(0)? {
        Token::Integer(int) => {
          if int.is_negative() {
            return parser.locate(ParseError::ArrayLengthNegative(int));
          }

          _ = parser.stream.next();
          Some(int)
        },
        _ => None,
      };

      array.push(length);

      parser.expect(Token::BracketRight)?;
    }

    Ok(Type {
      identifier,
      array,
    })
  }
}

/// An operator that requires left and right arguments.
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

/// An operator requiring one value.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnaryOperator {
  /// Bitwise NOT.
  BitwiseNot,

  /// Logical (boolean) NOT.
  LogicalNot,

  /// Negation.
  Negative,

  /// No-op.
  Positive,
}

impl Parse for UnaryOperator {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let operator = match parser.stream.next()? {
      Token::Dash => Self::Negative,
      Token::Exclamation => Self::LogicalNot,
      Token::Plus => Self::Positive,
      Token::Tilde => Self::BitwiseNot,
      token => return parser.unexpected(token),
    };

    Ok(operator)
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
      Token::String(value) => Literal::String(value),
      token => return parser.unexpected(token),
    };

    Ok(literal)
  }
}

/// An expression that can be evaluated.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expression {
  /// Two sub-expressions joined by a binary operation.
  Binary {
    /// The expression on the left-hand side.
    left: Box<Expression>,

    /// The operator defining the binary operation.
    operator: BinaryOperator,

    /// The expression on the right-hand side.
    right: Box<Expression>,
  },

  /// A single scoped identifier.
  Identifier(Scoped),

  /// A single literal.
  Literal(Literal),

  /// A unary operation applied to a sub-expression.
  Unary {
    /// The operator defining the unary operation.
    operator: UnaryOperator,

    /// The expression on which the operation is applied.
    inner: Box<Expression>,
  },
}

impl Parse for Expression {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mut expression = match parser.stream.peek(0)? {
      // Identifiers.
      Token::Identifier(_) => {
        Expression::Identifier(parser.parse::<Scoped>()?)
      },

      // Left parenthesis.
      Token::ParenthesisLeft => {
        // Consume the left parenthesis.
        _ = parser.stream.next();

        // Parse the inner expression.
        let inner = parser.parse::<Expression>()?;

        // Check that the expression is capped by a right parenthesis.
        let end = parser.stream.next()?;
        if end != Token::ParenthesisRight {
          return parser.unexpected(end);
        }

        inner
      },

      // Literals.
      Token::Character(_)
      | Token::Float(_)
      | Token::Integer(_)
      | Token::String(_) => {
        let literal = parser.parse::<Literal>()?;
        Expression::Literal(literal)
      },

      // Unary operators.
      Token::Dash
      | Token::Exclamation
      | Token::Plus
      | Token::Tilde => {
        let operator = parser.parse::<UnaryOperator>()?;
        let inner = parser.parse::<Expression>()?;

        Expression::Unary {
          operator,
          inner: Box::new(inner),
        }
      },

      // Unexpected token.
      token => return parser.unexpected(token),
    };

    // TODO: Improve efficiency. Use a hash set.
    const BINARY_OPERATORS: [Token; 18] = [
      Token::Ampersand,
      Token::AngleLeft,
      Token::AngleLeftEquals,
      Token::AngleRight,
      Token::AngleRightEquals,
      Token::Asterisk,
      Token::Caret,
      Token::Dash,
      Token::DoubleAmpersand,
      Token::DoubleAngleLeft,
      Token::DoubleAngleRight,
      Token::DoubleEquals,
      Token::DoublePipe,
      Token::ExclamationEquals,
      Token::Percent,
      Token::Pipe,
      Token::Plus,
      Token::Slash,
    ];

    if BINARY_OPERATORS.contains(&parser.stream.peek(0)?) {
      let operator = parser.parse::<BinaryOperator>()?;
      let right = parser.parse::<Expression>()?;

      expression = Expression::Binary {
        left: Box::new(expression),
        operator,
        right: Box::new(right),
      };
    }

    Ok(expression)
  }
}

/// A function parameter.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Parameter {
  /// The variable identifier of the parameter.
  pub identifier: Identifier,

  /// The type identifier associated with the variable.
  pub typ: Type,
}

impl Parse for Parameter {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let identifier = parser.parse::<Identifier>()?;
    parser.expect(Token::Colon)?;

    let typ = parser.parse::<Type>()?;

    Ok(Parameter {
      identifier,
      typ,
    })
  }
}

impl Parse for Vec<Parameter> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mut parameters = Vec::new();

    loop {
      // Stop parsing the parameter list if it has reached the end.
      // If it has not reached the end, then an identifier must be present.
      if !matches!(parser.stream.peek(0)?, Token::Identifier(_)) {
        break;
      }

      let parameter = parser.parse::<Parameter>()?;
      parameters.push(parameter);

      // No comma indicates that the parameter list is done.
      // Having a trailing comma intentionally does not error.
      if parser.stream.peek(0)? != Token::Comma {
        break;
      }

      // Advance past the comma.
      _ = parser.stream.next();
    }

    Ok(parameters)
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

/// An assignment operation.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Assignment {
  /// The lvalue identifier to which the rvalue will be assigned.
  pub lvalue: Identifier,

  /// The operator defining the particular update operation.
  pub operator: AssignmentOperator,

  /// The rvalue expression evaluated before assignment.
  pub rvalue: Expression,
}

impl Parse for Assignment {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let lvalue = parser.parse::<Identifier>()?;
    let operator = parser.parse::<AssignmentOperator>()?;
    let rvalue = parser.parse::<Expression>()?;

    Ok(Assignment {
      lvalue,
      operator,
      rvalue,
    })
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

/// An if statement, optionally with an else block.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct If {
  /// The condition that determines whether the if statements execute.
  pub condition: Expression,

  /// The statements evaluated if the condition is true.
  pub statements: Vec<Statement>,

  /// The statements evaluated if the condition is false (else block).
  pub otherwise: Option<Vec<Statement>>,
}

impl Parse for If {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::If)?;

    let condition = parser.parse::<Expression>()?;

    parser.expect(Token::BraceLeft)?;

    let statements = parser.parse::<Vec<Statement>>()?;

    parser.expect(Token::BraceRight)?;

    let otherwise = match parser.stream.peek(0)? {
      Token::Else => {
        _ = parser.stream.next();

        parser.expect(Token::BraceLeft)?;

        let statements = parser.parse::<Vec<Statement>>()?;

        parser.expect(Token::BraceRight)?;

        Some(statements)
      },
      _ => None,
    };

    Ok(If {
      condition,
      statements,
      otherwise,
    })
  }
}

/// A for loop.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct For {
  /// The variable defined as the loop iteration element.
  pub variable: Identifier,

  /// The range expression that yields the element at every loop iteration.
  pub range: Expression,

  /// The statements executed every iteration of the loop.
  pub statements: Vec<Statement>,
}

impl Parse for For {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::For)?;

    let variable = parser.parse::<Identifier>()?;

    parser.expect(Token::In)?;

    let range = parser.parse::<Expression>()?;

    parser.expect(Token::BraceLeft)?;

    let statements = parser.parse::<Vec<Statement>>()?;

    parser.expect(Token::BraceRight)?;

    Ok(For {
      variable,
      range,
      statements,
    })
  }
}

/// A while loop.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct While {
  /// The condition which breaks the for loop upon evaluating to false.
  pub condition: Expression,

  /// The statements executed every iteration of the loop.
  pub statements: Vec<Statement>,
}

impl Parse for While {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::While)?;

    let condition = parser.parse::<Expression>()?;

    parser.expect(Token::BraceLeft)?;

    let statements = parser.parse::<Vec<Statement>>()?;

    parser.expect(Token::BraceRight)?;

    Ok(While {
      condition,
      statements,
    })
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
      Token::Identifier(_) => {
        let statement = match parser.stream.peek(1)? {
          // An assignment statement.
          // Example: "foo &= bar;"
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
            Statement::Assignment(parser.parse::<Assignment>()?)
          },

          // An expression statement.
          // Examples:
          // - "x++;"
          // - "next();"
          // - "1;"
          _ => {
            Statement::Expression(parser.parse::<Expression>()?)
          },
        };

        parser.expect(Token::Semicolon)?;
        statement
      },

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

      token => return parser.unexpected(token),
    };

    Ok(statement)
  }
}

impl Parse for Vec<Statement> {
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

    Ok(statements)
  }
}

/// A function declaration.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Function {
  /// The identifier by which the function may be called.
  pub identifier: Identifier,

  /// The parameter list of the function.
  pub parameters: Vec<Parameter>,

  /// The optional return type identifier.
  pub return_type: Option<Type>,

  /// The statements executed when the function is called.
  pub statements: Vec<Statement>,
}

impl Parse for Function {
  fn parse(parser: &mut Parser) -> Result<Self> {
    parser.expect(Token::Function)?;

    let identifier = parser.parse::<Identifier>()?;

    parser.expect(Token::ParenthesisLeft)?;

    let parameters = parser.parse::<Vec<Parameter>>()?;

    parser.expect(Token::ParenthesisRight)?;

    let return_type = match parser.stream.peek(0)? {
      Token::DashAngleRight => {
        _ = parser.stream.next();
        Some(parser.parse::<Type>()?)
      },
      _ => None,
    };

    parser.expect(Token::BraceLeft)?;

    let statements = parser.parse::<Vec<Statement>>()?;

    parser.expect(Token::BraceRight)?;

    Ok(Function {
      identifier,
      parameters,
      return_type,
      statements,
    })
  }
}

impl Parse for Vec<Function> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mut functions = Vec::new();

    while parser.stream.peek(0)? == Token::Function {
      let function = parser.parse::<Function>()?;
      functions.push(function);
    }

    Ok(functions)
  }
}

/// An import of another source file.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Import {
  /// The list of files to be imported.
  pub files: Vec<Box<str>>,
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

    Ok(Import { files })
  }
}

impl Parse for Vec<Import> {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let mut imports = Vec::new();

    while parser.stream.peek(0)? == Token::Import {
      let import = parser.parse::<Import>()?;
      imports.push(import);
    }

    Ok(imports)
  }
}

/// A single compilation unit, corresponding to one source file.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Unit {
  /// The imports at the top of the source file.
  pub imports: Vec<Import>,

  /// The functions defined throughout the source file.
  pub functions: Vec<Function>,
}

impl Parse for Unit {
  fn parse(parser: &mut Parser) -> Result<Self> {
    let imports = parser.parse::<Vec<Import>>()?;
    let functions = parser.parse::<Vec<Function>>()?;

    parser.expect(Token::EOF)?;

    Ok(Unit {
      imports,
      functions,
    })
  }
}

/// An error that can occur while parsing the AST.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParseError {
  /// A static array length is negative.
  ArrayLengthNegative(BigInt),

  /// A token that appears is unexpected (does not fit the grammar).
  TokenUnexpected(Token),
}

impl Display for ParseError {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::ArrayLengthNegative(length) => {
        write!(f, "negative array length: {length}")
      },
      Self::TokenUnexpected(token) => {
        write!(f, "unexpected token: {token}")
      },
    }
  }
}

impl std::error::Error for ParseError {}
