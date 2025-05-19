//! Statement components of the AST.

use crate::{Result, Token, lexer::token::{TokenWriter, Tokenize}};
use super::{AssignmentOperator, Expression, Identifier, Parse, Parser, Type};

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
      Self::Empty => writer.write_one(Token::Semicolon)?,
      Self::Expression(expression) => {
        writer.write(expression)?;
        writer.write_one(Token::Semicolon)?;
      },
      Self::For(for_loop) => writer.write(for_loop)?,
      Self::If(if_statement) => writer.write(if_statement)?,
      Self::Return(expression) => {
        writer.write_one(Token::Return)?;

        if let Some(expression) = expression {
          writer.write(expression)?;
        }

        writer.write_one(Token::Semicolon)?;
      },
      Self::While(while_loop) => writer.write(while_loop)?,
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
