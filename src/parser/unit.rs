//! Compilation unit components of the AST.

use crate::{Result, Token, lexer::token::{TokenWriter, Tokenize}};
use super::{
  Class, Enum, Error, Extension, Function, Global, Import, Parse, Parser, Struct
};

/// A single compilation unit, corresponding to one source file.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Unit {
  /// The classes defined throughout the source file.
  pub classes: Box<[Class]>,

  /// The enums defined throughout the source file.
  pub enums: Box<[Enum]>,

  /// The extensions to classes defined throughout the source file.
  pub extensions: Box<[Extension]>,

  /// The functions defined throughout the source file.
  pub functions: Box<[Function]>,

  /// The global variable declarations throughout the source file.
  pub globals: Box<[Global]>,

  /// The imports at the top of the source file.
  pub imports: Box<[Import]>,

  /// The structs defined throughout the source file.
  pub structs: Box<[Struct]>,
}

impl Parse for Unit {
  fn parse(parser: &mut Parser) -> Result<Self> {
    // Imports must be at the top of the file.
    let imports = parser.consume::<Box<[Import]>>()?;

    let mut classes = Vec::new();
    let mut enums = Vec::new();
    let mut extensions = Vec::new();
    let mut functions = Vec::new();
    let mut globals = Vec::new();
    let mut structs = Vec::new();

    loop {
      // Check that the next token is a visibility modifier.
      match parser.stream.peek(0)? {
        Token::Private | Token::Public => {},
        Token::EOF => break,
        _ => return parser.locate(Error::VisibilityMissing),
      }

      match parser.stream.peek(1)? {
        Token::Class => classes.push(parser.consume::<Class>()?),
        Token::Const => {
          // If the declaration is not a function, assume it's a global.
          match parser.stream.peek(2)? {
            Token::Function => functions.push(parser.consume::<Function>()?),
            _ => globals.push(parser.consume::<Global>()?),
          }
        },
        Token::Enum => enums.push(parser.consume::<Enum>()?),
        Token::Extension => extensions.push(parser.consume::<Extension>()?),
        Token::Function => functions.push(parser.consume::<Function>()?),
        Token::Struct => structs.push(parser.consume::<Struct>()?),
        Token::Var => {
          globals.push(parser.consume::<Global>()?);
          parser.expect(Token::Semicolon)?;
        },
        token => return parser.unexpected(token),
      }
    }

    parser.expect(Token::EOF)?;

    Ok(Unit {
      classes: classes.into_boxed_slice(),
      enums: enums.into_boxed_slice(),
      extensions: extensions.into_boxed_slice(),
      functions: functions.into_boxed_slice(),
      globals: globals.into_boxed_slice(),
      imports,
      structs: structs.into_boxed_slice(),
    })
  }
}

impl Tokenize for Unit {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    writer.write(&*self.imports)?;
    writer.write(&*self.structs)?;
    writer.write(&*self.classes)?;
    writer.write(&*self.globals)?;
    writer.write(&*self.functions)?;

    Ok(())
  }
}

