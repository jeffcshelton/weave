//! Import components of the AST.

use crate::{
  Intern,
  Result,
  Token,
  lexer::token::{TokenWriter, Tokenize},
};
use super::{Parse, Parser};

/// An import of another source file.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Import {
  /// The list of files to be imported.
  pub files: Box<[Intern]>,
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

    let files = self.files
      .iter()
      .map(|file| Token::String(file.clone()))
      .collect::<Vec<_>>();

    writer.join(&files, Token::Comma)?;

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
      imports.push(parser.consume::<Import>()?);
    }

    Ok(imports.into_boxed_slice())
  }
}

impl Tokenize for [Import] {
  fn tokenize(&self, writer: &mut impl TokenWriter) -> Result<()> {
    for import in self {
      writer.write(import)?;
    }

    Ok(())
  }
}
