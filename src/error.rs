//! Weave error definitions and conversions.

use crate::{jit, lexer, parser, source::Point};
use std::{fmt::{self, Display, Formatter}, io, ops::Range, sync::Arc};

/// Any error that can be produced by Weave.
#[derive(Clone, Debug)]
pub enum Error {
  /// Error originating from a formatting operation.
  /// Wraps a `std::fmt::Error`.
  Format(fmt::Error),

  /// Error originating from an I/O operation.
  /// Wraps a `std::io::Error`.
  IO(Arc<io::Error>),

  /// Error originating from the JIT runtime.
  /// Wraps a `jit::Error`.
  JIT(jit::Error),

  /// Error originating from the lexer, with a location in a source file.
  /// Wraps a `lexer::Error`.
  Lex(lexer::Error, Point),

  /// Error originating from the parser, with a location in a source file.
  /// Wraps a `parser::Error`.
  Parse(parser::Error, Range<Point>),
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Error::Format(error) => write!(f, "format: {error}"),
      Error::IO(error) => write!(f, "io: {error}"),
      Error::JIT(error) => write!(f, "jit: {error}"),
      Error::Lex(error, point) => {
        write!(f, "scan @ {point}: {error}")
      },
      Error::Parse(error, range) => {
        write!(f, "parse @ ({} - {}): {error}", range.start, range.end)
      },
    }
  }
}

impl std::error::Error for Error {}

impl From<fmt::Error> for Error {
  fn from(error: fmt::Error) -> Self {
    Error::Format(error)
  }
}

impl From<io::Error> for Error {
  fn from(error: io::Error) -> Self {
    Error::IO(Arc::new(error))
  }
}

impl From<jit::Error> for Error {
  fn from(error: jit::Error) -> Self {
    Error::JIT(error)
  }
}

impl From<(lexer::Error, Point)> for Error {
  fn from((error, point): (lexer::Error, Point)) -> Self {
    Error::Lex(error, point)
  }
}

impl From<(parser::Error, Range<Point>)> for Error {
  fn from((error, range): (parser::Error, Range<Point>)) -> Self {
    Error::Parse(error, range)
  }
}

/// A result wrapping any error that can occur in Weave.
pub type Result<T> = std::result::Result<T, Error>;
