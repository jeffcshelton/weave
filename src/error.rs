//! Weave error definitions and conversions.

use crate::{parser::ParseError, lexer::LexError, source::Point};
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

  /// Error originating from the lexer, with a location in a source file.
  /// Wraps a `LexError`.
  Lex(LexError, Point),

  /// Error originating from the parser, with a location in a source file.
  /// Wraps a `ParseError`.
  Parse(ParseError, Range<Point>),
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Error::Format(error) => write!(f, "format: {error}"),
      Error::IO(error) => write!(f, "io: {error}"),
      Error::Parse(error, range) => {
        write!(f, "parse @ ({} - {}): {error}", range.start, range.end)
      },
      Error::Lex(error, point) => {
        write!(f, "scan @ {point}: {error}")
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

impl From<(ParseError, Range<Point>)> for Error {
  fn from((error, range): (ParseError, Range<Point>)) -> Self {
    Error::Parse(error, range)
  }
}

impl From<(LexError, Point)> for Error {
  fn from((error, point): (LexError, Point)) -> Self {
    Error::Lex(error, point)
  }
}

/// A result wrapping any error that can occur in Weave.
pub type Result<T> = std::result::Result<T, Error>;
