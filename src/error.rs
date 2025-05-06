//! Weave error definitions and conversions.

use crate::{parser::ParseError, scanner::ScanError, source::Point};
use std::{fmt::{self, Display, Formatter}, io, sync::Arc};

/// Any error that can be produced by Weave.
#[derive(Clone, Debug)]
pub enum Error {
  /// Error originating from an I/O operation.
  /// Wraps a `std::io::Error`.
  IO(Arc<io::Error>),

  /// Error originating from the parser.
  /// Wraps a `ParseError`.
  Parse(ParseError),

  /// Error originating from the scanner.
  /// Wraps a `ScanError`.
  Scan(ScanError, Point),
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Error::IO(error) => write!(f, "io: {error}"),
      Error::Parse(error) => write!(f, "parse: {error:?}"),
      Error::Scan(error, point) => {
        write!(f, "scan @ line {}, col {}: {error:?}", point.line, point.column)
      },
    }
  }
}

impl std::error::Error for Error {}

impl From<io::Error> for Error {
  fn from(error: io::Error) -> Self {
    Error::IO(Arc::new(error))
  }
}

impl From<ParseError> for Error {
  fn from(error: ParseError) -> Self {
    Error::Parse(error)
  }
}

impl From<(ScanError, Point)> for Error {
  fn from((error, point): (ScanError, Point)) -> Self {
    Error::Scan(error, point)
  }
}

/// A result wrapping any error that can occur in Weave.
pub type Result<T> = std::result::Result<T, Error>;
