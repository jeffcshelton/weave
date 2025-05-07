//! Weave error definitions and conversions.

use crate::{parser::ParseError, scanner::ScanError, source::Point};
use std::{fmt::{self, Display, Formatter}, io, ops::Range, sync::Arc};

/// Any error that can be produced by Weave.
#[derive(Clone, Debug)]
pub enum Error {
  /// Error originating from an I/O operation.
  /// Wraps a `std::io::Error`.
  IO(Arc<io::Error>),

  /// Error originating from the parser, with a location in a source file.
  /// Wraps a `ParseError`.
  Parse(ParseError, Range<Point>),

  /// Error originating from the scanner, with a location in a source file.
  /// Wraps a `ScanError`.
  Scan(ScanError, Point),
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Error::IO(error) => write!(f, "io: {error}"),
      Error::Parse(error, range) => {
        write!(f, "parse @ ({} - {}): {error}", range.start, range.end)
      },
      Error::Scan(error, point) => {
        write!(f, "scan @ {point}: {error}")
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

impl From<(ParseError, Range<Point>)> for Error {
  fn from((error, range): (ParseError, Range<Point>)) -> Self {
    Error::Parse(error, range)
  }
}

impl From<(ScanError, Point)> for Error {
  fn from((error, point): (ScanError, Point)) -> Self {
    Error::Scan(error, point)
  }
}

/// A result wrapping any error that can occur in Weave.
pub type Result<T> = std::result::Result<T, Error>;
