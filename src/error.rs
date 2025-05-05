use crate::{parser::ParseError, scanner::{SourcePosition, ScanError}};
use std::{fmt::{self, Display, Formatter}, io};

#[derive(Debug)]
pub enum Error {
  IO(io::Error),
  Parse(ParseError),
  Scan(ScanError, SourcePosition),
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
    Error::IO(error)
  }
}

impl From<ParseError> for Error {
  fn from(error: ParseError) -> Self {
    Error::Parse(error)
  }
}

impl From<(ScanError, SourcePosition)> for Error {
  fn from((error, point): (ScanError, SourcePosition)) -> Self {
    Error::Scan(error, point)
  }
}

pub type Result<T> = std::result::Result<T, Error>;
