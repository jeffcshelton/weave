
pub mod scope;
pub mod symbol;

use crate::Intern;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Error {
  SymbolRedefined(Intern),
  TypeNotInScope(Intern),
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::SymbolRedefined(ident) => {
        write!(f, "symbol '{ident}' is defined more than once")
      },
      Self::TypeNotInScope(ident) => {
        write!(f, "type '{ident}' not in scope")
      },
    }
  }
}

impl std::error::Error for Error {}
