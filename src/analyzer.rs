pub mod block;
pub mod expr;
pub mod func;
pub mod scope;
pub mod symbol;

pub use block::Block;
pub use expr::Expression;
pub use func::Function;
pub use scope::Scope;
pub use symbol::Symbol;

use crate::Intern;
use num::BigInt;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Error {
  SymbolNotFound(Intern),
  SymbolNotType(Intern),
  SymbolRedefined(Intern),
  TypeNotInScope(Intern),
  TypeTooBig(Intern, BigInt),
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::SymbolNotFound(symbol) => {
        write!(f, "symbol '{symbol}' not found")
      },
      Self::SymbolNotType(symbol) => {
        write!(f, "symbol '{symbol}' is not a type")
      },
      Self::SymbolRedefined(ident) => {
        write!(f, "symbol '{ident}' is defined more than once")
      },
      Self::TypeNotInScope(ident) => {
        write!(f, "type '{ident}' not in scope")
      },
      Self::TypeTooBig(ident, size) => {
        write!(f, "type '{ident}' is too big ({size} bytes)")
      },
    }
  }
}

impl std::error::Error for Error {}
