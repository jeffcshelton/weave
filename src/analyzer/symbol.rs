use crate::{parser, Intern};
use std::rc::Rc;

#[derive(Clone, Debug, Eq, Hash)]
pub enum SymbolKind {
  Function(Rc<parser::Function>),
  Type(Rc<parser::Type>),
  Variable(Intern),
}

impl SymbolKind {
  fn ptr(&self) -> *const () {
    match self {
      Self::Function(func) => Rc::as_ptr(func) as *const (),
      Self::Type(typ) => Rc::as_ptr(typ) as *const (),
      Self::Variable(var) => Rc::as_ptr(&**var) as *const (),
    }
  }
}

impl PartialEq for SymbolKind {
  fn eq(&self, other: &Self) -> bool {
    self.ptr() == other.ptr()
  }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Symbol {
  pub kind: SymbolKind,
  pub identifier: Intern,
}
