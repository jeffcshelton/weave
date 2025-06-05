use crate::{parser, Intern, Result};
use std::{
  collections::HashMap,
  hash::{Hash, Hasher},
  rc::Rc,
  sync::Mutex,
};
use super::{symbol::{self, Symbol, SymbolKind}, Error};

#[derive(Debug)]
pub struct Scope {
  level: u32,
  index: u32,

  children: Mutex<Vec<Rc<Self>>>,

  /// The immediate parent scope of this scope.
  ///
  /// Once the scope is initialized, this cannot be changed. This is enforced by
  /// the type system because outside consumers can only get an `Arc<Scope>`,
  /// which prevents mutability. Mutable fields are wrapped in `Mutex`.
  parent: Option<Rc<Self>>,
  symbols: Mutex<HashMap<Intern, Rc<Symbol>>>,
}

impl Hash for Scope {
  fn hash<H: Hasher>(&self, state: &mut H) {
    state.write_u32(self.level);
    state.write_u32(self.index);
  }
}

impl Scope {
  pub fn global() -> Rc<Self> {
    Rc::new(Self {
      level: 0,
      index: 0,
      children: Mutex::new(Vec::new()),
      parent: None,
      symbols: Mutex::new(HashMap::new()),
    })
  }

  pub fn new_child(self: Rc<Self>) -> Rc<Self> {
    let mut children = self.children
      .lock()
      .unwrap();

    let child = Rc::new(Self {
      level: self.level + 1,
      index: children.len() as u32,
      children: Mutex::new(Vec::new()),
      parent: Some(self.clone()),
      symbols: Mutex::new(HashMap::new()),
    });

    children.push(child.clone());
    child
  }

  pub fn add_function(self: Rc<Self>, func: parser::Function) -> Result<()> {
    let mut child = self.new_child();

    let symbol = Symbol {
      kind: SymbolKind::Function(Rc::new(symbol::Function {})),
      identifier: func.identifier.intern(),
    };

    Ok(())
  }

  pub fn add_symbol(&mut self, symbol: Symbol) -> Result<Rc<Symbol>> {
    let symbol = Rc::new(symbol);

    let old = self.symbols
      .lock()
      .unwrap()
      .insert(symbol.identifier.clone(), symbol.clone());

    if let Some(old) = old {
      return Err(Error::SymbolRedefined(old.identifier.clone()).into());
    }

    Ok(symbol)
  }

  pub fn find_symbol(&self, identifier: &Intern) -> Option<Rc<Symbol>> {
    let symbols = self.symbols.lock().unwrap();

    // First check if the immediate scope directly contains the type identifier.
    if let Some(typ) = symbols.get(identifier) {
      Some(typ.clone())
    } else {
      // Drop the symbols lock so that it will not be held excessively.
      drop(symbols);

      // If not in the immediate scope, check the parent scope.
      //
      // This will recurse until the global scope, stopping at the first outer
      // scope that contains the type. If no enclosing scope is found to contain
      // the type, then `None` will be returned all the way up the call stack.
      self.parent
        .as_ref()
        .and_then(|parent| parent.find_symbol(identifier))
    }
  }
}

impl PartialEq for Scope {
  fn eq(&self, other: &Self) -> bool {
    self.level == other.level && self.index == other.index
  }
}

impl Eq for Scope {}
