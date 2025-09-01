use crate::{Intern, Result, ToIntern, parser};
use hashbrown::HashMap;
use std::{
  cell::RefCell,
  hash::{Hash, Hasher},
  rc::{Rc, Weak},
};
use super::{symbol::{base_symbols, Symbol, Type}, Error};

#[derive(Debug)]
pub struct Scope {
  /// The identifier of the scope, or `None` if anonymous.
  ident: Option<Intern>,

  level: u32,
  index: u32,

  children: RefCell<Vec<Rc<Self>>>,

  /// The immediate parent scope of this scope.
  ///
  /// Once the scope is initialized, this cannot be changed. This is enforced by
  /// the type system due to the absence of `RefCell` and the fact that all
  /// creation methods return an `Rc<Scope>`. Additionally, the substitution of
  /// `Weak` for `Rc` prevents reference cycles which cause memory leaks. Once
  /// the root of the scope tree is dropped, all child nodes will be dropped.
  parent: Option<Weak<Self>>,
  symbols: RefCell<HashMap<Intern, Rc<Symbol>>>,
}

impl Hash for Scope {
  fn hash<H: Hasher>(&self, state: &mut H) {
    // TODO: Reconsider this hash function.
    // It works as long as global scopes aren't mixed.
    state.write_u32(self.level);
    state.write_u32(self.index);
  }
}

impl Scope {
  pub fn global() -> Rc<Self> {
    Rc::new(Self {
      ident: Some("global".intern()),
      level: 0,
      index: 0,
      children: RefCell::new(Vec::new()),
      parent: None,
      symbols: RefCell::new(base_symbols()),
    })
  }

  pub fn new_child(self: &Rc<Self>, ident: Option<Intern>) -> Rc<Self> {
    let mut children = self.children.borrow_mut();

    let child = Rc::new(Self {
      ident,
      level: self.level + 1,
      index: children.len() as u32,
      children: RefCell::new(Vec::new()),
      parent: Some(Rc::downgrade(self)),
      symbols: RefCell::new(HashMap::new()),
    });

    children.push(child.clone());
    child
  }

  fn add_function(self: &Rc<Self>, func: &parser::Function) -> Result<()> {
    // Add all function parameters as variable symbols.
    for param in &func.parameters {
      self.add_variable(&param.identifier)?;
    }

    self.add_block(&func.block)?;
    Ok(())
  }

  fn add_block(self: &Rc<Self>, block: &parser::Block) -> Result<()> {
    use parser::Statement;

    for stmt in &block.statements {
      match stmt {
        Statement::Declaration(decl) => {
          // Declarations do not create a new child scope.
          self.add_variable(&decl.variable)?;
        },
        Statement::For(for_loop) => {
          // Create a child scope for the loop.
          let child = self.new_child(None);

          // Add the loop variable to the scope.
          child.add_variable(&for_loop.variable)?;

          // Add the for loop's enclosed block to the scope.
          child.add_block(&for_loop.block)?;
        },
        Statement::If(if_stmt) => {
          // Create a child scope with only the statement's inner block.
          // If statements cannot have declarations in the header.
          let child = self.new_child();
          child.add_block(&if_stmt.block)?;
        },
        Statement::While(while_loop) => {
          // Create a child scope with only the loop's inner block.
          // While loops cannot have declarations in the header.
          let child = self.new_child();
          child.add_block(&while_loop.block)?;
        },

        // All other types of statements create no symbol declarations.
        _ => {},
      }
    }

    Ok(())
  }

  fn add_variable(self: &Rc<Self>, decl: &parser::Declaration) -> Result<()> {
    // self.add_symbol(Symbol {
    //   kind: SymbolKind::Variable,
    //   identifier: ident.intern(),
    // })?;

    Ok(())
  }

  fn add_type(self: &Rc<Self>, typ: &parser::Type) -> Result<()> {
    // let mut child = self.new_child();



    Ok(())
  }

  pub fn add_symbol(self: &Rc<Self>, symbol: Symbol) -> Result<Rc<Symbol>> {
    let symbol = Rc::new(symbol);

    let old = self.symbols
      .borrow_mut()
      .insert(symbol.ident(), symbol.clone());

    // Check if the insert overwrote an existing symbol.
    // If so, there is a name conflict, which is the programmer's error.
    if let Some(old) = old {
      return Err(Error::SymbolRedefined(old.ident()).into());
    }

    // Create a child scope node for the symbol's enclosed scope.
    if !matches!(*symbol, Symbol::Variable(_)) {
      let child = self.new_child();

      match &*symbol {
        Symbol::Function(func) => child.add_function(func)?,
        Symbol::Type(typ) => child.add_type(typ)?,
        Symbol::Variable(_) => unreachable!(),
      }
    }

    Ok(symbol)
  }

  pub fn resolve<'i, I>(
    &self,
    path: &'i [I],
  ) -> Option<Rc<Symbol>> where &'i I: Into<Intern> {
    if path.len() > 1 {
      let name = (&path[0]).into();
      let compare = &Some(name);

      let children = self.children.borrow();
      let subscope = children
        .iter()
        .find(|child| &child.ident == compare)?;

      subscope.resolve(&path[1..])
    } else {
      let ident = path.get(0)?.into();
      self.find_symbol(&ident)
    }
  }

  pub fn find_symbol(&self, identifier: &Intern) -> Option<Rc<Symbol>> {
    let symbols = self.symbols.borrow();

    // First check if the immediate scope directly contains the type identifier.
    if let Some(typ) = symbols.get(identifier) {
      Some(typ.clone())
    } else {
      // If not in the immediate scope, check the parent scope.
      //
      // This will recurse until the global scope, stopping at the first outer
      // scope that contains the type. If no enclosing scope is found to contain
      // the type, then `None` will be returned all the way up the call stack.
      self.parent
        .as_ref()
        .and_then(|weak| weak.upgrade())
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
