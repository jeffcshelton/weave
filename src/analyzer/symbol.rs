use crate::{parser::{self, Identifier}, Intern, Result, ToIntern};
use hashbrown::HashMap;
use num_traits::ToPrimitive;
use std::{hash::{Hash, Hasher}, iter::Iterator, rc::Rc};
use super::{Error, scope::Scope};

// TODO: Reconsider the encapsulation here. Lots of struct members are public
// when they very well should be private.

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Function {
  pub ident: Intern,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeMember {
  pub ident: Intern,
  pub offset: usize,
  pub typ: Rc<Type>,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Type {
  // TODO: Consider removing this. It may not be necessary information.
  /// The subtypes upon which this type depends.
  pub dependencies: Box<[Rc<Type>]>,

  /// The identifier of the type in source code.
  ///
  /// This is not guaranteed to be unique with multiple scopes.
  pub ident: Intern,

  /// The members of the type that may be accessed only with an instance.
  pub instance_members: HashMap<Intern, TypeMember>,

  /// The members of the type that may be accessed without an instance.
  pub static_members: HashMap<Intern, TypeMember>,

  /// The alignment, in bytes, of an instance of the type in memory.
  pub alignment: usize,

  /// The size, in bytes, of an instance of the type in memory.
  /// `None` if the type is dynamically sized.
  pub size: Option<usize>,
}

impl Type {
  pub fn id(self: Rc<Type>) -> usize {
    Rc::as_ptr(&self) as usize
  }

  pub fn dummy() -> Rc<Self> {
    unimplemented!()
  }
}

pub trait ToType {
  /// Converts a parsed syntax object into a concrete type, using the context of
  /// the containing scope and adding it to the scope when finished.
  fn to_type(&self, scope: &Scope) -> Result<Rc<Type>>;
}

impl ToType for parser::TypeExpression {
  fn to_type(&self, scope: &Scope) -> Result<Rc<Type>> {
    use parser::TypeExpression as TE;

    // For all subtypes that are components of the evaluated type, they must be
    // accessible in the same scope as the final type.

    let typ = match self {
      TE::Array { base, dimensions } => {
        let base = base.to_type(scope)?;

        let ident = Intern::unique();
        let alignment = base.alignment;
        let mut size = base.size;

        for dim in dimensions.iter().rev() {
          // If the size of the array is not specified, then the array as a
          // whole becomes an unsized type, no matter if the elements are
          // sized.
          //
          // If the dimension is specified but the size of the previous
          // iteration is dynamic, then the compiler will automatically convert
          // the unsized type to being heap allocated, so the size of each
          // element is assumed to be pointer-sized.
          //
          // Otherwise, the size is just multiplied by the next dimension. This
          // is the typical case.
          size = dim
            .as_ref()
            .map(|dim| dim * size.unwrap_or(size_of::<usize>()))
            .map(|expanded|
              expanded
                .to_usize()
                .ok_or_else(|| Error::TypeTooBig(ident.clone(), expanded))
            )
            .transpose()?;
        }

        // Rc::new(Type {
        //   dependencies: Box::new([base]),
        //   ident: Intern::unique(),
        //
        //   alignment,
        //   size,
        // })
        //
        Type::dummy()
      },

      TE::Enclosed(expr) => {
        return expr.to_type(scope);
      },
      TE::Pointer { base, is_const } => {
        let base = base.to_type(scope)?;
        let ident = Intern::unique();

        Rc::new(Type {
          dependencies: Box::new([base]),
          ident,

          // TODO: Add methods.
          instance_members: HashMap::new(),
          static_members: HashMap::new(),
          alignment: align_of::<usize>(),
          size: Some(size_of::<usize>()),
        })
      },
      TE::Reference { base, is_const } => {
        let base = base.to_type(scope)?;
        let ident = Intern::unique();

        Rc::new(Type {
          dependencies: Box::new([base]),
          ident,

          // TODO: Add methods.
          instance_members: HashMap::new(),
          static_members: HashMap::new(),
          alignment: align_of::<usize>(),
          size: Some(size_of::<usize>()),
        })
      },
      TE::Scoped(path) => {
        // Converts the scoped identifier into a string for readability.
        // TODO: Replace with `intersperse` when stabilized.
        fn path_string(path: &[Identifier]) -> String {
          path
            .iter()
            .map(|component| component.as_ref())
            .collect::<Vec<_>>()
            .join("::")
        }

        // let symbol = scope
        //   .resolve(path)
        //   .ok_or_else(|| Error::SymbolNotFound(path_string(path)))?;
        //
        // let Symbol::Type(typ) = &*symbol else {
        //   return Err(Error::SymbolNotType(path_string(path)).into())
        // };
        //
        // return Ok(typ.clone());
        //
        Type::dummy()
      },
      TE::Tuple(tuple) => {
        // dependencies = Vec::new();
        // ident = Intern::unique();
        // size = BigInt::ZERO;
        //
        // for item in tuple {
        //   let subtype = item.to_type(scope)?;
        //
        //   size += subtype.size.unwrap_or(size_of::<usize>());
        //   dependencies.push(subtype);
        // }
        //
        // let Some(size) = size.to_usize() else {
        //   return Err(Error::TypeTooBig(ident, size).into());
        // };

        Type::dummy()
      },
    };

    // Create a new type object.
    // let typ = Rc::new(Type {
    //   dependencies: dependencies.into_boxed_slice(),
    //   instance_members,
    //   static_members,
    // });

    // scope.add_type(typ);
    Ok(typ)
  }
}

// impl ToType for parser::Struct {
//   fn to_type(&self, scope: &Scope) -> Result<Rc<Type>> {
//     
//   }
// }

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Variable {
  /// The identifier of the variable in source code.
  ///
  /// This is not guaranteed to be unique with multiple scopes.
  pub ident: Intern,
  pub typ: Rc<Type>,
}

#[derive(Clone, Debug)]
pub enum Symbol {
  Function(Rc<Function>),
  Type(Rc<Type>),
  Variable(Rc<Variable>),
}

impl Symbol {
  pub fn ident(&self) -> Intern {
    match self {
      Self::Function(func) => &func.ident,
      Self::Type(typ) => &typ.ident,
      Self::Variable(var) => &var.ident,
    }.clone()
  }

  /// Returns an opaque pointer to the underlying member.
  /// Since `Rc`s can't share the same pointers, these are unique.
  fn ptr(&self) -> *const () {
    match self {
      Self::Function(func) => Rc::as_ptr(func) as *const (),
      Self::Type(typ) => Rc::as_ptr(typ) as *const (),
      Self::Variable(decl) => Rc::as_ptr(decl) as *const (),
    }
  }
}

impl Hash for Symbol {
  fn hash<H: Hasher>(&self, state: &mut H) {
    state.write_usize(self.ptr() as usize);
  }
}

impl PartialEq for Symbol {
  fn eq(&self, other: &Self) -> bool {
    self.ptr() == other.ptr()
  }
}

impl Eq for Symbol {}

pub trait ToSymbol {
  fn to_symbol(&self, scope: &Scope) -> Result<Symbol>;
}

// impl ToSymbol for parser::Struct {
//   fn to_symbol(&self, scope: &Scope) -> Result<Symbol> {
//     Symbol::Type(Rc::new(self.to_type(scope)?))
//   }
// }

macro_rules! base_type {
  ($ident:literal, align: $align:literal) => {
    Symbol::Type(Rc::new(Type {
      dependencies: Box::new([]),
      ident: $ident.intern(),
      instance_members: hashbrown::HashMap::new(),
      static_members: hashbrown::HashMap::new(),
      alignment: $align,
      size: None,
    }))
  };

  ($ident:literal, size: $size:literal, align: $align:literal) => {
    Symbol::Type(Rc::new(Type {
      dependencies: Box::new([]),
      ident: $ident.intern(),
      instance_members: hashbrown::HashMap::new(),
      static_members: hashbrown::HashMap::new(),
      alignment: $align,
      size: Some($size),
    }))
  };
}

// pub fn base_types() -> [Symbol; 13] {
//   [
//     base_type!("bool", size: 1, align: 1),
//     base_type!("char", size: 1, align: 1),
//     base_type!("string", size: 16, align: 8),
//     base_type!("f32", size: 4, align: 4),
//     base_type!("f64", size: 8, align: 8),
//     base_type!("i8", size: 1, align: 1),
//     base_type!("i16", size: 2, align: 2),
//     base_type!("i32", size: 4, align: 4),
//     base_type!("i64", size: 8, align: 8),
//     base_type!("u8", size: 1, align: 1),
//     base_type!("u16", size: 2, align: 2),
//     base_type!("u32", size: 4, align: 4),
//     base_type!("u64", size: 8, align: 8),
//   ]
// }
//
// pub fn base_symbols() -> HashMap<Intern, Rc<Symbol>> {
//   let mut symbols = HashMap::new();
//
//   for typ in base_types() {
//     symbols.insert(typ.ident(), Rc::new(typ));
//   }
//
//   symbols
// }
