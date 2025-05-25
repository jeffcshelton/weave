//! Components for string interning and caching for memory and performance.

use std::{
  borrow::Borrow,
  collections::HashSet,
  fmt::{self, Debug, Display, Formatter},
  ops::Deref,
  sync::{Arc, LazyLock, Mutex},
};

/// A global pool holding all interned strings.
static POOL: LazyLock<Mutex<HashSet<Intern>>> = LazyLock::new(|| {
  Mutex::new(HashSet::new())
});

/// An interned string, holding an internal reference to the string pool.
///
/// All interned strings are immutable, as one instance cannot be mutated
/// without mutating all references to it.
#[derive(Clone, Hash)]
pub struct Intern(Arc<str>);

impl Borrow<str> for Intern {
  fn borrow(&self) -> &str {
    &self.0
  }
}

impl Borrow<Arc<str>> for Intern {
  fn borrow(&self) -> &Arc<str> {
    &self.0
  }
}

impl Debug for Intern {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "\"{}\" <{:p}>", self.0, self.0.as_ptr())
  }
}

impl Deref for Intern {
  type Target = str;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl Display for Intern {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    fmt::Display::fmt(&self.0, f)
  }
}

impl Eq for Intern {}

impl<S: AsRef<str>> From<S> for Intern {
  fn from(string: S) -> Self {
    let string = string.as_ref();
    let mut pool = POOL.lock().unwrap();

    match pool.get(string) {
      Some(interned) => interned.clone(),
      None => {
        let interned = Intern(Arc::from(string));
        pool.insert(interned.clone());
        interned
      },
    }
  }
}

impl PartialEq for Intern {
  fn eq(&self, other: &Self) -> bool {
    Arc::ptr_eq(&self.0, &other.0)
  }
}
