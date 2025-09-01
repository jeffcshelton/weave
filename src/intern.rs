//! Components for string interning and caching for memory and performance.

use std::{
  borrow::Borrow,
  collections::HashSet,
  fmt::{self, Debug, Display, Formatter},
  ops::Deref,
  sync::{Arc, LazyLock, Mutex},
};

/// A global pool holding all interned strings.
///
/// At first glance, the existence of this global variable may seem to
/// contradict one of the central principles of this compiler, which is not to
/// have global state. However, the real guiding theme is that global state
/// cannot materially change the behavior of the compiler if, say, multiple
/// independent compilations were to be done during the lifetime of a single
/// execution instead of one.
///
/// Interning and pooling are purely for _performance_ reasons, and they do not
/// materially affect the behavior of the compiler in terms of output. They do
/// not violate the principle, as the output with or without interning is
/// identical.
static POOL: LazyLock<Mutex<HashSet<Intern>>> = LazyLock::new(|| {
  Mutex::new(HashSet::new())
});

/// An interned string, holding an internal reference to the string pool.
///
/// All interned strings are immutable, as one instance cannot be mutated
/// without mutating all references to it.
#[derive(Clone, Default, Hash, Ord, PartialOrd)]
pub struct Intern(Arc<str>);

impl Intern {
  /// Generates and interns a unique, random string to be used internally.
  ///
  /// This is used for objects that require an unique identifier but have no
  /// obvious human-readable name.
  pub fn unique() -> Intern {
    use rand::distr::{Alphanumeric, SampleString};

    // Consider optimizing this for 1 allocation instead of 2 using unsafe code
    // if it becomes a performance bottleneck. Also, with 16 random characters,
    // checking if the string is in the pool should not be necessary.

    let mut pool = POOL.lock().unwrap();
    let mut interned;

    // Generate random 16-character strings until the pool does not already
    // contain it (this should be basically guaranteed on the first try).
    loop {
      let random = Alphanumeric.sample_string(&mut rand::rng(), 16);
      interned = Intern(Arc::from(random));

      if !pool.contains(&interned) {
        break;
      }
    }

    pool.insert(interned.clone());
    interned
  }
}

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

/// A trait for string-like types that can be interned.
pub trait ToIntern {
  /// Converts a string-like type to an interned string.
  fn intern(self) -> Intern;
}

impl<T> ToIntern for T where Intern: From<T> {
  fn intern(self) -> Intern {
    Intern::from(self)
  }
}
