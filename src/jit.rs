//! Just-in-time compilation.

use crate::{parser::{Function, Import, Unit}, Lexer, Parser, Result};
use std::{collections::HashSet, fmt::{self, Display, Formatter}, path::Path};

/// The just-in-time compilation runtime.
#[derive(Clone, Debug, Default)]
pub struct JIT {
  /// Set of all absolute paths of all imports in the source tree.
  /// This is recorded and checked to avoid double imports.
  imports: HashSet<Box<Path>>,
}

impl JIT {
  /// Adds a function to the JIT source tree.
  pub fn add_function(&mut self, _function: Function) -> Result<()> {
    Ok(())
  }

  /// Adds a single import line, and all of its descedent imports, into the JIT
  /// source tree.
  pub fn add_import(
    &mut self,
    import: Import,
    base: impl AsRef<Path>,
  ) -> Result<()> {
    // Construct the import set using all files specified in the import line.
    let mut import_set = ImportSet::with_base(base);
    for file in import.files {
      import_set.update(&*file)?;
    }

    // Check that the language of the import is supported.
    let language = import_set.language()?;
    if !language.supported() {
      return Err(Error::LanguageUnsupported(language).into());
    }

    let mut lexer = Lexer::empty();

    for source in import_set.sources {
      // If the import source has already been read and added, then don't add it
      // a second time.
      if self.imports.contains(&source) {
        continue;
      }

      // Load the source into the lexer for streaming.
      lexer.load(&source)?;

      // Parse the whole source file into a compilation unit.
      let mut parser = Parser::new(lexer.stream());
      let unit = parser.consume::<Unit>()?;

      // Add this import's unit.
      self.imports.insert(source.clone());
      self.add_unit(unit, &source)?;
    }

    Ok(())
  }

  /// Adds a compilation unit to the JIT runtime.
  pub fn add_unit(&mut self, unit: Unit, path: impl AsRef<Path>) -> Result<()> {
    let path = path.as_ref();
    let Unit { functions, imports } = unit;

    for function in functions {
      self.add_function(function)?;
    }

    // SAFETY: This unwrap is safe because if the path here is being passed in
    // with the unit, then it must have already been read to produce the `Unit`.
    // The documentation of `parent` indicates that it only returns `None` when
    // the path is a root or a prefix, which cannot happen for any valid file.
    //
    // Also, if somehow these invariants are not withheld, the right course of
    // action here is to fail anyway. While panicking is not the ideal course of
    // failure, it achieves the same goal with a slightly worse message.
    let absolute = path.canonicalize()?;
    let base = absolute.parent().unwrap();

    for import in imports {
      self.add_import(import, base)?;
    }

    Ok(())
  }

  /// Runs a single source file, including its imports, starting at the main
  /// function.
  pub fn run(path: impl AsRef<Path>) -> Result<()> {
    let path = path.as_ref();

    // Lex and parse the base compilation unit.
    let mut lexer = Lexer::from_path(path)?;
    let mut parser = Parser::new(lexer.stream());
    let unit = parser.consume::<Unit>()?;

    // Add the base compilation unit to the JIT runtime.
    let mut jit = JIT::default();
    jit.add_unit(unit, path)?;

    Ok(())
  }
}

/// A recognized file type, determined by the extension.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum FileType {
  /// C header file
  CHeader,

  /// C source file
  CSource,

  /// C++ header file
  CppHeader,

  /// C++ source file
  CppSource,

  /// Objective-C source file
  ObjectiveC,

  /// Swift source file
  Swift,

  /// Weave source file
  Weave,
}

impl FileType {
  /// Constructs a `FileType` from a path to the corresponding file.
  pub fn from_path(path: impl AsRef<Path>) -> Result<Self> {
    let path = path.as_ref();
    let extension = path
      .extension()
      .and_then(|ext| ext.to_str())
      .ok_or_else(|| Error::FileTypeUnrecognized(Box::from(path)))?;

    let file_type = match extension {
      "h" => Self::CHeader,
      "c" => Self::CSource,
      "hh" | "hpp" | "hxx" => Self::CppHeader,
      "cc" | "cpp" | "cxx" => Self::CppSource,
      "swift" => Self::Swift,
      "w" => Self::Weave,
      _ => return Err(Error::FileTypeUnrecognized(Box::from(path)).into()),
    };

    Ok(file_type)
  }

  /// Returns whether the file type is a header file.
  pub fn is_header(self) -> bool {
    matches!(self, Self::CHeader | Self::CppHeader)
  }

  /// Returns whether the file type is a source file.
  pub fn is_source(self) -> bool {
    matches!(
      self,
      Self::CSource
      | Self::CppSource
      | Self::ObjectiveC
      | Self::Swift
      | Self::Weave,
    )
  }

  /// Returns the corresponding language of the file type.
  ///
  /// For some file types, such as C headers, there is a _preferred_ language,
  /// but this does not entirely restrict the set of languages which may use
  /// files of that type. These cases must be handled separately; the preferred
  /// language is returned by this function.
  pub fn language(self) -> Language {
    match self {
      Self::CHeader => Language::C,
      Self::CSource => Language::C,
      Self::CppHeader => Language::Cpp,
      Self::CppSource => Language::Cpp,
      Self::ObjectiveC => Language::ObjectiveC,
      Self::Swift => Language::Swift,
      Self::Weave => Language::Weave,
    }
  }
}

impl Display for FileType {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    let string = match self {
      Self::CHeader => "C (header)",
      Self::CSource => "C (source)",
      Self::CppHeader => "C++ (header)",
      Self::CppSource => "C++ (source)",
      Self::ObjectiveC => "Objective-C",
      Self::Swift => "Swift",
      Self::Weave => "Weave",
    };

    write!(f, "{string}")
  }
}

/// A set of imports sharing a common base path and language.
///
/// One `ImportSet` corresponds to a single import line.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImportSet {
  base: Box<Path>,
  headers: HashSet<Box<Path>>,
  sources: HashSet<Box<Path>>,
  language: Option<Language>,
}

impl ImportSet {
  /// Constructs a new `ImportSet` with a specified absolute base path.
  pub fn with_base(base: impl AsRef<Path>) -> Self {
    Self {
      base: Box::from(base.as_ref()),
      headers: HashSet::new(),
      sources: HashSet::new(),
      language: None,
    }
  }

  /// Returns the language of the import set.
  pub fn language(&self) -> Result<Language> {
    self.language.ok_or(Error::LanguageAmbiguous.into())
  }

  /// Attempts to update the import set by adding a new file.
  pub fn update(&mut self, path: impl AsRef<Path>) -> Result<bool> {
    let path = path.as_ref();
    let file_type = FileType::from_path(path)?;

    // Determine the common language of the import.
    // Also, check that the files are compatible with each other.
    if let Some(language) = self.language {
      if !language.compatible(file_type) {
        return Err(Error::FileTypeIncompatible(file_type, language).into());
      }
    } else {
      self.language = Some(file_type.language());
    }

    // Calculate the full path by joining the base with the relative.
    let path = Box::from(self.base.join(path));

    let updated = if file_type.is_source() {
      self.sources.insert(path)
    } else if file_type.is_header() {
      self.headers.insert(path)
    } else {
      false
    };

    Ok(updated)
  }
}

/// A programming language recognized by the Weave compiler.
///
/// A language being on this list does not imply that it is supported.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Language {
  /// The C programming language.
  C,

  /// The C++ programming language.
  Cpp,

  /// The Objective-C programming language.
  ObjectiveC,

  /// The Swift programming language.
  Swift,

  /// The Weave programming language.
  Weave,
}

impl Language {
  /// Whether the file type given can be used in this language.
  pub fn compatible(self, file_type: FileType) -> bool {
    // There is a special exception for C headers, as the same file type is used
    // for C, C++, and Objective-C headers.
    if file_type == FileType::CHeader {
      return [Self::C, Self::Cpp, Self::ObjectiveC].contains(&self);
    }

    self == file_type.language()
  }

  /// Whether this language is supported by the Weave compiler for integration.
  pub fn supported(self) -> bool {
    matches!(self, Self::Weave)
  }
}

impl Display for Language {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    let string = match self {
      Self::C => "C",
      Self::Cpp => "C++",
      Self::ObjectiveC => "Objective-C",
      Self::Swift => "Swift",
      Self::Weave => "Weave",
    };

    write!(f, "{string}")
  }
}

/// An error originating from or relating to the JIT runtime.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Error {
  /// The file type is incompatible with the pre-determined language.
  FileTypeIncompatible(FileType, Language),

  /// An import path was specified which does not have a recognized file type.
  FileTypeUnrecognized(Box<Path>),

  /// The language of an import cannot be determined by the file types.
  LanguageAmbiguous,

  /// The language detected in an import is unsupported for integration.
  LanguageUnsupported(Language),
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::FileTypeIncompatible(file_type, language) => {
        write!(f, "file type '{file_type}' is incompatible with '{language}'")
      },
      Self::FileTypeUnrecognized(path) => {
        write!(f, "unrecognized file type for '{}'", path.display())
      },
      Self::LanguageAmbiguous => {
        write!(f, "import language ambiguous")
      },
      Self::LanguageUnsupported(language) => {
        write!(f, "unsupported language {language}")
      },
    }
  }
}

impl std::error::Error for Error {}
