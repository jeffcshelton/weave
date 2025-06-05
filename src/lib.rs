#![warn(missing_docs)]

//! The compiler library for the Weave programming language.

pub mod analyzer;
pub mod error;
pub mod jit;
pub mod lexer;
pub mod parser;
pub mod source;

pub use error::{Error, Result};
pub use parser::Parser;
pub use lexer::{Lexer, token::Token};

mod intern;
pub use intern::Intern;
