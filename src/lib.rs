#![warn(missing_docs)]

//! The compiler library for the Weave programming language.

pub mod error;
pub mod parser;
pub mod scanner;
pub mod source;

pub use error::{Error, Result};
pub use parser::Parser;
pub use scanner::Scanner;
