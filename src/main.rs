//! The Weave compiler terminal application.

use clap::{Parser as CommandParser, Subcommand};
use std::process;
use weave::{
  analyzer::Scope, jit::JIT, lexer::token::Tokenize, parser::{Expression, Unit}, Lexer, Parser, Result
};

#[derive(Clone, Debug, Subcommand)]
enum Command {
  Analyze { path: String },
  Expr { path: String },
  Lex { paths: Vec<String> },
  Parse { paths: Vec<String> },
  Run { path: String },
}

#[derive(Clone, CommandParser, Debug)]
struct Args {
  #[command(subcommand)]
  command: Command,
}

// `main` is reserved for error handling.
fn main() {
  if let Err(error) = delegate() {
    eprintln!("\x1b[31;1merror\x1b[0m: {error}");
    process::exit(1);
  }
}

fn delegate() -> Result<()> {
  let args = Args::parse();

  // Set the target configuration.
  // TODO: Support cross-compilation.

  match args.command {
    Command::Analyze { path } => {
      let mut lexer = Lexer::from_path(&path)?;
      let mut parser = Parser::new(lexer.stream());
      let unit = parser.consume::<Unit>()?;
      let scope = Scope::from_unit(&unit);

      println!("{scope:#?}");
    },
    Command::Expr { path } => {
      let mut lexer = Lexer::from_path(&path)?;
      let mut parser = Parser::new(lexer.stream());
      let expr = parser.consume::<Expression>()?;

      println!("{}", expr.parenthesized().tokens());
    },
    Command::Lex { paths } => {
      let mut lexer = Lexer::empty();

      for path in &paths {
        lexer.load(path)?;

        println!("--- \x1b[1m{path}\x1b[0m ---");

        for token in lexer.stream() {
          println!("{:?}", token?);
        }

        println!();
      }
    },
    Command::Parse { paths } => {
      let mut lexer = Lexer::empty();

      for path in &paths {
        lexer.load(path)?;

        let mut parser = Parser::new(lexer.stream());
        let unit = parser.consume::<Unit>()?;

        println!("--- \x1b[1m{path}\x1b[0m ---");
        println!("{unit:#?}");
        println!();
      }
    },
    Command::Run { path } => {
      JIT::run(path)?;
    },
  }

  Ok(())
}
