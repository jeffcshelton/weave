use std::process;

use clap::{Parser as CommandParser, Subcommand};
use weave::{Parser, Result, Scanner, parser::Unit};

#[derive(Clone, Debug, Subcommand)]
enum Command {
  Parse {
    paths: Vec<String>,
  },
  Scan {
    paths: Vec<String>,
  },
}

#[derive(Clone, CommandParser, Debug)]
struct Args {
  #[command(subcommand)]
  command: Command,
}

// Reserve main for error handling.
fn main() {
  if let Err(error) = delegate() {
    eprintln!("\x1b[31;1merror\x1b[0m: {error}");
    process::exit(1);
  }
}

fn delegate() -> Result<()> {
  let args = Args::parse();

  match args.command {
    Command::Parse { paths } => {
      let mut scanner = Scanner::empty();

      for path in &paths {
        scanner.load(path)?;

        let mut parser = Parser::new(scanner.stream());
        let unit = parser.parse::<Unit>()?;

        println!("--- \x1b[1m{path}\x1b[0m ---");
        println!("{unit:#?}");
        println!();
      }
    },
    Command::Scan { paths } => {
      let mut scanner = Scanner::empty();

      for path in &paths {
        scanner.load(path)?;

        println!("--- \x1b[1m{path}\x1b[0m ---");

        for token in scanner.stream() {
          println!("{:?}", token?);
        }

        println!();
      }
    },
  }

  Ok(())
}
