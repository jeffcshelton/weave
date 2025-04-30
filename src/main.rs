mod error;
mod parser;
mod scanner;

use std::fs::File;

pub use error::{Error, Result};

use clap::{Parser, Subcommand};
use scanner::Scanner;

#[derive(Clone, Debug, Subcommand)]
enum Command {
  Scan {
    path: String,
  },
}

#[derive(Clone, Debug, Parser)]
struct Args {
  #[command(subcommand)]
  command: Command,
}

fn main() -> Result<()> {
  let args = Args::parse();

  match args.command {
    Command::Scan { path } => {
      let file = File::open(path)?;
      let mut scanner = Scanner::new(file)?;
      let mut stream = scanner.stream();

      while let Some(token) = stream.next() {
        println!("{:?}", token?);
      }
    },
  }

  Ok(())
}
