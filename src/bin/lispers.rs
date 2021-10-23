extern crate lispers;
use anyhow::Result;

use std::{env, process};

use lispers::parse::Parser;
use lispers::tokenize::Tokenizer;

fn run() -> Result {
    let program = env::args().nth(1).unwrap();
    let toks = Tokenizer::new().tokenize(&program)?;
    println!("tok:\n\n{:?}", toks);
    let mut parser = Parser::new();
    let ast = parser.parse(toks.as_slice())?;

    println!("ast:\n\n{:?}", ast);
}

fn main() {
    if env::args().len() < 2 {
        eprintln!("Usage: lispers PROGRAM");
        process::exit(1);
    }

    if let Err(e) = run() {
        eprintln!("error: {:?}", e);
        process::exit(1);
    }
}
