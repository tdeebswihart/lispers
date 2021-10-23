extern crate lispers;
use anyhow::Result;

use std::{env, process};

use lispers::interpret::VM;
use lispers::parse::Parser;
use lispers::tokenize::Tokenizer;

fn run() -> Result<()> {
    let program = env::args().nth(1).unwrap();
    //println!("prog: {}", program);
    let toks = Tokenizer::new().tokenize(&program)?;
    if toks.len() == 0 {
        return Ok(());
    }
    //println!("tok:\n\n{:?}", toks);

    let mut parser = Parser::new();
    let ast = parser.parse(toks.as_slice())?;
    if ast.len() == 0 {
        return Ok(());
    }
    //println!("ast:\n\n{:?}", ast);

    let mut vm = VM::new();
    vm.interpret(ast)?;
    Ok(())
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
