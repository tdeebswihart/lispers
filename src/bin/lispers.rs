extern crate lispers;

use std::{env, process};

use lispers::parse::Parser;
use lispers::tokenize::Tokenizer;
fn main() {
    if env::args().len() < 2 {
        eprintln!("Usage: lispers PROGRAM");
        process::exit(1);
    }
    let program = env::args().nth(1).unwrap();
    let toks = match Tokenizer::new().tokenize(&program) {
        Ok(a) => a,
        Err(e) => panic!(e),
    };
    println!("tok:\n\n{:?}", toks);
    let mut parser = Parser::new();
    let ast = match parser.parse(toks.as_slice()) {
        Ok(a) => a,
        Err(e) => panic!("{}", e.to_string()),
    };

    println!("ast:\n\n{:?}", ast);
}
