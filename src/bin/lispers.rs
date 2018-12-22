extern crate lispers;

use std::{env, process};

use lispers::tokenize::Tokenizer;
fn main() {
    if env::args().len() < 2 {
        eprintln!("Usage: lispers PROGRAM");
        process::exit(1);
    }
    let program = env::args().nth(1).unwrap();
    println!("Parsing {}", program);
    let toks = match Tokenizer::parse(&program) {
        Ok(a) => a,
        Err(e) => panic!(e),
    };
    println!("{:?}", toks);
}
