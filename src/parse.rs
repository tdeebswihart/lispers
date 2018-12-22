use crate::tokenize::Token;
use std::io::{self, Write};
use std::str::FromStr;
/// Allowable expressions for Lispy ulator
/// variable reference -> variable name, whose value is the var's value
/// constant literal   -> 12, -3.45e+6, etc
/// conditional        -> (if test consequence alternative)
/// definition         -> (define symbol expression)
/// procedure call     -> (prog arg...)

#[derive(Debug)]
enum Lit {
    Integer(i64),
    Float(f32),
    Str(String),
    Ident(String),
}

impl ToString for Lit {
    fn to_string(&self) -> String {
        match self {
            Lit::Integer(i) => i.to_string(),
            Lit::Float(f) => f.to_string(),
            Lit::Ident(s) => s.clone(),
            Lit::Str(s) => s.clone(),
        }
    }
}

#[derive(Debug)]
enum Ast {
    Literal(Lit),
    List(Vec<Ast>),
}

// Exprs are lists of other exprs...

impl Ast {
    fn pprint(&self) {
        match self {
            Ast::Literal(a) => print!("{:?}", a),
            Ast::List(asts) => {
                print!("(");
                for ast in asts.iter() {
                    ast.pprint();
                }
                print!(")");
            }
        }
        io::stdout().flush().unwrap();
    }
}

#[derive(Debug)]
enum ParseError {
    SyntaxError(String),
    UnexpectedEOF,
}

struct Parser {}

fn parse(tokens: &[Token]) -> Result<Ast, ParseError> {
    Err(ParseError::UnexpectedEOF)
}
