use std::io::{self, Write};
use std::str::FromStr;
/// Allowable expressions for Lispy Calculator
/// variable reference -> variable name, whose value is the var's value
/// constant literal   -> 12, -3.45e+6, etc
/// conditional        -> (if test consequence alternative)
/// definition         -> (define symbol expression)
/// procedure call     -> (prog arg...)

#[derive(Debug)]
enum CalcAtom {
    Integer(i64),
    Float(f32),
    Symbol(String),
}

impl ToString for CalcAtom {
    fn to_string(&self) -> String {
        match self {
            CalcAtom::Integer(i) => i.to_string(),
            CalcAtom::Float(f) => f.to_string(),
            CalcAtom::Symbol(s) => s.clone(),
        }
    }
}

impl FromStr for CalcAtom {
    type Err = ParseError;
    fn from_str(token: &str) -> Result<CalcAtom, Self::Err> {
        use self::CalcAtom::*;
        Ok(match token.parse::<i64>() {
            Ok(i) => Integer(i),
            Err(_) => match token.parse::<f32>() {
                Ok(f) => Float(f),
                Err(_) => Symbol(token.to_string()),
            },
        })
    }
}

#[derive(Debug)]
enum CalcAst {
    Atom(CalcAtom),
    List(Vec<CalcAst>),
}

impl CalcAst {
    fn pprint(&self) {
        match self {
            CalcAst::Atom(a) => print!("{:?}", a),
            CalcAst::List(asts) => {
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
