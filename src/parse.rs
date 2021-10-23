use crate::tokenize::{Loc, Token};
use snafu::Snafu;
use std::fmt;
use std::iter::Peekable;
/// Allowable expressions for Lispy ulator
/// variable reference -> variable name, whose value is the var's value
/// constant literal   -> 12, -3.45e+6, etc
/// conditional        -> (if test consequence alternative)
/// definition         -> (define symbol expression)
/// procedure call     -> (prog arg...)

pub type Identifier = String;
pub type Binding = String;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Atom {
    /// Literals
    Integer(i64),
    Float(f32),
    Str(String),
    Unit,
    Ident(Identifier),
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Atom::*;
        match self {
            Unit => write!(f, "()"),
            Integer(i) => write!(f, "{}", i),
            Float(i) => write!(f, "{}", i),
            Str(s) => write!(f, "{}", s),
            Ident(i) => write!(f, "{}", i),
        }
    }
}

// ([p1 p2...] expr)
// type Form = (Vec<Identifier>, Expr);

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Atom),
    // Let(Vec<Binding>, Vec<Expr>),
    // Should Let be a macro? (let [bindings] exprs) ->
    // (do (def bind_ident1 bind_expr1))
    // Do(Vec<Expr>),
    // TODO: Allow for multiple List(Bracket, Expr) entries
    Lambda(Vec<Binding>, Box<Expr>),
    Vector(Vec<Binding>),
    Call(Identifier, Vec<Expr>),
    // If I want there to be multiple forms, then Def should take
    // pairs of (Vec<Binding>, Box<Expr>)
    // TODO: defn should be a macro that rewrites to (def ident lambda)
    // Defn(Identifier, Vec<Binding>, Vec<Expr>),
    // (def ident (expr))
    Definition(Identifier, Box<Expr>),
    List(Vec<Expr>),
}

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            Literal(a) => a.to_string(),
            Lambda(bindings, body) => format!("(fn [{}] {})", bindings.join(" "), body.to_string()),
            Vector(exprs) => format!(
                "[{}]",
                exprs
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Call(ident, exprs) => format!(
                "({} {})",
                ident,
                exprs
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Definition(ident, expr) => format!("(def {} {})", ident, expr.to_string()),
            List(exprs) => format!(
                "({})",
                exprs
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
        }
    }
}

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("syntax error: {} @ {}", err, loc.to_string()))]
    SyntaxError { err: String, loc: Loc },
    #[snafu(display("unexpected token {:?}", tok))]
    UnexpectedToken { tok: Token },
    #[snafu(display("{:?} is not a valid binding", val.to_string()))]
    InvalidBinding { val: Expr },
    #[snafu(display("unexpected eof"))]
    UnexpectedEOF,
}

use self::Atom::*;
use self::Expr::*;

pub struct Parser {}

type Result<I, E = Error> = std::result::Result<(Option<Expr>, Option<Peekable<I>>), E>;

// I'm lazy so use macros to simplify specifying return values
macro_rules! both {
    ($token:expr, $iter:ident) => {
        Ok((Some($token), Some($iter)))
    };
}

impl Parser {
    pub fn new() -> Self {
        Parser {}
    }

    pub fn parse(&mut self, tokens: &[Token]) -> std::result::Result<Vec<Expr>, Error> {
        let mut exprs = vec![];
        let mut tokiter = tokens.into_iter().peekable();
        loop {
            match self.consume(tokiter)? {
                (rs, Some(ti)) => {
                    if let Some(expr) = rs {
                        exprs.push(expr);
                    }
                    tokiter = ti;
                }
                (rs, None) => {
                    if let Some(expr) = rs {
                        exprs.push(expr);
                    }
                    break;
                }
            }
        }
        Ok(exprs)
    }

    fn consume<'a, I>(&mut self, mut tokiter: Peekable<I>) -> Result<I>
    where
        I: Iterator<Item = &'a Token>,
    {
        if let Some(token) = tokiter.next() {
            match token {
                Token::SexprBegin(l) => self.consume_sexpr(tokiter, l),
                Token::Ident(s, _l) => both!(Literal(Ident(s.clone())), tokiter),
                Token::Literal(s, _l) => {
                    let atom = match s.parse::<i64>() {
                        Ok(i) => Integer(i),
                        Err(_) => match s.parse::<f32>() {
                            Ok(f) => Float(f),
                            Err(_) => Str(s.trim_matches('"').to_string()),
                        },
                    };
                    both!(Literal(atom), tokiter)
                }
                Token::BracketBegin(l) => self.consume_bracket(tokiter, l),
                _ => Err(Error::UnexpectedToken { tok: token.clone() }),
            }
        } else {
            // No remaining tokens. This is only spot where this is fine
            Ok((None, None))
        }
    }

    /// Consume some amount of a token iterator in an attempt to build the contents of a s-expression
    fn consume_sexpr<'a, I>(&mut self, mut tokiter: Peekable<I>, start_loc: &Loc) -> Result<I>
    where
        I: Iterator<Item = &'a Token>,
    {
        let mut exprs = vec![];
        while let Some(next) = tokiter.peek() {
            if let Token::SexprEnd(_l) = next {
                // Punch through
                let _ = tokiter.next();
                let l = exprs.len();
                if l == 0 {
                    // unit
                    return both!(Literal(Unit), tokiter);
                }
                return match exprs.first().unwrap() {
                    Expr::Literal(Atom::Ident(i)) => match i.as_str() {
                        "def" => match exprs.as_slice() {
                            // (def ident $expr)
                            [_, Literal(Ident(i)), ex] => {
                                both!(Definition(i.clone(), Box::new(ex.clone())), tokiter)
                            }
                            _ => Err(Error::SyntaxError {
                                err: format!("Invalid `def` expression '{:?}'", exprs),
                                loc: *start_loc,
                            }),
                        },
                        // Lambda definition. (fn $params $expr)
                        "fn" => match exprs.as_slice() {
                            [_, Vector(bindings), body] => {
                                both!(Lambda(bindings.clone(), Box::new(body.clone())), tokiter)
                            }
                            _ => Err(Error::SyntaxError {
                                err: format!("Invalid lambda (fn...) expression '{:?}'", exprs),
                                loc: *start_loc,
                            }),
                        },
                        // An analysis pass will ensure that the identifier is in scope and that the args are correct
                        _ => both!(Call(i.clone(), exprs[1..].to_vec()), tokiter),
                    },
                    // An analysis pass will find anything up with this
                    _ => both!(Expr::List(exprs), tokiter),
                };
            } else {
                if let (rs, Some(ti)) = self.consume(tokiter)? {
                    if let Some(expr) = rs {
                        exprs.push(expr);
                    }
                    tokiter = ti;
                } else {
                    // TODO: include location information for EOF when we can
                    return Err(Error::UnexpectedEOF);
                }
            }
        } // If we reach here, then we've run out of tokens before finding the
          // SexprEnd token.
        Err(Error::UnexpectedEOF)
    }

    fn consume_bracket<'a, I>(&mut self, mut tokiter: Peekable<I>, _start_loc: &Loc) -> Result<I>
    where
        I: Iterator<Item = &'a Token>,
    {
        let mut bindings = vec![];
        while let Some(next) = tokiter.peek() {
            if let Token::BracketEnd(_) = next {
                // Punch through
                let _ = tokiter.next();
                return both!(Vector(bindings), tokiter);
            } else {
                match self.consume(tokiter)? {
                    (Some(Literal(Ident(binding))), Some(ti)) => {
                        bindings.push(binding);
                        tokiter = ti;
                    }
                    (Some(expr), Some(_ti)) => return Err(Error::InvalidBinding { val: expr }),
                    (None, Some(ti)) => {
                        tokiter = ti;
                    }
                    (_, None) => return Err(Error::UnexpectedEOF),
                }
            }
        } // If we reach here, then we've run out of tokens before finding the
          // SexprEnd token.
        Err(Error::UnexpectedEOF)
    }
}
