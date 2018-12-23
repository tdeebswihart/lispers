#![feature(slice_patterns)]
use crate::tokenize::{Loc, Token};
use std::io::{self, Write};
use std::iter::Peekable;
use std::str::FromStr;
/// Allowable expressions for Lispy ulator
/// variable reference -> variable name, whose value is the var's value
/// constant literal   -> 12, -3.45e+6, etc
/// conditional        -> (if test consequence alternative)
/// definition         -> (define symbol expression)
/// procedure call     -> (prog arg...)

type Identifier = String;

#[derive(Debug, Clone)]
pub enum Atom {
    /// Literals
    Integer(i64),
    Float(f32),
    Str(String),
    Unit,
    Ident(Identifier),
}

impl ToString for Atom {
    fn to_string(&self) -> String {
        match self {
            Atom::Integer(i) => i.to_string(),
            Atom::Float(f) => f.to_string(),
            Atom::Ident(i) => i.clone(),
            Atom::Str(s) => s.clone(),
            Atom::Unit => "()".to_string(),
        }
    }
}

/// Bindings are of the form [ident expr ident2 expr2...]
type Binding = (Identifier, Expr);

/// A Form is a single variant of a lambda. Syntactically, they're written as ([Param*] Expr) pairs
// ([p1 p2...] expr)
type Form = (Vec<Identifier>, Expr);

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Atom),
    // Let(Vec<Binding>, Vec<Expr>),
    // Should Let be a macro? (let [bindings] exprs) ->
    // (do (def bind_ident1 bind_expr1))
    // Do(Vec<Expr>),
    // TODO: Allow for multiple List(Bracket, Expr) entries
    Lambda(Vec<Expr>),
    Vector(Vec<Expr>),
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
            Lambda(exprs) => format!(
                "(fn {})",
                exprs
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
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

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(String, Loc),
    UnexpectedToken(Token),
    UnexpectedEOF,
}

impl ToString for ParseError {
    fn to_string(&self) -> String {
        use self::ParseError::*;
        match self {
            SyntaxError(s, l) => format!("Syntax Error: {} @ {}", s, l.to_string()),
            UnexpectedToken(tok) => format!("Unexpected token {:?}", tok),
            UnexpectedEOF => format!("Unexpected EOF!"),
        }
    }
}

use self::Atom::*;
use self::Expr::*;
use self::ParseError::*;

pub struct Parser {}

type ParseResult<I> = Result<(Option<Expr>, Option<Peekable<I>>), ParseError>;

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

    pub fn parse(&mut self, tokens: &[Token]) -> Result<Expr, ParseError> {
        use self::Expr::*;
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
        Ok(List(exprs))
    }

    fn consume<'a, I>(&mut self, mut tokiter: Peekable<I>) -> ParseResult<I>
    where
        I: Iterator<Item = &'a Token>,
    {
        if let Some(token) = tokiter.next() {
            match token {
                Token::SexprBegin(l) => self.consume_sexpr(tokiter, l),
                Token::SexprEnd(_l) => Err(UnexpectedToken(token.clone())),
                Token::Ident(s, _l) => both!(Literal(Ident(s.clone())), tokiter),
                Token::Literal(s, _l) => {
                    let atom = match s.parse::<i64>() {
                        Ok(i) => Integer(i),
                        Err(_) => match s.parse::<f32>() {
                            Ok(f) => Float(f),
                            Err(_) => Str(s.clone()),
                        },
                    };
                    both!(Literal(atom), tokiter)
                }
                Token::BracketBegin(l) => self.consume_bracket(tokiter, l),
                Token::BracketEnd(_l) => Err(UnexpectedToken(token.clone())),
                _ => Err(UnexpectedToken(token.clone())),
            }
        } else {
            // No remaining tokens. This is only spot where this is fine
            Ok((None, None))
        }
    }

    /// Consume some amount of a token iterator in an attempt to build the contents of a s-expression
    fn consume_sexpr<'a, I>(&mut self, mut tokiter: Peekable<I>, start_loc: &Loc) -> ParseResult<I>
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
                            _ => Err(SyntaxError(
                                format!("Invalid `def` expression '{:?}'", exprs),
                                *start_loc,
                            )),
                        },
                        // Lambda definition. (fn $params $expr)
                        "fn" => match exprs.as_slice() {
                            [_, Vector(is), ex] => {
                                both!(Lambda(vec![Vector(is.clone()), ex.clone()]), tokiter)
                            }
                            _ => Err(SyntaxError(
                                format!("Invalid lambda (fn...) expression '{:?}'", exprs),
                                *start_loc,
                            )),
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
                    return Err(UnexpectedEOF);
                }
            }
        } // If we reach here, then we've run out of tokens before finding the
          // SexprEnd token.
        Err(UnexpectedEOF)
    }

    fn consume_bracket<'a, I>(
        &mut self,
        mut tokiter: Peekable<I>,
        start_loc: &Loc,
    ) -> ParseResult<I>
    where
        I: Iterator<Item = &'a Token>,
    {
        let mut exprs = vec![];
        while let Some(next) = tokiter.peek() {
            if let Token::BracketEnd(_) = next {
                // Punch through
                let _ = tokiter.next();
                return both!(Vector(exprs), tokiter);
            } else {
                if let (rs, Some(ti)) = self.consume(tokiter)? {
                    if let Some(expr) = rs {
                        exprs.push(expr);
                    }
                    tokiter = ti;
                } else {
                    // TODO: include location information for EOF when we can
                    return Err(UnexpectedEOF);
                }
            }
        } // If we reach here, then we've run out of tokens before finding the
          // SexprEnd token.
        Err(UnexpectedEOF)
    }
}
