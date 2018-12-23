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

#[derive(Debug)]
enum Atom {
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
            Atom::Unit => "()",
        }
    }
}

/// Bindings are of the form [ident expr ident2 expr2...]
type Binding = (Identifier, Expr);

/// A Form is a single variant of a lambda. Syntactically, they're written as ([Param*] Expr) pairs
type Form = (Vec<Identifier>, Expr);

#[derive(Debug)]
enum Expr {
    Literal(Atom),
    // Let(Vec<Binding>, Vec<Expr>),
    // Should Let be a macro? (let [bindings] exprs) ->
    // (do (def bind_ident1 bind_expr1))
    // Do(Vec<Expr>),
    // TODO: any way to verify we have at least one form?
    Lambda(Vec<Form>),
    // If I want there to be multiple forms, then Def should take
    // pairs of (Vec<Binding>, Box<Expr>)
    // TODO: defn should be a macro that rewrites to (def ident lambda)
    // Defn(Identifier, Vec<Binding>, Vec<Expr>),
    // (def ident (expr))
    Definition(Identifier, Box<Expr>),
    List(Vec<Expr>),
}

impl Expr {
    fn pprint(&self) {
        match self {
            Expr::Literal(a) => print!("{:?}", a),
            Expr::Definition(ident, bndgs) => (println!("(def {:?} {:?})", ident, bndgs)),
            Expr::List(exprs) => {
                print!("(");
                for expr in exprs.iter() {
                    expr.pprint();
                }
                print!(")");
            }
        }
        io::stdout().flush().unwrap();
    }
}

#[derive(Debug)]
enum ParseError {
    SyntaxError(String, Loc),
    UnexpectedToken(Token),
    UnexpectedEOF,
}

use Atom::*;
use Expr::*;
use ParseError::*;

struct Parser {}

type ParseResult<I> = Result<(Option<Expr>, Option<Peekable<I>>), ParseError>;
type TokenIter<'a> = Peekable<Iterator<Item = &'a Token>>;

impl Parser {
    fn new() -> Self {
        Parser {}
    }

    fn parse(&mut self, tokens: &[Token]) -> Result<Expr, ParseError> {
        use self::Expr::*;
        let mut exprs = vec![];
        let tokiter = tokens.iter().peekable();
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
                // TODO
                Token::SexprEnd(l) => Err(UnexpectedToken(token.clone())),
                // TODO
                Token::Ident(s, l) => Err(UnexpectedToken(token.clone())),
                // TODO
                Token::Literal(s, l) => Err(UnexpectedToken(token.clone())),
                // TODO
                Token::BracketBegin(l) => Err(UnexpectedToken(token.clone())),
                // TODO
                Token::BracketEnd(l) => Err(UnexpectedToken(token.clone())),
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
        let exprs = vec![];
        while let Some(next) = tokiter.peek() {
            if let Token::SexprEnd(l) = next {
                // done with this x-expr
                // TODO: ensure the Sexpr is well-formed and figure out its final type
                // TODO: look for the following idents as the first token:
                //       fn -> Expr::Lambda
                //       def -> Expr::Definition
                // The following should be errors:
                // first == Literal
                // Type-checking will later verify that the args are of the right type, etc.
                // TODO: check what kind of expr this is based on the first ident
                let l = exprs.len();
                if l == 0 {
                    // unit
                    return Ok((Some(Literal(Unit)), Some(tokiter)));
                }
                return match exprs.first().unwrap() {
                    Expr::Literal(Atom::Ident(i)) => match i.as_str() {
                        "def" => match exprs.as_slice() {
                            [_, Literal(Ident(i)), List(es)] => Ok((
                                Some(Definition(i.clone(), Box::new(List(*es)))),
                                Some(tokiter),
                            )),
                            [_, Literal(Ident(i)), Literal(l)] => Ok((
                                Some(Definition(i.clone(), Box::new(Literal(*l)))),
                                Some(tokiter),
                            )),
                            _ => Err(SyntaxError(
                                format!("Invalid `def` expression '{:?}'", exprs),
                                start_loc.clone(),
                            )),
                        },
                        // Lambda definition
                        "fn" => match exprs.as_slice() {
                            [_, Literal(Ident(i)), List(es)] => Ok((
                                Some(Definition(i.clone(), Box::new(List(*es)))),
                                Some(tokiter),
                            )),
                            [_, Literal(Ident(i)), Literal(l)] => Ok((
                                Some(Definition(i.clone(), Box::new(Literal(*l)))),
                                Some(tokiter),
                            )),
                            _ => Err(SyntaxError(
                                format!("Invalid `def` expression '{:?}'", exprs),
                                start_loc.clone(),
                            )),
                        },
                    },
                    // An analysis pass will find anything up with this
                    _ => Ok((Some(Expr::List(exprs)), Some(tokiter))),
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
        Err(ParseError::UnexpectedEOF)
    }

    fn consume_defn<'a, I>(&mut self, mut tokiter: Peekable<I>) -> ParseResult<I>
    where
        I: Iterator<Item = &'a Token>,
    {
        Ok((None, Some(tokiter)))
    }

    fn consume_lambda<'a, I>(&mut self, mut tokiter: Peekable<I>) -> ParseResult<I>
    where
        I: Iterator<Item = &'a Token>,
    {
        Ok((None, Some(tokiter)))
    }

    fn consume_lit<'a, I>(&mut self, mut tokiter: Peekable<I>) -> ParseResult<I>
    where
        I: Iterator<Item = &'a Token>,
    {
        Ok((None, Some(tokiter)))
    }
}
