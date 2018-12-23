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
enum Expr {
    Literal(Atom),
    // Let(Vec<Binding>, Vec<Expr>),
    // Should Let be a macro? (let [bindings] exprs) ->
    // (do (def bind_ident1 bind_expr1))
    // Do(Vec<Expr>),
    // TODO: Allow for multiple List(BracketExpr, Expr) entries
    Lambda(Vec<Expr>),
    BracketExpr(Vec<Identifier>),
    Call(Identifier, Vec<Expr>),
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
            Expr::Definition(ident, bndgs) => println!("(def {:?} {:?})", ident, bndgs),
            Expr::Lambda(exprs) => {
                print!("(fn ");
                for expr in exprs.iter() {
                    expr.pprint();
                }
                print!(")");
            }
            Expr::Call(i, exprs) => {
                print!("(");
                for expr in exprs.iter() {
                    expr.pprint();
                }
                print!(")");
            }
            Expr::BracketExpr(idents) => {
                print!("[");
                for ident in idents.iter() {
                    print!("{} ", ident);
                }
                print!("]");
            }
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

use self::Atom::*;
use self::Expr::*;
use self::ParseError::*;

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
        let mut tokiter = tokens.iter().peekable();
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
                Token::SexprEnd(_l) => Err(UnexpectedToken(token.clone())),
                // TODO
                Token::Ident(s, _l) => Ok((Some(Literal(Ident(s.clone()))), Some(tokiter))),
                // TODO
                Token::Literal(s, _l) => Err(UnexpectedToken(token.clone())),
                // TODO
                Token::BracketBegin(_l) => Err(UnexpectedToken(token.clone())),
                // TODO
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
                            [_, Literal(Ident(i)), List(es)] => {
                                Ok((Some(Definition(*i, Box::new(List(*es)))), Some(tokiter)))
                            }
                            [_, Literal(Ident(i)), Literal(l)] => {
                                Ok((Some(Definition(*i, Box::new(Literal(*l)))), Some(tokiter)))
                            }
                            _ => Err(SyntaxError(
                                format!("Invalid `def` expression '{:?}'", exprs),
                                *start_loc,
                            )),
                        },
                        // Lambda definition
                        "fn" => match exprs.as_slice() {
                            [_, BracketExpr(is), List(es)] => Ok((
                                Some(Lambda(vec![BracketExpr(*is), List(*es)])),
                                Some(tokiter),
                            )),
                            _ => Err(SyntaxError(
                                format!("Invalid lambda `fn` expression '{:?}'", exprs),
                                *start_loc,
                            )),
                        },
                        // An analysis pass will ensure that the identifier is in scope and that the args are correct
                        _ => Ok((Some(Call(*i, exprs[1..].to_vec())), Some(tokiter))),
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

    fn consume_ident<'a, I>(
        &mut self,
        ident: String,
        mut tokiter: Peekable<I>,
        loc: &Loc,
    ) -> ParseResult<I>
    where
        I: Iterator<Item = &'a Token>,
    {
        Ok((None, Some(tokiter)))
    }

    fn consume_bracket<'a, I>(&mut self, mut tokiter: Peekable<I>) -> ParseResult<I>
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
