extern crate lispers;

use lispers::tokenize::{Loc, Token, Tokenizer};

macro_rules! token {
    ($id:ident, $line:expr, $col:expr) => {
        Token::$id(Loc::new($line, $col))
    };
}

macro_rules! ident {
    ($v:expr, $line:expr, $col:expr) => {
        Token::Ident($v.to_string(), Loc::new($line, $col))
    };
}

macro_rules! lit {
    ($v:expr, $line:expr, $col:expr) => {
        Token::Literal($v.to_string(), Loc::new($line, $col))
    };
}

macro_rules! s {
    ($v:expr) => {
        $v.to_string()
    };
}

#[test]
fn test_basic_parse() {
    let source = "(+ 1 2 )";
    let toks = Tokenizer::parse(source).unwrap();
    let goal = [
        token!(SexprBegin, 0, 0),
        ident!("+", 0, 1),
        lit!("1", 0, 3),
        lit!("2", 0, 5),
        token!(SexprEnd, 0, 7),
    ];
    assert_eq!(goal.len(), toks.len());
    for (correct, candidate) in goal.iter().zip(toks.iter()) {
        assert_eq!(correct, candidate);
    }
}

#[test]
fn test_string_parse() {
    let source = "(+ 1 2 \"foo\")";
    let toks = Tokenizer::parse(source).unwrap();
    let goal = [
        token!(SexprBegin, 0, 0),
        ident!("+", 0, 1),
        lit!("1", 0, 3),
        lit!("2", 0, 5),
        lit!("\"foo\"", 0, 7),
        token!(SexprEnd, 0, 12),
    ];
    assert_eq!(goal.len(), toks.len());
    for (correct, candidate) in goal.iter().zip(toks.iter()) {
        assert_eq!(correct, candidate);
    }
}
