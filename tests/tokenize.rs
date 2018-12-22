extern crate lispers;

use lispers::tokenize::{Loc, Token, TokenizationError, Tokenizer};

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
fn test_basic_consume() {
    let source = "(+ 1 12 )";
    let toks = Tokenizer::new().tokenize(source).unwrap();
    let goal = [
        token!(SexprBegin, 0, 0),
        ident!("+", 0, 1),
        lit!("1", 0, 3),
        lit!("12", 0, 5),
        token!(SexprEnd, 0, 8),
    ];
    assert_eq!(goal.len(), toks.len());
    for (correct, candidate) in goal.iter().zip(toks.iter()) {
        assert_eq!(correct, candidate);
    }
}

#[test]
fn test_string_consume() {
    let source = "(+ 1 2 \"foo\")";
    let toks = Tokenizer::new().tokenize(source).unwrap();
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

#[test]
fn test_nested_consume() {
    let source = "(+ 1 (* 3 4) \"foo\")";
    let toks = Tokenizer::new().tokenize(source).unwrap();
    let goal = [
        token!(SexprBegin, 0, 0),
        ident!("+", 0, 1),
        lit!("1", 0, 3),
        token!(SexprBegin, 0, 5),
        ident!("*", 0, 6),
        lit!("3", 0, 8),
        lit!("4", 0, 10),
        token!(SexprEnd, 0, 11),
        lit!("\"foo\"", 0, 13),
        token!(SexprEnd, 0, 18),
    ];
    assert_eq!(goal.len(), toks.len());
    for (correct, candidate) in goal.iter().zip(toks.iter()) {
        assert_eq!(correct, candidate);
    }
}

#[test]
fn test_unexpected_char() {
    let source = "(+ 1 (* 3 4)))";
    let err = Tokenizer::new().tokenize(source).err().unwrap();
    match err {
        TokenizationError::UnexpectedChar(c, l) => {
            assert_eq!(c, ')');
            assert_eq!(l, Loc::new(0, 13));
        }
        _ => panic!("Wrong error type!"),
    }
}
