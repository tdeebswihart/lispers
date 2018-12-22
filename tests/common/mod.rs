#![macro_escape]

extern crate lispers;

use lispers::tokenize::{Loc, Token, TokenizationError, Tokenizer};

#[macro_export]
macro_rules! token {
    ($id:ident, $line:expr, $col:expr) => {
        Token::$id(Loc::new($line, $col))
    };
}

#[macro_export]
macro_rules! ident {
    ($v:expr, $line:expr, $col:expr) => {
        Token::Ident($v.to_string(), Loc::new($line, $col))
    };
}

#[macro_export]
macro_rules! lit {
    ($v:expr, $line:expr, $col:expr) => {
        Token::Literal($v.to_string(), Loc::new($line, $col))
    };
}
