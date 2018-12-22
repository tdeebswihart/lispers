use std::cmp::{Eq, PartialEq};
use std::str::FromStr;

/// `Loc`s define code locations. They are `Token`s that exist
/// in the (non-inclusive) range `[lbegin, lend)` on line `line`.
/// `begin` and `end` define the raw character range.
#[derive(Debug, PartialEq, Eq)]
pub struct Loc {
    line: usize,
    chr: usize,
}

impl Loc {
    pub fn new(l: usize, c: usize) -> Self {
        Loc { line: l, chr: c }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    SexprBegin(Loc),
    SexprEnd(Loc),

    /// Variable names, function names, etc.
    Ident(String, Loc),

    /// Literals (numbers, strings...)
    Literal(String, Loc),

    /// Data structures
    QuoteBegin(Loc),
    QuoteEnd(Loc),
    // TODO: implement the following
    VecBegin(Loc),
    VecEnd(Loc),
    SetBegin(Loc),
    SetEnd(Loc),
    HashMapBegin(Loc),
    HashMapEnd(Loc),
}
use self::Token::*;

#[derive(Debug)]
pub enum TokenizationError {
    UnexpectedEOF(Loc),
    UnexpectedChar(char, Loc),
}

fn is_term_start_ok(chr: char) -> bool {
    !chr.is_whitespace() || !"()[]{};!`|;\\".contains(chr)
}

fn is_term_ok(chr: char) -> bool {
    !(chr.is_whitespace() || chr.is_control() || "()[]{}".contains(chr))
}

fn is_comment(chr: char) -> bool {
    chr == ';'
}

pub struct Tokenizer {
    // TODO: handle lists: consuming ( with top of partial being a '
    line: usize,
    chr: usize,
    sexpr_depth: usize,
    bracket_depth: usize,
    quote_depth: usize,
    in_string: bool,
    commented: bool,
    partial: Vec<char>,
}

type TokenizeResult = Result<Option<Token>, TokenizationError>;

impl Tokenizer {
    pub fn new() -> Tokenizer {
        Tokenizer {
            line: 0,
            chr: 0,
            sexpr_depth: 0,
            bracket_depth: 0,
            in_string: false,
            quote_depth: 0,
            commented: false,
            partial: vec![],
        }
    }

    fn accept(&self, chr: char) -> bool {
        if chr.is_control() {
            return false;
        }
        if self.in_string {
            return true;
        }
        match chr {
            ')' => self.sexpr_depth > 0 || self.quote_depth > 0,
            ']' => self.bracket_depth > 0,
            _ => true,
        }
    }

    /// TODO: document
    pub fn parse(source: &str) -> Result<Vec<Token>, TokenizationError> {
        let mut tokenizer = Tokenizer::new();
        let mut toks = vec![];
        for chr in source.chars() {
            if let Some(tok) = tokenizer.consume(chr)? {
                toks.push(tok);
            }
        }
        Ok(toks)
    }

    /// Consume a single token of input
    fn consume(&mut self, chr: char) -> TokenizeResult {
        let loc = Loc::new(self.line, self.chr);
        if chr == '\n' {
            self.line += 1;
            self.chr = 0;
        } else {
            self.chr += 1;
        }
        if self.accept(chr) {
            if self.in_string {
                self.parse_string(loc, chr)
            } else if self.quote_depth > 0 {
                self.parse_quote(loc, chr)
            } else {
                self.parse_normal(loc, chr)
            }
        } else {
            Err(TokenizationError::UnexpectedChar(chr, loc))
        }
    }

    fn emit_ident(&mut self, loc: Loc) -> Option<Token> {
        let length = self.partial.len();
        let nloc = Loc::new(loc.line, loc.chr - length);
        if self.partial.len() > 0 {
            let ident = Ident(self.partial.iter().collect(), nloc);
            self.partial.clear();
            Some(ident)
        } else {
            None
        }
    }

    fn emit_literal(&mut self, loc: Loc) -> Option<Token> {
        let length = self.partial.len();
        let nloc = Loc::new(loc.line, loc.chr - length);
        if self.partial.len() > 0 {
            let lit = Literal(self.partial.iter().collect(), nloc);
            self.partial.clear();
            Some(lit)
        } else {
            None
        }
    }

    fn emit_token(&mut self, loc: Loc) -> Option<Token> {
        if let Some(c) = self.partial.first() {
            if c.is_numeric() || c == &'"' {
                self.emit_literal(loc)
            } else {
                self.emit_ident(loc)
            }
        } else {
            None
        }
    }

    /// TODO: test
    fn parse_string(&mut self, loc: Loc, chr: char) -> TokenizeResult {
        self.partial.push(chr);
        if chr == '"' {
            if let Some(prev) = self.partial.last() {
                if prev == &'\\' {
                    return Ok(None);
                }
            }
            self.in_string = false;
            // The emit function expects us to already be past the closing character
            Ok(self.emit_literal(Loc::new(loc.line, loc.chr + 1)))
        } else {
            Ok(None)
        }
    }

    /// TODO: implement parsing of quoted sexprs
    fn parse_quote(&mut self, loc: Loc, chr: char) -> Result<Option<Token>, TokenizationError> {
        // todo
        Err(TokenizationError::UnexpectedEOF(loc))
    }

    /// TODO test
    /// TODO:
    fn parse_normal(&mut self, loc: Loc, chr: char) -> Result<Option<Token>, TokenizationError> {
        if !is_term_ok(chr) {
            if self.partial.len() > 1 {
                return Ok(self.emit_token(loc));
            } else if self.partial.len() == 1 {
                // special exception for single quotes
                let c = self.partial.first().unwrap();
                if c != &'\'' {
                    return Ok(self.emit_token(loc));
                }
            }
        }
        match chr {
            '(' => {
                if let Some(c) = self.partial.last() {
                    if c == &'\'' {
                        self.quote_depth += 1;
                        Ok(Some(QuoteBegin(loc)))
                    } else {
                        Ok(None)
                    }
                } else {
                    self.sexpr_depth += 1;
                    Ok(Some(SexprBegin(loc)))
                }
            }
            ')' => {
                // TODO:%s/T: can make a tree here if I want w w
                self.sexpr_depth -= 1;
                Ok(Some(SexprEnd(loc)))
            }
            '"' => {
                self.partial.push(chr);
                self.in_string = true;
                Ok(None)
            }
            // TODO: implement [] parsing for let expression simplifications
            _ => {
                self.partial.push(chr);
                Ok(None)
            }
        }
    }
}
