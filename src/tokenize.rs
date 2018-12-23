/// `Loc`s define code locations. They are `Token`s that exist
/// in the (non-inclusive) range `[lbegin, lend)` on line `line`.
/// `begin` and `end` define the raw character range.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Loc {
    line: usize,
    column: usize,
}

impl Loc {
    pub fn new(l: usize, c: usize) -> Self {
        Loc { line: l, column: c }
    }
}

impl ToString for Loc {
    fn to_string(&self) -> String {
        format!("{}:{}", self.line, self.column)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
    BracketBegin(Loc),
    BracketEnd(Loc),
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
    column: usize,
    loc: Loc,
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
            column: 0,
            loc: Loc::new(0, 0),
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

    /// TODO: document this atrocity
    pub fn tokenize(&mut self, source: &str) -> Result<Vec<Token>, TokenizationError> {
        let mut toks = vec![];
        let mut chars = source.chars().peekable();
        loop {
            if let Some(chr) = chars.next() {
                if let Some(tok) = self.consume(chr)? {
                    toks.push(tok);
                } else if let Some(next_char) = chars.peek() {
                    // TODO: the loc swapping is gross. I need a better solution than peeking
                    // like just emitting a vector of tokens at the end rather than
                    // kicking them off as I go...
                    // use lalrpop or something instead

                    // nothing was emitted. check to see if this finishes the
                    // partial Token
                    if !self.in_string && !is_term_ok(*next_char) {
                        let loc = self.loc;
                        self.loc = Loc::new(self.loc.line, self.loc.column + 1);
                        if self.partial.len() > 1 {
                            let ntok = self.emit_token().unwrap();
                            toks.push(ntok);
                        } else if self.partial.len() == 1 {
                            // special exception for single quotes
                            let c = self.partial.first().unwrap();
                            if c != &'\'' {
                                let ntok = self.emit_token().unwrap();
                                toks.push(ntok);
                            }
                        }
                        self.loc = loc;
                    }
                }
            } else {
                break;
            }
        }
        Ok(toks)
    }

    /// Consume a single token of input
    fn consume(&mut self, chr: char) -> TokenizeResult {
        self.loc = Loc::new(self.line, self.column);
        if chr == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }

        if self.accept(chr) {
            if self.in_string {
                self.consume_string(chr)
            } else if self.quote_depth > 0 {
                self.consume_quote(chr)
            } else if self.commented {
                self.consume_comment(chr)
            } else {
                self.consume_normal(chr)
            }
        } else {
            Err(TokenizationError::UnexpectedChar(chr, self.loc))
        }
    }

    fn emit_ident(&mut self, loc: Loc) -> Option<Token> {
        if self.partial.len() > 0 {
            let length = self.partial.len();
            let nloc = Loc::new(loc.line, loc.column - length);
            let ident = Ident(self.partial.iter().collect(), nloc);
            self.partial.clear();
            Some(ident)
        } else {
            None
        }
    }

    fn emit_literal(&mut self, loc: Loc) -> Option<Token> {
        if self.partial.len() > 0 {
            let length = self.partial.len();
            let nloc = Loc::new(loc.line, loc.column - length);
            let lit = Literal(self.partial.iter().collect(), nloc);
            self.partial.clear();
            Some(lit)
        } else {
            None
        }
    }

    fn emit_token(&mut self) -> Option<Token> {
        if let Some(c) = self.partial.first() {
            if c.is_numeric() || c == &'"' {
                self.emit_literal(self.loc)
            } else {
                self.emit_ident(self.loc)
            }
        } else {
            None
        }
    }

    /// TODO: document
    fn consume_string(&mut self, chr: char) -> TokenizeResult {
        self.partial.push(chr);
        if chr == '"' {
            if let Some(prev) = self.partial.last() {
                if prev == &'\\' {
                    return Ok(None);
                }
            }
            self.in_string = false;
            // The emit function expects us to already be past the closing character
            Ok(self.emit_literal(Loc::new(self.loc.line, self.loc.column + 1)))
        } else {
            Ok(None)
        }
    }

    /// TODO: implement parsing of quoted sexprs
    fn consume_quote(&mut self, chr: char) -> TokenizeResult {
        // todo
        Err(TokenizationError::UnexpectedEOF(self.loc))
    }

    fn consume_comment(&mut self, chr: char) -> TokenizeResult {
        if chr == '\n' {
            self.commented = false;
        }
        Ok(None)
    }

    /// TODO: Document
    fn consume_normal(&mut self, chr: char) -> TokenizeResult {
        // TODO: by returning early we lose `chr`
        if chr.is_whitespace() {
            return Ok(None);
        } else if is_comment(chr) {
            self.commented = true;
            return Ok(None);
        }
        match chr {
            '(' => {
                if let Some(c) = self.partial.last() {
                    if c == &'\'' {
                        self.quote_depth += 1;
                        Ok(Some(QuoteBegin(self.loc)))
                    } else {
                        Ok(None)
                    }
                } else {
                    self.sexpr_depth += 1;
                    Ok(Some(SexprBegin(self.loc)))
                }
            }
            ')' => {
                // TODO:%s/T: can make a tree here if I want w w
                self.sexpr_depth -= 1;
                Ok(Some(SexprEnd(self.loc)))
            }
            '[' => {
                self.bracket_depth += 1;
                Ok(Some(BracketBegin(self.loc)))
            }
            ']' => {
                // TODO:%s/T: can make a tree here if I want w w
                self.bracket_depth -= 1;
                Ok(Some(BracketEnd(self.loc)))
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
