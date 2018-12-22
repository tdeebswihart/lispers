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

fn parse_toks(tokens: &[&str]) -> Result<(usize, CalcAst), ParseError> {
    use self::CalcAst::*;
    use self::ParseError::*;
    let mut consumed = 0;
    if tokens.len() == 0 {
        return Err(UnexpectedEOF);
    }
    let mut asts: Vec<CalcAst> = vec![];
    while consumed < tokens.len() {
        let tok = tokens.get(consumed).unwrap();
        consumed += 1;
        println!("consuming {:?}", tok);
        match tok.as_ref() {
            "(" => {
                let (eaten, nast) = parse_toks(&tokens[consumed..])?;
                consumed += eaten;
                asts.push(nast);
            }
            ")" => {
                return Ok((consumed, List(asts)));
            }
            _ => asts.push(Atom(tok.parse::<CalcAtom>()?)),
        }
    }
    Ok((consumed, List(asts)))
}

/// Builds the AST top-down
fn parse_tokens(tokens: &[&str]) -> Result<CalcAst, ParseError> {
    println!("parse_tokens({:?})", tokens.join(" "));
    use self::CalcAst::*;
    use self::ParseError::*;
    if tokens.len() == 0 {
        return Err(UnexpectedEOF);
    }

    let tok = tokens.first().unwrap();
    println!("Considering: {:?}", tok);
    match tok.as_ref() {
        "(" => {
            // This is wrong
            // Need the first ) without a preceeding (
            // TODO: () is an ok form in certain cases. It's an implicit `nil`
            let close_idx = tokens[1..]
                .iter()
                .position(|c| c == &")")
                .ok_or(SyntaxError("Matching ) not found!".to_string()))?;
            let mut ast_list: Vec<CalcAst> = vec![];
            println!("Parsing subast: {:?}", tokens[1..close_idx].join(" "));
            for ntok in tokens[1..close_idx].iter() {
                ast_list.push(parse_tokens(&[ntok])?);
            }
            Ok(List(ast_list))
        }
        ")" => Err(SyntaxError(")".to_string())),
        _ => Ok(Atom(tok.parse::<CalcAtom>()?)),
    }
}

/// `Loc`s define code locations. They are `Token`s that exist
/// in the (non-inclusive) range `[lbegin, lend)` on line `line`.
/// `begin` and `end` define the raw character range.
#[derive(Debug)]
struct Loc {
    line: usize,
    chr: usize,
}

impl Loc {
    fn new(l: usize, c: usize) -> Self {
        Loc { line: l, chr: c }
    }
}

#[derive(Debug)]
enum Token {
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

enum TokenizationError {
    UnexpectedEOF(Loc),
    UnexpectedChar(char, Loc),
}

fn is_term_start_ok(chr: char) -> bool {
    !chr.is_whitespace() || !"()[]{};!`|;\\".contains(chr)
}

fn is_term_ok(chr: char) -> bool {
    !chr.is_whitespace() | !chr.is_control() || "()[]{}".contains(chr)
}

fn is_comment(chr: char) -> bool {
    chr == ';'
}

struct Tokenizer {
    // TODO: handle lists: consuming ( with top of partial being a '
    line: usize,
    chr: usize,
    idx: usize,
    sexpr_depth: usize,
    bracket_depth: usize,
    quote_depth: usize,
    in_string: bool,
    commented: bool,
    partial: Vec<char>,
}

type TokenizeResult = Result<Option<Token>, TokenizationError>;

impl Tokenizer {
    fn new() -> Tokenizer {
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

    fn parse(source: &str) -> Result<Vec<Token>, TokenizationError> {
        let mut tokenizer = Tokenizer::new();
        let mut toks = vec![];
        for (idx, chr) in source.char_indices() {
            if let Some(tok) = tokenizer.consume(idx, chr)? {
                toks.push(tok);
            }
        }
        Ok(toks)
    }

    fn consume(self, idx: usize, chr: char) -> TokenizeResult {
        let loc = Loc::new(self.line, self.chr);
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

    fn parse_string(self, loc: Loc, chr: char) -> TokenizeResult {
        self.partial.push(chr);
        if chr == '"' {
            self.in_string = false;
            // TODO: get the correct starting loc for quoted strings
            Ok(Some(Ident(self.partial.iter().collect(), loc)))
        } else {
            Ok(None)
        }
    }

    fn parse_quote(self, loc: Loc, chr: char) -> Result<Option<Token>, TokenizationError> {
        // todo
        Err(TokenizationError::UnexpectedEOF(loc))
    }

    fn parse_normal(self, loc: Loc, chr: char) -> Result<Option<Token>, TokenizationError> {
        if !is_term_ok(chr) {
            if self.partial.len() > 1 {
                let ident = Ident(self.partial.iter().collect(), loc);
                self.partial.clear();
                return Ok(Some(ident));
            } else if self.partial.len() == 1 {
                // special exception for single quotes
                let c = self.partial.first().unwrap();
                if c != &'\'' {
                    let ident = Ident(self.partial.iter().collect(), loc);
                    self.partial.clear();
                    return Ok(Some(ident));
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
            // TODO: implement [] parsing for let expression simplifications
            _ => {
                self.partial.push(chr);
                Ok(None)
            }
        }
    }
}

fn main() {
    let program = "(1 foo bar (baz bee bop))";
    let toks = match Tokenizer::parse(program) {
        Ok(a) => a,
        Err(e) => panic!(e),
    };
    println!("{:?}", toks);
}
