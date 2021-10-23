pub mod common;
use lispers::parse::Atom::*;
use lispers::parse::Expr::*;
use lispers::parse::Parser;
use lispers::tokenize::{Loc, Token};

#[test]
fn test_parse() {
    // These are the tokens for "(+ 1 2) (fn [a b] (* a b))"
    let toks = vec![
        Token::SexprBegin(Loc { line: 0, column: 0 }),
        Token::Ident("+".to_string(), Loc { line: 0, column: 1 }),
        Token::Literal("1".to_string(), Loc { line: 0, column: 3 }),
        Token::Literal("2".to_string(), Loc { line: 0, column: 5 }),
        Token::SexprEnd(Loc { line: 0, column: 6 }),
        Token::SexprBegin(Loc { line: 0, column: 8 }),
        Token::Ident("fn".to_string(), Loc { line: 0, column: 9 }),
        Token::BracketBegin(Loc {
            line: 0,
            column: 12,
        }),
        Token::Ident(
            "a".to_string(),
            Loc {
                line: 0,
                column: 13,
            },
        ),
        Token::Ident(
            "b".to_string(),
            Loc {
                line: 0,
                column: 15,
            },
        ),
        Token::BracketEnd(Loc {
            line: 0,
            column: 16,
        }),
        Token::SexprBegin(Loc {
            line: 0,
            column: 18,
        }),
        Token::Ident(
            "*".to_string(),
            Loc {
                line: 0,
                column: 19,
            },
        ),
        Token::Ident(
            "a".to_string(),
            Loc {
                line: 0,
                column: 21,
            },
        ),
        Token::Ident(
            "b".to_string(),
            Loc {
                line: 0,
                column: 23,
            },
        ),
        Token::SexprEnd(Loc {
            line: 0,
            column: 24,
        }),
        Token::SexprEnd(Loc {
            line: 0,
            column: 25,
        }),
    ];
    let goal = List(vec![
        Call(
            "+".to_string(),
            vec![Literal(Integer(1)), Literal(Integer(2))],
        ),
        Lambda(vec![
            Vector(vec![
                Literal(Ident("a".to_string())),
                Literal(Ident("b".to_string())),
            ]),
            Call(
                "*".to_string(),
                vec![
                    Literal(Ident("a".to_string())),
                    Literal(Ident("b".to_string())),
                ],
            ),
        ]),
    ]);
    let mut parser = Parser::new();
    let ast = parser.parse(toks.as_slice()).unwrap();
    assert_eq!(goal, ast);
}
