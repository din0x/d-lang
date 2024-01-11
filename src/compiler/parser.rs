use crate::compiler::lexer::Operator;

use super::lexer::{Token, TokenKind};

pub fn parse_ast(tokens: &[Token]) -> Expr {
    let expr_parsers = [
        parse_comparison,
        parse_additive,
        parse_multipicative,
        parse_primary,
    ]
    .as_slice();
    let mut parser = ParserData {
        tokens,
        expr_parsers,
    };

    parse_lower_level(&mut parser)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub info: ExprInfo,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    Illegal,
    Binary(BinOperator, Box<Expr>, Box<Expr>),
    Int(i64),
    String(Box<str>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Equal,
    NotEqual,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ExprInfo {
    pub length: usize,
    pub position: usize,
}

fn parse_expr(parser: )

fn parse_lower_level(parser: &mut ParserData) -> Expr {
    let mut new_parser = *parser;
    new_parser.expr_parsers = &new_parser.expr_parsers[1..];

    let expr = parser.expr_parsers[0](&mut new_parser);
    parser.tokens = new_parser.tokens;
    expr
}

macro_rules! generate_condition {
    // I want to die
    ($parser:ident, $operator:expr) => {
        $parser.current().kind == TokenKind::Operator($operator)
    };
    ($parser:ident, $operator:expr, $($tl:expr)+) => {
        $parser.current().kind == TokenKind::Operator($operator) ||
        generate_condition!($parser, $($tl)+)
    };
}

macro_rules! generate_match_patterns {
    // Kill me
    ($at:expr, $bind:expr) => {
        if ($at.kind == TokenKind::Operator($bind.0)) {
            $bind.1
        }
        else {
            dbg!("Operator didn't match??? {}", $at);
            panic!()
        }};
    ($at:expr, $bind:expr, $($tl:expr),+) => (
        if ($at.kind == TokenKind::Operator($bind.0)) {
            $bind.1
        } else {
            generate_match_patterns!($at, $($tl),+)
        }
    )
}

macro_rules! make_binary_expr_parser {
    ($name:ident, $($binds:expr),+) => {
        fn $name(parser: &mut ParserData) -> Expr {
            let mut left = parse_lower_level(parser);

            // I want to die after making this fucking macro
            // It took me 3 fucking hours
            // I fucking hate it
            while (generate_condition!(parser, $($binds.0),+)) && !parser.eof() {
                let operator = parser.pop();
                let binop = generate_match_patterns!(
                    operator,
                    $($binds),+
                );

                let right = parse_lower_level(parser);

                let length = right.info.position + right.info.length - left.info.position;
                let position = left.info.position;

                left = Expr {
                    kind: ExprKind::Binary(binop, Box::new(left), Box::new(right)),
                    info: ExprInfo { length, position },
                }
            }

            left
        }
    };
}

make_binary_expr_parser!(
    parse_additive,
    (Operator::Plus, BinOperator::Addition),
    (Operator::Minus, BinOperator::Subtraction)
);

make_binary_expr_parser!(
    parse_multipicative,
    (Operator::Star, BinOperator::Multiplication),
    (Operator::Slash, BinOperator::Division)
);

make_binary_expr_parser!(
    parse_comparison,
    (Operator::Equal, BinOperator::Equal),
    (Operator::NotEqual, BinOperator::NotEqual)
);

fn parse_parenthesis(parser: &mut ParserData) -> Expr {
    if parser.current().kind != TokenKind::LParen {
        return parse_lower_level(parser);
    }
    
    let start = parser.pop().info.location;
    
    let expr = parse_
}

fn parse_primary(parser: &mut ParserData) -> Expr {
    let current = parser.pop();

    let kind = match &current.kind {
        TokenKind::String(string) => ExprKind::String(string.clone()),
        TokenKind::Int(int) => ExprKind::Int(*int),
        _ => ExprKind::Illegal,
    };

    Expr {
        kind,
        info: ExprInfo {
            length: current.info.length,
            position: current.info.location,
        },
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct ParserData<'a> {
    tokens: &'a [Token],
    expr_parsers: &'a [fn(&mut ParserData) -> Expr],
}

impl<'a> ParserData<'a> {
    fn current(&self) -> &Token {
        &self.tokens[0]
    }

    fn eof(&self) -> bool {
        self.current().kind == TokenKind::Eof
    }

    fn pop(&mut self) -> Token {
        let current = self.current().clone();

        if current.kind != TokenKind::Eof {
            self.tokens = &self.tokens[1..];
        }

        current
    }
}
