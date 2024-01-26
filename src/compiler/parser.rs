use std::fmt::Display;

use crate::compiler::lexer::Operator;

use super::lexer::{Keyword, Token, TokenKind};

const EXPR_PARSERS: &[fn(&mut ParserData) -> Expr] = &[
    parse_variable_declaration,
    parse_assignment,
    parse_comparison,
    parse_additive,
    parse_multipicative,
    parse_parenthesis,
    parse_primary,
];

pub fn parse_ast(tokens: &[Token]) -> Expr {
    let mut parser = ParserData {
        tokens,
        expr_parsers: EXPR_PARSERS,
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
    IllegalExpr(IllegalExpr),
    Binary(BinOperator, Box<Expr>, Box<Expr>),
    VariableDeclaration(VariableDeclaration),
    Assignment(Box<Assignment>),
    Var(Box<str>),
    Int(i64),
    String(Box<str>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IllegalExpr {
    UnexpectedToken(UnexpectedToken),
    IllegalChar(char),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnexpectedToken {
    pub unexpacted: TokenKind,
    pub expected: Option<TokenKind>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    More,
    MoreOrEqual,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VariableDeclaration {
    pub name: Box<str>,
    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assignment {
    pub left: Expr,
    pub right: Expr,
}

impl Display for BinOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Addition => "+",
            Self::Subtraction => "-",
            Self::Multiplication => "*",
            Self::Division => "/",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::Less => "<",
            Self::LessOrEqual => "<=",
            Self::More => ">",
            Self::MoreOrEqual => ">=",
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ExprInfo {
    pub length: usize,
    pub position: usize,
}

fn parse_expr(parser: &mut ParserData) -> Expr {
    let mut new_parser = ParserData {
        tokens: parser.tokens,
        expr_parsers: EXPR_PARSERS,
    };
    let expr = parse_lower_level(&mut new_parser);

    parser.tokens = new_parser.tokens;
    expr
}

fn parse_lower_level(parser: &mut ParserData) -> Expr {
    if parser.expr_parsers.is_empty() {
        let unexpacted = parser.pop();

        let mut illegal_expr = IllegalExpr::UnexpectedToken(UnexpectedToken {
            unexpacted: unexpacted.kind.clone(),
            expected: None,
        });

        if let TokenKind::Illegal(c) = unexpacted.kind {
            illegal_expr = IllegalExpr::IllegalChar(c);
        }

        return Expr {
            kind: ExprKind::IllegalExpr(illegal_expr),
            info: ExprInfo {
                length: unexpacted.info.length,
                position: unexpacted.info.location,
            },
        };
    }

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
    ($parser:ident, $operator:expr, $($tl:expr),+) => {
        $parser.current().kind == TokenKind::Operator($operator) ||
        generate_condition!($parser, $($tl),+)
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
    (Operator::NotEqual, BinOperator::NotEqual),
    (Operator::Less, BinOperator::Less),
    (Operator::LessOrEqual, BinOperator::LessOrEqual),
    (Operator::More, BinOperator::More),
    (Operator::MoreOrEqual, BinOperator::MoreOrEqual)
);

fn parse_variable_declaration(parser: &mut ParserData) -> Expr {
    if parser.current().kind != TokenKind::Keyword(Keyword::Let) {
        return parse_lower_level(parser);
    }

    let position = parser.pop().info.location;

    let name = parser.pop();
    let equal_sign = parser.pop();
    let expr = parse_lower_level(parser);
    let end = expr.info.position;

    if let TokenKind::Identifier(iden) = name.kind {
        if equal_sign.kind == TokenKind::Operator(Operator::Assignment) {
            return Expr {
                kind: ExprKind::VariableDeclaration(VariableDeclaration {
                    name: iden.into(),
                    value: Box::new(expr),
                }),
                info: ExprInfo {
                    length: end - position,
                    position,
                },
            };
        }
        return Expr {
            kind: ExprKind::IllegalExpr(IllegalExpr::UnexpectedToken(UnexpectedToken {
                unexpacted: equal_sign.kind,
                expected: Some(TokenKind::Operator(Operator::Assignment)),
            })),
            info: ExprInfo {
                length: equal_sign.info.length,
                position: equal_sign.info.location,
            },
        };
    }

    Expr {
        kind: ExprKind::IllegalExpr(IllegalExpr::UnexpectedToken(UnexpectedToken {
            unexpacted: name.kind,
            expected: Some(TokenKind::Identifier("".into())),
        })),
        info: ExprInfo {
            length: name.info.length,
            position: name.info.location,
        },
    }
}

fn parse_assignment(parser: &mut ParserData) -> Expr {
    let mut left = parse_lower_level(parser);
    let start = left.info.position;

    if parser.current().kind == TokenKind::Operator(Operator::Assignment) {
        parser.pop();
        let right = parse_lower_level(parser);
        let position = right.info.position;

        left = Expr {
            kind: ExprKind::Assignment(Box::new(Assignment { left: left, right })),
            info: ExprInfo {
                position: start,
                length: position - start,
            },
        }
    }

    left
}

fn parse_parenthesis(parser: &mut ParserData) -> Expr {
    if parser.current().kind != TokenKind::LParen {
        return parse_lower_level(parser);
    }

    parser.pop();

    let expr = parse_expr(parser);

    if parser.current().kind == TokenKind::RParen {
        parser.pop();

        return expr;
    }

    let unexpected_token = parser.pop();

    return Expr {
        kind: ExprKind::IllegalExpr(IllegalExpr::UnexpectedToken(UnexpectedToken {
            unexpacted: unexpected_token.kind,
            expected: None,
        })),
        info: ExprInfo {
            length: unexpected_token.info.length,
            position: unexpected_token.info.location,
        },
    };
}

fn parse_primary(parser: &mut ParserData) -> Expr {
    let info;
    let kind;
    {
        let current = parser.current();
        info = current.info;

        kind = match current.kind {
            TokenKind::String(ref string) => ExprKind::String(string.clone()),
            TokenKind::Int(int) => ExprKind::Int(int),
            TokenKind::Identifier(ref name) => ExprKind::Var(name.clone()),
            _ => return parse_lower_level(parser),
        };

        parser.pop();
    }

    Expr {
        kind,
        info: ExprInfo {
            length: info.length,
            position: info.location,
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
