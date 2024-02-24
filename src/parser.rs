use crate::ast::{
    Arg, Assign, Binary, Binop, Block, Call, Decl, Expr, ExprKind, Function, If, Illegal, Unary,
    UNARY_OPERATORS,
};

use super::lexer::{Info, Keyword, Operator, Punctuation, Token, TokenKind};

const EXPR_PARSERS: &[fn(&mut ParserData) -> Expr] = &[
    parse_if_else,
    parse_variable_declaration,
    parse_function,
    parse_assignment,
    parse_comparison,
    parse_additive,
    parse_multipicative,
    parse_unary,
    parse_call,
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
        let unexpected = parser.pop();
        return Expr {
            kind: ExprKind::Illegal(Box::new(Illegal {
                found: unexpected.kind.clone(),
                expected: None,
            })),
            info: Info {
                length: unexpected.info.length,
                position: unexpected.info.position,
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
            dbg!("operator didn't match??? {}", $at);
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
                let op = generate_match_patterns!(
                    operator,
                    $($binds),+
                );

                let right = parse_lower_level(parser);

                let length = right.info.position + right.info.length - left.info.position;
                let position = left.info.position;

                left = Expr {
                    kind: ExprKind::Binary(Box::new(Binary {
                        op,
                        left,
                        right,
                    })),
                    info: Info { length, position },
                }
            }

            left
        }
    };
}

make_binary_expr_parser!(
    parse_additive,
    (Operator::Plus, Binop::Addition),
    (Operator::Minus, Binop::Subtraction)
);

make_binary_expr_parser!(
    parse_multipicative,
    (Operator::Star, Binop::Multiplication),
    (Operator::Slash, Binop::Division)
);

make_binary_expr_parser!(
    parse_comparison,
    (Operator::Equal, Binop::Equal),
    (Operator::NotEqual, Binop::NotEqual),
    (Operator::Less, Binop::Less),
    (Operator::LessOrEqual, Binop::LessOrEqual),
    (Operator::More, Binop::More),
    (Operator::MoreOrEqual, Binop::MoreOrEqual)
);

fn parse_if_else(parser: &mut ParserData) -> Expr {
    if parser.current().kind != TokenKind::Keyword(Keyword::If) {
        return parse_lower_level(parser);
    }

    let position = parser.current().info.position;
    parser.pop();

    let condition = parse_lower_level(parser);
    let block = parse_block(parser);

    let mut else_expr = None;
    let end = block.info.position + block.info.length;
    if parser.current().kind == TokenKind::Keyword(Keyword::Else) {
        parser.pop();
        if parser.current().kind == TokenKind::Keyword(Keyword::If) {
            else_expr = Some(parse_if_else(parser));
        } else {
            else_expr = Some(parse_block(parser));
        }
    }

    Expr {
        kind: ExprKind::If(Box::new(If {
            condition,
            block,
            else_expr,
        })),
        info: Info {
            position,
            length: end - position,
        },
    }
}

fn parse_block(parser: &mut ParserData) -> Expr {
    let position = parser.current().info.position;
    if parser.current().kind != TokenKind::LSquirly {
        return Expr {
            kind: ExprKind::Illegal(Box::new(Illegal {
                found: parser.current().kind.clone(),
                expected: Some(TokenKind::LSquirly),
            })),
            info: Info {
                position: parser.current().info.position,
                length: 1,
            },
        };
    } else {
        parser.pop();
    }

    let mut content = vec![];
    let mut tail = None;
    while parser.current().kind != TokenKind::RSquirly {
        let expr = parse_expr(parser);
        match parser.current().kind {
            TokenKind::Punctuation(Punctuation::Semicolon) => {
                content.push(expr);
                parser.pop();
            }
            TokenKind::RSquirly => {
                tail = Some(expr);
                break;
            }
            _ => break,
        }
    }

    let end;
    if parser.current().kind != TokenKind::RSquirly {
        return Expr {
            kind: ExprKind::Illegal(Box::new(Illegal {
                found: parser.current().kind.clone(),
                expected: Some(TokenKind::RSquirly),
            })),
            info: Info {
                position: parser.current().info.position,
                length: 1,
            },
        };
    } else {
        end = parser.current().info.position;
        parser.pop();
    }

    Expr {
        kind: ExprKind::Block(Box::new(Block {
            content: content.into_boxed_slice(),
            tail,
        })),
        info: Info {
            position,
            length: end - position + 1,
        },
    }
}

fn parse_variable_declaration(parser: &mut ParserData) -> Expr {
    if parser.current().kind != TokenKind::Keyword(Keyword::Let) {
        return parse_lower_level(parser);
    }

    let position = parser.pop().info.position;

    let name = parser.pop();
    let equal_sign = parser.pop();
    let expr = parse_lower_level(parser);
    let end = expr.info.position;

    if let TokenKind::Identifier(iden) = name.kind {
        if equal_sign.kind == TokenKind::Operator(Operator::Assignment) {
            return Expr {
                kind: ExprKind::Decl(Decl {
                    name: iden,
                    value: Box::new(expr),
                }),
                info: Info {
                    length: end - position,
                    position,
                },
            };
        }
        return Expr {
            kind: ExprKind::Illegal(Box::new(Illegal {
                found: equal_sign.kind,
                expected: Some(TokenKind::Operator(Operator::Assignment)),
            })),
            info: Info {
                length: equal_sign.info.length,
                position: equal_sign.info.position,
            },
        };
    }

    Expr {
        kind: ExprKind::Illegal(Box::new(Illegal {
            found: name.kind,
            expected: Some(TokenKind::Identifier("".into())),
        })),
        info: Info {
            length: name.info.length,
            position: name.info.position,
        },
    }
}

fn parse_args(parser: &mut ParserData) -> Result<Box<[Arg]>, (Illegal, Info)> {
    if parser.current().kind != TokenKind::LParen {
        return Err((
            Illegal {
                found: parser.current().kind.clone(),
                expected: Some(TokenKind::LParen),
            },
            Info {
                position: parser.current().info.position,
                length: parser.current().info.length,
            },
        ));
    }

    parser.pop();

    let mut args = vec![];

    while parser.current().kind != TokenKind::RParen {
        let info = parser.current().info;
        let TokenKind::Identifier(name) = parser.current().kind.clone() else {
            return Err((
                Illegal {
                    found: parser.current().kind.clone(),
                    expected: Some(TokenKind::Identifier("".into())),
                },
                Info {
                    position: parser.current().info.position,
                    length: parser.current().info.length,
                },
            ));
        };

        parser.pop();

        if parser.current().kind != TokenKind::Punctuation(Punctuation::Colon) {
            return Err((
                Illegal {
                    found: parser.current().kind.clone(),
                    expected: Some(TokenKind::Punctuation(Punctuation::Colon)),
                },
                Info {
                    position: parser.current().info.position,
                    length: parser.current().info.length,
                },
            ));
        }
        parser.pop();

        let r#type = parse_expr(parser);

        args.push(Arg {
            name,
            r#type,
            info: Info {
                position: info.position,
                length: info.length,
            },
        });

        if parser.current().kind == TokenKind::RParen {
            break;
        }

        if parser.current().kind != TokenKind::Punctuation(Punctuation::Comma) {
            return Err((
                Illegal {
                    found: parser.current().kind.clone(),
                    expected: Some(TokenKind::Punctuation(Punctuation::Colon)),
                },
                Info {
                    position: parser.current().info.position,
                    length: parser.current().info.length,
                },
            ));
        }

        parser.pop();
    }

    parser.pop();
    Ok(args.into_boxed_slice())
}

fn parse_function(parser: &mut ParserData) -> Expr {
    if parser.current().kind != TokenKind::Keyword(Keyword::Fn) {
        return parse_lower_level(parser);
    }

    let position = parser.current().info.position;
    parser.pop();

    let name = if let TokenKind::Identifier(name) = parser.current().kind.clone() {
        parser.pop();
        Ok(name)
    } else {
        Err(parser.current().clone())
    };

    let args = parse_args(parser);

    let r#type = if parser.current().kind == TokenKind::Operator(Operator::Arrow) {
        parser.pop();
        Some(parse_expr(parser))
    } else {
        None
    };

    let body = parse_block(parser);

    let Ok(name) = name else {
        let err = name.err().unwrap();
        return Expr {
            kind: ExprKind::Illegal(Box::new(Illegal {
                found: err.kind.clone(),
                expected: Some(TokenKind::Identifier("".into())),
            })),
            info: Info {
                position: err.info.position,
                length: err.info.length,
            },
        };
    };

    let args = match args {
        Ok(args) => args,
        Err(err) => {
            return Expr {
                kind: ExprKind::Illegal(Box::new(err.0)),
                info: Info {
                    position: err.1.position,
                    length: err.1.length,
                },
            }
        }
    };

    let length = body.info.position + body.info.length - position;
    Expr {
        kind: ExprKind::Function(Box::new(Function {
            name,
            args,
            r#type,
            body,
        })),
        info: Info { position, length },
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
            kind: ExprKind::Assign(Box::new(Assign { left, right })),
            info: Info {
                position: start,
                length: position - start,
            },
        }
    }

    left
}

fn parse_unary(parser: &mut ParserData) -> Expr {
    if !UNARY_OPERATORS
        .iter()
        .map(|x| x.0)
        .any(|x| TokenKind::Operator(x) == parser.current().kind)
    {
        return parse_lower_level(parser);
    }
    let position = parser.current().info.position;
    let op = parser.pop().kind;

    let expr = parse_lower_level(parser);
    let expr_position = expr.info.position;

    Expr {
        kind: ExprKind::Unary(Box::new(Unary {
            op: UNARY_OPERATORS
                .iter()
                .find(|x| TokenKind::Operator(x.0) == op)
                .expect("")
                .1,
            expr,
        })),
        info: Info {
            length: expr_position - position,
            position,
        },
    }
}

fn parse_call(parser: &mut ParserData) -> Expr {
    let mut expr = parse_lower_level(parser);

    let position = expr.info.position;

    while parser.current().kind == TokenKind::LParen {
        parser.pop();

        let mut args = vec![];

        while parser.current().kind != TokenKind::RParen {
            args.push(parse_expr(parser));

            if parser.current().kind == TokenKind::Punctuation(Punctuation::Comma) {
                parser.pop();
            }
        }

        parser.pop();

        let length = expr.info.position - position;

        expr = Expr {
            kind: ExprKind::Call(Box::new(Call {
                expr,
                args: args.into_boxed_slice(),
            })),
            info: Info { length, position },
        }
    }

    expr
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

    Expr {
        kind: ExprKind::Illegal(Box::new(Illegal {
            found: unexpected_token.kind,
            expected: None,
        })),
        info: Info {
            length: unexpected_token.info.length,
            position: unexpected_token.info.position,
        },
    }
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
            TokenKind::Bool(b) => ExprKind::Bool(b),
            TokenKind::Identifier(ref name) => ExprKind::Var(name.clone()),
            TokenKind::LSquirly => return parse_block(parser),
            _ => return parse_lower_level(parser),
        };

        parser.pop();
    }

    Expr {
        kind,
        info: Info {
            length: info.length,
            position: info.position,
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
