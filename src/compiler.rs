mod lexer;
mod parser;
mod typing;

pub use lexer::{Operator, Token, TokenInfo, TokenKind};
pub use parser::{BinOperator, Expr, ExprInfo, ExprKind};

use self::typing::Type;

pub fn compile(code: &str) -> Result<Expr, Error> {
    let tokens = lexer::parse_tokens(code);
    //    let kinds: Vec<&lexer::TokenKind> = tokens.iter().map(|x| &x.kind).collect();
    //    dbg!(kinds);

    let expr = parser::parse_ast(&tokens);
    //    dbg!(&expr);

    typing::is_valid(&expr)?;
    Ok(expr)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    pub errors: Vec<OneError>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct OneError {
    pub kind: ErrorKind,
    pub info: ExprInfo,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorKind {
    InvalidExpr(TokenKind),
    BinOperatorUsage(BinOperator, Type, Type),
}

impl Error {
    pub fn new(kind: ErrorKind, info: ExprInfo) -> Error {
        Error {
            errors: vec![OneError { kind, info }],
        }
    }

    pub fn from_two(mut err0: Error, mut err1: Error) -> Error {
        err0.errors.append(&mut err1.errors);
        Error {
            errors: err0.errors,
        }
    }
}
