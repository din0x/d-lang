mod lexer;
mod parser;
mod typing;

use std::fmt::Display;

pub use lexer::TokenKind;
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

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for err in self.errors.iter() {
            s += &format!("Error: {}\n", err.kind);
        }

        // pop last new line
        s.pop();

        write!(f, "{}", s)
    }
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

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = match self {
            ErrorKind::BinOperatorUsage(op, l, r) => {
                format!("Cannot use '{}' operator with '{}' and '{}'", op, l, r)
            }
            ErrorKind::InvalidExpr(token) => format!("Unexpected token '{}'", token),
        };

        write!(f, "{}", s)
    }
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
