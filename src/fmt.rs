use crate::compiler::{Error, ErrorKind, lexer::{TokenKind, Operator}};
use std::fmt::Display;

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

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
            Self::Equal => "=",
            Self::NotEqual => "!="
        };

        write!(f, "{}", s)
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Illegal(c) => format!("'{}'", c),
            Self::Eof => "EOF".into(),
            Self::Int(i) => i.to_string(),
            Self::String(s) => format!(r#""{}""#, s),
            Self::Operator(op) => format!("{}", op),
            Self::LParen => "(".into(),
            Self::RParen => ")".into()
        };

        write!(f, "{}", s)
    }
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
