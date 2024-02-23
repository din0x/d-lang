use std::fmt::Display;

use crate::{
    ast::{BinOperator, ExprInfo, UnaryOperator, UnexpectedToken},
    typing::Type,
};

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
    IllagalChar(char),
    SyntaxError(UnexpectedToken),
    BinOperatorUsage(BinOperator, Type, Type),
    UnaryOperatorUsage(UnaryOperator, Type),
    BadCall(Type),
    WrongArgCount(WrongArgCount),
    NoIdentifier(Box<str>),
    TypeMissmatch(TypeMissmatch),
    AssignmentToTemporary,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct WrongArgCount {
    pub expected: usize,
    pub found: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeMissmatch {
    pub expected: Type,
    pub found: Type,
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

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = match self {
            ErrorKind::BinOperatorUsage(op, l, r) => {
                format!("Cannot use '{}' operator with '{}' and '{}'", op, l, r)
            }
            ErrorKind::UnaryOperatorUsage(op, t) => {
                format!("Cannot use '{}' operator with '{}'", op, t)
            }
            ErrorKind::IllagalChar(c) => {
                format!("Found unexpected character '{}'", c)
            }
            ErrorKind::SyntaxError(token) => {
                let mut s = format!("Unexpected token '{}'", token.unexpacted);

                if let Some(expected) = token.expected.clone() {
                    s += format!(", expected '{}'", expected).as_str();
                };

                s
            }
            ErrorKind::WrongArgCount(err) => {
                format!(
                    "Function requires {} arguments, but {} were given",
                    err.expected, err.found
                )
            }
            ErrorKind::BadCall(t) => {
                format!("Cannot call '{}'", t)
            }
            ErrorKind::NoIdentifier(name) => {
                format!("Cannot find '{}' in current scope", name)
            }
            ErrorKind::TypeMissmatch(err) => {
                format!("Expected '{}', found '{}'", err.expected, err.found)
            }
            ErrorKind::AssignmentToTemporary => "Trying to assign to a temporary value".into(),
        };

        write!(f, "{}", s)
    }
}
