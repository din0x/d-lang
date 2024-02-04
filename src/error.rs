use crate::{
    ast::{BinOperator, ExprInfo, UnaryOperator, UnexpectedToken},
    typing::Type,
};
use std::fmt::Display;

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
    NoIdentifier(Box<str>),
    TypeMissmatch(TypeMissmatch),
    AssignmentToTemporary,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
