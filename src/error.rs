use std::{
    fmt::{Display, Write},
    path::Path,
};

use crate::{
    ast::{Binop, Illegal, Unop},
    lexer::Info,
    typing::Type,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    pub errors: Vec<OneError>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct OneError {
    pub kind: ErrorKind,
    pub info: Info,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorKind {
    SyntaxError(Illegal),
    BinOperatorUsage(Binop, Type, Type),
    UnaryOperatorUsage(Unop, Type),
    BadCall(Type),
    WrongArgCount(WrongArgCount),
    NoIdentifier(Box<str>),
    TypeMismatch(TypeMismatch),
    AssignmentToTemporary,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct WrongArgCount {
    pub expected: usize,
    pub found: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeMismatch {
    pub expected: Type,
    pub found: Type,
}

impl Error {
    pub fn new(kind: ErrorKind, info: Info) -> Error {
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

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = match self {
            ErrorKind::BinOperatorUsage(op, l, r) => {
                format!("cannot use '{}' operator with '{}' and '{}'", op, l, r)
            }
            ErrorKind::UnaryOperatorUsage(op, t) => {
                format!("cannot use '{}' operator with '{}'", op, t)
            }
            ErrorKind::SyntaxError(token) => {
                let mut s = format!("unexpected token '{}'", token.found);

                if let Some(expected) = token.expected.clone() {
                    s += format!(", expected '{}'", expected).as_str();
                };

                s
            }
            ErrorKind::WrongArgCount(err) => {
                format!(
                    "function requires {} arguments, but {} were given",
                    err.expected, err.found
                )
            }
            ErrorKind::BadCall(t) => {
                format!("cannot call '{}'", t)
            }
            ErrorKind::NoIdentifier(name) => {
                format!("cannot find '{}' in current scope", name)
            }
            ErrorKind::TypeMismatch(err) => {
                format!("expected '{}', found '{}'", err.expected, err.found)
            }
            ErrorKind::AssignmentToTemporary => "trying to assign to a temporary value".into(),
        };

        write!(f, "{}", s)
    }
}

pub fn err_format(error: Error, path: &Path, file: &str) -> String {
    let mut s = String::new();
    let mut err_index = 0;
    let err_count = error.errors.len();

    for err in error.errors {
        err_index += 1;

        let mut snippet = String::new();
        let mut i = 0;
        let mut line: usize = 1;
        let mut line_start = 0;

        for l in file.lines() {
            line_start = i;
            i += l.len();

            // account for crlf or lf
            if let Some('\r') = file.chars().nth(i) {
                i += 1;
            }
            i += 1;

            if i > err.info.position {
                snippet = l.to_string();
                break;
            }
            line += 1;
        }

        let under = " ".repeat(err.info.position - line_start) + &"^".repeat(err.info.length);

        let message = err.kind;
        let path = path.display();
        let line_padding = &" ".repeat(line.to_string().len());
        let col = err.info.position - line_start + 1;

        _ = write!(
            s,
            "\x1b[0;31m\x1b[1merror\x1b[0m: {message}\n \x1b[0;96m->\x1b[0m {path}:{line}:{col}\n\x1b[0;96m{line} |\x1b[0m {snippet}\n\x1b[0;96m{line_padding}   {under}\x1b[0m\n"
        );

        if err_index != err_count {
            _ = write!(s, "\n");
        }
    }

    s
}
