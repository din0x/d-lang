use std::fmt::Display;

use crate::lexer::{Info, Operator, TokenKind};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub info: Info,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    Illegal(Box<Illegal>),
    Binary(Box<Binary>),
    Unary(Box<Unary>),
    Call(Box<Call>),
    Decl(Decl),
    Function(Box<Function>),
    Assign(Box<Assign>),
    If(Box<If>),
    Block(Box<Block>),
    Var(Box<str>),
    Int(i64),
    Bool(bool),
    String(Box<str>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Binary {
    pub op: Binop,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call {
    pub expr: Expr,
    pub args: Box<[Expr]>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub name: Box<str>,
    pub args: Box<[Arg]>,
    pub r#type: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Arg {
    pub name: Box<str>,
    pub r#type: Expr,
    pub info: Info,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Unary {
    pub op: Unop,
    pub expr: Expr,
}

pub const UNARY_OPERATORS: &[(Operator, Unop)] = &[
    (Operator::Plus, Unop::Plus),
    (Operator::Minus, Unop::Minus),
    (Operator::Not, Unop::Not),
];

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Unop {
    Plus,
    Minus,
    Not,
}

impl Display for Unop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            UNARY_OPERATORS
                .iter()
                .find(|x| x.1 == *self)
                .expect("unexpected unary operator")
                .0
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    pub condition: Expr,
    pub block: Expr,
    pub else_expr: Option<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub content: Box<[Expr]>,
    pub tail: Option<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Illegal {
    pub found: TokenKind,
    pub expected: Option<TokenKind>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Binop {
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
pub struct Decl {
    pub name: Box<str>,
    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assign {
    pub left: Expr,
    pub right: Expr,
}

impl Display for Binop {
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
