use std::fmt::Display;

use crate::lexer::{Operator, TokenKind};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub info: ExprInfo,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    IllegalExpr(IllegalExpr),
    Binary(BinOperator, Box<Expr>, Box<Expr>),
    Unary(Box<UnaryExpr>),
    Call(Box<Call>),
    VariableDeclaration(VariableDeclaration),
    Function(Box<Function>),
    Assignment(Box<Assignment>),
    IfExpr(Box<IfExpr>),
    Block(Box<Block>),
    Var(Box<str>),
    Int(i64),
    Bool(bool),
    String(Box<str>),
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
    pub info: ExprInfo,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOperator,
    pub expr: Expr,
}

pub const UNARY_OPERATORS: &[(Operator, UnaryOperator)] = &[
    (Operator::Plus, UnaryOperator::Plus),
    (Operator::Minus, UnaryOperator::Minus),
    (Operator::Not, UnaryOperator::Not),
];

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
}

impl Display for UnaryOperator {
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
pub struct IfExpr {
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
