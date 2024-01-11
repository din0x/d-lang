use std::fmt::Display;

use super::{
    parser::{BinOperator, Expr, ExprInfo, ExprKind},
    Error, ErrorKind,
};

pub fn is_valid(expr: &Expr) -> Result<(), Error> {
    get_type(expr)?;
    Ok(())
}

pub fn get_type(expr: &Expr) -> Result<Type, Error> {
    match &expr.kind {
        ExprKind::UnexpectedToken(token) => Err(Error::new(ErrorKind::InvalidExpr(token.clone()), expr.info)),
        ExprKind::Int(_) => Ok(Type::Int),
        ExprKind::String(_) => Ok(Type::String),
        ExprKind::Binary(op, l, r) => get_type_bin_expr(*op, &l, &r, expr.info),
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Type {
    Int,
    String,
    Bool,
    _Type,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::Int => "Int",
            Type::String => "String",
            Type::Bool => "Bool",
            Type::_Type => "Type",
        };

        write!(f, "{}", s)
    }
}

fn get_type_bin_expr(op: BinOperator, l: &Expr, r: &Expr, info: ExprInfo) -> Result<Type, Error> {
    let left = get_type(l);
    let right = get_type(r);

    match (op, left, right) {
        (BinOperator::Addition, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Int),
        (BinOperator::Subtraction, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Int),
        (BinOperator::Multiplication, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Int),
        (BinOperator::Division, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Int),
        (BinOperator::Equal, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Bool),
        (BinOperator::NotEqual, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Bool),
        (BinOperator::Addition, Ok(Type::String), Ok(Type::String)) => Ok(Type::String),
        (BinOperator::Equal, Ok(Type::String), Ok(Type::String)) => Ok(Type::Bool),
        (BinOperator::NotEqual, Ok(Type::String), Ok(Type::String)) => Ok(Type::Bool),
        (BinOperator::Multiplication, Ok(Type::String), Ok(Type::Int)) => Ok(Type::String),
        (BinOperator::Multiplication, Ok(Type::Int), Ok(Type::String)) => Ok(Type::String),
        (_, Ok(left), Ok(right)) => Err(Error::new(
            ErrorKind::BinOperatorUsage(op, left, right),
            info,
        )),
        (_, Err(err0), Err(err1)) => Err(Error::from_two(err0, err1)),
        (_, Err(err), _) => Err(err),
        (_, _, Err(err)) => Err(err),
    }
}
