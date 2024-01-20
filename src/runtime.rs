use std::fmt::Display;

use crate::compiler::{BinOperator, Expr, ExprKind};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    Int(i64),
    String(Box<str>),
    Bool(bool),
    _Type(Type),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Type {
    Int,
    String,
    Bool,
    Type,
}

pub fn eval(expr: Expr) -> Value {
    match expr.kind {
        ExprKind::UnexpectedToken(_) => panic!("Illegal exprassion"),
        ExprKind::Int(i) => Value::Int(i),
        ExprKind::String(s) => Value::String(s.clone()),
        ExprKind::Binary(op, l, r) => eval_binary_expr(op, *l, *r),
    }
}

pub fn get_type(v: &Value) -> Type {
    match v {
        Value::Int(_) => Type::Int,
        Value::String(_) => Type::String,
        Value::Bool(_) => Type::Bool,
        Value::_Type(_) => Type::Type,
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Value::Int(i) => i.to_string(),
            Value::String(s) => format!(r#""{}""#, s.to_string()),
            Value::Bool(b) => b.to_string(),
            Value::_Type(_) => "Type".into(),
        };

        write!(f, "{}", s)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::Int => "Int",
            Type::String => "String",
            Type::Bool => "Bool",
            Type::Type => "Type",
        };

        write!(f, "{}", s)
    }
}

fn eval_binary_expr(op: BinOperator, l: Expr, r: Expr) -> Value {
    let left = eval(l);
    let right = eval(r);

    match (op, left, right) {
        (BinOperator::Addition, Value::Int(i0), Value::Int(i1)) => Value::Int(i0 + i1),
        (BinOperator::Subtraction, Value::Int(i0), Value::Int(i1)) => Value::Int(i0 - i1),
        (BinOperator::Multiplication, Value::Int(i0), Value::Int(i1)) => Value::Int(i0 * i1),
        (BinOperator::Division, Value::Int(i0), Value::Int(i1)) => Value::Int(i0 / i1),
        (BinOperator::Equal, Value::Int(i0), Value::Int(i1)) => Value::Bool(i0 == i1),
        (BinOperator::NotEqual, Value::Int(i0), Value::Int(i1)) => Value::Bool(i0 != i1),
        (BinOperator::Less, Value::Int(i0), Value::Int(i1)) => Value::Bool(i0 < i1),
        (BinOperator::LessOrEqual, Value::Int(i0), Value::Int(i1)) => Value::Bool(i0 <= i1),
        (BinOperator::More, Value::Int(i0), Value::Int(i1)) => Value::Bool(i0 > i1),
        (BinOperator::MoreOrEqual, Value::Int(i0), Value::Int(i1)) => Value::Bool(i0 >= i1),
        (BinOperator::Addition, Value::String(s0), Value::String(s1)) => {
            Value::String((s0.to_string() + &s1).into_boxed_str())
        }
        (BinOperator::Equal, Value::String(s0), Value::String(s1)) => Value::Bool(s0 == s1),
        (BinOperator::NotEqual, Value::String(s0), Value::String(s1)) => Value::Bool(s0 != s1),
        (BinOperator::Multiplication, Value::String(s), Value::Int(i)) => {
            Value::String(s.repeat(i as usize).into_boxed_str())
        }
        (BinOperator::Multiplication, Value::Int(i), Value::String(s)) => {
            Value::String(s.repeat(i as usize).into_boxed_str())
        }
        (BinOperator::Equal, Value::Bool(b0), Value::Bool(b1)) => Value::Bool(b0 == b1),
        (BinOperator::NotEqual, Value::Bool(b0), Value::Bool(b1)) => Value::Bool(b0 != b1),
        (_, left, right) => panic!(
            "Cannot use '{}' operator with '{}' and  '{}'",
            op,
            get_type(&left),
            get_type(&right)
        ),
    }
}
