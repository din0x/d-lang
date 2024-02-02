use std::{fmt::Display, rc::Rc, cell::RefCell, borrow::Borrow};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    Int(i64),
    String(Box<str>),
    Bool(bool),
    _Type(Type),
    Unit,
}

pub fn get_type(v: &Value) -> Type {
    match v {
        Value::Int(_) => Type::Int,
        Value::String(_) => Type::String,
        Value::Bool(_) => Type::Bool,
        Value::_Type(_) => Type::Type,
        Value::Unit => Type::Unit,
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EvalResult {
    pub data: Rc<RefCell<Value>>,
}

impl EvalResult {
    pub fn unit() -> EvalResult {
        Self::new(Value::Unit)
    }

    pub fn new(value: Value) -> EvalResult {
        EvalResult {
            data: Rc::new(RefCell::new(value)),
        }
    }

    pub fn get_value(&self) -> Value {
        let binding = (*self.data).borrow().clone();
        let data: &Value = binding.borrow();
        data.clone()
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Value::Int(i) => i.to_string(),
            Value::String(s) => format!(r#""{}""#, s.to_string()),
            Value::Bool(b) => b.to_string(),
            Value::_Type(_) => "Type".into(),
            Value::Unit => "()".into(),
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Type {
    Int,
    String,
    Bool,
    Type,
    Unit,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::Int => "Int",
            Type::String => "String",
            Type::Bool => "Bool",
            Type::Type => "Type",
            Type::Unit => "Unit",
        };

        write!(f, "{}", s)
    }
}
