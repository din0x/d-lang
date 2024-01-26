use std::{borrow::Borrow, cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::compiler::{Assignment, BinOperator, Expr, ExprKind, VariableDeclaration};

pub fn run(expr: Expr, scope: &mut Scope) -> Value {
    clone_rc_value(eval(expr, scope))
}

fn eval(expr: Expr, scope: &mut Scope) -> Rc<RefCell<Value>> {
    match expr.kind {
        ExprKind::IllegalExpr(_) => panic!("Illegal exprassion"),
        ExprKind::Int(i) => Rc::new(RefCell::new(Value::Int(i))),
        ExprKind::String(s) => Rc::new(RefCell::new(Value::String(s.clone()))),
        ExprKind::Binary(op, l, r) => eval_binary_expr(op, *l, *r, scope),
        ExprKind::VariableDeclaration(var) => eval_declaration(var, scope),
        ExprKind::Var(name) => eval_var(name, scope),
        ExprKind::Assignment(assignment) => eval_assignment(*assignment, scope),
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    Int(i64),
    String(Box<str>),
    Bool(bool),
    _Type(Type),
    Unit,
}

fn clone_rc_value(v: Rc<RefCell<Value>>) -> Value {
    let binding = (*v).borrow().clone();
    let v: &Value = binding.borrow();
    v.clone()
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
pub struct Scope {
    parent: Option<Rc<Scope>>,
    vars: HashMap<Box<str>, Rc<RefCell<Value>>>,
}

impl Scope {
    pub fn new(parent: Option<Rc<Scope>>) -> Scope {
        Scope {
            parent,
            vars: HashMap::new(),
        }
    }

    fn declare(&mut self, name: Box<str>, value_type: Value) {
        self.vars.insert(name, Rc::new(RefCell::new(value_type)));
    }

    fn lookup(&self, name: &Box<str>) -> Rc<RefCell<Value>> {
        self.vars
            .get(name)
            .map(|x| x.clone())
            .expect(format!("Cannot find '{}' in current scope", name).as_str())
    }
}

impl Default for Scope {
    fn default() -> Self {
        Scope::new(None)
    }
}

fn eval_binary_expr(op: BinOperator, l: Expr, r: Expr, scope: &mut Scope) -> Rc<RefCell<Value>> {
    let left = clone_rc_value(eval(l, scope));
    let right = clone_rc_value(eval(r, scope));

    Rc::new(RefCell::new(match (op, left, right) {
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
    }))
}

fn eval_declaration(var: VariableDeclaration, scope: &mut Scope) -> Rc<RefCell<Value>> {
    let value = clone_rc_value(eval(*var.value, scope));

    scope.declare(var.name, value);
    Rc::new(RefCell::new(Value::Unit))
}

fn eval_var(name: Box<str>, scope: &mut Scope) -> Rc<RefCell<Value>> {
    scope.lookup(&name)
}

fn eval_assignment(assignment: Assignment, scope: &mut Scope) -> Rc<RefCell<Value>> {
    let left = eval(assignment.left, scope);
    let right = eval(assignment.right, scope);

    *(*left).borrow_mut() = clone_rc_value(right);

    Rc::new(RefCell::new(Value::Unit))
}
