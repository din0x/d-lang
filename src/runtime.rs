use crate::ast::{Binary, Call, Function};

use super::ast::{Assign, Binop, Block, Decl, Expr, ExprKind, If, Unary, Unop};

pub fn run(expr: Expr, scope: &mut Scope) -> Value {
    eval(expr, scope).get_value()
}

fn eval(expr: Expr, scope: &mut Scope) -> EvalResult {
    match expr.kind {
        ExprKind::Illegal(_) => panic!("illegal exprassion"),
        ExprKind::Int(i) => EvalResult::new(Value::Int(i)),
        ExprKind::Bool(b) => EvalResult::new(Value::Bool(b)),
        ExprKind::String(s) => EvalResult::new(Value::String(s.clone())),
        ExprKind::Binary(expr) => eval_binary_expr(*expr, scope),
        ExprKind::Unary(unary) => eval_unary(*unary, scope),
        ExprKind::Call(expr) => eval_call(*expr, scope),
        ExprKind::Decl(var) => eval_declaration(var, scope),
        ExprKind::Function(f) => eval_func(*f, scope),
        ExprKind::Var(expr) => eval_var(expr, scope),
        ExprKind::Assign(expr) => eval_assignment(*expr, scope),
        ExprKind::Block(expr) => eval_block(*expr, scope),
        ExprKind::If(expr) => eval_if_expr(*expr, scope),
    }
}

fn eval_call(call: Call, scope: &mut Scope) -> EvalResult {
    let Value::Func(f) = eval(call.expr, scope).get_value() else {
        unreachable!("type checker should ensure this is a function")
    };

    let mut new_scope = Scope::new(Some(scope.clone()));

    for (value, name) in call
        .args
        .iter()
        .map(|x| eval(x.clone(), scope).get_value())
        .zip(f.args.iter().map(|x| x.name.clone()))
    {
        new_scope.declare(name, value);
    }

    eval(f.body, &mut new_scope)
}

fn eval_binary_expr(expr: Binary, scope: &mut Scope) -> EvalResult {
    use Binop::*;
    use Value::*;

    let left = eval(expr.left, scope).get_value();
    let right = eval(expr.right, scope).get_value();

    EvalResult::new(match (expr.op, left, right) {
        (Addition, Int(i0), Int(i1)) => Int(i0 + i1),
        (Subtraction, Int(i0), Int(i1)) => Int(i0 - i1),
        (Multiplication, Int(i0), Int(i1)) => Int(i0 * i1),
        (Division, Int(i0), Int(i1)) => Int(i0 / i1),
        (Equal, Int(i0), Int(i1)) => Bool(i0 == i1),
        (NotEqual, Int(i0), Int(i1)) => Bool(i0 != i1),
        (Less, Int(i0), Int(i1)) => Bool(i0 < i1),
        (LessOrEqual, Int(i0), Int(i1)) => Bool(i0 <= i1),
        (More, Int(i0), Int(i1)) => Bool(i0 > i1),
        (MoreOrEqual, Int(i0), Int(i1)) => Bool(i0 >= i1),
        (Addition, String(s0), String(s1)) => String((s0.to_string() + &s1).into_boxed_str()),
        (Equal, String(s0), String(s1)) => Bool(s0 == s1),
        (NotEqual, String(s0), String(s1)) => Bool(s0 != s1),
        (Multiplication, String(s), Int(i)) => String(s.repeat(i as usize).into_boxed_str()),
        (Multiplication, Int(i), String(s)) => String(s.repeat(i as usize).into_boxed_str()),
        (Equal, Bool(b0), Bool(b1)) => Bool(b0 == b1),
        (NotEqual, Bool(b0), Bool(b1)) => Bool(b0 != b1),
        (_, left, right) => panic!(
            "cannot use '{}' operator with '{}' and  '{}'",
            expr.op,
            get_type(&left),
            get_type(&right)
        ),
    })
}

fn eval_func(f: Function, scope: &mut Scope) -> EvalResult {
    let Function {
        name,
        args,
        body,
        r#type,
    } = f;

    let args = args
        .into_vec()
        .into_iter()
        .map(|arg| Arg {
            name: arg.name,
            r#type: match eval(arg.r#type, scope).get_value() {
                Value::Type(t) => t,
                _ => unreachable!("type checker should have ensured we dont get here"),
            },
        })
        .collect();

    let output = r#type
        .map(|x| match eval(x, scope).get_value() {
            Value::Type(t) => t,
            _ => unreachable!("type checker should have ensured we dont get here"),
        })
        .unwrap_or(Type::Unit);

    let func = Func { args, output, body };

    scope.declare(name, Value::Func(func));

    EvalResult::unit()
}

fn eval_declaration(var: Decl, scope: &mut Scope) -> EvalResult {
    let value = eval(*var.value, scope).get_value();

    scope.declare(var.name, value);
    EvalResult::unit()
}

fn eval_var(name: Box<str>, scope: &mut Scope) -> EvalResult {
    scope.lookup(&name)
}

fn eval_assignment(assignment: Assign, scope: &mut Scope) -> EvalResult {
    let left = eval(assignment.left, scope);
    let right = eval(assignment.right, scope);

    *(*left.data).borrow_mut() = right.get_value();

    EvalResult::unit()
}

fn eval_block(expr: Block, scope: &mut Scope) -> EvalResult {
    let mut scope = Scope::new(Some(scope.clone()));
    for expr in Vec::from(expr.content) {
        eval(expr, &mut scope);
    }

    if let Some(tail) = expr.tail {
        return eval(tail, &mut scope);
    }

    EvalResult::unit()
}

fn eval_unary(expr: Unary, scope: &mut Scope) -> EvalResult {
    use Unop::*;
    let value = eval(expr.expr, scope);

    let value = match (expr.op, value.get_value()) {
        (Plus, Value::Int(i)) => Value::Int(i),
        (Minus, Value::Int(i)) => Value::Int(-i),
        (Not, Value::Bool(b)) => Value::Bool(!b),
        _ => panic!("Unexprected unary operator"),
    };

    EvalResult::new(value)
}

fn eval_if_expr(expr: If, scope: &mut Scope) -> EvalResult {
    let result = eval(expr.condition, scope).get_value();
    let Value::Bool(condition) = result else {
        panic!("condition was {}", result)
    };

    let mut result;
    let has_else_block = expr.else_expr.is_some();
    if condition {
        result = eval(expr.block, scope);
    } else {
        match expr.else_expr {
            Some(else_block) => result = eval(else_block, scope),
            None => result = EvalResult::unit(),
        };
    }

    if !has_else_block {
        result = EvalResult::unit();
    }

    result
}

use std::{borrow::Borrow, cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EvalResult {
    pub data: Rc<RefCell<Value>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    Int(i64),
    String(Box<str>),
    Bool(bool),
    Type(Type),
    Func(Func),
    Unit,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Func {
    args: Box<[Arg]>,
    output: Type,
    body: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Arg {
    name: Box<str>,
    r#type: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Int,
    String,
    Bool,
    Func(Box<[Type]>, Box<Type>),
    #[allow(clippy::enum_variant_names)]
    Type,
    Unit,
}

pub fn get_type(v: &Value) -> Type {
    match v {
        Value::Int(_) => Type::Int,
        Value::String(_) => Type::String,
        Value::Bool(_) => Type::Bool,
        Value::Type(_) => Type::Type,
        Value::Func(f) => Type::Func(
            f.args.iter().map(|x| x.clone().r#type).collect(),
            Box::new(f.output.clone()),
        ),
        Value::Unit => Type::Unit,
    }
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
            Value::String(s) => format!(r#""{}""#, s),
            Value::Bool(b) => b.to_string(),
            Value::Func(func) => {
                let s: Vec<String> = func
                    .args
                    .as_ref()
                    .iter()
                    .map(|x| format!("{}", x.r#type))
                    .collect();

                return write!(f, "fn({}) -> {}", s.join(", "), func.output);
            }
            Value::Type(_) => "Type".into(),
            Value::Unit => "()".into(),
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
            Type::Unit => "Unit",
            Type::Func(args, output) => {
                use std::fmt::Write;
                let s: String = args.as_ref().iter().fold(String::new(), |mut output, x| {
                    let _ = write!(output, "{x}");
                    output
                });

                return write!(f, "fn({}) -> {}", s, output);
            }
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Scope(Rc<RefCell<ScopeContent>>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ScopeContent {
    parent: Option<Scope>,
    vars: HashMap<Box<str>, EvalResult>,
}

impl Scope {
    pub fn new(parent: Option<Scope>) -> Scope {
        Scope(Rc::new(RefCell::new(ScopeContent {
            parent,
            vars: HashMap::new(),
        })))
    }

    pub fn prelude(&mut self) {
        ([
            ("Int", Type::Int),
            ("Bool", Type::Bool),
            ("Unit", Type::Unit),
            ("String", Type::String),
        ])
        .map(|x| self.declare(x.0.into(), Value::Type(x.1)));
    }

    pub fn declare(&mut self, name: Box<str>, value: Value) {
        self.0
            .as_ref()
            .borrow_mut()
            .vars
            .insert(name, EvalResult::new(value));
    }

    fn get_scope(&self, name: &str) -> Option<Scope> {
        if self.0.as_ref().borrow().vars.contains_key(name) {
            return Some(self.clone());
        }

        if let Some(ref parent) = self.0.as_ref().borrow().parent {
            return parent.get_scope(name);
        }

        None
    }

    pub fn lookup(&self, name: &str) -> EvalResult {
        self.get_scope(name)
            .unwrap_or_else(|| panic!("cannot find '{}' in current scope", name))
            .0
            .as_ref()
            .borrow()
            .vars
            .get(name)
            .unwrap_or_else(|| panic!("cannot find '{}' in current scope", name))
            .clone()
    }
}

impl Default for Scope {
    fn default() -> Self {
        Scope::new(None)
    }
}
