mod scope;
mod value;

use crate::compiler::{
    Assignment, BinOperator, Block, Expr, ExprKind, IfExpr, UnaryExpr, UnaryOperator,
    VariableDeclaration,
};
use value::*;

pub use scope::*;

pub fn run(expr: Expr, scope: &mut Scope) -> Value {
    eval(expr, scope).get_value()
}

fn eval(expr: Expr, scope: &mut Scope) -> EvalResult {
    match expr.kind {
        ExprKind::IllegalExpr(_) => panic!("illegal exprassion"),
        ExprKind::Int(i) => EvalResult::new(Value::Int(i)),
        ExprKind::Bool(b) => EvalResult::new(Value::Bool(b)),
        ExprKind::String(s) => EvalResult::new(Value::String(s.clone())),
        ExprKind::Binary(op, l, r) => eval_binary_expr(op, *l, *r, scope),
        ExprKind::Unary(unary) => eval_unary(*unary, scope),
        ExprKind::VariableDeclaration(var) => eval_declaration(var, scope),
        ExprKind::Var(expr) => eval_var(expr, scope),
        ExprKind::Assignment(expr) => eval_assignment(*expr, scope),
        ExprKind::Block(expr) => eval_block(*expr, scope),
        ExprKind::IfExpr(expr) => eval_if_expr(*expr, scope),
    }
}

fn eval_binary_expr(op: BinOperator, l: Expr, r: Expr, scope: &mut Scope) -> EvalResult {
    use BinOperator::*;
    use Value::*;

    let left = eval(l, scope).get_value();
    let right = eval(r, scope).get_value();

    EvalResult::new(match (op, left, right) {
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
            op,
            get_type(&left),
            get_type(&right)
        ),
    })
}

fn eval_declaration(var: VariableDeclaration, scope: &mut Scope) -> EvalResult {
    let value = eval(*var.value, scope).get_value();

    scope.declare(var.name, value);
    EvalResult::unit()
}

fn eval_var(name: Box<str>, scope: &mut Scope) -> EvalResult {
    scope.lookup(&name)
}

fn eval_assignment(assignment: Assignment, scope: &mut Scope) -> EvalResult {
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

fn eval_unary(expr: UnaryExpr, scope: &mut Scope) -> EvalResult {
    use UnaryOperator::*;
    let value = eval(expr.expr, scope);

    let value = match (expr.op, value.get_value()) {
        (Plus, Value::Int(i)) => Value::Int(i),
        (Minus, Value::Int(i)) => Value::Int(-i),
        (Not, Value::Bool(b)) => Value::Bool(!b),
        _ => panic!("Unexprected unary operator"),
    };

    EvalResult::new(value)
}

fn eval_if_expr(expr: IfExpr, scope: &mut Scope) -> EvalResult {
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
