use std::{collections::HashMap, fmt::Display};

use super::{
    parser::{BinOperator, Expr, ExprInfo, ExprKind, VariableDeclaration},
    Error, ErrorKind,
};

pub fn is_valid(expr: &Expr) -> Result<(), Error> {
    get_type(expr, &mut Scope::new())?;
    Ok(())
}

fn get_type(expr: &Expr, scope: &mut Scope) -> Result<Type, Error> {
    match &expr.kind {
        ExprKind::UnexpectedToken(token) => {
            Err(Error::new(ErrorKind::SyntaxError(token.clone()), expr.info))
        }
        ExprKind::Int(_) => Ok(Type::Int),
        ExprKind::String(_) => Ok(Type::String),
        ExprKind::Var(name) => {
            if let Some(t) = scope.lookup(name) {
                return Ok(t);
            }

            Err(Error::new(ErrorKind::NoIdentifier(name.clone()), expr.info))
        },
        ExprKind::Binary(op, l, r) => get_type_bin_expr(*op, &l, &r, expr.info, scope),
        ExprKind::VariableDeclaration(var) => get_type_var_declaration(var, scope),
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Type {
    Unit,
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
            Type::Unit => "Unit",
        };

        write!(f, "{}", s)
    }
}

struct Scope {
    vars: HashMap<Box<str>, Type>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            vars: HashMap::new(),
        }
    }

    fn declare(&mut self, name: Box<str>, value_type: Type) {
        self.vars.insert(name, value_type);
    }

    fn lookup(&self, name: &Box<str>) -> Option<Type> {
        self.vars.get(name).map(|x| *x)
    }
}

fn get_type_var_declaration(var: &VariableDeclaration, scope: &mut Scope) -> Result<Type, Error> {
    let value = get_type(&var.value, scope)?;

    scope.declare(var.name.clone(), value);

    Ok(Type::Unit)
}

fn get_type_bin_expr(
    op: BinOperator,
    l: &Expr,
    r: &Expr,
    info: ExprInfo,
    scope: &mut Scope,
) -> Result<Type, Error> {
    let left = get_type(l, scope);
    let right = get_type(r, scope);

    match (op, left, right) {
        (BinOperator::Addition, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Int),
        (BinOperator::Subtraction, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Int),
        (BinOperator::Multiplication, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Int),
        (BinOperator::Division, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Int),
        (BinOperator::Equal, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Bool),
        (BinOperator::NotEqual, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Bool),
        (BinOperator::Less, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Bool),
        (BinOperator::LessOrEqual, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Bool),
        (BinOperator::More, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Bool),
        (BinOperator::MoreOrEqual, Ok(Type::Int), Ok(Type::Int)) => Ok(Type::Bool),
        (BinOperator::Addition, Ok(Type::String), Ok(Type::String)) => Ok(Type::String),
        (BinOperator::Equal, Ok(Type::String), Ok(Type::String)) => Ok(Type::Bool),
        (BinOperator::NotEqual, Ok(Type::String), Ok(Type::String)) => Ok(Type::Bool),
        (BinOperator::Multiplication, Ok(Type::String), Ok(Type::Int)) => Ok(Type::String),
        (BinOperator::Multiplication, Ok(Type::Int), Ok(Type::String)) => Ok(Type::String),
        (BinOperator::Equal, Ok(Type::Bool), Ok(Type::Bool)) => Ok(Type::Bool),
        (BinOperator::NotEqual, Ok(Type::Bool), Ok(Type::Bool)) => Ok(Type::Bool),
        (_, Ok(left), Ok(right)) => Err(Error::new(
            ErrorKind::BinOperatorUsage(op, left, right),
            info,
        )),
        (_, Err(err0), Err(err1)) => Err(Error::from_two(err0, err1)),
        (_, Err(err), _) => Err(err),
        (_, _, Err(err)) => Err(err),
    }
}
