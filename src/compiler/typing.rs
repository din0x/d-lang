use std::{collections::HashMap, fmt::Display, rc::Rc, cell::RefCell};

use super::{
    parser::{BinOperator, Expr, ExprInfo, ExprKind, VariableDeclaration, Assignment},
    Error, ErrorKind, TypeMissmatch,
};

pub fn is_valid(expr: &Expr, mut scope: Scope) -> Result<(), Error> {
    get_type(expr, &mut scope)?;
    Ok(())
}

fn get_type(expr: &Expr, scope: &mut Scope) -> Result<TypeAndScopeInfo, Error> {
    match &expr.kind {
        ExprKind::UnexpectedToken(token) => {
            Err(Error::new(ErrorKind::SyntaxError(token.clone()), expr.info))
        }
        ExprKind::Int(_) => Ok(Type::Int.into()),
        ExprKind::String(_) => Ok(Type::String.into()),
        ExprKind::Var(name) => {
            if let Some(t) = scope.lookup(name) {
                return Ok(TypeAndScopeInfo{
                    tp: t,
                    scope: Some(scope.clone()),
                })
            }

            Err(Error::new(ErrorKind::NoIdentifier(name.clone()), expr.info))
        },
        ExprKind::Binary(op, l, r) => get_type_bin_expr(*op, &l, &r, expr.info, scope),
        ExprKind::VariableDeclaration(var) => get_type_var_declaration(var, scope),
        ExprKind::Assignment(assignment) => get_type_assignment(assignment, expr.info, scope),
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

impl Into<TypeAndScopeInfo> for Type {
    fn into(self) -> TypeAndScopeInfo {
        TypeAndScopeInfo {
            tp: self,
            scope: None,
        }
    }
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

#[derive(Debug, PartialEq, Eq, Clone)]
struct TypeAndScopeInfo {
    tp: Type,
    scope: Option<Scope>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Scope {
    vars: Rc<RefCell<HashMap<Box<str>, Type>>>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            vars: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    fn declare(&mut self, name: Box<str>, value_type: Type) {
        self.vars.borrow_mut().insert(name, value_type);
    }

    fn lookup(&self, name: &Box<str>) -> Option<Type> {
        self.vars.borrow().get(name).map(|x| *x)
    }
}

fn get_type_var_declaration(var: &VariableDeclaration, scope: &mut Scope) -> Result<TypeAndScopeInfo, Error> {
    let value = get_type(&var.value, scope)?;

    scope.declare(var.name.clone(), value.tp);

    Ok(Type::Unit.into())
}

fn get_type_assignment(assignment: &Assignment, info: ExprInfo, scope: &mut Scope) -> Result<TypeAndScopeInfo, Error> {
    let left = get_type(&assignment.left, scope)?;

    let right = get_type(&assignment.right, scope)?;

    if left.tp != right.tp {
        return Err(
            Error::new(ErrorKind::TypeMissmatch(TypeMissmatch {
                expected: left.tp,
                found: right.tp,
            }), info)
        );
    }

    Ok(Type::Unit.into())
}

fn get_type_bin_expr(
    op: BinOperator,
    l: &Expr,
    r: &Expr,
    info: ExprInfo,
    scope: &mut Scope,
    ) -> Result<TypeAndScopeInfo, Error> {
    let left = get_type(l, scope);
    let right = get_type(r, scope);

    match (left, right) {
        (Ok(left), Ok(right)) => get_bin_expr_result_type(left.tp, right.tp, op, info),
        (Err(err0), Err(err1)) => Err(Error::from_two(err0, err1)),
        (Err(err), _) => Err(err),
        (_, Err(err)) => Err(err),
    }
}

fn get_bin_expr_result_type(left: Type, right: Type, op: BinOperator, info: ExprInfo) -> Result<TypeAndScopeInfo, Error> {
    let result = match (op, left, right) {
        (BinOperator::Addition, Type::Int, Type::Int) => Ok(Type::Int),
        (BinOperator::Subtraction, Type::Int, Type::Int) => Ok(Type::Int),
        (BinOperator::Multiplication, Type::Int, Type::Int) => Ok(Type::Int),
        (BinOperator::Division, Type::Int, Type::Int) => Ok(Type::Int),
        (BinOperator::Equal, Type::Int, Type::Int) => Ok(Type::Bool),
        (BinOperator::NotEqual, Type::Int, Type::Int) => Ok(Type::Bool),
        (BinOperator::More, Type::Int, Type::Int) => Ok(Type::Bool),
        (BinOperator::MoreOrEqual, Type::Int, Type::Int) => Ok(Type::Bool),
        (BinOperator::Less, Type::Int, Type::Int) => Ok(Type::Bool),
        (BinOperator::LessOrEqual, Type::Int, Type::Int) => Ok(Type::Bool),
        (BinOperator::Addition, Type::String, Type::String) => Ok(Type::String),
        (BinOperator::Equal, Type::String, Type::String) => Ok(Type::Bool),
        (BinOperator::NotEqual, Type::String, Type::String) => Ok(Type::Bool),
        (BinOperator::Multiplication, Type::String, Type::Int) => Ok(Type::String),
        (BinOperator::Multiplication, Type::Int, Type::String) => Ok(Type::String),
        (BinOperator::Equal, Type::Bool, Type::Bool) => Ok(Type::Bool),
        (BinOperator::NotEqual, Type::Bool, Type::Bool) => Ok(Type::Bool),
        (_, left, right) => Err(Error::new(
            ErrorKind::BinOperatorUsage(op, left, right),
            info,
        )),
    };

    result.map(|x| x.into())
}
