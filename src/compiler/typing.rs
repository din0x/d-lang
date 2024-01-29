use std::{borrow::Borrow, cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use super::{
    parser::{
        Assignment, BinOperator, Block, Expr, ExprInfo, ExprKind, IfExpr, IllegalExpr, UnaryExpr,
        UnaryOperator, VariableDeclaration,
    },
    Error, ErrorKind, TypeMissmatch,
};

pub fn is_valid(expr: &Expr, mut scope: Scope) -> Result<(), Error> {
    get_type(expr, &mut scope)?;
    Ok(())
}

fn get_type(expr: &Expr, scope: &mut Scope) -> Result<TypeAndScopeInfo, Error> {
    match &expr.kind {
        ExprKind::IllegalExpr(illegal) => match illegal {
            IllegalExpr::IllegalChar(c) => Err(Error::new(ErrorKind::IllagalChar(*c), expr.info)),
            IllegalExpr::UnexpectedToken(token) => {
                Err(Error::new(ErrorKind::SyntaxError(token.clone()), expr.info))
            }
        },
        ExprKind::Int(_) => Ok(Type::Int.into()),
        ExprKind::String(_) => Ok(Type::String.into()),
        ExprKind::Var(name) => {
            if let Some(t) = scope.lookup(name) {
                return Ok(TypeAndScopeInfo {
                    tp: t,
                    scope: Some(scope.clone()),
                });
            }

            Err(Error::new(ErrorKind::NoIdentifier(name.clone()), expr.info))
        }
        ExprKind::Binary(op, l, r) => get_type_bin_expr(*op, &l, &r, expr.info, scope),
        ExprKind::Unary(unary) => get_type_unary_expr(unary, expr.info, scope),
        ExprKind::VariableDeclaration(var) => get_type_var_declaration(var, scope),
        ExprKind::Assignment(assignment) => get_type_assignment(assignment, expr.info, scope),
        ExprKind::IfExpr(if_expr) => get_type_if_else(if_expr, expr.info, scope),
        ExprKind::Block(block) => get_type_block(block, scope),
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
pub struct Scope(Rc<RefCell<ScopeContent>>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ScopeContent {
    parent: Option<Scope>,
    vars: HashMap<Box<str>, Type>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope(Rc::new(RefCell::new(ScopeContent {
            parent: None,
            vars: HashMap::new(),
        })))
    }

    fn with_parent(parent: &Scope) -> Scope {
        Scope(Rc::new(RefCell::new(ScopeContent {
            parent: Some(parent.clone()),
            vars: HashMap::new(),
        })))
    }

    fn get_scope(&self, name: &Box<str>) -> Option<Scope> {
        if (*self.0).borrow().vars.contains_key(name) {
            return Some(self.clone());
        }

        if let Some(scope) = (*self.0).borrow().parent.clone() {
            return scope.borrow().get_scope(name);
        }

        None
    }

    fn declare(&mut self, name: Box<str>, value_type: Type) {
        self.0.borrow_mut().vars.insert(name, value_type);
    }

    fn lookup(&self, name: &Box<str>) -> Option<Type> {
        (*self.get_scope(name)?.0)
            .borrow()
            .vars
            .get(name)
            .map(|x| *x)
    }
}

fn get_type_if_else(
    if_else: &IfExpr,
    info: ExprInfo,
    scope: &mut Scope,
) -> Result<TypeAndScopeInfo, Error> {
    let condition_type = get_type(&if_else.condition, scope)?;
    if condition_type.tp != Type::Bool {
        return Err(Error::new(
            ErrorKind::TypeMissmatch(TypeMissmatch {
                expected: Type::Bool,
                found: condition_type.tp,
            }),
            info,
        ));
    }

    let if_type = get_type(&if_else.block, scope)?;
    match if_else.else_expr {
        Some(ref block) => {
            let else_block_type = get_type(block, scope)?;
            if if_type.tp != else_block_type.tp {
                return Err(Error::new(
                    ErrorKind::TypeMissmatch(TypeMissmatch {
                        expected: if_type.tp,
                        found: else_block_type.tp,
                    }),
                    block.info,
                ));
            }
            return Ok(if_type);
        }
        None => {
            return Ok(TypeAndScopeInfo {
                tp: Type::Unit,
                scope: None,
            });
        }
    };
}

fn get_type_block(block: &Block, scope: &mut Scope) -> Result<TypeAndScopeInfo, Error> {
    for expr in block.content.iter() {
        // TODO: Require value to be ()
    }

    if let Some(ref tail) = block.tail {
        return get_type(tail, &mut Scope::with_parent(scope));
    }

    Ok(TypeAndScopeInfo {
        tp: Type::Unit,
        scope: None,
    })
}

fn get_type_var_declaration(
    var: &VariableDeclaration,
    scope: &mut Scope,
) -> Result<TypeAndScopeInfo, Error> {
    let value = get_type(&var.value, scope)?;

    scope.declare(var.name.clone(), value.tp);

    Ok(Type::Unit.into())
}

fn get_type_assignment(
    assignment: &Assignment,
    info: ExprInfo,
    scope: &mut Scope,
) -> Result<TypeAndScopeInfo, Error> {
    let left = get_type(&assignment.left, scope)?;

    let right = get_type(&assignment.right, scope)?;

    if left.scope == None {
        return Err(Error::new(ErrorKind::AssignmentToTemporary, info));
    }

    if left.tp != right.tp {
        return Err(Error::new(
            ErrorKind::TypeMissmatch(TypeMissmatch {
                expected: left.tp,
                found: right.tp,
            }),
            info,
        ));
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

fn get_type_unary_expr(expr: &UnaryExpr, info: ExprInfo, scope: &mut Scope) -> Result<TypeAndScopeInfo, Error> {
    use Type::*;
    use UnaryOperator::*;

    let t = match (expr.op, get_type(&expr.expr, scope)?.tp) {
        (Plus, Int) => Int,
        (Minus, Int) => Int,
        (Not, Bool) => Bool,
        (op, t) => return Err(Error::new(ErrorKind::UnaryOperatorUsage(op, t), info)),
    };

    Ok(TypeAndScopeInfo { tp: t, scope: None })
}

fn get_bin_expr_result_type(
    left: Type,
    right: Type,
    op: BinOperator,
    info: ExprInfo,
) -> Result<TypeAndScopeInfo, Error> {
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
