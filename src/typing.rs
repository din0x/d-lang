use std::{borrow::Borrow, cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::error::{Error, ErrorKind, TypeMissmatch};

use super::ast::{
    Assignment, BinOperator, Block, Expr, ExprInfo, ExprKind, Function, IfExpr, IllegalExpr,
    UnaryExpr, UnaryOperator, VariableDeclaration,
};

pub fn is_valid(expr: &Expr, scope: &mut Scope) -> Result<(), Error> {
    get_type(expr, scope)?;
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
        ExprKind::Bool(_) => Ok(Type::Bool.into()),
        ExprKind::Var(name) => {
            if let Some(t) = scope.lookup(name) {
                return Ok(TypeAndScopeInfo {
                    tp: t,
                    scope: Some(scope.clone()),
                });
            }

            Err(Error::new(ErrorKind::NoIdentifier(name.clone()), expr.info))
        }
        ExprKind::Function(func) => get_type_func(func, expr.info, scope),
        ExprKind::Binary(op, l, r) => get_type_bin_expr(*op, l, r, expr.info, scope),
        ExprKind::Unary(unary) => get_type_unary_expr(unary, expr.info, scope),
        ExprKind::Assignment(assignment) => get_type_assignment(assignment, expr.info, scope),
        ExprKind::IfExpr(if_expr) => get_type_if_else(if_expr, expr.info, scope),
        ExprKind::Block(block) => get_type_block(block, scope),
        ExprKind::VariableDeclaration(var) => get_type_var_declaration(var, scope),
    }
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Unit,
    Int,
    String,
    Bool,
    Func(Box<Func>),
    #[allow(clippy::enum_variant_names)]
    Type(Option<Box<Self>>),
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Func {
    params: Box<[Param]>,
    output: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Param {
    name: Box<str>,
    r#type: Type,
}

impl From<Type> for TypeAndScopeInfo {
    fn from(val: Type) -> Self {
        TypeAndScopeInfo {
            tp: val,
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
            Type::Func(ref func) => {
                let mut args = String::new();
                for arg in func.params.iter() {
                    args += format!("{}, ", arg.r#type).as_str();
                }

                return write!(f, "fn({}) -> {}", args, func.output);
            }
            Type::Type(_) => "Type",
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

    fn get_scope(&self, name: &str) -> Option<Scope> {
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

    fn lookup(&self, name: &str) -> Option<Type> {
        (*self.get_scope(name)?.0).borrow().vars.get(name).cloned()
    }
}

fn get_type_func(
    func: &Function,
    info: ExprInfo,
    scope: &mut Scope,
) -> Result<TypeAndScopeInfo, Error> {
    let mut error = Error { errors: vec![] };
    let mut inner = Scope::with_parent(scope);
    let mut params = vec![];

    for arg in func.args.iter() {
        let t = match get_type(&arg.r#type, scope) {
            Ok(t) => {
                if let Type::Type(Some(t)) = t.tp {
                    t.as_ref().clone()
                } else {
                    error = Error::from_two(error, Error::new(ErrorKind::TypeMissmatch(TypeMissmatch{expected: Type::Type(None), found: t.tp}), arg.info));
                    continue;
                }
            },
            Err(err) => {
                error = Error::from_two(error, err);
                continue;
            }
        };

        params.push(Param {
            name: arg.name.clone(),
            r#type: t.clone(),
        });
        inner.declare(arg.name.clone(), t);
    }

    let expected = func
        .r#type
        .as_ref()
        .map(|x| get_type(x, scope))
        .unwrap_or(Ok(Type::Type(Some(Box::new(Type::Unit))).into()));
    let output = get_type(&func.body, &mut inner);

    match (expected, output.clone()) {
        (Ok(expected), Ok(output)) => {
            if let Type::Type(Some(expected)) = expected.tp {
                if &output.tp != expected.as_ref() {
                    error = Error::from_two(
                        error,
                        Error::new(
                            ErrorKind::TypeMissmatch(TypeMissmatch {
                                expected: expected.as_ref().clone(),
                                found: output.tp,
                            }),
                            func.body.info,
                        ),
                    );
                }
            } else {
                error = Error::from_two(
                    error,
                    Error::new(
                        ErrorKind::TypeMissmatch(TypeMissmatch {
                            expected: Type::Type(None),
                            found: expected.tp,
                        }),
                        func.r#type.clone().map(|x| x.info).unwrap_or(info),
                    ),
                );
            }
        }
        (Err(err), Ok(_)) => error = Error::from_two(error, err),
        (Ok(_), Err(err)) => error = Error::from_two(error, err),
        (Err(err0), Err(err1)) => error = Error::from_two(error, Error::from_two(err0, err1)),
    }

    if !error.errors.is_empty() {
        return Err(error);
    }

    Ok(Type::Func(Box::new(Func {
        output: output.expect("output type should be ok").tp,
        params: params.into_boxed_slice(),
    }))
    .into())
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
            Ok(if_type)
        }
        None => Ok(TypeAndScopeInfo {
            tp: Type::Unit,
            scope: None,
        }),
    }
}

fn get_type_block(block: &Block, scope: &mut Scope) -> Result<TypeAndScopeInfo, Error> {
    for _expr in block.content.iter() {
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

    if left.scope.is_none() {
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

fn get_type_unary_expr(
    expr: &UnaryExpr,
    info: ExprInfo,
    scope: &mut Scope,
) -> Result<TypeAndScopeInfo, Error> {
    use Type::{Bool, Int};
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
