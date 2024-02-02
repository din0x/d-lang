use super::{EvalResult, Value};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

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

    pub fn declare(&mut self, name: Box<str>, value: Value) {
        self.0
            .as_ref()
            .borrow_mut()
            .vars
            .insert(name, EvalResult::new(value));
    }

    fn get_scope(&self, name: &Box<str>) -> Option<Scope> {
        if self.0.as_ref().borrow().vars.contains_key(name) {
            return Some(self.clone());
        }

        if let Some(ref parent) = self.0.as_ref().borrow().parent {
            return parent.get_scope(name);
        }

        None
    }

    pub fn lookup(&self, name: &Box<str>) -> EvalResult {
        self.get_scope(name)
            .expect(format!("Cannot find '{}' in current scope", name).as_str())
            .0
            .as_ref()
            .borrow()
            .vars
            .get(name)
            .expect(format!("Cannot find '{}' in current scope", name).as_str())
            .clone()
    }
}

impl Default for Scope {
    fn default() -> Self {
        Scope::new(None)
    }
}
