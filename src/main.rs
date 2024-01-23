mod compiler;
mod runtime;

use crate::runtime::Scope;

fn main() {
    let scope = compiler::Scope::new();

    let code = r###"let hello = ""== """###;

    let result = compiler::compile(code, scope);

    if let Ok(expr) = result {
        let mut scope = Scope::default();
        let v = runtime::eval(expr, &mut scope);
        println!("{}", v);
    } else if let Err(err) = result {
        println!("{}", err)
    }
}
