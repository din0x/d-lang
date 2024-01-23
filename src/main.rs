mod compiler;
mod runtime;

use crate::runtime::Scope;

fn main() {
    let code = r###"hello = 1"###;

    let result = compiler::compile(code);

    if let Ok(expr) = result {
        let mut scope = Scope::default();
        let v = runtime::eval(expr, &mut scope);
        println!("{}", v);
    } else if let Err(err) = result {
        println!("{}", err)
    }
}
