use crate::{compiler, runtime};
use std::io::{stdin, stdout, Write};

pub fn run() {
    let scope = compiler::Scope::new();
    let mut runtime_scope = runtime::Scope::default();

    loop {
        print!("> ");
        _ = stdout().flush();
        let mut code = String::new();
        _ = stdin().read_line(&mut code);

        let result = compiler::compile(code.as_str(), scope.clone());

        if let Ok(expr) = result {
            let v = runtime::run(expr, &mut runtime_scope);
            println!("{}", v);
        } else if let Err(err) = result {
            println!("{}", err)
        }
    }
}
