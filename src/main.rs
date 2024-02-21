mod ast;
mod error;
mod lexer;
mod parser;
mod runtime;
mod typing;

use std::io::{stdin, stdout, Write};

fn main() {
    let mut scope = typing::Scope::new();
    scope.prelude();
    let mut runtime_scope = runtime::Scope::default();
    runtime_scope.prelude();

    loop {
        print!("> ");
        _ = stdout().flush();
        let mut code = String::new();
        _ = stdin().read_line(&mut code);

        if code.trim().is_empty() {
            continue;
        }

        let tokens = lexer::parse_tokens(code.as_str());
        let ast = parser::parse_ast(&tokens);
        let valid = typing::is_valid(&ast, &mut scope);

        match valid {
            Ok(()) => {
                let v = runtime::run(ast, &mut runtime_scope);
                println!("{}", v);
            }
            Err(err) => println!("{}", err),
        }
    }
}
