use std::{fs::File, io::Read, path::Path};

mod ast;
mod error;
mod lexer;
mod parser;
mod runtime;
mod typing;

fn main() {
    use std::env;

    let args: Vec<_> = env::args().collect();

    if env::args().len() == 1 {
        run_cli();
    }

    let path = Path::new(&args[1]);

    exec(path);
}

fn create_scopes() -> (typing::Scope, runtime::Scope) {
    let mut scope = typing::Scope::new();
    scope.prelude();
    let mut runtime_scope = runtime::Scope::default();
    runtime_scope.prelude();

    (scope, runtime_scope)
}

fn run_cli() {
    use std::io::{stdin, stdout, Write};

    let (mut scope, mut runtime_scope) = create_scopes();

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

fn exec(path: &Path) {
    let file = File::open(path);
    let mut buf = String::default();
    match file {
        Ok(mut f) => _ = f.read_to_string(&mut buf),
        Err(err) => {
            dbg!(err);
            return
        },
    };

    let tokens = lexer::parse_tokens(&buf);
    let ast = parser::parse_ast(&tokens);
    
    let (mut scope, mut runtime_scope) = create_scopes();
    let result = typing::is_valid(&ast, &mut scope).map(|_| ast);

    match result {
        Ok(ast) => _ = runtime::run(ast, &mut runtime_scope),
        Err(err) => println!("{}", err),
    }
}