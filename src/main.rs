use std::{
    fs::File,
    io::{stdin, stdout, Read, Write},
    path::Path,
};

use error::err_format;

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
        let ast = parser::parse_ast(&tokens, false);
        let valid = typing::is_valid(&ast, &mut scope);

        match valid {
            Ok(()) => {
                let v = runtime::run(ast, &mut runtime_scope);
                println!("{}", v);
            }
            Err(err) => {
                print!("{}", err_format(err, Path::new("<stdin>"), &code));
                let _ = std::io::stdout().flush();
            }
        }
    }
}

fn exec(path: &Path) {
    let file = File::open(path);
    let mut buf = String::default();
    match file {
        Ok(mut f) => _ = f.read_to_string(&mut buf),
        Err(_) => {
            println!(
                "\x1b[31m\x1b[1merror\x1b[0m: cannot open file: {}",
                path.display()
            );
            return;
        }
    };

    let tokens = lexer::parse_tokens(&buf);
    let ast = parser::parse_ast(&tokens, true);

    let (mut scope, mut runtime_scope) = create_scopes();
    let result = typing::is_valid(&ast, &mut scope).map(|_| ast);

    match result {
        Ok(ast) => _ = runtime::run(ast, &mut runtime_scope),
        Err(err) => {
            print!("{}", err_format(err, path, &buf));
            let _ = std::io::stdout().flush();
        }
    }
}
