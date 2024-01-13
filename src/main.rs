mod compiler;
mod fmt;
mod runtime;

fn main() {
    let code = r###"(2 + 2) * 2"###;

    match compiler::compile(code) {
        Ok(expr) => {
            let result = runtime::eval(expr);
            println!("{}", result);
        }
        Err(err) => {
            println!("Compilation error: {}", err);
        }
    }
}
