mod compiler;
mod runtime;

fn main() {
    let code = r###"(2 + 2) * 2"###;

    let result = compiler::compile(code);

    if let Ok(expr) = result {
        let v = runtime::eval(expr);
        println!("{}", v);
    } else if let Err(err) = result {
        println!("{}", err)
    }
}
