use graf_lang::*;

fn main() {
    let path = std::env::args().nth(1).unwrap();
    let contents = std::fs::read_to_string(&path).unwrap();

    let code = parser::graf::program(&contents).unwrap();

    let mut rt = interpreter::Runtime::new(code, Vec::new());

    loop {
        if rt.step() {
            break;
        }
    }
}
