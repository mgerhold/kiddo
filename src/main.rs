use crate::parser::parse;
use lexer::tokenize;

mod lexer;
mod parser;
mod token;

fn main() {
    let filename = std::path::PathBuf::from("test.ceat");
    let source = std::fs::read_to_string(&filename).unwrap();
    let tokens = tokenize(&filename, &source).unwrap();
    let module = parse(&tokens).unwrap();
    dbg!(module);
}
