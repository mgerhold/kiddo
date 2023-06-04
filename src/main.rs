use lexer::tokenize;
use std::error::Error;

mod token;

mod lexer;

fn main() -> Result<(), Box<dyn Error>> {
    let filename = std::path::Path::new("test.ceat");
    let source = std::fs::read_to_string(filename)?;
    let tokens = tokenize(filename, &source)?;
    for token in tokens {
        println!("{token}");
    }
    Ok(())
}
