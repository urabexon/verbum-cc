use std::io::{self, Read};

mod generator;
mod parser;
mod tokenizer;

fn main() {
    let mut input = String::new();
    // let _ = io::stdin().read_to_string(&mut _input);
    io::stdin().read_to_string(&mut input).unwrap();

    let tokens = tokenizer::tokenize(&input);
    let expr = parser::parse(&tokens);
    let asm = generator::gen(&expr);

    print!("{asm}");
}
