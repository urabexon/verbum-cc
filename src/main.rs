use std::io::{self, Read};

mod generator;
mod parser;
mod tokenizer;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let mut lexer = tokenizer::Lexer::new(&input);
    let prog = parser::parse_program(&mut lexer);
    let asm = generator::gen_program(&prog);

    print!("{asm}");
}
