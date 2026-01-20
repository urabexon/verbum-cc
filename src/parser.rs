use crate::tokenizer::{Lexer, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
}

pub fn parse(lexer: &mut Lexer) -> Expr {
    let mut node = parse_num(lexer);

    loop {
        match lexer.next_token() {
            Token::Plus => {
                let rhs = parse_num(lexer);
                node = Expr::Add(Box::new(node), Box::new(rhs));
            }
            Token::Minus => {
                let rhs = parse_num(lexer);
                node = Expr::Sub(Box::new(node), Box::new(rhs));
            }
            Token::Eof => break,
            t => panic!("unexpected token: {:?}", t),
        }
    }

    node
}

fn parse_num(lexer: &mut Lexer) -> Expr {
    match lexer.next_token() {
        Token::Num(n) => Expr::Num(n),
        t => panic!("expected number, got {:?}", t),
    }
}
