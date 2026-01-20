use crate::tokenizer::Token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
}

pub fn parse(tokens: &[Token]) -> Expr {
    match tokens.get(0) {
        Some(Token::Num(n)) => Expr::Num(*n),
        _ => panic!("expected a number"),
    }
}
