use crate::tokenizer::{Lexer, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
}

pub fn parse(lexer: &mut Lexer) -> Expr {
    let node = parse_expr(lexer);
    match lexer.consume_token() {
        Token::Eof => node,
        t => panic!("unexpected token at end: {:?}", t),
    }
}

fn parse_expr(lexer: &mut Lexer) -> Expr {
    let mut node = parse_term(lexer);

    loop {
        match lexer.peek_token() {
            Token::Plus => {
                lexer.consume_token();
                let rhs = parse_term(lexer);
                node = Expr::Add(Box::new(node), Box::new(rhs));
            }
            Token::Minus => {
                lexer.consume_token();
                let rhs = parse_term(lexer);
                node = Expr::Sub(Box::new(node), Box::new(rhs));
            }
            _ => break,
        }
    }

    node
}

fn parse_term(lexer: &mut Lexer) -> Expr {
    let mut node = parse_factor(lexer);

    loop {
        match lexer.peek_token() {
            Token::Star => {
                lexer.consume_token();
                let rhs = parse_factor(lexer);
                node = Expr::Mul(Box::new(node), Box::new(rhs));
            }
            Token::Slash => {
                lexer.consume_token();
                let rhs = parse_factor(lexer);
                node = Expr::Div(Box::new(node), Box::new(rhs));
            }
            _ => break,
        }
    }

    node
}

fn parse_factor(lexer: &mut Lexer) -> Expr {
    match lexer.peek_token() {
        Token::Plus => {
            lexer.consume_token();
            parse_factor(lexer)
        }
        Token::Minus => {
            lexer.consume_token();
            Expr::Neg(Box::new(parse_factor(lexer)))
        }
        _ => match lexer.consume_token() {
            Token::Num(n) => Expr::Num(n),
            Token::LParen => {
                let node = parse_expr(lexer);
                match lexer.consume_token() {
                    Token::RParen => node,
                    t => panic!("expected ')', got {:?}", t),
                }
            }
            t => panic!("expected number or '(', got {:?}", t),
        },
    }
}
