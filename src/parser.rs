use crate::tokenizer::{Lexer, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    EqEq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
}

pub fn parse(lexer: &mut Lexer) -> Expr {
    let node = parse_equality(lexer);
    match lexer.consume_token() {
        Token::Eof => node,
        t => panic!("unexpected token at end: {:?}", t),
    }
}
fn parse_equality(lexer: &mut Lexer) -> Expr {
    let mut node = parse_relational(lexer);

    loop {
        match lexer.peek_token() {
            Token::EqEq => {
                lexer.consume_token();
                let rhs = parse_relational(lexer);
                node = Expr::EqEq(Box::new(node), Box::new(rhs));
            }
            Token::Ne => {
                lexer.consume_token();
                let rhs = parse_relational(lexer);
                node = Expr::Ne(Box::new(node), Box::new(rhs));
            }
            _ => break,
        }
    }

    node
}

fn parse_relational(lexer: &mut Lexer) -> Expr {
    let mut node = parse_add(lexer);

    loop {
        match lexer.peek_token() {
            Token::Lt => {
                lexer.consume_token();
                let rhs = parse_add(lexer);
                node = Expr::Lt(Box::new(node), Box::new(rhs));
            }
            Token::Le => {
                lexer.consume_token();
                let rhs = parse_add(lexer);
                node = Expr::Le(Box::new(node), Box::new(rhs));
            }
            Token::Gt => {
                lexer.consume_token();
                let rhs = parse_add(lexer);
                node = Expr::Gt(Box::new(node), Box::new(rhs));
            }
            Token::Ge => {
                lexer.consume_token();
                let rhs = parse_add(lexer);
                node = Expr::Ge(Box::new(node), Box::new(rhs));
            }
            _ => break,
        }
    }

    node
}

fn parse_add(lexer: &mut Lexer) -> Expr {
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
                let node = parse_equality(lexer);
                match lexer.consume_token() {
                    Token::RParen => node,
                    t => panic!("expected ')', got {:?}", t),
                }
            }
            t => panic!("expected number or '(', got {:?}", t),
        },
    }
}
