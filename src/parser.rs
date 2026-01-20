use crate::tokenizer::{Lexer, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
    Ident(String),
    Assign(String, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    EqEq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    ExprStmt(Expr),
    Block(Vec<Stmt>),
    If { cond: Expr, then: Box<Stmt>, els: Box<Stmt> },
    While { cond: Expr, body: Box<Stmt> },
    For { init: Expr, cond: Expr, update: Expr, body: Box<Stmt> },
    DoWhile { body: Box<Stmt>, cond: Expr },
}

pub fn parse_program(lexer: &mut Lexer) -> Vec<Stmt> {
    let mut stmts = Vec::new();
    while lexer.peek_token() != Token::Eof {
        stmts.push(parse_stmt(lexer));
    }
    stmts
}

fn parse_stmt(lexer: &mut Lexer) -> Stmt {
    match lexer.peek_token() {
        Token::If => return parse_if(lexer),
        Token::While => return parse_while(lexer),
        Token::For => return parse_for(lexer),
        Token::Do => return parse_do_while(lexer),
        Token::LBrace => return parse_block(lexer),
        _ => {}
    }

    let expr = parse_assign(lexer);

    match lexer.peek_token() {
        Token::Semi => { lexer.consume_token(); Stmt::ExprStmt(expr) }
        Token::Eof  => Stmt::ExprStmt(expr),
        t => panic!("expected ';' or EOF, got {:?}", t),
    }
}

fn parse_assign(lexer: &mut Lexer) -> Expr {
    let lhs = parse_or(lexer);

    if let Token::Eq = lexer.peek_token() {
        lexer.consume_token();
        let rhs = parse_assign(lexer);

        match lhs {
            Expr::Ident(name) => Expr::Assign(name, Box::new(rhs)),
            _ => panic!("invalid assignment target (left-hand side must be a variable)"),
        }
    } else {
        lhs
    }
}

fn parse_or(lexer: &mut Lexer) -> Expr {
    let mut node = parse_and(lexer);

    loop {
        match lexer.peek_token() {
            Token::OrOr => {
                lexer.consume_token();
                let rhs = parse_and(lexer);
                node = Expr::Or(Box::new(node), Box::new(rhs));
            }
            _ => break,
        }
    }

    node
}

fn parse_and(lexer: &mut Lexer) -> Expr {
    let mut node = parse_equality(lexer);

    loop {
        match lexer.peek_token() {
            Token::AndAnd => {
                lexer.consume_token();
                let rhs = parse_equality(lexer);
                node = Expr::And(Box::new(node), Box::new(rhs));
            }
            _ => break,
        }
    }

    node
}

fn parse_block(lexer: &mut Lexer) -> Stmt {
    match lexer.consume_token() {
        Token::LBrace => {}
        t => panic!("expected '{{', got {:?}", t),
    }
    let mut stmts = Vec::new();
    while lexer.peek_token() != Token::RBrace {
        stmts.push(parse_stmt(lexer));
    }
    match lexer.consume_token() {
        Token::RBrace => {}
        t => panic!("expected '}}', got {:?}", t),
    }
    Stmt::Block(stmts)
}

fn parse_if(lexer: &mut Lexer) -> Stmt {
    match lexer.consume_token() {
        Token::If => {}
        t => panic!("expected 'if', got {:?}", t),
    }
    match lexer.consume_token() {
        Token::LParen => {}
        t => panic!("expected '(', got {:?}", t),
    }
    let cond = parse_assign(lexer);
    match lexer.consume_token() {
        Token::RParen => {}
        t => panic!("expected ')', got {:?}", t),
    }

    let then_stmt = parse_stmt(lexer);

    match lexer.consume_token() {
        Token::Else => {}
        t => panic!("expected 'else', got {:?}", t),
    }

    let else_stmt = parse_stmt(lexer);

    Stmt::If {
        cond,
        then: Box::new(then_stmt),
        els: Box::new(else_stmt),
    }
}

fn parse_while(lexer: &mut Lexer) -> Stmt {
    match lexer.consume_token() {
        Token::While => {}
        t => panic!("expected 'while', got {:?}", t),
    }
    match lexer.consume_token() {
        Token::LParen => {}
        t => panic!("expected '(', got {:?}", t),
    }
    let cond = parse_assign(lexer);
    match lexer.consume_token() {
        Token::RParen => {}
        t => panic!("expected ')', got {:?}", t),
    }

    let body = parse_stmt(lexer);

    Stmt::While {
        cond,
        body: Box::new(body),
    }
}

fn parse_for(lexer: &mut Lexer) -> Stmt {
    match lexer.consume_token() {
        Token::For => {}
        t => panic!("expected 'for', got {:?}", t),
    }
    match lexer.consume_token() {
        Token::LParen => {}
        t => panic!("expected '(', got {:?}", t),
    }

    let init = parse_assign(lexer);
    match lexer.consume_token() {
        Token::Semi => {}
        t => panic!("expected ';' after for-init, got {:?}", t),
    }

    let cond = parse_assign(lexer);
    match lexer.consume_token() {
        Token::Semi => {}
        t => panic!("expected ';' after for-cond, got {:?}", t),
    }

    let update = parse_assign(lexer);
    match lexer.consume_token() {
        Token::RParen => {}
        t => panic!("expected ')' after for-update, got {:?}", t),
    }

    let body = parse_stmt(lexer);

    Stmt::For {
        init,
        cond,
        update,
        body: Box::new(body),
    }
}

fn parse_do_while(lexer: &mut Lexer) -> Stmt {
    match lexer.consume_token() {
        Token::Do => {}
        t => panic!("expected 'do', got {:?}", t),
    }

    let body = parse_stmt(lexer);

    match lexer.consume_token() {
        Token::While => {}
        t => panic!("expected 'while' after do-body, got {:?}", t),
    }
    match lexer.consume_token() {
        Token::LParen => {}
        t => panic!("expected '(', got {:?}", t),
    }
    let cond = parse_assign(lexer);
    match lexer.consume_token() {
        Token::RParen => {}
        t => panic!("expected ')', got {:?}", t),
    }
    match lexer.consume_token() {
        Token::Semi => {}
        t => panic!("expected ';' after do-while, got {:?}", t),
    }

    Stmt::DoWhile {
        body: Box::new(body),
        cond,
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
            Token::Percent => {
                lexer.consume_token();
                let rhs = parse_factor(lexer);
                node = Expr::Mod(Box::new(node), Box::new(rhs));
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
        Token::Not => {
            lexer.consume_token();
            Expr::Not(Box::new(parse_factor(lexer)))
        }
        _ => match lexer.consume_token() {
            Token::Num(n) => Expr::Num(n),
            Token::Ident(name) => Expr::Ident(name),
            Token::LParen => {
                let node = parse_assign(lexer);
                match lexer.consume_token() {
                    Token::RParen => node,
                    t => panic!("expected ')', got {:?}", t),
                }
            }
            t => panic!("expected number, identifier, or '(', got {:?}", t),
        },
    }
}
