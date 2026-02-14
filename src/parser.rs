use crate::tokenizer::{Lexer, Token};
use crate::types::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
    Ident(String),
    Assign(String, Box<Expr>),
    DerefAssign(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Addr(Box<Expr>),
    Deref(Box<Expr>),
    Index(Box<Expr>, Box<Expr>),  // arr[i]
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
    BitAnd(Box<Expr>, Box<Expr>),
    BitOr(Box<Expr>, Box<Expr>),
    BitXor(Box<Expr>, Box<Expr>),
    BitNot(Box<Expr>),
    Shl(Box<Expr>, Box<Expr>),
    Shr(Box<Expr>, Box<Expr>),
    PreInc(String),   // ++i
    PreDec(String),   // --i
    PostInc(String),  // i++
    PostDec(String),  // i--
    Str(Vec<u8>),     // "hello"
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarDecl {
    pub name: String,
    pub ty: Type,
    pub init: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    ExprStmt(Expr),
    VarDecl(VarDecl),
    Block(Vec<Stmt>),
    If { cond: Expr, then: Box<Stmt>, els: Box<Stmt> },
    While { cond: Expr, body: Box<Stmt> },
    For { init: Expr, cond: Expr, update: Expr, body: Box<Stmt> },
    DoWhile { body: Box<Stmt>, cond: Expr },
    Return(Expr),
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDef {
    pub name: String,
    pub ret_ty: Option<Type>,
    pub params: Vec<(String, Option<Type>)>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub functions: Vec<FnDef>,
    pub main_body: Vec<Stmt>,
}

fn is_type_specifier(token: &Token) -> bool {
    matches!(token, Token::Int | Token::Char | Token::Void)
}

fn parse_base_type(lexer: &mut Lexer) -> Type {
    match lexer.consume_token() {
        Token::Int => Type::Int,
        Token::Char => Type::Char,
        Token::Void => Type::Void,
        t => panic!("expected type specifier, got {:?}", t),
    }
}

fn parse_type(lexer: &mut Lexer) -> Type {
    let mut ty = parse_base_type(lexer);
    while lexer.peek_token() == Token::Star {
        lexer.consume_token();
        ty = Type::Ptr(Box::new(ty));
    }
    ty
}

pub fn parse_program(lexer: &mut Lexer) -> Program {
    let mut functions = Vec::new();
    let mut main_body = Vec::new();

    while lexer.peek_token() != Token::Eof {
        if lexer.peek_token() == Token::Fn {
            functions.push(parse_fn_def(lexer));
        } else if is_type_specifier(&lexer.peek_token()) {
            match parse_typed_decl(lexer) {
                TypedDecl::Func(func) => functions.push(func),
                TypedDecl::Var(stmt) => main_body.push(stmt),
            }
        } else {
            main_body.push(parse_stmt(lexer));
        }
    }

    Program { functions, main_body }
}

fn parse_fn_def(lexer: &mut Lexer) -> FnDef {
    match lexer.consume_token() {
        Token::Fn => {}
        t => panic!("expected 'fn', got {:?}", t),
    }

    let name = match lexer.consume_token() {
        Token::Ident(name) => name,
        t => panic!("expected function name, got {:?}", t),
    };

    match lexer.consume_token() {
        Token::LParen => {}
        t => panic!("expected '(' after function name, got {:?}", t),
    }

    let mut params = Vec::new();
    if lexer.peek_token() != Token::RParen {
        loop {
            match lexer.consume_token() {
                Token::Ident(param) => params.push((param, None)),
                t => panic!("expected parameter name, got {:?}", t),
            }
            match lexer.peek_token() {
                Token::Comma => { lexer.consume_token(); }
                Token::RParen => break,
                t => panic!("expected ',' or ')' in parameter list, got {:?}", t),
            }
        }
    }

    match lexer.consume_token() {
        Token::RParen => {}
        t => panic!("expected ')' after parameters, got {:?}", t),
    }

    match lexer.consume_token() {
        Token::LBrace => {}
        t => panic!("expected '{{' for function body, got {:?}", t),
    }

    let mut body = Vec::new();
    while lexer.peek_token() != Token::RBrace {
        body.push(parse_stmt(lexer));
    }

    match lexer.consume_token() {
        Token::RBrace => {}
        t => panic!("expected '}}' after function body, got {:?}", t),
    }

    FnDef { name, ret_ty: None, params, body }
}

enum TypedDecl {
    Func(FnDef),
    Var(Stmt),
}

fn parse_typed_decl(lexer: &mut Lexer) -> TypedDecl {
    let mut ty = parse_type(lexer);

    let name = match lexer.consume_token() {
        Token::Ident(name) => name,
        t => panic!("expected identifier after type, got {:?}", t),
    };

    // Check for array declaration: int arr[10];
    if lexer.peek_token() == Token::LBracket {
        lexer.consume_token();
        let size = match lexer.consume_token() {
            Token::Num(n) => n as usize,
            t => panic!("expected array size, got {:?}", t),
        };
        match lexer.consume_token() {
            Token::RBracket => {}
            t => panic!("expected ']', got {:?}", t),
        }
        ty = Type::Array(Box::new(ty), size);

        // Handle initialization if present
        let init = if lexer.peek_token() == Token::Eq {
            lexer.consume_token();
            Some(parse_assign(lexer))
        } else {
            None
        };

        match lexer.consume_token() {
            Token::Semi => {}
            t => panic!("expected ';' after variable declaration, got {:?}", t),
        }

        return TypedDecl::Var(Stmt::VarDecl(VarDecl { name, ty, init }));
    }

    // Check if it's a function definition
    if lexer.peek_token() != Token::LParen {
        // It's a variable declaration
        let init = if lexer.peek_token() == Token::Eq {
            lexer.consume_token();
            Some(parse_assign(lexer))
        } else {
            None
        };

        match lexer.consume_token() {
            Token::Semi => {}
            t => panic!("expected ';' after variable declaration, got {:?}", t),
        }

        return TypedDecl::Var(Stmt::VarDecl(VarDecl { name, ty, init }));
    }

    // It's a function definition
    lexer.consume_token(); // consume '('

    let mut params = Vec::new();
    if lexer.peek_token() != Token::RParen {
        loop {
            let param_ty = parse_type(lexer);
            let param_name = match lexer.consume_token() {
                Token::Ident(name) => name,
                t => panic!("expected parameter name, got {:?}", t),
            };
            params.push((param_name, Some(param_ty)));

            match lexer.peek_token() {
                Token::Comma => { lexer.consume_token(); }
                Token::RParen => break,
                t => panic!("expected ',' or ')' in parameter list, got {:?}", t),
            }
        }
    }

    match lexer.consume_token() {
        Token::RParen => {}
        t => panic!("expected ')' after parameters, got {:?}", t),
    }

    match lexer.consume_token() {
        Token::LBrace => {}
        t => panic!("expected '{{' for function body, got {:?}", t),
    }

    let mut body = Vec::new();
    while lexer.peek_token() != Token::RBrace {
        body.push(parse_stmt(lexer));
    }

    match lexer.consume_token() {
        Token::RBrace => {}
        t => panic!("expected '}}' after function body, got {:?}", t),
    }

    TypedDecl::Func(FnDef { name, ret_ty: Some(ty), params, body })
}

fn parse_stmt(lexer: &mut Lexer) -> Stmt {
    match lexer.peek_token() {
        Token::If => return parse_if(lexer),
        Token::While => return parse_while(lexer),
        Token::For => return parse_for(lexer),
        Token::Do => return parse_do_while(lexer),
        Token::LBrace => return parse_block(lexer),
        Token::Return => return parse_return(lexer),
        Token::Break => {
            lexer.consume_token();
            match lexer.consume_token() {
                Token::Semi => {}
                t => panic!("expected ';' after break, got {:?}", t),
            }
            return Stmt::Break;
        }
        Token::Continue => {
            lexer.consume_token();
            match lexer.consume_token() {
                Token::Semi => {}
                t => panic!("expected ';' after continue, got {:?}", t),
            }
            return Stmt::Continue;
        }
        Token::Int | Token::Char | Token::Void => {
            return parse_var_decl(lexer);
        }
        _ => {}
    }

    let expr = parse_assign(lexer);

    match lexer.peek_token() {
        Token::Semi => { lexer.consume_token(); Stmt::ExprStmt(expr) }
        Token::Eof  => Stmt::ExprStmt(expr),
        t => panic!("expected ';' or EOF, got {:?}", t),
    }
}

fn parse_var_decl(lexer: &mut Lexer) -> Stmt {
    let mut ty = parse_type(lexer);

    let name = match lexer.consume_token() {
        Token::Ident(name) => name,
        t => panic!("expected variable name, got {:?}", t),
    };

    // Check for array declaration: int arr[10];
    if lexer.peek_token() == Token::LBracket {
        lexer.consume_token();
        let size = match lexer.consume_token() {
            Token::Num(n) => n as usize,
            t => panic!("expected array size, got {:?}", t),
        };
        match lexer.consume_token() {
            Token::RBracket => {}
            t => panic!("expected ']', got {:?}", t),
        }
        ty = Type::Array(Box::new(ty), size);
    }

    let init = if lexer.peek_token() == Token::Eq {
        lexer.consume_token();
        Some(parse_assign(lexer))
    } else {
        None
    };

    match lexer.consume_token() {
        Token::Semi => {}
        t => panic!("expected ';' after variable declaration, got {:?}", t),
    }

    Stmt::VarDecl(VarDecl { name, ty, init })
}

fn parse_return(lexer: &mut Lexer) -> Stmt {
    match lexer.consume_token() {
        Token::Return => {}
        t => panic!("expected 'return', got {:?}", t),
    }

    let expr = parse_assign(lexer);

    match lexer.consume_token() {
        Token::Semi => {}
        t => panic!("expected ';' after return, got {:?}", t),
    }

    Stmt::Return(expr)
}

fn parse_assign(lexer: &mut Lexer) -> Expr {
    let lhs = parse_or(lexer);

    match lexer.peek_token() {
        Token::Eq => {
            lexer.consume_token();
            let rhs = parse_assign(lexer);
            match lhs {
                Expr::Ident(name) => Expr::Assign(name, Box::new(rhs)),
                Expr::Deref(inner) => Expr::DerefAssign(inner, Box::new(rhs)),
                Expr::Index(arr, idx) => {
                    // arr[i] = val becomes *(arr + i) = val
                    let addr = Expr::Add(arr, idx);
                    Expr::DerefAssign(Box::new(addr), Box::new(rhs))
                }
                _ => panic!("invalid assignment target"),
            }
        }
        Token::PlusEq | Token::MinusEq | Token::StarEq | Token::SlashEq | Token::PercentEq => {
            let op = lexer.consume_token();
            let rhs = parse_assign(lexer);
            match lhs {
                Expr::Ident(ref name) => {
                    let lhs_expr = Box::new(Expr::Ident(name.clone()));
                    let rhs_expr = Box::new(rhs);
                    let compound = match op {
                        Token::PlusEq => Expr::Add(lhs_expr, rhs_expr),
                        Token::MinusEq => Expr::Sub(lhs_expr, rhs_expr),
                        Token::StarEq => Expr::Mul(lhs_expr, rhs_expr),
                        Token::SlashEq => Expr::Div(lhs_expr, rhs_expr),
                        Token::PercentEq => Expr::Mod(lhs_expr, rhs_expr),
                        _ => unreachable!(),
                    };
                    Expr::Assign(name.clone(), Box::new(compound))
                }
                _ => panic!("compound assignment requires variable on left-hand side"),
            }
        }
        _ => lhs,
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
    let mut node = parse_bitor(lexer);

    loop {
        match lexer.peek_token() {
            Token::AndAnd => {
                lexer.consume_token();
                let rhs = parse_bitor(lexer);
                node = Expr::And(Box::new(node), Box::new(rhs));
            }
            _ => break,
        }
    }

    node
}

fn parse_bitor(lexer: &mut Lexer) -> Expr {
    let mut node = parse_bitxor(lexer);

    loop {
        match lexer.peek_token() {
            Token::Pipe => {
                lexer.consume_token();
                let rhs = parse_bitxor(lexer);
                node = Expr::BitOr(Box::new(node), Box::new(rhs));
            }
            _ => break,
        }
    }

    node
}

fn parse_bitxor(lexer: &mut Lexer) -> Expr {
    let mut node = parse_bitand(lexer);

    loop {
        match lexer.peek_token() {
            Token::Caret => {
                lexer.consume_token();
                let rhs = parse_bitand(lexer);
                node = Expr::BitXor(Box::new(node), Box::new(rhs));
            }
            _ => break,
        }
    }

    node
}

fn parse_bitand(lexer: &mut Lexer) -> Expr {
    let mut node = parse_equality(lexer);

    loop {
        match lexer.peek_token() {
            Token::Amp => {
                lexer.consume_token();
                let rhs = parse_equality(lexer);
                node = Expr::BitAnd(Box::new(node), Box::new(rhs));
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

    let else_stmt = if lexer.peek_token() == Token::Else {
        lexer.consume_token();
        parse_stmt(lexer)
    } else {
        Stmt::Block(vec![])
    };

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
    let mut node = parse_shift(lexer);

    loop {
        match lexer.peek_token() {
            Token::Lt => {
                lexer.consume_token();
                let rhs = parse_shift(lexer);
                node = Expr::Lt(Box::new(node), Box::new(rhs));
            }
            Token::Le => {
                lexer.consume_token();
                let rhs = parse_shift(lexer);
                node = Expr::Le(Box::new(node), Box::new(rhs));
            }
            Token::Gt => {
                lexer.consume_token();
                let rhs = parse_shift(lexer);
                node = Expr::Gt(Box::new(node), Box::new(rhs));
            }
            Token::Ge => {
                lexer.consume_token();
                let rhs = parse_shift(lexer);
                node = Expr::Ge(Box::new(node), Box::new(rhs));
            }
            _ => break,
        }
    }

    node
}

fn parse_shift(lexer: &mut Lexer) -> Expr {
    let mut node = parse_add(lexer);

    loop {
        match lexer.peek_token() {
            Token::Shl => {
                lexer.consume_token();
                let rhs = parse_add(lexer);
                node = Expr::Shl(Box::new(node), Box::new(rhs));
            }
            Token::Shr => {
                lexer.consume_token();
                let rhs = parse_add(lexer);
                node = Expr::Shr(Box::new(node), Box::new(rhs));
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
        Token::Tilde => {
            lexer.consume_token();
            Expr::BitNot(Box::new(parse_factor(lexer)))
        }
        Token::Amp => {
            lexer.consume_token();
            Expr::Addr(Box::new(parse_factor(lexer)))
        }
        Token::Star => {
            lexer.consume_token();
            Expr::Deref(Box::new(parse_factor(lexer)))
        }
        Token::PlusPlus => {
            lexer.consume_token();
            match lexer.peek_token() {
                Token::Ident(name) => {
                    lexer.consume_token();
                    Expr::PreInc(name)
                }
                t => panic!("expected identifier after '++', got {:?}", t),
            }
        }
        Token::MinusMinus => {
            lexer.consume_token();
            match lexer.peek_token() {
                Token::Ident(name) => {
                    lexer.consume_token();
                    Expr::PreDec(name)
                }
                _ => Expr::Neg(Box::new(Expr::Neg(Box::new(parse_factor(lexer)))))
            }
        }
        _ => match lexer.consume_token() {
            Token::Num(n) => Expr::Num(n),
            Token::Str(s) => {
                let mut expr = Expr::Str(s);
                while lexer.peek_token() == Token::LBracket {
                    lexer.consume_token();
                    let index = parse_assign(lexer);
                    match lexer.consume_token() {
                        Token::RBracket => {}
                        t => panic!("expected ']', got {:?}", t),
                    }
                    expr = Expr::Index(Box::new(expr), Box::new(index));
                }
                expr
            }
            Token::Ident(name) => {
                let mut expr = match lexer.peek_token() {
                    Token::PlusPlus => {
                        lexer.consume_token();
                        Expr::PostInc(name)
                    }
                    Token::MinusMinus => {
                        lexer.consume_token();
                        Expr::PostDec(name)
                    }
                    Token::LParen => {
                        lexer.consume_token();
                        let mut args = Vec::new();
                        if lexer.peek_token() != Token::RParen {
                            loop {
                                args.push(parse_assign(lexer));
                                match lexer.peek_token() {
                                    Token::Comma => { lexer.consume_token(); }
                                    Token::RParen => break,
                                    t => panic!("expected ',' or ')' in argument list, got {:?}", t),
                                }
                            }
                        }
                        match lexer.consume_token() {
                            Token::RParen => {}
                            t => panic!("expected ')' after arguments, got {:?}", t),
                        }
                        Expr::Call(name, args)
                    }
                    _ => Expr::Ident(name),
                };
                // Handle array indexing: arr[i], arr[i][j], etc.
                while lexer.peek_token() == Token::LBracket {
                    lexer.consume_token();
                    let index = parse_assign(lexer);
                    match lexer.consume_token() {
                        Token::RBracket => {}
                        t => panic!("expected ']', got {:?}", t),
                    }
                    expr = Expr::Index(Box::new(expr), Box::new(index));
                }
                expr
            }
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
