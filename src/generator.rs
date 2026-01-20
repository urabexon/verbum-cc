use crate::parser::{Expr, Stmt};

pub fn gen_program(stmts: &[Stmt]) -> String {
    let mut out = String::new();
    out.push_str(".intel_syntax noprefix\n.global _start\n\n_start:\n");

    let mut lg = LabelGen::new();

    if stmts.is_empty() {
        out.push_str("    mov rax, 0\n");
    } else {
        for stmt in stmts {
            gen_stmt(stmt, &mut lg, &mut out);
        }
    }

    out.push_str("    mov rdi, rax\n");
    out.push_str("    mov rax, 60\n");
    out.push_str("    syscall\n");
    out
}

struct LabelGen {
    n: usize,
}

impl LabelGen {
    fn new() -> Self {
        Self { n: 0 }
    }
    fn next(&mut self, prefix: &str) -> String {
        let s = format!(".L{}_{}", prefix, self.n);
        self.n += 1;
        s
    }
}

fn gen_stmt(stmt: &Stmt, lg: &mut LabelGen, out: &mut String) {
    match stmt {
        Stmt::ExprStmt(e) => {
            gen_expr(e, lg, out);
        }
        Stmt::Block(stmts) => {
            for s in stmts {
                gen_stmt(s, lg, out);
            }
        }
        Stmt::If { cond, then, els } => {
            let l_else = lg.next("else");
            let l_end = lg.next("endif");

            gen_expr(cond, lg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str(&format!("    je {}\n", l_else));

            gen_stmt(then, lg, out);
            out.push_str(&format!("    jmp {}\n", l_end));

            out.push_str(&format!("{}:\n", l_else));
            gen_stmt(els, lg, out);

            out.push_str(&format!("{}:\n", l_end));
        }
        Stmt::While { cond, body } => {
            let l_begin = lg.next("while_begin");
            let l_end = lg.next("while_end");

            out.push_str(&format!("{}:\n", l_begin));
            gen_expr(cond, lg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str(&format!("    je {}\n", l_end));

            gen_stmt(body, lg, out);
            out.push_str(&format!("    jmp {}\n", l_begin));

            out.push_str(&format!("{}:\n", l_end));
        }
    }
}

fn gen_cmp(lhs: &Expr, rhs: &Expr, cc: &str, lg: &mut LabelGen, out: &mut String) {
    gen_expr(lhs, lg, out);
    out.push_str("    push rax\n");
    gen_expr(rhs, lg, out);
    out.push_str("    pop rdi\n");
    out.push_str("    cmp rdi, rax\n");
    out.push_str(&format!("    set{} al\n", cc));
    out.push_str("    movzx rax, al\n");
}

fn gen_expr(expr: &Expr, lg: &mut LabelGen, out: &mut String) {
    match expr {
        Expr::Num(n) => {
            out.push_str(&format!("    mov rax, {}\n", n));
        }

        Expr::Add(lhs, rhs) => {
            gen_expr(lhs, lg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, lg, out);
            out.push_str("    pop rdi\n");
            out.push_str("    add rax, rdi\n");
        }
        Expr::Sub(lhs, rhs) => {
            gen_expr(lhs, lg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, lg, out);
            out.push_str("    pop rdi\n");
            out.push_str("    sub rdi, rax\n");
            out.push_str("    mov rax, rdi\n");
        }
        Expr::Mul(lhs, rhs) => {
            gen_expr(lhs, lg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, lg, out);
            out.push_str("    pop rdi\n");
            out.push_str("    imul rax, rdi\n");
        }
        Expr::Div(lhs, rhs) => {
            gen_expr(lhs, lg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, lg, out);
            out.push_str("    mov rdi, rax\n");
            out.push_str("    pop rax\n");
            out.push_str("    cqo\n");
            out.push_str("    idiv rdi\n");
        }

        Expr::Neg(inner) => {
            gen_expr(inner, lg, out);
            out.push_str("    neg rax\n");
        }

        Expr::EqEq(lhs, rhs) => gen_cmp(lhs, rhs, "e", lg, out),
        Expr::Ne(lhs, rhs) => gen_cmp(lhs, rhs, "ne", lg, out),
        Expr::Lt(lhs, rhs) => gen_cmp(lhs, rhs, "l", lg, out),
        Expr::Le(lhs, rhs) => gen_cmp(lhs, rhs, "le", lg, out),
        Expr::Gt(lhs, rhs) => gen_cmp(lhs, rhs, "g", lg, out),
        Expr::Ge(lhs, rhs) => gen_cmp(lhs, rhs, "ge", lg, out),
    }
}
