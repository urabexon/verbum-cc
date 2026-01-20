use crate::parser::Expr;

pub fn gen(expr: &Expr) -> String {
    let mut out = String::new();
    out.push_str(".intel_syntax noprefix\n.global _start\n\n_start:\n");

    gen_expr(expr, &mut out);

    out.push_str("    mov rdi, rax\n");
    out.push_str("    mov rax, 60\n");
    out.push_str("    syscall\n");
    out
}

fn gen_expr(expr: &Expr, out: &mut String) {
    match expr {
        Expr::Num(n) => {
            out.push_str(&format!("    mov rax, {}\n", n));
        }
        Expr::Add(lhs, rhs) => {
            gen_expr(lhs, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, out);
            out.push_str("    pop rdi\n");
            out.push_str("    add rax, rdi\n");
        }
        Expr::Sub(lhs, rhs) => {
            gen_expr(lhs, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, out);
            out.push_str("    pop rdi\n");
            out.push_str("    sub rdi, rax\n");
            out.push_str("    mov rax, rdi\n");
        }
        Expr::Mul(lhs, rhs) => {
            gen_expr(lhs, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, out);
            out.push_str("    pop rdi\n");
            out.push_str("    imul rax, rdi\n");
        }
        Expr::Div(lhs, rhs) => {
            gen_expr(lhs, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, out);
            out.push_str("    mov rdi, rax\n");
            out.push_str("    pop rax\n");
            out.push_str("    cqo\n");
            out.push_str("    idiv rdi\n");
        }
        Expr::Neg(expr) => {
            gen_expr(expr, out);
            out.push_str("    neg rax\n");
        }
    }
}
