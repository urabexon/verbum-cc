use crate::parser::Expr;

pub fn gen(expr: &Expr) -> String {
    let n = match expr {
        Expr::Num(v) => *v,
    };

    format!(
        r#".intel_syntax noprefix
.global _start

_start:
    mov rax, 60
    mov rdi, {}
    syscall
"#,
        n
    )
}
