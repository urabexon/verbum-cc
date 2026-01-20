use std::collections::{HashMap, HashSet};
use crate::parser::{Expr, Stmt};

struct CodeGen {
    vars: HashMap<String, i64>,
    stack_size: i64,
    label_gen: LabelGen,
}

impl CodeGen {
    fn new(var_names: HashSet<String>) -> Self {
        let mut vars = HashMap::new();
        let mut offset = 0i64;

        for name in var_names {
            offset -= 8;
            vars.insert(name, offset);
        }

        let stack_size = if offset == 0 {
            0
        } else {
            ((-offset + 15) / 16) * 16
        };

        Self {
            vars,
            stack_size,
            label_gen: LabelGen::new(),
        }
    }

    fn var_addr(&self, name: &str) -> String {
        match self.vars.get(name) {
            Some(offset) => format!("[rbp{}]", offset),
            None => panic!("undefined variable: {}", name),
        }
    }
}

pub fn gen_program(stmts: &[Stmt]) -> String {
    let var_names = collect_vars_stmts(stmts);

    let mut cg = CodeGen::new(var_names);

    let mut out = String::new();
    out.push_str(".intel_syntax noprefix\n.global _start\n\n_start:\n");

    out.push_str("    push rbp\n");
    out.push_str("    mov rbp, rsp\n");
    if cg.stack_size > 0 {
        out.push_str(&format!("    sub rsp, {}\n", cg.stack_size));
    }

    if stmts.is_empty() {
        out.push_str("    mov rax, 0\n");
    } else {
        for stmt in stmts {
            gen_stmt(stmt, &mut cg, &mut out);
        }
    }

    out.push_str("    mov rsp, rbp\n");
    out.push_str("    pop rbp\n");

    out.push_str("    mov rdi, rax\n");
    out.push_str("    mov rax, 60\n");
    out.push_str("    syscall\n");
    out
}

//=============================================================================
// collect variables
//=============================================================================

fn collect_vars_stmts(stmts: &[Stmt]) -> HashSet<String> {
    let mut vars = HashSet::new();
    for stmt in stmts {
        collect_vars_stmt(stmt, &mut vars);
    }
    vars
}

fn collect_vars_stmt(stmt: &Stmt, vars: &mut HashSet<String>) {
    match stmt {
        Stmt::ExprStmt(e) => collect_vars_expr(e, vars),
        Stmt::Block(stmts) => {
            for s in stmts {
                collect_vars_stmt(s, vars);
            }
        }
        Stmt::If { cond, then, els } => {
            collect_vars_expr(cond, vars);
            collect_vars_stmt(then, vars);
            collect_vars_stmt(els, vars);
        }
        Stmt::While { cond, body } => {
            collect_vars_expr(cond, vars);
            collect_vars_stmt(body, vars);
        }
        Stmt::For { init, cond, update, body } => {
            collect_vars_expr(init, vars);
            collect_vars_expr(cond, vars);
            collect_vars_expr(update, vars);
            collect_vars_stmt(body, vars);
        }
        Stmt::DoWhile { body, cond } => {
            collect_vars_stmt(body, vars);
            collect_vars_expr(cond, vars);
        }
    }
}

fn collect_vars_expr(expr: &Expr, vars: &mut HashSet<String>) {
    match expr {
        Expr::Num(_) => {}
        Expr::Ident(name) => {
            vars.insert(name.clone());
        }
        Expr::Assign(name, rhs) => {
            vars.insert(name.clone());
            collect_vars_expr(rhs, vars);
        }
        Expr::Add(lhs, rhs)
        | Expr::Sub(lhs, rhs)
        | Expr::Mul(lhs, rhs)
        | Expr::Div(lhs, rhs)
        | Expr::Mod(lhs, rhs)
        | Expr::And(lhs, rhs)
        | Expr::Or(lhs, rhs)
        | Expr::EqEq(lhs, rhs)
        | Expr::Ne(lhs, rhs)
        | Expr::Lt(lhs, rhs)
        | Expr::Le(lhs, rhs)
        | Expr::Gt(lhs, rhs)
        | Expr::Ge(lhs, rhs) => {
            collect_vars_expr(lhs, vars);
            collect_vars_expr(rhs, vars);
        }
        Expr::Neg(inner) | Expr::Not(inner) => {
            collect_vars_expr(inner, vars);
        }
    }
}

//=============================================================================
// code generation
//=============================================================================

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

fn gen_stmt(stmt: &Stmt, cg: &mut CodeGen, out: &mut String) {
    match stmt {
        Stmt::ExprStmt(e) => {
            gen_expr(e, cg, out);
        }
        Stmt::Block(stmts) => {
            for s in stmts {
                gen_stmt(s, cg, out);
            }
        }
        Stmt::If { cond, then, els } => {
            let l_else = cg.label_gen.next("else");
            let l_end = cg.label_gen.next("endif");

            gen_expr(cond, cg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str(&format!("    je {}\n", l_else));

            gen_stmt(then, cg, out);
            out.push_str(&format!("    jmp {}\n", l_end));

            out.push_str(&format!("{}:\n", l_else));
            gen_stmt(els, cg, out);

            out.push_str(&format!("{}:\n", l_end));
        }
        Stmt::While { cond, body } => {
            let l_begin = cg.label_gen.next("while_begin");
            let l_end = cg.label_gen.next("while_end");

            out.push_str(&format!("{}:\n", l_begin));
            gen_expr(cond, cg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str(&format!("    je {}\n", l_end));

            gen_stmt(body, cg, out);
            out.push_str(&format!("    jmp {}\n", l_begin));

            out.push_str(&format!("{}:\n", l_end));
        }

        Stmt::For { init, cond, update, body } => {
            let l_begin = cg.label_gen.next("for_begin");
            let l_end = cg.label_gen.next("for_end");

            gen_expr(init, cg, out);

            out.push_str(&format!("{}:\n", l_begin));
            gen_expr(cond, cg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str(&format!("    je {}\n", l_end));

            gen_stmt(body, cg, out);
            gen_expr(update, cg, out);
            out.push_str(&format!("    jmp {}\n", l_begin));

            out.push_str(&format!("{}:\n", l_end));
        }

        Stmt::DoWhile { body, cond } => {
            let l_begin = cg.label_gen.next("do_begin");

            out.push_str(&format!("{}:\n", l_begin));
            gen_stmt(body, cg, out);
            gen_expr(cond, cg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str(&format!("    jne {}\n", l_begin));
        }
    }
}

fn gen_cmp(lhs: &Expr, rhs: &Expr, cc: &str, cg: &mut CodeGen, out: &mut String) {
    gen_expr(lhs, cg, out);
    out.push_str("    push rax\n");
    gen_expr(rhs, cg, out);
    out.push_str("    pop rdi\n");
    out.push_str("    cmp rdi, rax\n");
    out.push_str(&format!("    set{} al\n", cc));
    out.push_str("    movzx rax, al\n");
}

fn gen_expr(expr: &Expr, cg: &mut CodeGen, out: &mut String) {
    match expr {
        Expr::Num(n) => {
            out.push_str(&format!("    mov rax, {}\n", n));
        }

        Expr::Ident(name) => {
            let addr = cg.var_addr(name);
            out.push_str(&format!("    mov rax, {}\n", addr));
        }

        Expr::Assign(name, rhs) => {
            gen_expr(rhs, cg, out);
            let addr = cg.var_addr(name);
            out.push_str(&format!("    mov {}, rax\n", addr));
        }

        Expr::Add(lhs, rhs) => {
            gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, cg, out);
            out.push_str("    pop rdi\n");
            out.push_str("    add rax, rdi\n");
        }
        Expr::Sub(lhs, rhs) => {
            gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, cg, out);
            out.push_str("    pop rdi\n");
            out.push_str("    sub rdi, rax\n");
            out.push_str("    mov rax, rdi\n");
        }
        Expr::Mul(lhs, rhs) => {
            gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, cg, out);
            out.push_str("    pop rdi\n");
            out.push_str("    imul rax, rdi\n");
        }
        Expr::Div(lhs, rhs) => {
            gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, cg, out);
            out.push_str("    mov rdi, rax\n");
            out.push_str("    pop rax\n");
            out.push_str("    cqo\n");
            out.push_str("    idiv rdi\n");
        }

        Expr::Mod(lhs, rhs) => {
            gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, cg, out);
            out.push_str("    mov rdi, rax\n");
            out.push_str("    pop rax\n");
            out.push_str("    cqo\n");
            out.push_str("    idiv rdi\n");
            out.push_str("    mov rax, rdx\n");
        }

        Expr::Neg(inner) => {
            gen_expr(inner, cg, out);
            out.push_str("    neg rax\n");
        }

        Expr::Not(inner) => {
            gen_expr(inner, cg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str("    sete al\n");
            out.push_str("    movzx rax, al\n");
        }

        Expr::And(lhs, rhs) => {
            let l_false = cg.label_gen.next("and_false");
            let l_end = cg.label_gen.next("and_end");

            gen_expr(lhs, cg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str(&format!("    je {}\n", l_false));

            gen_expr(rhs, cg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str(&format!("    je {}\n", l_false));

            out.push_str("    mov rax, 1\n");
            out.push_str(&format!("    jmp {}\n", l_end));

            out.push_str(&format!("{}:\n", l_false));
            out.push_str("    mov rax, 0\n");

            out.push_str(&format!("{}:\n", l_end));
        }

        Expr::Or(lhs, rhs) => {
            let l_true = cg.label_gen.next("or_true");
            let l_end = cg.label_gen.next("or_end");

            gen_expr(lhs, cg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str(&format!("    jne {}\n", l_true));

            gen_expr(rhs, cg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str(&format!("    jne {}\n", l_true));

            out.push_str("    mov rax, 0\n");
            out.push_str(&format!("    jmp {}\n", l_end));

            out.push_str(&format!("{}:\n", l_true));
            out.push_str("    mov rax, 1\n");

            out.push_str(&format!("{}:\n", l_end));
        }

        Expr::EqEq(lhs, rhs) => gen_cmp(lhs, rhs, "e", cg, out),
        Expr::Ne(lhs, rhs) => gen_cmp(lhs, rhs, "ne", cg, out),
        Expr::Lt(lhs, rhs) => gen_cmp(lhs, rhs, "l", cg, out),
        Expr::Le(lhs, rhs) => gen_cmp(lhs, rhs, "le", cg, out),
        Expr::Gt(lhs, rhs) => gen_cmp(lhs, rhs, "g", cg, out),
        Expr::Ge(lhs, rhs) => gen_cmp(lhs, rhs, "ge", cg, out),
    }
}
