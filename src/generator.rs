use std::collections::{HashMap, HashSet};
use crate::parser::{Expr, Stmt, Program, FnDef};

struct CodeGen {
    vars: HashMap<String, i64>,
    stack_size: i64,
    label_gen: LabelGen,
    return_label: Option<String>,
}

impl CodeGen {
    fn new(var_names: HashSet<String>, params: &[String]) -> Self {
        let mut vars = HashMap::new();
        let mut offset = 0i64;

        for param in params {
            offset -= 8;
            vars.insert(param.clone(), offset);
        }

        for name in var_names {
            if !vars.contains_key(&name) {
                offset -= 8;
                vars.insert(name, offset);
            }
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
            return_label: None,
        }
    }

    fn var_addr(&self, name: &str) -> String {
        match self.vars.get(name) {
            Some(offset) => format!("[rbp{}]", offset),
            None => panic!("undefined variable: {}", name),
        }
    }
}

pub fn gen_program(prog: &Program) -> String {
    let mut out = String::new();
    out.push_str(".intel_syntax noprefix\n");

    for func in &prog.functions {
        gen_function(func, &mut out);
    }

    out.push_str(".global _start\n\n_start:\n");

    let var_names = collect_vars_stmts(&prog.main_body);
    let mut cg = CodeGen::new(var_names, &[]);

    out.push_str("    push rbp\n");
    out.push_str("    mov rbp, rsp\n");
    if cg.stack_size > 0 {
        out.push_str(&format!("    sub rsp, {}\n", cg.stack_size));
    }

    if prog.main_body.is_empty() {
        out.push_str("    mov rax, 0\n");
    } else {
        for stmt in &prog.main_body {
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

fn gen_function(func: &FnDef, out: &mut String) {
    let var_names = collect_vars_stmts(&func.body);
    let mut cg = CodeGen::new(var_names, &func.params);

    let return_label = cg.label_gen.next(&format!("{}_ret", func.name));
    cg.return_label = Some(return_label.clone());

    out.push_str(&format!("\n{}:\n", func.name));

    out.push_str("    push rbp\n");
    out.push_str("    mov rbp, rsp\n");
    if cg.stack_size > 0 {
        out.push_str(&format!("    sub rsp, {}\n", cg.stack_size));
    }

    let arg_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
    for (i, param) in func.params.iter().enumerate() {
        if i >= 6 {
            panic!("too many parameters (max 6 supported)");
        }
        let addr = cg.var_addr(param);
        out.push_str(&format!("    mov {}, {}\n", addr, arg_regs[i]));
    }

    for stmt in &func.body {
        gen_stmt(stmt, &mut cg, out);
    }

    out.push_str("    mov rax, 0\n");

    out.push_str(&format!("{}:\n", return_label));
    out.push_str("    mov rsp, rbp\n");
    out.push_str("    pop rbp\n");
    out.push_str("    ret\n");
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
        Stmt::Return(expr) => {
            collect_vars_expr(expr, vars);
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
        Expr::Call(_, args) => {
            for arg in args {
                collect_vars_expr(arg, vars);
            }
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
        Expr::Neg(inner) | Expr::Not(inner) | Expr::Addr(inner) | Expr::Deref(inner) => {
            collect_vars_expr(inner, vars);
        }
        Expr::DerefAssign(addr, value) => {
            collect_vars_expr(addr, vars);
            collect_vars_expr(value, vars);
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

        Stmt::Return(expr) => {
            gen_expr(expr, cg, out);
            if let Some(ref label) = cg.return_label {
                out.push_str(&format!("    jmp {}\n", label));
            }
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

fn gen_addr(expr: &Expr, cg: &mut CodeGen, out: &mut String) {
    match expr {
        Expr::Ident(name) => {
            let offset = cg.vars.get(name).expect(&format!("undefined variable: {}", name));
            out.push_str(&format!("    lea rax, [rbp{}]\n", offset));
        }
        Expr::Deref(inner) => {
            gen_expr(inner, cg, out);
        }
        _ => panic!("cannot take address of non-lvalue expression"),
    }
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

        Expr::Call(name, args) => {
            if args.len() > 6 {
                panic!("too many arguments (max 6 supported)");
            }

            for arg in args.iter() {
                gen_expr(arg, cg, out);
                out.push_str("    push rax\n");
            }

            let arg_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
            for i in (0..args.len()).rev() {
                out.push_str(&format!("    pop {}\n", arg_regs[i]));
            }

            out.push_str(&format!("    call {}\n", name));
        }

        Expr::Addr(inner) => {
            gen_addr(inner, cg, out);
        }

        Expr::Deref(inner) => {
            gen_expr(inner, cg, out);
            out.push_str("    mov rax, [rax]\n");
        }

        Expr::DerefAssign(addr, value) => {
            gen_expr(value, cg, out);
            out.push_str("    push rax\n");
            gen_expr(addr, cg, out);
            out.push_str("    pop rdi\n");
            out.push_str("    mov [rax], rdi\n");
            out.push_str("    mov rax, rdi\n");
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
