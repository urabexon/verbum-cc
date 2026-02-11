use std::collections::HashMap;
use crate::parser::{Expr, Stmt, Program, FnDef};
use crate::types::Type;

#[allow(dead_code)]
struct VarInfo {
    offset: i64,
    ty: Type,
}

struct CodeGen {
    vars: HashMap<String, VarInfo>,
    stack_size: i64,
    label_gen: LabelGen,
    return_label: Option<String>,
    break_labels: Vec<String>,
    continue_labels: Vec<String>,
}

impl CodeGen {
    fn new(var_infos: HashMap<String, Type>, params: &[(String, Option<Type>)]) -> Self {
        let mut vars = HashMap::new();
        let mut offset = 0i64;

        for (param, ty) in params {
            let param_ty = ty.clone().unwrap_or(Type::Int);
            offset -= 8;
            vars.insert(param.clone(), VarInfo { offset, ty: param_ty });
        }

        for (name, ty) in var_infos {
            if !vars.contains_key(&name) {
                let size = ty.size().max(8);
                offset -= size;
                vars.insert(name, VarInfo { offset, ty });
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
            break_labels: Vec::new(),
            continue_labels: Vec::new(),
        }
    }

    fn var_addr(&self, name: &str) -> String {
        match self.vars.get(name) {
            Some(info) => format!("[rbp{}]", info.offset),
            None => panic!("undefined variable: {}", name),
        }
    }

    #[allow(dead_code)]
    fn var_type(&self, name: &str) -> Option<&Type> {
        self.vars.get(name).map(|info| &info.ty)
    }

    fn add_var(&mut self, name: String, ty: Type) {
        if !self.vars.contains_key(&name) {
            let size = ty.size().max(8);
            let current_min = self.vars.values()
                .map(|v| v.offset)
                .min()
                .unwrap_or(0);
            let new_offset = current_min - size;
            self.vars.insert(name, VarInfo { offset: new_offset, ty });
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

    let var_infos = collect_vars_stmts(&prog.main_body);
    let mut cg = CodeGen::new(var_infos, &[]);

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
    let var_infos = collect_vars_stmts(&func.body);
    let mut cg = CodeGen::new(var_infos, &func.params);

    let return_label = cg.label_gen.next(&format!("{}_ret", func.name));
    cg.return_label = Some(return_label.clone());

    out.push_str(&format!("\n{}:\n", func.name));

    out.push_str("    push rbp\n");
    out.push_str("    mov rbp, rsp\n");
    if cg.stack_size > 0 {
        out.push_str(&format!("    sub rsp, {}\n", cg.stack_size));
    }

    let arg_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
    for (i, (param, _)) in func.params.iter().enumerate() {
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

fn collect_vars_stmts(stmts: &[Stmt]) -> HashMap<String, Type> {
    let mut vars = HashMap::new();
    for stmt in stmts {
        collect_vars_stmt(stmt, &mut vars);
    }
    vars
}

fn collect_vars_stmt(stmt: &Stmt, vars: &mut HashMap<String, Type>) {
    match stmt {
        Stmt::ExprStmt(e) => collect_vars_expr(e, vars),
        Stmt::VarDecl(decl) => {
            vars.insert(decl.name.clone(), decl.ty.clone());
            if let Some(ref init) = decl.init {
                collect_vars_expr(init, vars);
            }
        }
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
        Stmt::Break | Stmt::Continue => {}
    }
}

fn collect_vars_expr(expr: &Expr, vars: &mut HashMap<String, Type>) {
    match expr {
        Expr::Num(_) => {}
        Expr::Ident(name) => {
            if !vars.contains_key(name) {
                vars.insert(name.clone(), Type::Int);
            }
        }
        Expr::Assign(name, rhs) => {
            if !vars.contains_key(name) {
                vars.insert(name.clone(), Type::Int);
            }
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
        | Expr::Ge(lhs, rhs)
        | Expr::BitAnd(lhs, rhs)
        | Expr::BitOr(lhs, rhs)
        | Expr::BitXor(lhs, rhs)
        | Expr::Shl(lhs, rhs)
        | Expr::Shr(lhs, rhs) => {
            collect_vars_expr(lhs, vars);
            collect_vars_expr(rhs, vars);
        }
        Expr::Neg(inner) | Expr::Not(inner) | Expr::BitNot(inner) | Expr::Addr(inner) | Expr::Deref(inner) => {
            collect_vars_expr(inner, vars);
        }
        Expr::DerefAssign(addr, value) => {
            collect_vars_expr(addr, vars);
            collect_vars_expr(value, vars);
        }
        Expr::PreInc(name) | Expr::PreDec(name) | Expr::PostInc(name) | Expr::PostDec(name) => {
            if !vars.contains_key(name) {
                vars.insert(name.clone(), Type::Int);
            }
        }
        Expr::Index(arr, idx) => {
            collect_vars_expr(arr, vars);
            collect_vars_expr(idx, vars);
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
        Stmt::VarDecl(decl) => {
            cg.add_var(decl.name.clone(), decl.ty.clone());
            if let Some(ref init) = decl.init {
                gen_expr(init, cg, out);
                let addr = cg.var_addr(&decl.name);
                out.push_str(&format!("    mov {}, rax\n", addr));
            }
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

            cg.break_labels.push(l_end.clone());
            cg.continue_labels.push(l_begin.clone());

            out.push_str(&format!("{}:\n", l_begin));
            gen_expr(cond, cg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str(&format!("    je {}\n", l_end));

            gen_stmt(body, cg, out);
            out.push_str(&format!("    jmp {}\n", l_begin));

            out.push_str(&format!("{}:\n", l_end));

            cg.break_labels.pop();
            cg.continue_labels.pop();
        }

        Stmt::For { init, cond, update, body } => {
            let l_begin = cg.label_gen.next("for_begin");
            let l_continue = cg.label_gen.next("for_continue");
            let l_end = cg.label_gen.next("for_end");

            gen_expr(init, cg, out);

            cg.break_labels.push(l_end.clone());
            cg.continue_labels.push(l_continue.clone());

            out.push_str(&format!("{}:\n", l_begin));
            gen_expr(cond, cg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str(&format!("    je {}\n", l_end));

            gen_stmt(body, cg, out);

            out.push_str(&format!("{}:\n", l_continue));
            gen_expr(update, cg, out);
            out.push_str(&format!("    jmp {}\n", l_begin));

            out.push_str(&format!("{}:\n", l_end));

            cg.break_labels.pop();
            cg.continue_labels.pop();
        }

        Stmt::DoWhile { body, cond } => {
            let l_begin = cg.label_gen.next("do_begin");
            let l_cond = cg.label_gen.next("do_cond");
            let l_end = cg.label_gen.next("do_end");

            cg.break_labels.push(l_end.clone());
            cg.continue_labels.push(l_cond.clone());

            out.push_str(&format!("{}:\n", l_begin));
            gen_stmt(body, cg, out);

            out.push_str(&format!("{}:\n", l_cond));
            gen_expr(cond, cg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str(&format!("    jne {}\n", l_begin));

            out.push_str(&format!("{}:\n", l_end));

            cg.break_labels.pop();
            cg.continue_labels.pop();
        }

        Stmt::Return(expr) => {
            gen_expr(expr, cg, out);
            if let Some(ref label) = cg.return_label {
                out.push_str(&format!("    jmp {}\n", label));
            }
        }

        Stmt::Break => {
            if let Some(label) = cg.break_labels.last() {
                out.push_str(&format!("    jmp {}\n", label));
            } else {
                panic!("break outside of loop");
            }
        }

        Stmt::Continue => {
            if let Some(label) = cg.continue_labels.last() {
                out.push_str(&format!("    jmp {}\n", label));
            } else {
                panic!("continue outside of loop");
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

#[allow(dead_code)]
fn gen_cmp_typed(lhs: &Expr, rhs: &Expr, cc: &str, cg: &mut CodeGen, out: &mut String) -> Type {
    gen_cmp(lhs, rhs, cc, cg, out);
    Type::Int
}

fn gen_addr(expr: &Expr, cg: &mut CodeGen, out: &mut String) {
    match expr {
        Expr::Ident(name) => {
            let info = cg.vars.get(name).expect(&format!("undefined variable: {}", name));
            out.push_str(&format!("    lea rax, [rbp{}]\n", info.offset));
        }
        Expr::Deref(inner) => {
            gen_expr(inner, cg, out);
        }
        Expr::Index(arr, idx) => {
            // Address of arr[i] is arr + i * elem_size
            let arr_ty = gen_expr(arr, cg, out);
            out.push_str("    push rax\n");
            gen_expr(idx, cg, out);

            if let Some(elem_ty) = arr_ty.element_type() {
                let elem_size = elem_ty.size();
                if elem_size != 1 {
                    out.push_str(&format!("    imul rax, {}\n", elem_size));
                }
            }
            out.push_str("    pop rdi\n");
            out.push_str("    add rax, rdi\n");
        }
        _ => panic!("cannot take address of non-lvalue expression"),
    }
}

fn gen_expr(expr: &Expr, cg: &mut CodeGen, out: &mut String) -> Type {
    match expr {
        Expr::Num(_) => {
            gen_expr_no_type(expr, cg, out);
            Type::Int
        }

        Expr::Ident(name) => {
            let info = cg.vars.get(name).expect(&format!("undefined variable: {}", name));
            let ty = info.ty.clone();
            let offset = info.offset;

            // For arrays, return address (arrays decay to pointers)
            if let Type::Array(elem, _) = &ty {
                out.push_str(&format!("    lea rax, [rbp{}]\n", offset));
                Type::Ptr(elem.clone())
            } else {
                out.push_str(&format!("    mov rax, [rbp{}]\n", offset));
                ty
            }
        }

        Expr::Assign(name, rhs) => {
            let ty = gen_expr(rhs, cg, out);
            let addr = cg.var_addr(name);
            out.push_str(&format!("    mov {}, rax\n", addr));
            ty
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
            Type::Int // function return type assumed to be int for now
        }

        Expr::Addr(inner) => {
            let inner_ty = get_expr_type(inner, cg);
            gen_addr(inner, cg, out);
            Type::Ptr(Box::new(inner_ty))
        }

        Expr::Deref(inner) => {
            let inner_ty = gen_expr(inner, cg, out);
            if let Some(elem_ty) = inner_ty.element_type() {
                let elem_size = elem_ty.size();
                if elem_size == 1 {
                    out.push_str("    movzx rax, byte ptr [rax]\n");
                } else {
                    out.push_str("    mov rax, [rax]\n");
                }
                elem_ty.clone()
            } else {
                out.push_str("    mov rax, [rax]\n");
                Type::Int
            }
        }

        Expr::Index(arr, idx) => {
            // arr[i] is equivalent to *(arr + i)
            let arr_ty = gen_expr(arr, cg, out);
            out.push_str("    push rax\n");
            gen_expr(idx, cg, out);

            // Scale index by element size
            if let Some(elem_ty) = arr_ty.element_type() {
                let elem_size = elem_ty.size();
                if elem_size != 1 {
                    out.push_str(&format!("    imul rax, {}\n", elem_size));
                }
                out.push_str("    pop rdi\n");
                out.push_str("    add rax, rdi\n");

                // Dereference
                if elem_size == 1 {
                    out.push_str("    movzx rax, byte ptr [rax]\n");
                } else {
                    out.push_str("    mov rax, [rax]\n");
                }
                elem_ty.clone()
            } else {
                out.push_str("    pop rdi\n");
                out.push_str("    add rax, rdi\n");
                out.push_str("    mov rax, [rax]\n");
                Type::Int
            }
        }

        Expr::DerefAssign(addr, value) => {
            // Get element type from addr expression
            let addr_ty = get_expr_type(addr, cg);
            let elem_ty = addr_ty.element_type().cloned().unwrap_or(Type::Int);
            let elem_size = elem_ty.size();

            gen_expr(value, cg, out);
            out.push_str("    push rax\n");
            gen_expr(addr, cg, out);
            out.push_str("    pop rdi\n");

            if elem_size == 1 {
                out.push_str("    mov [rax], dil\n");
            } else {
                out.push_str("    mov [rax], rdi\n");
            }
            out.push_str("    mov rax, rdi\n");
            elem_ty
        }

        Expr::Add(lhs, rhs) => {
            let lhs_ty = gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            let rhs_ty = gen_expr(rhs, cg, out);
            out.push_str("    pop rdi\n");

            // Pointer arithmetic: scale the integer operand
            if let Some(elem_ty) = lhs_ty.element_type() {
                // lhs is pointer/array, scale rhs
                let elem_size = elem_ty.size();
                if elem_size != 1 {
                    out.push_str(&format!("    imul rax, {}\n", elem_size));
                }
                out.push_str("    add rax, rdi\n");
                lhs_ty
            } else if let Some(elem_ty) = rhs_ty.element_type() {
                // rhs is pointer/array, scale lhs (in rdi)
                let elem_size = elem_ty.size();
                if elem_size != 1 {
                    out.push_str(&format!("    imul rdi, {}\n", elem_size));
                }
                out.push_str("    add rax, rdi\n");
                rhs_ty
            } else {
                out.push_str("    add rax, rdi\n");
                Type::Int
            }
        }

        Expr::Sub(lhs, rhs) => {
            let lhs_ty = gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            let rhs_ty = gen_expr(rhs, cg, out);
            out.push_str("    pop rdi\n");

            // Pointer arithmetic: scale the integer operand
            if let Some(elem_ty) = lhs_ty.element_type() {
                if rhs_ty.is_pointer() {
                    // pointer - pointer: result is scaled by element size
                    out.push_str("    sub rdi, rax\n");
                    out.push_str("    mov rax, rdi\n");
                    let elem_size = elem_ty.size();
                    if elem_size != 1 {
                        out.push_str("    cqo\n");
                        out.push_str(&format!("    mov rdi, {}\n", elem_size));
                        out.push_str("    idiv rdi\n");
                    }
                    Type::Int
                } else {
                    // pointer - int: scale the int
                    let elem_size = elem_ty.size();
                    if elem_size != 1 {
                        out.push_str(&format!("    imul rax, {}\n", elem_size));
                    }
                    out.push_str("    sub rdi, rax\n");
                    out.push_str("    mov rax, rdi\n");
                    lhs_ty
                }
            } else {
                out.push_str("    sub rdi, rax\n");
                out.push_str("    mov rax, rdi\n");
                Type::Int
            }
        }

        Expr::Mul(lhs, rhs) => {
            gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, cg, out);
            out.push_str("    pop rdi\n");
            out.push_str("    imul rax, rdi\n");
            Type::Int
        }

        Expr::Div(lhs, rhs) => {
            gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, cg, out);
            out.push_str("    mov rdi, rax\n");
            out.push_str("    pop rax\n");
            out.push_str("    cqo\n");
            out.push_str("    idiv rdi\n");
            Type::Int
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
            Type::Int
        }

        Expr::Neg(inner) => {
            gen_expr(inner, cg, out);
            out.push_str("    neg rax\n");
            Type::Int
        }

        Expr::Not(inner) => {
            gen_expr(inner, cg, out);
            out.push_str("    cmp rax, 0\n");
            out.push_str("    sete al\n");
            out.push_str("    movzx rax, al\n");
            Type::Int
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
            Type::Int
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
            Type::Int
        }

        Expr::EqEq(lhs, rhs) => { gen_cmp(lhs, rhs, "e", cg, out); Type::Int }
        Expr::Ne(lhs, rhs) => { gen_cmp(lhs, rhs, "ne", cg, out); Type::Int }
        Expr::Lt(lhs, rhs) => { gen_cmp(lhs, rhs, "l", cg, out); Type::Int }
        Expr::Le(lhs, rhs) => { gen_cmp(lhs, rhs, "le", cg, out); Type::Int }
        Expr::Gt(lhs, rhs) => { gen_cmp(lhs, rhs, "g", cg, out); Type::Int }
        Expr::Ge(lhs, rhs) => { gen_cmp(lhs, rhs, "ge", cg, out); Type::Int }

        Expr::BitAnd(lhs, rhs) => {
            gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, cg, out);
            out.push_str("    pop rdi\n");
            out.push_str("    and rax, rdi\n");
            Type::Int
        }
        Expr::BitOr(lhs, rhs) => {
            gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, cg, out);
            out.push_str("    pop rdi\n");
            out.push_str("    or rax, rdi\n");
            Type::Int
        }
        Expr::BitXor(lhs, rhs) => {
            gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, cg, out);
            out.push_str("    pop rdi\n");
            out.push_str("    xor rax, rdi\n");
            Type::Int
        }
        Expr::BitNot(inner) => {
            gen_expr(inner, cg, out);
            out.push_str("    not rax\n");
            Type::Int
        }
        Expr::Shl(lhs, rhs) => {
            gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, cg, out);
            out.push_str("    mov rcx, rax\n");
            out.push_str("    pop rax\n");
            out.push_str("    shl rax, cl\n");
            Type::Int
        }
        Expr::Shr(lhs, rhs) => {
            gen_expr(lhs, cg, out);
            out.push_str("    push rax\n");
            gen_expr(rhs, cg, out);
            out.push_str("    mov rcx, rax\n");
            out.push_str("    pop rax\n");
            out.push_str("    sar rax, cl\n");
            Type::Int
        }

        Expr::PreInc(name) => {
            let addr = cg.var_addr(&name);
            out.push_str(&format!("    mov rax, {}\n", addr));
            out.push_str("    add rax, 1\n");
            out.push_str(&format!("    mov {}, rax\n", addr));
            Type::Int
        }
        Expr::PreDec(name) => {
            let addr = cg.var_addr(&name);
            out.push_str(&format!("    mov rax, {}\n", addr));
            out.push_str("    sub rax, 1\n");
            out.push_str(&format!("    mov {}, rax\n", addr));
            Type::Int
        }
        Expr::PostInc(name) => {
            let addr = cg.var_addr(&name);
            out.push_str(&format!("    mov rax, {}\n", addr));
            out.push_str("    mov rdi, rax\n");
            out.push_str("    add rdi, 1\n");
            out.push_str(&format!("    mov {}, rdi\n", addr));
            Type::Int
        }
        Expr::PostDec(name) => {
            let addr = cg.var_addr(&name);
            out.push_str(&format!("    mov rax, {}\n", addr));
            out.push_str("    mov rdi, rax\n");
            out.push_str("    sub rdi, 1\n");
            out.push_str(&format!("    mov {}, rdi\n", addr));
            Type::Int
        }
    }
}

// Helper to generate expression without needing type
fn gen_expr_no_type(expr: &Expr, _cg: &mut CodeGen, out: &mut String) {
    match expr {
        Expr::Num(n) => {
            out.push_str(&format!("    mov rax, {}\n", n));
        }
        _ => panic!("gen_expr_no_type: unsupported expression"),
    }
}

// Get type of expression without generating code
fn get_expr_type(expr: &Expr, cg: &CodeGen) -> Type {
    match expr {
        Expr::Num(_) => Type::Int,
        Expr::Ident(name) => {
            let info = cg.vars.get(name).expect(&format!("undefined variable: {}", name));
            if let Type::Array(elem, _) = &info.ty {
                Type::Ptr(elem.clone())
            } else {
                info.ty.clone()
            }
        }
        Expr::Addr(inner) => {
            let inner_ty = get_expr_type(inner, cg);
            Type::Ptr(Box::new(inner_ty))
        }
        Expr::Deref(inner) => {
            let inner_ty = get_expr_type(inner, cg);
            inner_ty.element_type().cloned().unwrap_or(Type::Int)
        }
        Expr::Index(arr, _) => {
            let arr_ty = get_expr_type(arr, cg);
            arr_ty.element_type().cloned().unwrap_or(Type::Int)
        }
        Expr::Add(lhs, rhs) => {
            let lhs_ty = get_expr_type(lhs, cg);
            let rhs_ty = get_expr_type(rhs, cg);
            if lhs_ty.is_pointer() { lhs_ty }
            else if rhs_ty.is_pointer() { rhs_ty }
            else { Type::Int }
        }
        Expr::Sub(lhs, _) => {
            let lhs_ty = get_expr_type(lhs, cg);
            if lhs_ty.is_pointer() { lhs_ty } else { Type::Int }
        }
        _ => Type::Int,
    }
}
