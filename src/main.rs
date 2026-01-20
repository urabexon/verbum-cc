use std::io::{self, Read};

fn main() {
    let mut _input = String::new();
    let _ = io::stdin().read_to_string(&mut _input);

    let asm = r#"
.intel_syntax noprefix
.global _start

_start:
    mov rax, 60
    mov rdi, 42
    syscall
"#;

    print!("{asm}");
}
