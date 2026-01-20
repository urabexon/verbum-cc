# Verbum-cc ⚙️

Verbum-cc is a minimal C compiler written in Rust.

This project is an educational compiler implementation that translates a small subset of C into x86-64 assembly, directly linked and executed on Linux.

The goal of this project is not performance or completeness, but to understand:
- how a language is tokenized and parsed,
- how syntax trees are evaluated,
- how values are mapped onto registers and the stack,
- and how a program finally becomes a running process.

## Motivation
This project is inspired by low-level and system programming experiences, particularly in environments where understanding memory, stack frames, and ABI matters.

While the target language is C, the compiler itself is written in Rust to:
- make ownership and lifetime explicit,
- reduce accidental undefined behavior,
- and focus on compiler design rather than memory bugs.