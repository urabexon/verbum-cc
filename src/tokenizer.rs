#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Num(i64),
    Eof,
    Plus,
    Minus,
    // Asterisk,
    // Slash,
    // Percent,
    // Caret,
    // Ampersand,
    // Pipe,
    // LessThan,
    // GreaterThan,
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let bytes = input.as_bytes();
    let mut i = 0;
    let mut tokens = Vec::new();

    while i < bytes.len() {
        let c = bytes[i];

        if c.is_ascii_whitespace() {
            i += 1;
            continue;
        }

        if c == b'+' {
            tokens.push(Token::Plus);
            i += 1;
            continue;
        }

        if c == b'-' {
            tokens.push(Token::Minus);
            i += 1;
            continue;
        }

        if c.is_ascii_digit() {
            let mut val: i64 = 0;
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                val = val * 10 + (bytes[i] - b'0') as i64;
                i += 1;
            }
            tokens.push(Token::Num(val));
            continue;
        }

        panic!("unexpected character: {}", c as char);
    }

    tokens.push(Token::Eof);
    tokens
}
