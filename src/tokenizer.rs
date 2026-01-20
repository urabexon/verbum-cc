#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Num(i64),
    Plus,
    Minus,
    Eof,
}

pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
        while self.pos < self.input.len() && self.input[self.pos].is_ascii_whitespace() {
            self.pos += 1;
        }

        if self.pos >= self.input.len() {
            return Token::Eof;
        }

        let c = self.input[self.pos];

        if c == b'+' {
            self.pos += 1;
            return Token::Plus;
        }
        if c == b'-' {
            self.pos += 1;
            return Token::Minus;
        }

        if c.is_ascii_digit() {
            let mut val: i64 = 0;
            while self.pos < self.input.len() && self.input[self.pos].is_ascii_digit() {
                val = val * 10 + (self.input[self.pos] - b'0') as i64;
                self.pos += 1;
            }
            return Token::Num(val);
        }

        panic!("unexpected character: {}", c as char);
    }
}
