#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Num(i64),
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    Eof,
}

pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
    peeked: Option<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
            peeked: None,
        }
    }

    pub fn peek_token(&mut self) -> Token {
        if let Some(t) = &self.peeked {
            return t.clone();
        }
        let t = self.read_token();
        self.peeked = Some(t.clone());
        t
    }

    pub fn consume_token(&mut self) -> Token {
        if let Some(t) = self.peeked.take() {
            return t;
        }
        self.read_token()
    }

    fn read_token(&mut self) -> Token {
        while self.pos < self.input.len() && self.input[self.pos].is_ascii_whitespace() {
            self.pos += 1;
        }

        if self.pos >= self.input.len() {
            return Token::Eof;
        }

        let c = self.input[self.pos];

        match c {
            b'+' => {
                self.pos += 1;
                return Token::Plus;
            }
            b'-' => {
                self.pos += 1;
                return Token::Minus;
            }
            b'*' => {
                self.pos += 1;
                return Token::Star;
            }
            b'/' => {
                self.pos += 1;
                return Token::Slash;
            }
            b'(' => {
                self.pos += 1;
                return Token::LParen;
            }
            b')' => {
                self.pos += 1;
                return Token::RParen;
            }
            _ => {}
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
