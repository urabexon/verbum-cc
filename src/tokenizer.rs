#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Num(i64),
    Ident(String),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,  // %
    LParen,
    RParen,
    Eof,
    Eq,       // =
    EqEq,     // ==
    Ne,       // !=
    Lt,       // <
    Le,       // <=
    Gt,       // >
    Ge,       // >=
    Not,      // !
    AndAnd,   // &&
    OrOr,     // ||
    If,
    Else,
    While,
    For,
    Do,
    Fn,       // fn
    Return,   // return
    LBrace,   // {
    RBrace,   // }
    Semi,     // ;
    Comma,    // ,
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
            b'=' => {
                self.pos += 1;
                if self.pos < self.input.len() && self.input[self.pos] == b'=' {
                    self.pos += 1;
                    return Token::EqEq;
                }
                return Token::Eq;
            }
            b'!' => {
                self.pos += 1;
                if self.pos < self.input.len() && self.input[self.pos] == b'=' {
                    self.pos += 1;
                    return Token::Ne;
                }
                return Token::Not;
            }
            b'&' => {
                self.pos += 1;
                if self.pos < self.input.len() && self.input[self.pos] == b'&' {
                    self.pos += 1;
                    return Token::AndAnd;
                }
                panic!("unexpected token '&' (did you mean '&&')?");
            }
            b'|' => {
                self.pos += 1;
                if self.pos < self.input.len() && self.input[self.pos] == b'|' {
                    self.pos += 1;
                    return Token::OrOr;
                }
                panic!("unexpected token '|' (did you mean '||')?");
            }
            b'<' => {
                self.pos += 1;
                if self.pos < self.input.len() && self.input[self.pos] == b'=' {
                    self.pos += 1;
                    return Token::Le;
                }
                return Token::Lt;
            }
            b'>' => {
                self.pos += 1;
                if self.pos < self.input.len() && self.input[self.pos] == b'=' {
                    self.pos += 1;
                    return Token::Ge;
                }
                return Token::Gt;
            }
            _ => {}
        }
        
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
            b'%' => {
                self.pos += 1;
                return Token::Percent;
            }
            b'(' => {
                self.pos += 1;
                return Token::LParen;
            }
            b')' => {
                self.pos += 1;
                return Token::RParen;
            }
            b'{' => { 
                self.pos += 1; 
                return Token::LBrace;
            }
            b'}' => { 
                self.pos += 1;
                return Token::RBrace;
            }
            b';' => {
                self.pos += 1;
                return Token::Semi;
            }
            b',' => {
                self.pos += 1;
                return Token::Comma;
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

        if c.is_ascii_alphabetic() || c == b'_' {
            let start = self.pos;
            self.pos += 1;
            while self.pos < self.input.len() {
                let ch = self.input[self.pos];
                if ch.is_ascii_alphanumeric() || ch == b'_' {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            let s = std::str::from_utf8(&self.input[start..self.pos]).unwrap();
            return match s {
                "if" => Token::If,
                "else" => Token::Else,
                "while" => Token::While,
                "for" => Token::For,
                "do" => Token::Do,
                "fn" => Token::Fn,
                "return" => Token::Return,
                _ => Token::Ident(s.to_string()),
            };
        }


        panic!("unexpected character: {}", c as char);
    }
}
