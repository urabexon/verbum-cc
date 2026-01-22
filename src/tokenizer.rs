#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Num(i64),
    Ident(String),
    Plus,
    Minus,
    PlusPlus,   // ++
    MinusMinus, // --
    Star,
    Slash,
    Percent,  // %
    LParen,
    RParen,
    Eof,
    Eq,       // =
    EqEq,     // ==
    PlusEq,   // +=
    MinusEq,  // -=
    StarEq,   // *=
    SlashEq,  // /=
    PercentEq,// %=
    Ne,       // !=
    Lt,       // <
    Le,       // <=
    Gt,       // >
    Ge,       // >=
    Not,      // !
    Amp,      // & 
    Pipe,     // | (bit OR)
    Caret,    // ^ (bit XOR)
    Tilde,    // ~ (bit NOT)
    Shl,      // <<
    Shr,      // >>
    AndAnd,   // &&
    OrOr,     // ||
    If,
    Else,
    While,
    For,
    Do,
    Break,
    Continue,
    Fn,
    Return,
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

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            while self.pos < self.input.len() && self.input[self.pos].is_ascii_whitespace() {
                self.pos += 1;
            }

            if self.pos >= self.input.len() {
                return;
            }

            if self.pos + 1 < self.input.len() {
                if self.input[self.pos] == b'/' && self.input[self.pos + 1] == b'/' {
                    self.pos += 2;
                    while self.pos < self.input.len() && self.input[self.pos] != b'\n' {
                        self.pos += 1;
                    }
                    continue;
                }
                if self.input[self.pos] == b'/' && self.input[self.pos + 1] == b'*' {
                    self.pos += 2;
                    while self.pos + 1 < self.input.len() {
                        if self.input[self.pos] == b'*' && self.input[self.pos + 1] == b'/' {
                            self.pos += 2;
                            break;
                        }
                        self.pos += 1;
                    }
                    continue;
                }
            }

            return;
        }
    }

    fn read_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();

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
                return Token::Amp;
            }
            b'|' => {
                self.pos += 1;
                if self.pos < self.input.len() && self.input[self.pos] == b'|' {
                    self.pos += 1;
                    return Token::OrOr;
                }
                return Token::Pipe;
            }
            b'^' => {
                self.pos += 1;
                return Token::Caret;
            }
            b'~' => {
                self.pos += 1;
                return Token::Tilde;
            }
            b'<' => {
                self.pos += 1;
                if self.pos < self.input.len() {
                    if self.input[self.pos] == b'=' {
                        self.pos += 1;
                        return Token::Le;
                    }
                    if self.input[self.pos] == b'<' {
                        self.pos += 1;
                        return Token::Shl;
                    }
                }
                return Token::Lt;
            }
            b'>' => {
                self.pos += 1;
                if self.pos < self.input.len() {
                    if self.input[self.pos] == b'=' {
                        self.pos += 1;
                        return Token::Ge;
                    }
                    if self.input[self.pos] == b'>' {
                        self.pos += 1;
                        return Token::Shr;
                    }
                }
                return Token::Gt;
            }
            _ => {}
        }
        
        match c {
            b'+' => {
                self.pos += 1;
                if self.pos < self.input.len() {
                    if self.input[self.pos] == b'=' {
                        self.pos += 1;
                        return Token::PlusEq;
                    }
                    if self.input[self.pos] == b'+' {
                        self.pos += 1;
                        return Token::PlusPlus;
                    }
                }
                return Token::Plus;
            }
            b'-' => {
                self.pos += 1;
                if self.pos < self.input.len() {
                    if self.input[self.pos] == b'=' {
                        self.pos += 1;
                        return Token::MinusEq;
                    }
                    if self.input[self.pos] == b'-' {
                        self.pos += 1;
                        return Token::MinusMinus;
                    }
                }
                return Token::Minus;
            }
            b'*' => {
                self.pos += 1;
                if self.pos < self.input.len() && self.input[self.pos] == b'=' {
                    self.pos += 1;
                    return Token::StarEq;
                }
                return Token::Star;
            }
            b'/' => {
                self.pos += 1;
                if self.pos < self.input.len() && self.input[self.pos] == b'=' {
                    self.pos += 1;
                    return Token::SlashEq;
                }
                return Token::Slash;
            }
            b'%' => {
                self.pos += 1;
                if self.pos < self.input.len() && self.input[self.pos] == b'=' {
                    self.pos += 1;
                    return Token::PercentEq;
                }
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

        if c == b'\'' {
            self.pos += 1;
            if self.pos >= self.input.len() {
                panic!("unexpected end of input in character literal");
            }

            let ch = if self.input[self.pos] == b'\\' {
                self.pos += 1;
                if self.pos >= self.input.len() {
                    panic!("unexpected end of input in escape sequence");
                }
                let escaped = match self.input[self.pos] {
                    b'n' => b'\n',
                    b't' => b'\t',
                    b'r' => b'\r',
                    b'\\' => b'\\',
                    b'\'' => b'\'',
                    b'0' => 0,
                    c => panic!("unknown escape sequence: \\{}", c as char),
                };
                self.pos += 1;
                escaped
            } else {
                let ch = self.input[self.pos];
                self.pos += 1;
                ch
            };

            if self.pos >= self.input.len() || self.input[self.pos] != b'\'' {
                panic!("expected closing quote for character literal");
            }
            self.pos += 1;

            return Token::Num(ch as i64);
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
                "break" => Token::Break,
                "continue" => Token::Continue,
                "fn" => Token::Fn,
                "return" => Token::Return,
                _ => Token::Ident(s.to_string()),
            };
        }


        panic!("unexpected character: {}", c as char);
    }
}
