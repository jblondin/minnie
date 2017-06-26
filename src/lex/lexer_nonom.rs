use lex::{Position, Token, TokenType};

pub struct Lexer {
    chars: Vec<char>,
    // read_pos: Position,
    file_pos: Position,
    index: usize,
    read_index: usize,
    ch: char,
}

impl Lexer {
    fn new(input: String, file_pos: Position, index: usize) -> Lexer {
        let mut lexer = Lexer {
            chars: input.chars().collect(),
            file_pos: file_pos,
            index: index,
            read_index: index,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }
    pub fn from_str(input: &str) -> Lexer {
        Lexer::new(input.to_string(), Position::start(), 0)
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.eat_whitespace();

        let start_file_pos = self.file_pos;
        let tok = match self.ch {

            '='  => self.check_twochar_equal(TokenType::Equal, TokenType::DoubleEqual),
            '!'  => self.check_twochar_equal(TokenType::Not, TokenType::NotEqual),
            '<'  => self.check_twochar_equal(TokenType::LessThan, TokenType::LessThanEqual),
            '>'  => self.check_twochar_equal(TokenType::GreaterThan, TokenType::GreaterThanEqual),

            '+'  => self.check_twochar_equal(TokenType::Plus, TokenType::PlusEqual),
            '-'  => self.check_twochar_equal(TokenType::Minus, TokenType::MinusEqual),
            '*'  => self.check_twochar_equal(TokenType::Asterisk, TokenType::AsteriskEqual),
            '/'  => self.check_twochar_equal(TokenType::Slash, TokenType::SlashEqual),
            '^'  => self.check_twochar_equal(TokenType::Caret, TokenType::CaretEqual),

            ';'  => TokenType::Semicolon,
            ','  => TokenType::Comma,

            '('  => TokenType::LParen,
            ')'  => TokenType::RParen,
            '{'  => TokenType::LBrace,
            '}'  => TokenType::RBrace,
            '['  => TokenType::LBracket,
            ']'  => TokenType::RBracket,

            '\0' => TokenType::Eof,

            oth  => {
                if oth.is_xid_start() { // identifier start
                    return Some(self.read_identifier().into_token(start_file_pos));
                } else if oth.is_digit(10) { // number literal start
                    return Some(self.read_number_literal().into_token(start_file_pos));
                } else {
                    TokenType::Illegal(oth.to_string())
                }
            },
        }.into_token(start_file_pos);
        self.read_char();
        if tok.is_type(&TokenType::Eof) {
            None
        } else {
            Some(tok)
        }
    }

    fn eat_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            if self.ch == '\n' {
                self.file_pos.line += 1;
                self.file_pos.character = 0;
            }
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> TokenType {
        let mut s = String::new();
        assert!(self.ch.is_xid_start());
        s.push(self.ch);
        self.read_char();
        while self.ch.is_xid_continue() {
            s.push(self.ch);
            self.read_char();
        }
        if let Some(keyword) = TokenType::keyword(&s) {
            return keyword.clone();
        }
        TokenType::Identifier(s)
    }

    fn read_number_literal(&mut self) -> TokenType {
        let mut s = String::new();
        assert!(self.ch.is_digit(10));
        s.push(self.ch);
        let orig_ch = self.ch;
        self.read_char();

        let base = if orig_ch == '0' {
            match self.ch {
                'x' => { s.push(self.ch); self.read_char(); 16 },
                'o' => { s.push(self.ch); self.read_char(); 8 },
                'b' => { s.push(self.ch); self.read_char(); 2 },
                _   => { 10 },
            }
        } else { 10 };

        while self.ch.is_digit(base) || self.ch == '_' {
            s.push(self.ch);
            self.read_char();
        }
        TokenType::NumberLiteral(s)
    }

    fn read_char(&mut self) {
        if self.read_index >= self.chars.len() {
            self.ch = '\0';
        } else {
            self.ch = self.chars[self.read_index];
            self.index = self.read_index;
            self.file_pos.character += 1;
            self.read_index += 1;
        }
    }

    fn peek_char(&self) -> char {
        if self.read_index >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.read_index]
        }
    }

    fn check_twochar_equal(&mut self, tt_default: TokenType, tt_if_equal: TokenType)
            -> TokenType {
        match self.peek_char() {
            '=' => {
                self.read_char();
                tt_if_equal
            },
            _ => tt_default
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn print_tokens(input: &str) {
        let mut lexer = Lexer::from_str(input);

        while let Some(token) = lexer.next_token() {
            println!("{:?}", token);
        }
    }
    fn assert_input_matches(input: &str, expected: &[Token]) {
        let mut lexer = Lexer::from_str(input);

        let mut i = 0;
        while let Some(token) = lexer.next_token() {
            assert_eq!(token, expected[i]);
            i += 1;
        }
    }

    #[test]
    fn test_next_token_simple() {
        let input = "=+(){},;";
        let expected = [
            TokenType::Equal.into_token(Position::new(1,1)),
            TokenType::Plus.into_token(Position::new(1,2)),
            TokenType::LParen.into_token(Position::new(1,3)),
            TokenType::RParen.into_token(Position::new(1,4)),
            TokenType::LBrace.into_token(Position::new(1,5)),
            TokenType::RBrace.into_token(Position::new(1,6)),
            TokenType::Comma.into_token(Position::new(1,7)),
            TokenType::Semicolon.into_token(Position::new(1,8)),
        ];
        assert_input_matches(input, &expected);
    }

    #[test]
    fn test_next_token_operators_single() {
        let input = "= + / - * ^ ! < >";
        let expected = [
            TokenType::Equal.into_token(Position::new(1, 1)),
            TokenType::Plus.into_token(Position::new(1, 3)),
            TokenType::Slash.into_token(Position::new(1, 5)),
            TokenType::Minus.into_token(Position::new(1, 7)),
            TokenType::Asterisk.into_token(Position::new(1, 9)),
            TokenType::Caret.into_token(Position::new(1, 11)),
            TokenType::Not.into_token(Position::new(1, 13)),
            TokenType::LessThan.into_token(Position::new(1, 15)),
            TokenType::GreaterThan.into_token(Position::new(1, 17)),
        ];
        assert_input_matches(input, &expected);
    }

    #[test]
    fn test_next_token_operators_double() {
        let input = "== != <= >=";
        let expected = [
            TokenType::DoubleEqual.into_token(Position::new(1, 1)),
            TokenType::NotEqual.into_token(Position::new(1, 4)),
            TokenType::LessThanEqual.into_token(Position::new(1, 7)),
            TokenType::GreaterThanEqual.into_token(Position::new(1, 10)),
        ];
        print_tokens(input);
        assert_input_matches(input, &expected);
    }
    #[test]
    fn test_next_token_keywords() {
        let input = "fn let if else return true false";
        let expected = [
            TokenType::Function.into_token(Position::new(1, 1)),
            TokenType::Let.into_token(Position::new(1, 4)),
            TokenType::If.into_token(Position::new(1, 8)),
            TokenType::Else.into_token(Position::new(1, 11)),
            TokenType::Return.into_token(Position::new(1, 16)),
            TokenType::True.into_token(Position::new(1, 23)),
            TokenType::False.into_token(Position::new(1, 28)),
        ];
        assert_input_matches(input, &expected)
    }

    #[test]
    fn test_next_token_multiline() {
        let input = "
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
        ";
        let expected = [
            TokenType::Let.into_token(Position::new(2, 1)),
            TokenType::Identifier("five".to_string()).into_token(Position::new(2, 5)),
            TokenType::Equal.into_token(Position::new(2, 10)),
            TokenType::NumberLiteral("5".to_string()).into_token(Position::new(2, 12)),
            TokenType::Semicolon.into_token(Position::new(2, 13)),
            TokenType::Let.into_token(Position::new(3, 1)),
            TokenType::Identifier("ten".to_string()).into_token(Position::new(3, 5)),
            TokenType::Equal.into_token(Position::new(3, 9)),
            TokenType::NumberLiteral("10".to_string()).into_token(Position::new(3, 11)),
            TokenType::Semicolon.into_token(Position::new(3, 13)),
            TokenType::Let.into_token(Position::new(4, 1)),
            TokenType::Identifier("add".to_string()).into_token(Position::new(4, 5)),
            TokenType::Equal.into_token(Position::new(4, 9)),
            TokenType::Function.into_token(Position::new(4, 11)),
            TokenType::LParen.into_token(Position::new(4, 13)),
            TokenType::Identifier("x".to_string()).into_token(Position::new(4, 14)),
            TokenType::Comma.into_token(Position::new(4, 15)),
            TokenType::Identifier("y".to_string()).into_token(Position::new(4, 17)),
            TokenType::RParen.into_token(Position::new(4, 18)),
            TokenType::LBrace.into_token(Position::new(4, 20)),
            TokenType::Identifier("x".to_string()).into_token(Position::new(5, 5)),
            TokenType::Plus.into_token(Position::new(5, 7)),
            TokenType::Identifier("y".to_string()).into_token(Position::new(5, 9)),
            TokenType::Semicolon.into_token(Position::new(5, 10)),
            TokenType::RBrace.into_token(Position::new(6, 1)),
            TokenType::Semicolon.into_token(Position::new(6, 2)),
            TokenType::Let.into_token(Position::new(7, 1)),
            TokenType::Identifier("result".to_string()).into_token(Position::new(7, 5)),
            TokenType::Equal.into_token(Position::new(7, 12)),
            TokenType::Identifier("add".to_string()).into_token(Position::new(7, 14)),
            TokenType::LParen.into_token(Position::new(7, 17)),
            TokenType::Identifier("five".to_string()).into_token(Position::new(7, 18)),
            TokenType::Comma.into_token(Position::new(7, 22)),
            TokenType::Identifier("ten".to_string()).into_token(Position::new(7, 24)),
            TokenType::RParen.into_token(Position::new(7, 27)),
            TokenType::Semicolon.into_token(Position::new(7, 28)),
        ];

        assert_input_matches(input, &expected);
    }

}

