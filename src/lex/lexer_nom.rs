use lex::token::{TokenType};
use nom;

pub struct Lexer {}
impl Lexer {
    pub fn lex(input: &str) -> Vec<TokenType> {
        let result = lex(input);
        let (_, tokens) = result.unwrap();
        tokens
    }
}

macro_rules! default_if (
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => ({
        let i_ = $i.clone();
        if $submac!(i_, $($args)*) {
            $submac2!(i_, $($args2)*)
        } else {
            nom::IResult::Error(error_position!(nom::ErrorKind::Custom(1), $i))
        }
    });
);

named!(lex<&str, Vec<TokenType>>, alt!(
    ws!(default_if!(call!(|s: &str| { s.is_empty() }), value!(vec![]))) |
    ws!(many0!(lex_single_token))
));

named!(lex_single_token<&str, TokenType>, alt!(
    lex_punctuation |
    lex_operators |
    lex_keywords |
    lex_identifier |
    // lex_number_literal |
    lex_illegal
));

named!(lex_punctuation<&str, TokenType>, alt!(
    do_parse!(tag!(",") >> (TokenType::Comma)) |
    do_parse!(tag!(";") >> (TokenType::Semicolon)) |
    do_parse!(tag!("(") >> (TokenType::LParen)) |
    do_parse!(tag!(")") >> (TokenType::RParen)) |
    do_parse!(tag!("{") >> (TokenType::LBrace)) |
    do_parse!(tag!("}") >> (TokenType::RBrace)) |
    do_parse!(tag!("[") >> (TokenType::LBracket)) |
    do_parse!(tag!("]") >> (TokenType::RBracket))
));

named!(lex_operators<&str, TokenType>, alt!(
    do_parse!(tag!("==") >> (TokenType::DoubleEqual)) |
    do_parse!(tag!("=")  >> (TokenType::Equal)) |
    do_parse!(tag!("!=") >> (TokenType::NotEqual)) |
    do_parse!(tag!("!")  >> (TokenType::Not)) |
    do_parse!(tag!("<=") >> (TokenType::LessThanEqual)) |
    do_parse!(tag!("<")  >> (TokenType::LessThan)) |
    do_parse!(tag!(">=") >> (TokenType::GreaterThanEqual)) |
    do_parse!(tag!(">")  >> (TokenType::GreaterThan)) |

    do_parse!(tag!("+=") >> (TokenType::PlusEqual)) |
    do_parse!(tag!("+")  >> (TokenType::Plus)) |
    do_parse!(tag!("-=") >> (TokenType::MinusEqual)) |
    do_parse!(tag!("-")  >> (TokenType::Minus)) |
    do_parse!(tag!("*=") >> (TokenType::AsteriskEqual)) |
    do_parse!(tag!("*")  >> (TokenType::Asterisk)) |
    do_parse!(tag!("/=") >> (TokenType::SlashEqual)) |
    do_parse!(tag!("/")  >> (TokenType::Slash)) |
    do_parse!(tag!("^=") >> (TokenType::CaretEqual)) |
    do_parse!(tag!("^")  >> (TokenType::Caret))
));

named!(lex_keywords<&str, TokenType>, alt!(
    do_parse!(tag!("let")       >> (TokenType::Let)) |
    do_parse!(tag!("fn")        >> (TokenType::Function)) |
    do_parse!(tag!("if")        >> (TokenType::If)) |
    do_parse!(tag!("else")      >> (TokenType::Else)) |
    do_parse!(tag!("return")    >> (TokenType::Return)) |
    do_parse!(tag!("true")      >> (TokenType::True)) |
    do_parse!(tag!("false")     >> (TokenType::False))
));

named!(lex_identifier<&str, TokenType>, do_parse!(
    init: verify!(take_s!(1), |s: &str| { s.chars().all(|c| c.is_xid_start()) }) >>
    res: take_while_s!(call!(|c: char| c.is_xid_continue())) >>
    (TokenType::Identifier([init, res].concat()))
));

named!(lex_illegal<&str, TokenType>, do_parse!(take_s!(1) >> (TokenType::IllegalChar)));

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_input_matches(input: &str, expected: &[TokenType]) {
        let tokens = Lexer::lex(input);
        println!("{:?}", tokens);
        assert_eq!(tokens.len(), expected.len());
        for (parsed_tok, expected_tok) in tokens.iter().zip(expected.iter()) {
            assert_eq!(parsed_tok, expected_tok);
        }
    }

    #[test]
    fn test_lex_illegal() {
        let input = "☕";
        let expected = [TokenType::IllegalChar];
        assert_input_matches(input, &expected);
    }

    #[test]
    fn test_lex_punctuation() {
        let input = "[](){},;";
        let expected = [
            TokenType::LBracket,
            TokenType::RBracket,
            TokenType::LParen,
            TokenType::RParen,
            TokenType::LBrace,
            TokenType::RBrace,
            TokenType::Comma,
            TokenType::Semicolon,
        ];
        assert_input_matches(input, &expected);
    }

    #[test]
    fn test_lex_operators() {
        let input = "= == ! != < <= > >= + += - -= * *= / /= ^ ^=";
        let expected = [
            TokenType::Equal,
            TokenType::DoubleEqual,
            TokenType::Not,
            TokenType::NotEqual,
            TokenType::LessThan,
            TokenType::LessThanEqual,
            TokenType::GreaterThan,
            TokenType::GreaterThanEqual,
            TokenType::Plus,
            TokenType::PlusEqual,
            TokenType::Minus,
            TokenType::MinusEqual,
            TokenType::Asterisk,
            TokenType::AsteriskEqual,
            TokenType::Slash,
            TokenType::SlashEqual,
            TokenType::Caret,
            TokenType::CaretEqual,
        ];
        assert_input_matches(input, &expected);
    }

    #[test]
    fn test_lex_keywords() {
        let input = "let fn if else return true false";
        let expected = [
            TokenType::Let,
            TokenType::Function,
            TokenType::If,
            TokenType::Else,
            TokenType::Return,
            TokenType::True,
            TokenType::False,
        ];
        assert_input_matches(input, &expected);
    }

    #[test]
    fn test_lex_identifiers() {
        let input = "foo bar baz ª··9ℼ     ℝℨℤℤↈ９ ９uise";
        let expected = [
            TokenType::Identifier("foo".to_string()),
            TokenType::Identifier("bar".to_string()),
            TokenType::Identifier("baz".to_string()),
            TokenType::Identifier("ª··9ℼ".to_string()), // ª is XID_Start, rest are XID_Continue
            TokenType::Identifier("ℝℨℤℤↈ９".to_string()), // ℝ is XID_Start, rest are XID_Continue
            TokenType::IllegalChar, // ９ is XID_Continue but not XID_Start
            TokenType::Identifier("uise".to_string()) // u is XID_Start, rest are XID_Continue
        ];
        assert_input_matches(input, &expected);
    }

    #[test]
    fn test_foo() {
        let input = "\t";
        let tokens = Lexer::lex(input);
        assert_eq!(tokens.len(), 0);
    }
}
