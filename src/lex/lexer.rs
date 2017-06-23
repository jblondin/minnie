use std::num::ParseIntError;

use nom;

use errors::*;

use lex::token::{Token, TokenType, Literal, NumberLiteral};
use lex::span::Span;

pub struct Lexer {}
impl Lexer {
    pub fn lex(input: &str) -> Result<Vec<Token>> {
        let result = lex(Span::start(input));
        match result {
            nom::IResult::Done(_, tokens) => Ok(tokens),
            nom::IResult::Incomplete(needed) => {
                let num_needed = match needed {
                    nom::Needed::Unknown => "".to_string(),
                    nom::Needed::Size(cnt) => format!("{} ", cnt),
                };
                Err(ErrorKind::LexerNoSpan(format!(
                    "Incomplete lex: {}more characters expected", num_needed)).into())
            },
            nom::IResult::Error(err) => Err(ErrorKind::LexerNoSpan(
                format!("Lex failed: {}", err)).into()),
        }.and_then(|tokens| {
            match tokens.iter().find(|&token| token.ty == TokenType::Illegal).cloned() {
                Some(token) => {
                    Err(ErrorKind::LexerToken(format!("Illegal character '{}'",
                        token.span.as_slice()), token).into())
                },
                None => Ok(tokens)
            }
        })
    }
}

enum CustomNomError {
    NoDefault,
    IntegerParse,
    Unimplemented
}

macro_rules! default_if (
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => ({
        let i_ = $i.clone();
        if $submac!(i_, $($args)*) {
            $submac2!(i_, $($args2)*)
        } else {
            nom::IResult::Error(error_position!(nom::ErrorKind::Custom(
                CustomNomError::NoDefault as u32), $i))
        }
    });
);

named!(lex<Span, Vec<Token>>, alt!(
    ws!(default_if!(call!(|s: Span| { s.is_empty() }), value!(vec![]))) |
    ws!(many0!(lex_single_token))
));

named!(lex_single_token<Span, Token>, alt_complete!(
    lex_punctuation |
    lex_operators |
    lex_keywords |
    lex_identifier |
    lex_literal |
    lex_illegal
));

macro_rules! tag_span {
    ($i:expr, $tag:expr) => ({
        tag!($i, Span::new($tag, 0, 0, 0))
    });
}

named!(lex_punctuation<Span, Token>, alt!(
    do_parse!(span: tag_span!(",") >> (Token::new(TokenType::Comma, span))) |
    do_parse!(span: tag_span!(";") >> (Token::new(TokenType::Semicolon, span))) |
    do_parse!(span: tag_span!("(") >> (Token::new(TokenType::LParen, span))) |
    do_parse!(span: tag_span!(")") >> (Token::new(TokenType::RParen, span))) |
    do_parse!(span: tag_span!("{") >> (Token::new(TokenType::LBrace, span))) |
    do_parse!(span: tag_span!("}") >> (Token::new(TokenType::RBrace, span))) |
    do_parse!(span: tag_span!("[") >> (Token::new(TokenType::LBracket, span))) |
    do_parse!(span: tag_span!("]") >> (Token::new(TokenType::RBracket, span)))
));

named!(lex_operators<Span, Token>, alt!(
    do_parse!(span: tag_span!("==") >> (Token::new(TokenType::DoubleEqual, span))) |
    do_parse!(span: tag_span!("=")  >> (Token::new(TokenType::Equal, span))) |
    do_parse!(span: tag_span!("!=") >> (Token::new(TokenType::NotEqual, span))) |
    do_parse!(span: tag_span!("!")  >> (Token::new(TokenType::Not, span))) |
    do_parse!(span: tag_span!("<=") >> (Token::new(TokenType::LessThanEqual, span))) |
    do_parse!(span: tag_span!("<")  >> (Token::new(TokenType::LessThan, span))) |
    do_parse!(span: tag_span!(">=") >> (Token::new(TokenType::GreaterThanEqual, span))) |
    do_parse!(span: tag_span!(">")  >> (Token::new(TokenType::GreaterThan, span))) |

    do_parse!(span: tag_span!("+=") >> (Token::new(TokenType::PlusEqual, span))) |
    do_parse!(span: tag_span!("+")  >> (Token::new(TokenType::Plus, span))) |
    do_parse!(span: tag_span!("-=") >> (Token::new(TokenType::MinusEqual, span))) |
    do_parse!(span: tag_span!("-")  >> (Token::new(TokenType::Minus, span))) |
    do_parse!(span: tag_span!("*=") >> (Token::new(TokenType::AsteriskEqual, span))) |
    do_parse!(span: tag_span!("*")  >> (Token::new(TokenType::Asterisk, span))) |
    do_parse!(span: tag_span!("/=") >> (Token::new(TokenType::SlashEqual, span))) |
    do_parse!(span: tag_span!("/")  >> (Token::new(TokenType::Slash, span))) |
    do_parse!(span: tag_span!("^=") >> (Token::new(TokenType::CaretEqual, span))) |
    do_parse!(span: tag_span!("^")  >> (Token::new(TokenType::Caret, span)))
));

named!(lex_keywords<Span, Token>, alt!(
    do_parse!(span: tag_span!("let")       >> (Token::new(TokenType::Let, span))) |
    do_parse!(span: tag_span!("fn")        >> (Token::new(TokenType::Function, span))) |
    do_parse!(span: tag_span!("if")        >> (Token::new(TokenType::If, span))) |
    do_parse!(span: tag_span!("else")      >> (Token::new(TokenType::Else, span))) |
    do_parse!(span: tag_span!("return")    >> (Token::new(TokenType::Return, span))) |
    do_parse!(span: tag_span!("true")      >> (Token::new(TokenType::True, span))) |
    do_parse!(span: tag_span!("false")     >> (Token::new(TokenType::False, span)))
));

macro_rules! take_while_first_rest {
    ($i:expr, $prefix_cnt:expr, $firstmac:ident!( $($firstargs:tt)* ),
            $restmac:ident!( $($restargs:tt)* ), $tycrmac:ident!( $($tycrargs:tt)* )) => ({

        use nom::{InputLength, InputIter, Slice};
        let input = $i;

        match input.slice_index($prefix_cnt) {
            None => nom::IResult::Incomplete(nom::Needed::Size($prefix_cnt)),
            Some(init_index) => {
                if $firstmac!(input.slice(..init_index).as_slice(), $($firstargs)*) {
                    match input.slice(init_index..).position(|c| !$restmac!(c, $($restargs)*)) {
                        Some(index) => {
                            let index = index + init_index;
                            let Span { offset, line, column, .. } = input;
                            match $tycrmac!(input.slice(..index).as_slice(), $($tycrargs)*) {
                                Ok(ty) => nom::IResult::Done(input.slice(index..),
                                    Token::new(ty, Span::new(input.slice(..index).as_slice(),
                                        offset, line, column))),
                                Err(_) => nom::IResult::Error(error_position!(
                                    nom::ErrorKind::Custom(
                                        CustomNomError::IntegerParse as u32), input))
                            }

                        },
                        None => {
                            match $tycrmac!(input.slice(..input.input_len()).as_slice(),
                                    $($tycrargs)*) {
                                Ok(ty) => nom::IResult::Done(input.slice(input.input_len()..),
                                    Token::new(ty, input)),
                                Err(_) => nom::IResult::Error(error_position!(
                                    nom::ErrorKind::Custom(
                                        CustomNomError::IntegerParse as u32), input))
                            }
                        }
                    }
                } else {
                    nom::IResult::Error(error_position!(nom::ErrorKind::Verify, input))
                }
            }
        }
    });
    ($i:expr, $prefix_cnt:expr, $firstexp:expr, $restmac:ident!( $($restargs:tt)* ),
            $tycrmac:ident!( $($tycrargs:tt)* )) => (
        take_while_first_rest!($i, $prefix_cnt, call!($firstexp), $restmac!($($restargs)*),
            $tycrmac!($($tycrargs)*));
    );
    ($i:expr, $prefix_cnt:expr, $firstmac:ident!( $($firstargs:tt)* ), $restexp:expr,
            $tycrmac:ident!( $($tycrargs:tt)* )) => (
        take_while_first_rest!($i, $prefix_cnt, $firstmac!($($firstargs)*), call!($restexp),
            $tycrmac!($($tycrargs)*));
    );
    ($i:expr, $prefix_cnt:expr, $firstmac:ident!( $($firstargs:tt)* ),
            $restmac:ident!( $($restargs:tt)* ), $tycrexp:expr) => (
        take_while_first_rest!($i, $prefix_cnt, $firstmac!($($firstargs)*),
            $restmac!($($restargs)*), call!($tycrexp));
    );
    (i:expr, $prefix_cnt:expr, $firstexp:expr, $restexp:expr,
            $tycrmac:ident!( $($tycrargs:tt)* )) => (
        take_while_first_rest!($i, $prefix_cnt, call!($firstexp), call!($restexp),
            $tycrmac!($($tycrargs)*));
    );
    (i:expr, $prefix_cnt:expr, $firstexp:expr, $restmac:ident!( $($restargs:tt)* ),
            $tycrexp:expr) => (
        take_while_first_rest!($i, $prefix_cnt, call!($firstexp), $restmac!($($restargs)*),
            call!($tycrexp));
    );
    (i:expr, $prefix_cnt:expr, $firstmac:ident!( $($firstargs:tt)* ), $restexp:expr,
            $tycrexp:expr) => (
        take_while_first_rest!($i, $prefix_cnt, call!($firstexp), call!($restexp),
            $tycrmac!($($tycrargs)*));
    );
    ($i:expr, $prefix_cnt:expr, $firstexp:expr, $restexp:expr, $tycrexp:expr) => (
        take_while_first_rest!($i, $prefix_cnt, call!($firstexp), call!($restexp), call!($tycrexp));
    );
}

named!(lex_identifier<Span, Token>, take_while_first_rest!(
    1,
    call!(|s: &str| s.chars().all(|c| c.is_xid_start())),
    call!(|c: char| c.is_xid_continue()),
    call!(|s: &str| -> Result<TokenType> { Ok(TokenType::Identifier(s.to_string())) })
));

named!(lex_literal<Span, Token>, alt!(
    lex_number_literal |
    lex_bool_literal |
    lex_string_literal
));

named!(lex_number_literal<Span, Token>, alt!(
    lex_integer_literal |
    lex_float_literal
));

named!(lex_integer_literal<Span, Token>, alt!(
    lex_hex_literal |
    lex_oct_literal |
    lex_bin_literal |
    lex_dec_literal
));

#[inline]
fn parse_int(s: &str, radix: u32, start_at: usize) -> ::std::result::Result<i64, ParseIntError> {
    let mut ret = String::new();
    let chars: Vec<char> = s.chars().filter(|&c: &char| c != '_').collect();
    for c in chars { ret.push(c); }
    i64::from_str_radix(&(ret.as_ref() as &str)[start_at..], radix)
}
named!(lex_hex_literal<Span, Token>, take_while_first_rest!(
    2,
    call!(|s: &str| s == "0x"),
    call!(|c: char| c.is_digit(16) || c == '_'),
    call!(|s: &str| parse_int(s, 16, 2).map(|i| TokenType::Literal(Literal::Number(
        NumberLiteral::Int(i)))))
));

named!(lex_oct_literal<Span, Token>, take_while_first_rest!(
    2,
    call!(|s: &str| s == "0o"),
    call!(|c: char| c.is_digit(8) || c == '_'),
    call!(|s: &str| parse_int(s, 8, 2).map(|i| TokenType::Literal(Literal::Number(
        NumberLiteral::Int(i)))))
));

named!(lex_bin_literal<Span, Token>, take_while_first_rest!(
    2,
    call!(|s: &str| s == "0b"),
    call!(|c: char| c.is_digit(2) || c == '_'),
    call!(|s: &str| parse_int(s, 2, 2).map(|i| TokenType::Literal(Literal::Number(
        NumberLiteral::Int(i)))))
));

named!(lex_dec_literal<Span, Token>, take_while_first_rest!(
    1,
    call!(|s: &str| s.chars().all(|c| c.is_digit(10))),
    call!(|c: char| c.is_digit(10) || c == '_'),
    call!(|s: &str| parse_int(s, 10, 0).map(|i| TokenType::Literal(Literal::Number(
        NumberLiteral::Int(i)))))
));

named!(lex_float_literal<Span, Token>,
    call!(|i| nom::IResult::Error(error_position!(
        nom::ErrorKind::Custom(CustomNomError::Unimplemented as u32), i))));

named!(lex_bool_literal<Span, Token>,
    call!(|i| nom::IResult::Error(error_position!(
        nom::ErrorKind::Custom(CustomNomError::Unimplemented as u32), i))));

named!(lex_string_literal<Span, Token>,
    call!(|i| nom::IResult::Error(error_position!(
        nom::ErrorKind::Custom(CustomNomError::Unimplemented as u32), i))));

named!(lex_illegal<Span, Token>, do_parse!(
    span: take!(1) >> (Token::new(TokenType::Illegal, span))));

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use super::*;

    fn assert_match<T, F>(input: &str, expected: &[T], f: F) where F: Fn(&Token, &T) -> bool,
            T: Debug {
        let tokens = Lexer::lex(input).unwrap();
        println!("{:?}", tokens);
        assert_eq!(tokens.len(), expected.len());
        for (parsed_tok, expected_tok) in tokens.iter().zip(expected.iter()) {
            if !f(parsed_tok, expected_tok) {
                panic!("parsed token {:?} does not match expected token {:?}",
                    parsed_tok, expected_tok);
            }
        }
    }
    fn assert_tokens_match(input: &str, expected: &[Token]) {
        assert_match(input, expected, |parsed, expctd| { parsed.eq(expctd) });
    }
    fn assert_tokentypes_match(input: &str, expected: &[TokenType]) {
        assert_match(input, expected, |parsed, expctd| { parsed.is_type(expctd) });
    }
    fn assert_illegal_token(input: &str, expected_span: Span) {
        match Lexer::lex(input).unwrap_err() {
            ErrorKind::LexerToken(_, token) => {
                assert!(token.is_type(&TokenType::Illegal));
                assert_eq!(token.span, expected_span);
            },
            e => {
                panic!("Expected positional lexer error, found: {}", e);
            }
        }
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
        assert_tokentypes_match(input, &expected);
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
        assert_tokentypes_match(input, &expected);
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
        assert_tokentypes_match(input, &expected);
    }

    #[test]
    fn test_lex_identifiers() {
        let input = "foo bar baz ª··9ℼ     ℝℨℤℤↈ９";
        let expected = [
            TokenType::Identifier("foo".to_string()),
            TokenType::Identifier("bar".to_string()),
            TokenType::Identifier("baz".to_string()),
            TokenType::Identifier("ª··9ℼ".to_string()), // ª is XID_Start, rest are XID_Continue
            TokenType::Identifier("ℝℨℤℤↈ９".to_string()), // ℝ is XID_Start, rest are XID_Continue
        ];
        assert_tokentypes_match(input, &expected);

        let input = " ９uise"; // ９ is XID_Continue but not XID_Start
        assert_illegal_token(input, Span::new("９", 1, 1, 2));
    }

    #[test]
    fn test_lex_dec_literal() {
        let input = "1502 0000 052 1_234_567 1_2_3";
        let expected = [
            TokenType::Literal(Literal::Number(NumberLiteral::Int(1502))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int(0))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int(52))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int(1234567))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int(1_2_3))),
        ];
        assert_tokentypes_match(input, &expected);
    }

    #[test]
    fn test_lex_hex_literal() {
        let input = "0x1D02 0x000 0x052 0x1_234 0x1_C_3 0xDEADBEEF";
        const P16: [i64; 8] = [1, 16, 256, 4096, 65536, 1048576, 16777216, 268435456];
        let expected = [
            TokenType::Literal(Literal::Number(NumberLiteral::Int(P16[3] + 13*P16[2] + 2))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int(0))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int(5*16 + 2))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int(P16[3] + 2*P16[2] + 3*16 + 4))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int(P16[2] + 12*16 + 3))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int(13*P16[7] + 14*P16[6] + 10*P16[5]
                + 13*P16[4] + 11*P16[3] + 14*P16[2] + 14*P16[1] + 15*P16[0])))
        ];
        assert_tokentypes_match(input, &expected);
    }

    #[test]
    fn test_lex_oct_literal() {
        let input = "0o1502 0o000 0o052 0o1_234 0o1_2_3";
        const P8: [i64; 4] = [1, 8, 64, 512];
        let expected = [
            TokenType::Literal(Literal::Number(NumberLiteral::Int(1*P8[3] + 5*P8[2] + 2))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int(0))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int(5*8 + 2))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int(P8[3] + 2*P8[2] + 3*P8[1] + 4))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int(P8[2] + 2*
                P8[1] + 3)))
        ];
        assert_tokentypes_match(input, &expected);
    }

    #[test]
    fn test_lex_bin_literal() {
        let input = "0b101 0b001001 0b10_10";
        let expected = [
            TokenType::Literal(Literal::Number(NumberLiteral::Int((1 << 2) + 1))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int((1 << 3) + 1))),
            TokenType::Literal(Literal::Number(NumberLiteral::Int((1 << 3) + (1 << 1)))),
        ];
        assert_tokentypes_match(input, &expected);
    }

    #[test]
    fn test_lex_illegal() {
        let input = "\n ☕";
        assert_illegal_token(input, Span::new("☕", 2, 2, 2));
    }

    #[test]
    fn test_all_whitespace() {
        let input = "\t  \n";
        let tokens = Lexer::lex(input).unwrap();
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn test_multiline() {
        let input = "
let add = fn(x, y) {
    x + y;
};
        ";
        let expected = [
            Token::new(TokenType::Let, Span::new("let", 1, 2, 1)),
            Token::new(TokenType::Identifier("add".to_string()), Span::new("add", 5, 2, 5)),
            Token::new(TokenType::Equal, Span::new("=", 9, 2, 9)),
            Token::new(TokenType::Function, Span::new("fn", 11, 2, 11)),
            Token::new(TokenType::LParen, Span::new("(", 13, 2, 13)),
            Token::new(TokenType::Identifier("x".to_string()), Span::new("x", 14, 2, 14)),
            Token::new(TokenType::Comma, Span::new(",", 15, 2, 15)),
            Token::new(TokenType::Identifier("y".to_string()), Span::new("y", 17, 2, 17)),
            Token::new(TokenType::RParen, Span::new(")", 18, 2, 18)),
            Token::new(TokenType::LBrace, Span::new("{", 20, 2, 20)),
            Token::new(TokenType::Identifier("x".to_string()), Span::new("x", 26, 3, 5)),
            Token::new(TokenType::Plus, Span::new("+", 28, 3, 7)),
            Token::new(TokenType::Identifier("y".to_string()), Span::new("y", 30, 3, 9)),
            Token::new(TokenType::Semicolon, Span::new(";", 31, 3, 10)),
            Token::new(TokenType::RBrace, Span::new("}", 33, 4, 1)),
            Token::new(TokenType::Semicolon, Span::new(";", 34, 4, 2)),
        ];

        assert_tokens_match(input, &expected);
    }

}
