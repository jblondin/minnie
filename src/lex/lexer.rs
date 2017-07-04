use std::str::FromStr;

use nom::{self, IResult};
use unicode_xid::UnicodeXID;

use errors::*;

use CustomNomError;
use nom_util::stringify_number;
use lex::token::{SpToken, Token};
use lex::span::Span;
use lex::escape::escape;

pub struct Lexer {}
impl Lexer {
    pub fn lex(input: &str) -> Result<Vec<SpToken>> {
        match lex(Span::start(input)) {
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
            match tokens.iter().find(|&token| token.token == Token::Illegal).cloned() {
                Some(token) => {
                    Err(ErrorKind::LexerToken(format!("Illegal character '{}'",
                        token.span.as_slice()), token).into())
                },
                None => Ok(tokens)
            }
        })
    }
}

named!(lex<Span, Vec<SpToken>>, alt!(
    ws!(default_if!(call!(|s: Span| { s.is_empty() }), value!(vec![]))) |
    ws!(many0!(lex_single_token))
));

named!(lex_single_token<Span, SpToken>, alt_complete!(
    lex_punctuation |
    lex_operators |
    lex_keywords |
    lex_literal |
    lex_identifier |
    lex_illegal
));

named!(lex_punctuation<Span, SpToken>, alt!(
    do_parse!(span: tag_span!(",") >> (SpToken::new(Token::Comma, span))) |
    do_parse!(span: tag_span!(";") >> (SpToken::new(Token::Semicolon, span))) |
    do_parse!(span: tag_span!("(") >> (SpToken::new(Token::LParen, span))) |
    do_parse!(span: tag_span!(")") >> (SpToken::new(Token::RParen, span))) |
    do_parse!(span: tag_span!("{") >> (SpToken::new(Token::LBrace, span))) |
    do_parse!(span: tag_span!("}") >> (SpToken::new(Token::RBrace, span))) |
    do_parse!(span: tag_span!("[") >> (SpToken::new(Token::LBracket, span))) |
    do_parse!(span: tag_span!("]") >> (SpToken::new(Token::RBracket, span)))
));

named!(lex_operators<Span, SpToken>, alt!(
    do_parse!(span: tag_span!("==") >> (SpToken::new(Token::DoubleEqual, span))) |
    do_parse!(span: tag_span!("=")  >> (SpToken::new(Token::Equal, span))) |
    do_parse!(span: tag_span!("!=") >> (SpToken::new(Token::NotEqual, span))) |
    do_parse!(span: tag_span!("!")  >> (SpToken::new(Token::Not, span))) |
    do_parse!(span: tag_span!("<=") >> (SpToken::new(Token::LessThanEqual, span))) |
    do_parse!(span: tag_span!("<")  >> (SpToken::new(Token::LessThan, span))) |
    do_parse!(span: tag_span!(">=") >> (SpToken::new(Token::GreaterThanEqual, span))) |
    do_parse!(span: tag_span!(">")  >> (SpToken::new(Token::GreaterThan, span))) |

    do_parse!(span: tag_span!("+=") >> (SpToken::new(Token::PlusEqual, span))) |
    do_parse!(span: tag_span!("+")  >> (SpToken::new(Token::Plus, span))) |
    do_parse!(span: tag_span!("-=") >> (SpToken::new(Token::MinusEqual, span))) |
    do_parse!(span: tag_span!("-")  >> (SpToken::new(Token::Minus, span))) |
    do_parse!(span: tag_span!("*=") >> (SpToken::new(Token::AsteriskEqual, span))) |
    do_parse!(span: tag_span!("*")  >> (SpToken::new(Token::Asterisk, span))) |
    do_parse!(span: tag_span!("/=") >> (SpToken::new(Token::SlashEqual, span))) |
    do_parse!(span: tag_span!("/")  >> (SpToken::new(Token::Slash, span))) |
    do_parse!(span: tag_span!("^=") >> (SpToken::new(Token::CaretEqual, span))) |
    do_parse!(span: tag_span!("^")  >> (SpToken::new(Token::Caret, span)))
));

named!(lex_keywords<Span, SpToken>, alt!(
    do_parse!(span: tag_span!("let")       >> (SpToken::new(Token::Let, span))) |
    do_parse!(span: tag_span!("fn")        >> (SpToken::new(Token::Function, span))) |
    do_parse!(span: tag_span!("if")        >> (SpToken::new(Token::If, span))) |
    do_parse!(span: tag_span!("else")      >> (SpToken::new(Token::Else, span))) |
    do_parse!(span: tag_span!("return")    >> (SpToken::new(Token::Return, span)))
));

named!(lex_identifier<Span, SpToken>, do_parse!(
    span: take_while_first_rest!(1, call!(|s: &str| s.chars().all(|c| UnicodeXID::is_xid_start(c))),
        call!(|c: char| UnicodeXID::is_xid_continue(c))) >>
    (SpToken::new(Token::Identifier(span.as_slice().to_string()), span))
));

named!(lex_literal<Span, SpToken>, alt!(
    lex_number_literal |
    lex_bool_literal |
    lex_string_literal
));

named!(lex_number_literal<Span, SpToken>, alt!(
    lex_float_literal |
    lex_integer_literal
));

named!(lex_integer_literal<Span, SpToken>, alt!(
    lex_hex_literal |
    lex_oct_literal |
    lex_bin_literal |
    lex_dec_literal
));

named!(lex_hex_literal<Span, SpToken>, preceded!(tag_span!("0x"), do_parse!(
    tup: map_res!(take_while!(call!(|c: char| c.is_digit(16) || c == '_')),
        |span| i64::from_str_radix(&stringify_number(span), 16).map(|res| (span, res))) >>
    (SpToken::new(Token::int(tup.1),
        tup.0))
)));

named!(lex_oct_literal<Span, SpToken>, preceded!(tag_span!("0o"), do_parse!(
    tup: map_res!(take_while!(call!(|c: char| c.is_digit(8) || c == '_')),
        |span| i64::from_str_radix(&stringify_number(span), 8).map(|res| (span, res))) >>
    (SpToken::new(Token::int(tup.1),
        tup.0))
)));

named!(lex_bin_literal<Span, SpToken>, preceded!(tag_span!("0b"), do_parse!(
    tup: map_res!(take_while!(call!(|c: char| c.is_digit(2) || c == '_')),
        |span| i64::from_str_radix(&stringify_number(span), 2).map(|res| (span, res))) >>
    (SpToken::new(Token::int(tup.1),
        tup.0))
)));

named!(lex_dec_literal<Span, SpToken>, do_parse!(
    tup: map_res!(take_while!(call!(|c: char| c.is_digit(10) || c == '_' )),
        |span| i64::from_str_radix(&stringify_number(span), 10).map(|res| (span, res))) >>
    (SpToken::new(Token::int(tup.1), tup.0))
));

fn float_to_token(span: Span, len: usize) -> nom::IResult<Span, SpToken> {
    use nom::Slice;
    let full_float = span.slice(..len);
    match f64::from_str(&stringify_number(full_float)) {
        Ok(float) => IResult::Done(span.slice(len..),
            SpToken::new(Token::float(float), full_float)),
        Err(_) => IResult::Error(error_position!(CustomNomError::LexFloat.into(), span)),
    }
}
fn exponent(span: Span) -> nom::IResult<Span, Span> {
    use nom::{InputLength, Slice};
    const EXP_CHAR_LOWER: &str = "e";
    const EXP_CHAR_UPPER: &str = "E";
    let exp_char_len = EXP_CHAR_LOWER.input_len();
    const SIGN_CHAR_PLUS: &str = "+";
    const SIGN_CHAR_MINUS: &str = "-";
    let sign_char_len = SIGN_CHAR_PLUS.input_len();

    complete!(span, do_parse!(
        alt!(tag_span!(EXP_CHAR_UPPER) | tag_span!(EXP_CHAR_LOWER)) >>
        has_sign: opt!(alt!(tag_span!(SIGN_CHAR_PLUS) | tag_span!(SIGN_CHAR_MINUS))) >>
        exp: take_while!(call!(|c: char| c.is_digit(10) || c == '_')) >>
        ({
            let exp_len = exp.input_len() + exp_char_len +
                if has_sign.is_some() { sign_char_len } else { 0 };
            span.slice(..exp_len)
        })
    ))
}
fn float_no_sep(span: Span) -> nom::IResult<Span, SpToken> {
    use nom::InputLength;
    match take_while!(span, call!(|c: char| c.is_digit(10) || c == '_')) {
        IResult::Done(rest, mantissa) => {
            match exponent(rest) {
                IResult::Done(_, exp) => {
                    float_to_token(span, mantissa.input_len() + exp.input_len())
                },
                IResult::Error(e) => IResult::Error(e), // exponent is _required_ if no sep exists
                IResult::Incomplete(needed) => IResult::Incomplete(needed),
            }
        },
        IResult::Error(e) => IResult::Error(e),
        IResult::Incomplete(needed) => IResult::Incomplete(needed),
    }
}
fn float_with_sep(span: Span) -> nom::IResult<Span, SpToken> {
    use nom::{InputLength, Slice};
    const SEP_CHAR: &str = ".";
    match terminated!(span, take_while!(call!(|c: char| c.is_digit(10) || c == '_')),
            tag_span!(SEP_CHAR)) {
        IResult::Done(rest, int_part) => {
            let fractional = opt!(rest, take_while!(call!(|c: char| c.is_digit(10) || c == '_')));
            match fractional {
                IResult::Done(_, frac_part) => {
                    let sep_len = SEP_CHAR.input_len();
                    let int_part_len = int_part.input_len();
                    let mantissa_len = match frac_part {
                        Some(frac) => int_part_len + sep_len + frac.input_len(),
                        None => int_part_len + sep_len
                    };
                    match exponent(span.slice(mantissa_len..)) {
                        IResult::Done(_, exp) => {
                            float_to_token(span, mantissa_len + exp.input_len())
                        },
                        IResult::Error(_) => {
                            // exponent optional if '.' exists
                            float_to_token(span, mantissa_len)
                        },
                        IResult::Incomplete(needed) => IResult::Incomplete(needed),
                    }
                },
                IResult::Error(e) => IResult::Error(e),
                IResult::Incomplete(needed) => IResult::Incomplete(needed),
            }
        },
        IResult::Error(e) => IResult::Error(e),
        IResult::Incomplete(_) => {
            // treat incompletes as failures (usually due to missing SEP_CHAR)
            IResult::Error(error_position!(nom::ErrorKind::Complete, span))
        },
    }
}
named!(lex_float_literal<Span, SpToken>, alt!(float_with_sep | float_no_sep));

named!(lex_bool_literal<Span, SpToken>, alt!(
    do_parse!(span: tag_span!("true")  >> (SpToken::new(Token::bool(true), span))) |
    do_parse!(span: tag_span!("false") >> (SpToken::new(Token::bool(false), span)))
));

fn string(span: Span) -> nom::IResult<Span, SpToken> {
    use nom::{InputLength, Slice};

    let mut index = 0;
    let mut s = String::new();
    while index < span.input_len() {
        let rest = span.slice(index..);
        let nextchar_len = match take!(rest, 1) {
            nom::IResult::Done(_, c_span) => {
                let c = c_span.as_slice();
                match c {
                    "\u{005C}" => { // escape character (backslash)
                        const ESCAPE_CHAR_LEN: usize = 1; // length of a backslash
                        // look-ahead
                        if index + ESCAPE_CHAR_LEN < span.input_len() {
                            let escape_seq = span.slice(index + ESCAPE_CHAR_LEN..);
                            match escape(escape_seq) {
                                nom::IResult::Done(_, (escaped, escaped_size)) => {
                                    s.extend(escaped.chars());
                                    index += escaped_size;
                                },
                                nom::IResult::Error(_) => {
                                    // invalid escape
                                    return nom::IResult::Error(error_position!(
                                        CustomNomError::InvalidEscape.into(), rest))
                                },
                                nom::IResult::Incomplete(needed) => {
                                    return nom::IResult::Incomplete(needed);
                                }
                            }
                        } else {
                            return nom::IResult::Error(error_position!(
                                CustomNomError::UnclosedString.into(), rest))
                        }
                    },
                    "\u{0022}" => { // double-quote
                        // non-escaped quote, finalize
                        return nom::IResult::Done(span.slice(index..),
                            SpToken::new(Token::string(s), span.slice(..index)));
                    },
                    c => {
                        s.extend(c.chars());
                    }
                }
                c_span.input_len()
            },
            nom::IResult::Error(e) => { return nom::IResult::Error(e); },
            nom::IResult::Incomplete(_) => { return nom::IResult::Error(error_position!(
                CustomNomError::UnclosedString.into(), rest)); },
        };
        index += nextchar_len;
    }
    nom::IResult::Error(error_position!(CustomNomError::UnclosedString.into(), span))
}
named!(lex_string_literal<Span, SpToken>, delimited!(
    tag_span!("\""),
    string,
    tag_span!("\"")
));

named!(lex_illegal<Span, SpToken>, do_parse!(
    span: take!(1) >> (SpToken::new(Token::Illegal, span))));

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use super::*;

    fn assert_match<T, F>(input: &str, expected: &[T], f: F) where F: Fn(&SpToken, &T) -> bool,
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
    fn assert_sptokens_match(input: &str, expected: &[SpToken]) {
        assert_match(input, expected, |parsed, expctd| { parsed.eq(expctd) });
    }
    fn assert_tokens_match(input: &str, expected: &[Token]) {
        assert_match(input, expected, |parsed, expctd| { parsed.is_token(expctd) });
    }
    fn assert_illegal_token(input: &str, expected_span: Span) {
        match Lexer::lex(input).unwrap_err() {
            ErrorKind::LexerToken(_, token) => {
                assert!(token.is_token(&Token::Illegal));
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
            Token::LBracket,
            Token::RBracket,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
        ];
        assert_tokens_match(input, &expected);
    }

    #[test]
    fn test_lex_operators() {
        let input = "= == ! != < <= > >= + += - -= * *= / /= ^ ^=";
        let expected = [
            Token::Equal,
            Token::DoubleEqual,
            Token::Not,
            Token::NotEqual,
            Token::LessThan,
            Token::LessThanEqual,
            Token::GreaterThan,
            Token::GreaterThanEqual,
            Token::Plus,
            Token::PlusEqual,
            Token::Minus,
            Token::MinusEqual,
            Token::Asterisk,
            Token::AsteriskEqual,
            Token::Slash,
            Token::SlashEqual,
            Token::Caret,
            Token::CaretEqual,
        ];
        assert_tokens_match(input, &expected);
    }

    #[test]
    fn test_lex_keywords() {
        let input = "let fn if else return";
        let expected = [
            Token::Let,
            Token::Function,
            Token::If,
            Token::Else,
            Token::Return,
        ];
        assert_tokens_match(input, &expected);
    }

    #[test]
    fn test_lex_identifiers() {
        let input = "foo bar baz ª··9ℼ     ℝℨℤℤↈ９";
        let expected = [
            Token::Identifier("foo".to_string()),
            Token::Identifier("bar".to_string()),
            Token::Identifier("baz".to_string()),
            Token::Identifier("ª··9ℼ".to_string()), // ª is XID_Start, rest are XID_Continue
            Token::Identifier("ℝℨℤℤↈ９".to_string()), // ℝ is XID_Start, rest are XID_Continue
        ];
        assert_tokens_match(input, &expected);

        let input = " ９uise"; // ９ is XID_Continue but not XID_Start
        assert_illegal_token(input, Span::new("９", 1, 1, 2));
    }

    #[test]
    fn test_lex_dec_literal() {
        let input = "1502 0000 052 1_234_567 1_2_3";
        let expected = [
            Token::int(1502),
            Token::int(0),
            Token::int(52),
            Token::int(1234567),
            Token::int(1_2_3),
        ];
        assert_tokens_match(input, &expected);
    }

    #[test]
    fn test_lex_hex_literal() {
        let input = "0x1D02 0x000 0x052 0x1_234 0x1_C_3 0xDEADBEEF";
        const P16: [i64; 8] = [1, 16, 256, 4096, 65536, 1048576, 16777216, 268435456];
        let expected = [
            Token::int(P16[3] + 13*P16[2] + 2),
            Token::int(0),
            Token::int(5*16 + 2),
            Token::int(P16[3] + 2*P16[2] + 3*16 + 4),
            Token::int(P16[2] + 12*16 + 3),
            Token::int(13*P16[7] + 14*P16[6] + 10*P16[5]
                + 13*P16[4] + 11*P16[3] + 14*P16[2] + 14*P16[1] + 15*P16[0])
        ];
        assert_tokens_match(input, &expected);
    }

    #[test]
    fn test_lex_oct_literal() {
        let input = "0o1502 0o000 0o052 0o1_234 0o1_2_3";
        const P8: [i64; 4] = [1, 8, 64, 512];
        let expected = [
            Token::int(1*P8[3] + 5*P8[2] + 2),
            Token::int(0),
            Token::int(5*8 + 2),
            Token::int(P8[3] + 2*P8[2] + 3*P8[1] + 4),
            Token::int(P8[2] + 2*
                P8[1] + 3)
        ];
        assert_tokens_match(input, &expected);

        let input = "0o429";
        // interpreted as an octal number 42, then a decimal number 9
        let expected = [
            Token::int(4*P8[1] + 2*P8[0]),
            Token::int(9),
        ];
        assert_tokens_match(input, &expected);
    }

    #[test]
    fn test_lex_bin_literal() {
        let input = "0b101 0b001001 0b10_10";
        let expected = [
            Token::int((1 << 2) + 1),
            Token::int((1 << 3) + 1),
            Token::int((1 << 3) + (1 << 1)),
        ];
        assert_tokens_match(input, &expected);
    }

    #[test]
    fn test_lex_float_literal() {
        let input = "20. 10.249 10.249_942 1_234_567.890_100";
        let expected = [
            Token::float(20.0),
            Token::float(10.249),
            Token::float(10.249942),
            Token::float(1234567.8901)
        ];
        assert_tokens_match(input, &expected);

        let input = "20.1e2 20.2e+3 20.3e-4 20.4E5 20.5E+6 2_0.6E-7 20.e8 20.9e1_0";
        let expected = [
            Token::float(2_010.),
            Token::float(20_200.),
            Token::float(0.00203),
            Token::float(2_040_000.),
            Token::float(20_500_000.),
            Token::float(0.00000206),
            Token::float(2_000_000_000.),
            Token::float(209_000_000_000.),
        ];
        assert_tokens_match(input, &expected);

        let input = "2e5 4e-2";
        let expected = [
            Token::float(200_000.),
            Token::float(0.04),
        ];
        assert_tokens_match(input, &expected);
    }

    #[test]
    fn test_lex_bool_literal() {
        let input = "true false false";
        let expected = [
            Token::bool(true),
            Token::bool(false),
            Token::bool(false),
        ];
        assert_tokens_match(input, &expected);
    }

    #[test]
    fn test_lex_string_literal() {
        // basic strings and escapes
        let input = r#""hello there" "all you people ☺" "how\nare you \"doing\"?""#;
        let expected = [
            Token::string("hello there"),
            Token::string("all you people ☺"),
            Token::string("how\nare you \"doing\"?"),
        ];
        assert_tokens_match(input, &expected);

        // one-byte escapes
        let input = r#""\x41\x2D\x5A" "\x61\x2D\x7A" "\x30\x2D\x39""#;
        let expected = [
            Token::string("A-Z"),
            Token::string("a-z"),
            Token::string("0-9"),
        ];
        assert_tokens_match(input, &expected);

        // unicode escapes
        let input = r#" "\u{263A}\u{2639}" "\u{1F525}" "\u{1F37E}\u{1F37F}" "#;
        let expected = [
            Token::string("☺☹"),
            Token::string("🔥"),
            Token::string("🍾🍿"),
        ];
        assert_tokens_match(input, &expected);

        // invalid escape codes
        let input = r#""\☺""#;
        assert_illegal_token(input, Span::new("\"", 0, 1, 1));

        let input = r#""\q""#;
        assert_illegal_token(input, Span::new("\"", 0, 1, 1));

       let input = r#""\x4☺""#;
       assert_illegal_token(input, Span::new("\"", 0, 1, 1));

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
            SpToken::new(Token::Let, Span::new("let", 1, 2, 1)),
            SpToken::new(Token::Identifier("add".to_string()), Span::new("add", 5, 2, 5)),
            SpToken::new(Token::Equal, Span::new("=", 9, 2, 9)),
            SpToken::new(Token::Function, Span::new("fn", 11, 2, 11)),
            SpToken::new(Token::LParen, Span::new("(", 13, 2, 13)),
            SpToken::new(Token::Identifier("x".to_string()), Span::new("x", 14, 2, 14)),
            SpToken::new(Token::Comma, Span::new(",", 15, 2, 15)),
            SpToken::new(Token::Identifier("y".to_string()), Span::new("y", 17, 2, 17)),
            SpToken::new(Token::RParen, Span::new(")", 18, 2, 18)),
            SpToken::new(Token::LBrace, Span::new("{", 20, 2, 20)),
            SpToken::new(Token::Identifier("x".to_string()), Span::new("x", 26, 3, 5)),
            SpToken::new(Token::Plus, Span::new("+", 28, 3, 7)),
            SpToken::new(Token::Identifier("y".to_string()), Span::new("y", 30, 3, 9)),
            SpToken::new(Token::Semicolon, Span::new(";", 31, 3, 10)),
            SpToken::new(Token::RBrace, Span::new("}", 33, 4, 1)),
            SpToken::new(Token::Semicolon, Span::new(";", 34, 4, 2)),
        ];

        assert_sptokens_match(input, &expected);
    }

}
