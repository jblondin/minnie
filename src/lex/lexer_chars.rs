use std::ops::{Deref, Range, RangeTo, RangeFrom, RangeFull};
use std::iter::Enumerate;

use nom::{self, AsChar, IResult, InputLength, InputIter, Slice, Compare, CompareResult, Needed};

use lex::token::{TokenType};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Char(char);

impl InputLength for Char {
    #[inline] fn input_len(&self) -> usize { 1 }
}
impl Deref for Char {
    type Target = char;

    fn deref(&self) -> &char { &self.0 }
}
impl<'a> AsChar for &'a Char {
    fn as_char(self) -> char { self.0 }
    fn is_alpha(self) -> bool { self.0.is_alphabetic() }
    fn is_alphanum(self) -> bool { self.0.is_alphanumeric() }
    fn is_dec_digit(self) -> bool { self.0.is_digit(10) }
    fn is_hex_digit(self) -> bool { self.0.is_digit(16) }
    fn is_oct_digit(self) -> bool { self.0.is_digit(8) }
}
impl Compare<Char> for Char {
    fn compare(&self, other: Char) -> CompareResult {
        if self.0 == other.0 {
            CompareResult::Ok
        } else {
            CompareResult::Error
        }
    }

    fn compare_no_case(&self, other: Char) -> CompareResult {
        if self.0.to_lowercase().eq(other.0.to_lowercase()) {
            CompareResult::Ok
        } else {
            CompareResult::Error
        }
    }
}

macro_rules! tag_char {
    ($i:expr, $s:expr) => ({
        match ($i).iter_elements().next().map(|&c| {
            let chars: Vec<Char> = $s.chars().map(|c| Char(c)).collect();
            Chars::new(&[c]).compare(Chars::from_vec(&chars))
        }) {
            None => IResult::Incomplete::<_,_,u32>(Needed::Size(1)),
            Some(cres) => {
                match cres {
                    CompareResult::Incomplete => IResult::Incomplete::<_,_,u32>(Needed::Unknown),
                    CompareResult::Error => IResult::Error(error_position!(
                        nom::ErrorKind::Tag, $i)),
                    CompareResult::Ok => IResult::Done($i.slice(1..),
                        $i.iter_elements().next().unwrap().as_char())
                }
            }
        }
    });
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Chars<'a> {
    pub chars: &'a str,
    pub start: usize,
    pub end: usize
}

impl<'a> Chars<'a> {
    pub fn new(s: &'a str) -> Self {
        Chars {
            chars: s,
            start: 0,
            end: s.len()
        }
    }
    // pub fn from_vec(vec: &'a Vec<Char>) -> Self {
    //     Chars::new(vec.as_slice())
    // }
    // pub fn from_str(s: &'a str) -> Self {
    //     let chars = s.chars().map(|c| Char(c)).collect();
    //     Chars::from_vec(&chars)
    // }
    // pub fn from_str(s: &'static str) -> Self {
    //     let chars: Vec<Char> = s.chars().map(|c| Char(c)).collect();
    //     let charlen = chars.len();
    //     Chars {
    //         chars: chars.as_slice(),
    //         start: 0,
    //         end: charlen,
    //     }
    // }
    pub fn len(&self) -> usize {
        self.end - self.start
    }
    pub fn cmp_lowercase<'b>(&self, oth: &Chars<'b>) -> bool {
        let len = self.end - self.start;
        if len == oth.end - oth.start {
            // let mut eq = true;
            // for i in 0..len {
            //     if self.chars[self.start + i].0.to_lowercase().to_string()
            //             != oth.chars[oth.start + i].0.to_lowercase().to_string() {
            //         eq = false;
            //         break;
            //     }
            // }
            // eq
            self.chars[0..len].to_lowercase() == oth.chars[0..len].to_lowercase()
        } else {
            false
        }
    }
}

impl<'a, 'b> Compare<Chars<'b>> for Chars<'a> {
    fn compare(&self, other: Chars<'b>) -> CompareResult {
        let other_len = other.len();
        if self.len() < other_len {
            return CompareResult::Incomplete;
        }

        if &self.slice(..other_len) != &other {
            CompareResult::Error
        } else {
            CompareResult::Ok
        }
    }

    fn compare_no_case(&self, other: Chars<'b>) -> CompareResult {
        if self.len() < other.len() {
            return CompareResult::Incomplete;
        }

        if self.len() == other.len()
                && self.slice(..).to_lowercase() == other.slice(..).to_lowercase() {
            CompareResult::Ok
        } else {
            CompareResult::Error
        }
    }
}

impl<'a> InputLength for Chars<'a> {
    #[inline] fn input_len(&self) -> usize { self.chars.len() }
}

impl<'a> Slice<Range<usize>> for Chars<'a> {
    #[inline]
    fn slice(&self, r: Range<usize>) -> Self {
        Chars {
            chars: self.chars.slice(r.clone()),
            start: self.start + r.start,
            end: self.start + r.end,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Chars<'a> {
    #[inline]
    fn slice(&self, r: RangeTo<usize>) -> Self {
        Chars {
            chars: self.chars.slice(r.clone()),
            start: self.start,
            end: self.start + r.end
        }
    }
}

impl<'a> Slice<RangeFrom<usize>> for Chars<'a> {
    #[inline]
    fn slice(&self, r: RangeFrom<usize>) -> Self {
        Chars {
            chars: self.chars.slice(r.clone()),
            start: self.start + r.start,
            end: self.end
        }
    }
}

impl<'a> Slice<RangeFull> for Chars<'a> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Chars {
            chars: self.chars,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> InputIter for Chars<'a> {
    type Item = &'a Char;
    type RawItem = Char;
    type Iter = Enumerate<::std::slice::Iter<'a, Char>>;
    type IterElem = ::std::slice::Iter<'a, Char>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.chars.iter().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.chars.iter()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize> where P: Fn(Self::RawItem) -> bool {
        self.chars.iter().position(|b| predicate(b.clone()))
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
        if self.chars.len() >= count {
            Some(count)
        } else {
            None
        }
    }
}





pub struct Lexer;

impl Lexer {
    pub fn lex(chars: Chars) -> IResult<Chars, Vec<TokenType>> {
        lex_into_tokens(chars)
    }
}

named!(lex_into_tokens<Chars, Vec<TokenType>>, ws!(many0!(lex_into_token)));

named!(lex_into_token<Chars, TokenType>, alt!(
    lex_punctuation |
    // lex_operator |
    // lex_identifier |
    // lex_literal |
    lex_illegal
));

named!(lex_illegal<Chars, TokenType>, do_parse!(take!(1) >> (TokenType::IllegalChar)));

named!(lex_punctuation<Chars, TokenType>, alt!(
    do_parse!(tag_char!(",") >> (TokenType::Comma)) |
    do_parse!(tag_char!(";") >> (TokenType::Semicolon)) |
    do_parse!(tag_char!("(") >> (TokenType::LParen)) |
    do_parse!(tag_char!(")") >> (TokenType::RParen)) |
    do_parse!(tag_char!("{") >> (TokenType::LBrace)) |
    do_parse!(tag_char!("}") >> (TokenType::RBrace)) |
    do_parse!(tag_char!("[") >> (TokenType::LBracket)) |
    do_parse!(tag_char!("]") >> (TokenType::RBracket))
));

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_input_matches(input: &str, expected: &[TokenType]) {
        let chars = input.chars().map(|c| Char(c)).collect();
        let (_, tokens) = Lexer::lex(Chars::from_vec(&chars)).unwrap();
        // println!("{:?}", tokens);
        // let mut lexer = Lexer::from_str(input);
        for (parsed_tok, expected_tok) in tokens.iter().zip(expected.iter()) {
            assert_eq!(parsed_tok, expected_tok);
        }
        // let mut i = 0;
        // while let Some(token) = lexer.next_token() {
        //     assert_eq!(token, expected[i]);
        //     i += 1;
        // }
    }

    #[test]
    fn test_next_token_simple() {
        let input = "(){},;";
        let expected = [
            TokenType::LParen,
            TokenType::RParen,
            TokenType::LBrace,
            TokenType::RBrace,
            TokenType::Comma,
            TokenType::Semicolon,
        ];
        assert_input_matches(input, &expected);
    }
}