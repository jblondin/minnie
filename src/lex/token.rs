use std::fmt::Debug;
use std::borrow::Borrow;
use std::iter::Enumerate;
use std::ops::{Range, RangeTo, RangeFrom, RangeFull};

use nom::{InputLength, InputIter, Slice};

use lex::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct TokenSpan<'a, T> {
    pub token: T,

    pub span: Span<'a>,
}
impl<'a, T> TokenSpan<'a, T> where T: Debug + Clone + PartialEq {
    pub fn new(token: T, span: Span<'a>) -> TokenSpan<'a, T> {
        TokenSpan {
            token: token,
            span: span,
        }
    }
    pub fn is_token(&self, token: &T) -> bool {
        &self.token == token
    }
    pub fn matches_token(&self, other: &TokenSpan<'a, T>) -> bool {
        &self.token == &other.token
    }
}

impl<'a, T> InputLength for TokenSpan<'a, T> {
    fn input_len(&self) -> usize {
        self.span.input_len()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,
    Eof,

    // identifiers and literals
    Identifier(String),
    Literal(Literal),

    // operators
    Equal,
    DoubleEqual,
    Not,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Plus,
    PlusEqual,
    Minus,
    MinusEqual,
    Slash,
    SlashEqual,
    Asterisk,
    AsteriskEqual,
    Caret,
    CaretEqual,

    // delimiters
    Comma,
    Semicolon,

    // parentheses
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // keywords
    Function,
    Let,
    If,
    Else,
    Return,
}

pub type SpToken<'a> = TokenSpan<'a, Token>;

impl Token {
    pub fn into_token(self, span: Span) -> SpToken {
        SpToken::new(self, span)
    }
    pub fn float(f: f64) -> Token {
        Token::Literal(Literal::Number(NumberLiteral::Float(f)))
    }
    pub fn int(i: i64) -> Token {
        Token::Literal(Literal::Number(NumberLiteral::Int(i)))
    }
    pub fn string<T: Borrow<str>>(s: T) -> Token {
        Token::Literal(Literal::String(s.borrow().to_string()))
    }
    pub fn bool(b: bool) -> Token {
        Token::Literal(Literal::Bool(b))
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(NumberLiteral),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumberLiteral {
    Int(i64),
    Float(f64),
}

#[derive(PartialEq, Debug)]
pub struct Tokens<'a, T> where T: 'a {
    pub start: usize,
    pub end: usize,
    pub tokens: &'a [TokenSpan<'a, T>],
}
impl<'a, T> Clone for Tokens<'a, T> {
    fn clone(&self) -> Tokens<'a, T> {
        Tokens {
            start: self.start,
            end: self.end,
            tokens: self.tokens,
        }
    }
}
impl<'a, T> Copy for Tokens<'a, T> { }

impl<'a, T> Tokens<'a, T> {
    pub fn from_vec(v: &'a Vec<TokenSpan<'a, T>>) -> Self {
        Tokens {
            start: 0,
            end: v.len(),
            tokens: v.as_slice(),
        }
    }
    pub fn as_slice(&self) -> &'a [TokenSpan<'a, T>] {
        self.tokens
    }
    pub fn unwrap_first(&self) -> &'a TokenSpan<'a, T> {
        &self.tokens[0]
    }
}

impl<'a, T> InputLength for Tokens<'a, T> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'a, T> Slice<Range<usize>> for Tokens<'a, T> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Tokens {
            tokens: self.tokens.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a, T> Slice<RangeTo<usize>> for Tokens<'a, T> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a, T> Slice<RangeFrom<usize>> for Tokens<'a, T> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a, T> Slice<RangeFull> for Tokens<'a, T> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Tokens {
            tokens: self.tokens,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a, T> InputIter for Tokens<'a, T> where T: Clone {
    type Item = &'a TokenSpan<'a, T>;
    type RawItem = TokenSpan<'a, T>;
    type Iter = Enumerate<::std::slice::Iter<'a, TokenSpan<'a, T>>>;
    type IterElem = ::std::slice::Iter<'a, TokenSpan<'a, T>>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, TokenSpan<'a, T>>> {
        self.tokens.iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, TokenSpan<'a, T>> {
        self.tokens.iter()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize> where P: Fn(Self::RawItem) -> bool {
        self.tokens.iter().position(|b| predicate(b.clone()))
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
        if self.tokens.len() >= count {
            Some(count)
        } else {
            None
        }
    }
}

pub type SpTokens<'a> = Tokens<'a, Token>;
