use std::borrow::Borrow;
use std::iter::Enumerate;
use std::ops::{Range, RangeTo, RangeFrom, RangeFull};

use nom::{InputLength, InputIter, Slice};

use lex::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub ty: TokenType,

    pub span: Span<'a>,
}
impl<'a> Token<'a> {
    pub fn new(ty: TokenType, span: Span<'a>) -> Token<'a> {
        Token {
            ty: ty,
            span: span,
        }
    }
    pub fn is_type(&self, ty: &TokenType) -> bool {
        &self.ty == ty
    }
    pub fn matches_type(&self, other: &Token) -> bool {
        &self.ty == &other.ty
    }
}

impl<'a> InputLength for Token<'a> {
    fn input_len(&self) -> usize {
        self.span.input_len()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
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
impl TokenType {
    pub fn into_token(self, span: Span) -> Token {
        Token::new(self, span)
    }
    pub fn float(f: f64) -> TokenType {
        TokenType::Literal(Literal::Number(NumberLiteral::Float(f)))
    }
    pub fn int(i: i64) -> TokenType {
        TokenType::Literal(Literal::Number(NumberLiteral::Int(i)))
    }
    pub fn string<T: Borrow<str>>(s: T) -> TokenType {
        TokenType::Literal(Literal::String(s.borrow().to_string()))
    }
    pub fn bool(b: bool) -> TokenType {
        TokenType::Literal(Literal::Bool(b))
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

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Tokens<'a> {
    pub start: usize,
    pub end: usize,
    pub tokens: &'a [Token<'a>],
}

impl<'a> Tokens<'a> {
    pub fn from_vec(v: &'a Vec<Token<'a>>) -> Self {
        Tokens {
            start: 0,
            end: v.len(),
            tokens: v.as_slice(),
        }
    }
    pub fn as_slice(&self) -> &'a [Token<'a>] {
        self.tokens
    }
    pub fn unwrap_first(&self) -> &'a Token<'a> {
        &self.tokens[0]
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'a> Slice<Range<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Tokens {
            tokens: self.tokens.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> Slice<RangeFull> for Tokens<'a> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Tokens {
            tokens: self.tokens,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Token<'a>;
    type RawItem = Token<'a>;
    type Iter = Enumerate<::std::slice::Iter<'a, Token<'a>>>;
    type IterElem = ::std::slice::Iter<'a, Token<'a>>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, Token<'a>>> {
        self.tokens.iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, Token<'a>> {
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
