use std::borrow::Borrow;

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
