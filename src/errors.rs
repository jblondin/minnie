use std::fmt;
use std::error::Error;
use std::result;

use lex::{Span, Token};

#[derive(Debug)]
pub enum ErrorKind<'a> {
    LexerNoSpan(String),
    LexerSpan(String, Span<'a>),
    LexerToken(String, Token<'a>),
}
impl<'a> fmt::Display for ErrorKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::LexerNoSpan(ref s)           => write!(f, "{} (unknown position): {}",
                self.description(), s),
            ErrorKind::LexerSpan(ref s, ref span)   => write!(f, "{} at line {}, column {}: {}",
                self.description(), span.line, span.column, s),
            ErrorKind::LexerToken(ref s, ref token) => write!(f, "{} at line {}, column {}: {}",
                self.description(), token.span.line, token.span.column, s),
        }
    }
}

impl<'a> Error for ErrorKind<'a> {
    fn description(&self) -> &str { "lexer error" }
    fn cause(&self) -> Option<&Error> { None }
}

pub type Result<'a, T> = result::Result<T, ErrorKind<'a>>;
