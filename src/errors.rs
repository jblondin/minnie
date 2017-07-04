use std::fmt;
use std::error::Error;
use std::result;

use lex::{Span, SpToken};

#[derive(Debug)]
pub enum ErrorKind<'a> {
    LexerNoSpan(String),
    LexerSpan(String, Span<'a>),
    LexerToken(String, SpToken<'a>),
    Parser(String),
}
impl<'a> fmt::Display for ErrorKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::LexerNoSpan(ref s)           => write!(f, "{} (unknown position): {}",
                self.description(), s),
            ErrorKind::LexerSpan(ref s, ref span)   => write!(f, "{} at line {}, column {}: {}",
                self.description(), span.line, span.column, s),
            ErrorKind::LexerToken(ref s, ref ts) => write!(f, "{} at line {}, column {}: {}",
                self.description(), ts.span.line, ts.span.column, s),
            ErrorKind::Parser(ref s) => write!(f, "{}: {}", self.description(), s),
        }
    }
}

impl<'a> Error for ErrorKind<'a> {
    fn description(&self) -> &str {
        match *self {
            ErrorKind::LexerNoSpan(_)
                | ErrorKind::LexerSpan(_,_)
                | ErrorKind::LexerToken(_,_) => { "lexer error" },
            ErrorKind::Parser(_) => { "parser error" },
        }
    }
    fn cause(&self) -> Option<&Error> { None }
}

pub type Result<'a, T> = result::Result<T, ErrorKind<'a>>;
