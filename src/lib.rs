#![feature(box_syntax)]
#![feature(unicode)]
#![feature(trace_macros)]

#[macro_use] extern crate nom;

mod errors;

#[macro_use] mod nom_util;
pub use self::nom_util::CustomNomError;

pub mod lex;
pub mod parse;
pub mod eval;
