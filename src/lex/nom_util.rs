use nom;

use lex::span::Span;

pub enum CustomNomError {
    NoDefault               = 1,
    PredicateMatch          = 2,
    LexFloat                = 3,
    TakeWhileFirstRest      = 4,
    UnclosedString          = 5,
    InvalidEscape           = 6,
    Unicode                 = 7,

    Unimplemented           = 99,
}
impl CustomNomError {
    pub fn into(self) -> nom::ErrorKind {
        nom::ErrorKind::Custom(self as u32)
    }
}

macro_rules! default_if (
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => ({
        let i_ = $i.clone();
        if $submac!(i_, $($args)*) {
            $submac2!(i_, $($args2)*)
        } else {
            nom::IResult::Error(error_position!(CustomNomError::NoDefault.into(), $i))
        }
    });
);

macro_rules! tag_span {
    ($i:expr, $tag:expr) => ({
        let input = $i;
        let Span { offset, line, column, .. } = input;
        tag!(input, Span::new($tag, offset, line, column))
    });
}

macro_rules! take_while_first_rest {
    ($i:expr, $prefix_cnt:expr, $firstmac:ident!( $($firstargs:tt)* ),
            $restmac:ident!( $($restargs:tt)* )) => ({

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
                            nom::IResult::Done(input.slice(index..),
                                Span::new(input.slice(..index).as_slice(), offset, line, column))
                        },
                        None => {
                            nom::IResult::Done(input.slice(input.input_len()..), input)
                        }
                    }
                } else {
                    nom::IResult::Error(error_position!(
                        CustomNomError::TakeWhileFirstRest.into(), input))
                }
            }
        }
    });
    ($i:expr, $prefix_cnt:expr, $firstexp:expr, $restmac:ident!( $($restargs:tt)* )) => (
        take_while_first_rest!($i, $prefix_cnt, call!($firstexp), $restmac!($($restargs)*));
    );
    ($i:expr, $prefix_cnt:expr, $firstmac:ident!( $($firstargs:tt)* ), $restexp:expr) => (
        take_while_first_rest!($i, $prefix_cnt, $firstmac!($($firstargs)*), call!($restexp));
    );
    ($i:expr, $prefix_cnt:expr, $firstexp:expr, $restexp:expr) => (
        take_while_first_rest!($i, $prefix_cnt, call!($firstexp), call!($restexp));
    );
}

pub fn stringify_number(span: Span) -> String {
    let mut ret = String::new();
    let chars: Vec<char> = span.as_slice().chars().filter(|&c: &char| c != '_').collect();
    for c in chars { ret.push(c); }
    ret
}
