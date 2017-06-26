use std::char;

use nom::{IResult, InputLength};

use CustomNomError;
use lex::span::Span;

fn unicode(span: Span) -> IResult<Span, (String, usize)> {
    match take_while!(span, call!(|c: char| c.is_digit(16))) {
        IResult::Done(rest, codepoint) => {
            match u32::from_str_radix(codepoint.as_slice(), 16) {
                Ok(u) => {
                    match char::from_u32(u) {
                        Some(c) => {
                            let mut s = String::new();
                            s.push(c);
                            IResult::Done(rest, (s, codepoint.input_len()))
                        },
                        None => {
                            IResult::Error(error_position!(CustomNomError::Unicode.into(), span))
                        }
                    }
                },
                Err(_) => {
                    IResult::Error(error_position!(CustomNomError::Unicode.into(), span))
                }
            }
        },
        IResult::Incomplete(needed) => IResult::Incomplete(needed),
        IResult::Error(e) => IResult::Error(e),
    }
}

named_attr!(, pub escape<Span, (String, usize)>, alt_complete!(
    // single-char one-byte escapes
    do_parse!(span: tag_span!("n")  >> ("\n".to_string(), span.input_len())) |
    do_parse!(span: tag_span!("r")  >> ("\r".to_string(), span.input_len())) |
    do_parse!(span: tag_span!("t")  >> ("\t".to_string(), span.input_len())) |
    do_parse!(span: tag_span!("\\") >> ("\\".to_string(), span.input_len())) |
    do_parse!(span: tag_span!("0")  >> ("\0".to_string(), span.input_len())) |
    do_parse!(span: tag_span!("'")  >> ("'".to_string(), span.input_len())) |
    do_parse!(span: tag_span!("\"") >> ("\"".to_string(), span.input_len())) |

    // \x00 - \xFF one-byte escapes
    do_parse!(
        s: map_res!(map_res!(preceded!(tag_span!("x"), verify!(take!(2),
            |span: Span| span.as_slice().chars().all(|c| c.is_digit(16)))),
            |span: Span| u8::from_str_radix(span.as_slice(), 16)),
            |u: u8| String::from_utf8(vec![u])) >>
        (s, 3) // "x" and two single-char hex digits
    ) |

    // unicode escapes
    do_parse!(
        s_slen: preceded!(tag_span!("u"), delimited!(
            tag_span!("{"),
            unicode,
            tag_span!("}")
        )) >>
        ({
            let (s, slen) = s_slen;
            // "u", "{", "}" are all single-char, so add 3
            (s, slen + 3)
        })
    )
));
