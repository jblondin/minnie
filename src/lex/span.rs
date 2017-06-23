use std::ops::{Range, RangeTo, RangeFrom, RangeFull};
use std::str::{Chars, CharIndices};

use nom::{InputLength, InputIter, Offset, Slice, Compare, CompareResult, FindSubstring};

/// Lexer details about a token.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span<'a> {
    /// offset from the beginning of input, 0-indxed
    pub offset: usize,
    /// line number in the file, 1-indexed
    pub line: usize,
    /// column number in the file, 1-indexed
    pub column: usize,
    /// spanned part of the input
    input: &'a str,
}

impl<'a> Span<'a> {
    pub fn new(input: &'a str, offset: usize, line: usize, column: usize) -> Span<'a> {
        Span {
            offset: offset,
            line: line,
            column: column,
            input: input,
        }
    }
    pub fn start(input: &'a str) -> Span<'a> {
        Span::new(input, 0, 1, 1)
    }
    pub fn as_slice(&self) -> &'a str {
        self.input
    }
    pub fn is_empty(&self) -> bool {
        self.input.is_empty()
    }
}

impl<'a> InputLength for Span<'a> {
    fn input_len(&self) -> usize {
        self.input.len()
    }
}

impl<'a> InputIter for Span<'a> {
    type Item = char;
    type RawItem = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.input.iter_indices()
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.input.iter_elements()
    }
    fn position<P>(&self, predicate: P) -> Option<usize> where P: Fn(Self::RawItem) -> bool {
        self.input.position(predicate)
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
        self.input.slice_index(count)
    }
}

impl<'a, 'b> FindSubstring<&'b str> for Span<'a> {
    fn find_substring(&self, substring: &'b str) -> Option<usize> {
        self.input.find_substring(substring)
    }
}


impl<'a, 'b> Compare<Span<'b>> for Span<'a> {
    fn compare(&self, element: Span<'b>) -> CompareResult {
        self.input.compare(element.input)
    }
    fn compare_no_case(&self, element: Span<'b>) -> CompareResult {
        self.input.compare_no_case(element.input)
    }
}
impl<'a, 'b> Compare<&'b str> for Span<'a> {
    fn compare(&self, element: &'b str) -> CompareResult {
        self.input.compare(element)
    }
    fn compare_no_case(&self, element: &'b str) -> CompareResult {
        self.input.compare_no_case(element)
    }
}

impl<'a> Slice<Range<usize>> for Span<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        let sliced_input = &self.input[range];

        if sliced_input == self.input {
            return *self;
        }

        let slice_offset = self.input.offset(sliced_input);
        if slice_offset == 0 {
            return Span::new(sliced_input, self.offset, self.line, self.column);
        }

        let prefix = &self.input[..slice_offset];
        let (nnl, last_nl) = prefix.chars().enumerate().filter(|&(_, c)| c == '\n')
            .fold((0, None), |acc, (i, _)| { (acc.0 + 1, Some(i)) });
        Span {
            offset: self.offset + slice_offset,
            line: self.line + nnl,
            column: match last_nl {
                Some(lnl) => slice_offset - lnl,
                None =>  self.column + slice_offset
            },
            input: sliced_input,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Span<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Span::new(&self.input[range], self.offset, self.line, self.column)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Span<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.input_len())
    }
}

impl<'a> Slice<RangeFull> for Span<'a> {
    fn slice(&self, _: RangeFull) -> Self {
        Span::new(self.input, self.offset, self.line, self.column)
    }
}
