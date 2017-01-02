// Copyright 2016 Google Inc. All rights reserved.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

//! A regex parser yielding an AST.

use bit_set::BitSet;
use std::str::FromStr;
use std::usize;

use codepoint_len;
use MAX_RECURSION;
use Expr;
use Error;
use LookAround::*;
use Result;

const FLAG_CASEI: u32 = (1 << 0);
const FLAG_MULTI: u32 = (1 << 1);
const FLAG_DOTNL: u32 = (1 << 2);
const FLAG_SWAP_GREED: u32 = (1 << 3);
const FLAG_IGNORE_SPACE: u32 = (1 << 4);
const FLAG_UNICODE: u32 = (1 << 5);

pub struct Parser<'a> {
    re: &'a str,  // source
    backrefs: BitSet,
    flags: u32,
}

impl<'a> Parser<'a> {
    pub fn parse(re: &str) -> Result<(Expr, BitSet)> {
        let mut p = Parser::new(re);
        let (ix, result) = try!(p.parse_re(0, 0));
        if ix < re.len() {
            return Err(Error::ParseError);
        }
        Ok((result, p.backrefs))
    }

    fn new(re: &str) -> Parser {
        Parser {
            re: re,
            backrefs: BitSet::new(),
            flags: FLAG_UNICODE,
        }
    }

    fn parse_re(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let (ix, child) = try!(self.parse_branch(ix, depth));
        let mut ix = self.optional_whitespace(ix);
        if self.re[ix..].starts_with('|') {
            let mut children = vec![child];
            while self.re[ix..].starts_with('|') {
                ix += 1;
                let (next, child) = try!(self.parse_branch(ix, depth));
                children.push(child);
                ix = self.optional_whitespace(next);
            }
            return Ok((ix, Expr::Alt(children)))
        }
        Ok((ix, child))
    }

    fn parse_branch(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let mut children = Vec::new();
        let mut ix = ix;
        while ix < self.re.len() {
            let (next, child) = try!(self.parse_piece(ix, depth));
            if next == ix {
                break
            }
            if child != Expr::Empty {
                children.push(child);
            }
            ix = next;
        }
        match children.len() {
            0 => Ok((ix, Expr::Empty)),
            1 => Ok((ix, children.pop().unwrap())),
            _ => Ok((ix, Expr::Concat(children))),
        }
    }

    fn parse_piece(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let (ix, child) = try!(self.parse_atom(ix, depth));
        let mut ix = self.optional_whitespace(ix);
        if ix < self.re.len() {
            // fail when child is empty?
            let (lo, hi) = match self.re.as_bytes()[ix] {
                b'?' => (0, 1),
                b'*' => (0, usize::MAX),
                b'+' => (1, usize::MAX),
                b'{' => {
                    let (next, lo, hi) = try!(self.parse_repeat(ix));
                    ix = next - 1;
                    (lo, hi)
                }
                _ => return Ok((ix, child))
            };
            ix += 1;
            ix = self.optional_whitespace(ix);
            let mut greedy = true;
            if ix < self.re.len() && self.re.as_bytes()[ix] == b'?' {
                greedy = false;
                ix += 1;
            }
            greedy ^= self.flag(FLAG_SWAP_GREED);
            return Ok((ix, Expr::Repeat {
                child: Box::new(child),
                lo: lo,
                hi: hi,
                greedy: greedy,
            }))
        }
        Ok((ix, child))
    }

    // ix, lo, hi
    fn parse_repeat(&self, ix: usize) -> Result<(usize, usize, usize)> {
        let ix = self.optional_whitespace(ix + 1);  // skip opening '{'
        let bytes = self.re.as_bytes();
        if ix == self.re.len() {
            return Err(Error::InvalidRepeat);
        }
        let mut end = ix;
        let lo = if bytes[ix] == b',' {
            0
        } else if let Some((next, lo)) = parse_decimal(self.re, ix) {
            end = next;
            lo
        } else {
            return Err(Error::InvalidRepeat);
        };
        let ix = self.optional_whitespace(end);  // past lo number
        end = ix;
        let hi = match bytes[ix] {
            b'}' => lo,
            b',' => {
                end = self.optional_whitespace(ix + 1);  // past ','
                if let Some((next, hi)) = parse_decimal(self.re, end) {
                    end = next;
                    hi
                } else {
                    usize::MAX
                }
            }
            _ => return Err(Error::InvalidRepeat)
        };
        let ix = self.optional_whitespace(end);  // past hi number
        if ix == self.re.len() || bytes[ix] != b'}' {
            return Err(Error::InvalidRepeat);
        }
        Ok((ix + 1, lo, hi))
    }

    fn parse_atom(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let ix = self.optional_whitespace(ix);
        if ix == self.re.len() {
            return Ok((ix, Expr::Empty));
        }
        match self.re.as_bytes()[ix] {
            b'.' => {
                Ok((ix + 1, Expr::Any { newline: self.flag(FLAG_DOTNL) }))
            }
            b'^' => Ok((ix + 1,
                if self.flag(FLAG_MULTI) {
                    Expr::StartLine
                } else {
                    Expr::StartText
                }
            )),
            b'$' => Ok((ix + 1,
                if self.flag(FLAG_MULTI) {
                    Expr::EndLine
                } else {
                    Expr::EndText
                }
            )),
            b'(' => self.parse_group(ix, depth),
            b'\\' => self.parse_escape(ix),
            b'+' | b'*' | b'?' | b'|' | b')' | b']' | b'{' | b'}' =>
                Ok((ix, Expr::Empty)),
            b'[' => self.parse_class(ix),
            b'#' | b' ' | b'\r' | b'\n' | b'\t'
                    if self.flag(FLAG_IGNORE_SPACE) => {
                // recursion is bounded because whitespace won't land on '#'
                let ix = self.whitespace(ix);
                self.parse_atom(ix, depth)
            }
            b => {
                // TODO: maybe want to match multiple codepoints?
                let next = ix + codepoint_len(b);
                Ok((next, Expr::Literal {
                    val: String::from(&self.re[ix..next]),
                    casei: self.flag(FLAG_CASEI),
                }))
            }
        }
    }

    // ix points to \ character
    fn parse_escape(&mut self, ix: usize) -> Result<(usize, Expr)> {
        if ix + 1 == self.re.len() {
            return Err(Error::TrailingBackslash);
        }
        let bytes = self.re.as_bytes();
        let b = bytes[ix + 1];
        let mut end = ix + 2;
        let mut size = 1;
        if is_digit(b) {
            if let Some((end, group)) = parse_decimal(self.re, ix + 1) {
                // protect BitSet against unreasonably large value
                if group < self.re.len() / 2 {
                    self.backrefs.insert(group);
                    return Ok((end, Expr::Backref(group)));
                }
            }
            return Err(Error::InvalidBackref)
        } else if b == b'A' || b == b'z' || b == b'b' || b == b'B' {
            size = 0;
        } else if (b | 32) == b'd' || (b | 32) == b's' || (b | 32) == b'w' {
            // size = 1
        } else if (b | 32) == b'h' {
            let s = if b == b'h' {
                "[0-9A-Za-z]"
            } else {
                "[^0-9A-Za-z]"
            };
            let inner = String::from(s);
            return Ok((end, Expr::Delegate { inner: inner, size: size }));
        } else if b == b'x' {
            return self.parse_hex(end);
        } else if (b | 32) == b'p' {
            // allow whitespace?
            if end == self.re.len() {
                return Err(Error::TrailingBackslash);  // better name?
            }
            let b = bytes[end];
            end += 1;
            if b == b'{' {
                loop {
                    if end == self.re.len() {
                        return Err(Error::UnclosedUnicodeName);
                    }
                    if bytes[end] == b'}' {
                        break;
                    }
                    end += 1;
                }
                end += 1;
            }
        } else if b'a' <= (b | 32) && (b | 32) <= b'z' {
            return Err(Error::InvalidEscape);
        } else if 0x21 <= b && b <= 0x7f {
            // printable ASCII (excluding space)
            return Ok((end, make_literal(&self.re[ix + 1..end])));
        }
        // what to do with characters outside printable ASCII?
        let inner = String::from(&self.re[ix..end]);
        Ok((end, Expr::Delegate { inner: inner, size: size }))
    }

    // ix points after '\x', eg to 'A0' or '{12345}'
    fn parse_hex(&self, ix: usize) -> Result<(usize, Expr)> {
        if ix + 2 > self.re.len() {
            return Err(Error::InvalidHex);
        }
        let bytes = self.re.as_bytes();
        let b = bytes[ix];
        let (end, s) = if is_hex_digit(b) && is_hex_digit(bytes[ix + 1]) {
            let end = ix + 2;
            (end, &self.re[ix..end])
        } else if b == b'{' {
            let starthex = ix + 1;
            let mut endhex = starthex;
            loop {
                if endhex == self.re.len() {
                    return Err(Error::InvalidHex);
                }
                let b = bytes[endhex];
                if endhex > starthex && b == b'}' { break; }
                if is_hex_digit(b) && endhex < starthex + 6 {
                    endhex += 1;
                } else {
                    return Err(Error::InvalidHex);
                }
            }
            (endhex + 1, &self.re[starthex .. endhex])
        } else {
            return Err(Error::InvalidHex);
        };
        let codepoint = u32::from_str_radix(s, 16).unwrap();
        if let Some(c) = ::std::char::from_u32(codepoint) {
            let mut inner = String::with_capacity(4);
            inner.push(c);
            return Ok((end, Expr::Literal {
                val: inner,
                casei: self.flag(FLAG_CASEI),
            }));
        } else {
            return Err(Error::InvalidCodepointValue);
        }
    }

    fn parse_class(&self, ix: usize) -> Result<(usize, Expr)> {
        let bytes = self.re.as_bytes();
        let mut ix = ix + 1;  // skip opening '['
        let mut inner = String::new();
        inner.push('[');
        loop {
            ix = self.optional_whitespace(ix);
            if ix == self.re.len() {
                return Err(Error::InvalidClass);
            }
            let end = match bytes[ix] {
                b'\\' => {
                    if ix + 1 == self.re.len() {
                        return Err(Error::InvalidClass);
                    }
                    ix + 1 + codepoint_len(bytes[ix + 1])
                }
                b']' => break,
                b => ix + codepoint_len(b)
            };
            inner.push_str(&self.re[ix..end]);
            ix = end;
        }
        inner.push(']');
        let ix = ix + 1;  // skip closing ']'
        Ok((ix, Expr::Delegate { inner: inner, size: 1 }))
    }

    fn parse_group(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let depth = depth + 1;
        if depth >= MAX_RECURSION {
            return Err(Error::RecursionExceeded);
        }
        let ix = self.optional_whitespace(ix + 1);
        let (la, skip) = if self.re[ix..].starts_with("?=") {
            (Some(LookAhead), 2)
        } else if self.re[ix..].starts_with("?!") {
            (Some(LookAheadNeg), 2)
        } else if self.re[ix..].starts_with("?<=") {
            (Some(LookBehind), 3)
        } else if self.re[ix..].starts_with("?<!") {
            (Some(LookBehindNeg), 3)
        } else if self.re[ix..].starts_with('?') {
            return self.parse_flags(ix, depth);
        } else {
            (None, 0)
        };
        let ix = ix + skip;
        let (ix, child) = try!(self.parse_re(ix, depth));
        let ix = self.optional_whitespace(ix);
        if ix == self.re.len() {
            return Err(Error::UnclosedOpenParen);
        } else if self.re.as_bytes()[ix] != b')' {
            return Err(Error::ParseError);
        };
        let result = match la {
            Some(la) => Expr::LookAround(Box::new(child), la),
            None => Expr::Group(Box::new(child)),
        };
        Ok((ix + 1, result))
    }

    fn parse_flags(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let start = ix + 1;
        let mut ix = start;
        let mut neg = false;
        let oldflags = self.flags;
        loop {
            ix = self.optional_whitespace(ix);
            if ix == self.re.len() {
                return Err(Error::UnclosedOpenParen);
            }
            match self.re.as_bytes()[ix] {
                b'i' => self.update_flag(FLAG_CASEI, neg),
                b'm' => self.update_flag(FLAG_MULTI, neg),
                b's' => self.update_flag(FLAG_DOTNL, neg),
                b'U' => self.update_flag(FLAG_SWAP_GREED, neg),
                b'x' => self.update_flag(FLAG_IGNORE_SPACE, neg),
                b'u' => {
                    if neg {
                        return Err(Error::NonUnicodeUnsupported)
                    }
                }
                b'-' => {
                    if neg {
                        return Err(Error::UnknownFlag);  // more precise error?
                    }
                    neg = true;
                }
                b')' => {
                    if ix == start || neg && ix == start + 1 {
                        return Err(Error::UnknownFlag);
                    }
                    return Ok((ix + 1, Expr::Empty))
                }
                b':' => {
                    if neg && ix == start + 1 {
                        return Err(Error::UnknownFlag);
                    }
                    ix += 1;
                    let (ix, child) = try!(self.parse_re(ix, depth));
                    if ix == self.re.len() {
                        return Err(Error::UnclosedOpenParen);
                    } else if self.re.as_bytes()[ix] != b')' {
                        return Err(Error::ParseError);
                    };
                    self.flags = oldflags;
                    return Ok((ix + 1, child));
                }
                _ => return Err(Error::UnknownFlag)
            }
            ix += 1;
        }
    }

    fn flag(&self, flag: u32) -> bool {
        (self.flags & flag) != 0
    }

    fn update_flag(&mut self, flag: u32, neg: bool) {
        if neg {
            self.flags &= !flag;
        } else {
            self.flags |= flag;
        }
    }

    fn whitespace(&self, mut ix: usize) -> usize {
        let bytes = self.re.as_bytes();
        loop {
            if ix == self.re.len() {
                return ix;
            }
            match bytes[ix] {
                b'#' => {
                    match bytes[ix..].iter().position(|&c| c == b'\n') {
                        Some(x) => ix += x + 1,
                        None => return self.re.len()
                    }
                }
                b' ' | b'\r' | b'\n' | b'\t' => ix += 1,
                _ => return ix
            }
        }
    }

    fn optional_whitespace(&self, ix: usize) -> usize {
        if self.flag(FLAG_IGNORE_SPACE) {
            self.whitespace(ix)
        } else {
            ix
        }
    }
}

// return (ix, value)
fn parse_decimal(s: &str, ix: usize) -> Option<(usize, usize)> {
    let mut end = ix;
    while end < s.len() && is_digit(s.as_bytes()[end]) {
        end += 1;
    }
    usize::from_str(&s[ix..end]).ok().map(|val| (end, val))
}

fn is_digit(b: u8) -> bool {
    b'0' <= b && b <= b'9'
}

fn is_hex_digit(b: u8) -> bool {
    is_digit(b) || (b'a' <= (b | 32) && (b | 32) <= b'f')
}

pub fn make_literal(s: &str) -> Expr {
    Expr::Literal { val: String::from(s), casei: false }
}

#[cfg(test)]
mod tests {
    use std::usize;
    use Expr;
    use LookAround::*;
    use parse::make_literal;

    fn p(s: &str) -> Expr { Expr::parse(s).unwrap().0 }

    #[test]
    fn any() {
        assert_eq!(p("."), Expr::Any { newline: false });
        assert_eq!(p("(?s:.)"), Expr::Any { newline: true });
    }

    #[test]
    fn start_text() {
        assert_eq!(p("^"), Expr::StartText);
    }

    #[test]
    fn end_text() {
        assert_eq!(p("$"), Expr::EndText);
    }

    #[test]
    fn literal() {
        assert_eq!(p("a"), make_literal("a"));
    }

    #[test]
    fn literal_escape() {
        assert_eq!(p("\\'"), make_literal("'"));
        assert_eq!(p("\\\""), make_literal("\""));
        assert_eq!(p("\\xA0"), make_literal("\u{A0}"));
        assert_eq!(p("\\x{1F4A9}"), make_literal("\u{1F4A9}"));
    }

    #[test]
    fn hex_escape() {
        assert_eq!(p("\\h"), Expr::Delegate {
            inner: String::from("[0-9A-Za-z]"), size: 1 });
        assert_eq!(p("\\H"), Expr::Delegate {
            inner: String::from("[^0-9A-Za-z]"), size: 1 });
    }

    #[test]
    fn invalid_escape() {
        assert!(Expr::parse("\\").is_err());
        assert!(Expr::parse("\\q").is_err());
        assert!(Expr::parse("\\xAG").is_err());
        assert!(Expr::parse("\\xA").is_err());
        assert!(Expr::parse("\\x{}").is_err());
        assert!(Expr::parse("\\x{AG}").is_err());
        assert!(Expr::parse("\\x{42").is_err());
        assert!(Expr::parse("\\x{D800}").is_err());
        assert!(Expr::parse("\\x{110000}").is_err());
        assert!(Expr::parse("\\x{0000042}").is_err());
    }

    #[test]
    fn concat() {
        assert_eq!(p("ab"), Expr::Concat(vec![
            make_literal("a"),
            make_literal("b"),
        ]));
    }

    #[test]
    fn alt() {
        assert_eq!(p("a|b"), Expr::Alt(vec![
            make_literal("a"),
            make_literal("b"),
        ]));
    }

    #[test]
    fn group() {
        assert_eq!(p("(a)"), Expr::Group(Box::new(
            make_literal("a"),
        )));
    }

    #[test]
    fn repeat() {
        assert_eq!(p("a{2,42}"), Expr::Repeat{ child: Box::new(make_literal("a")),
            lo: 2, hi: 42, greedy: true });
        assert_eq!(p("a{2,}"), Expr::Repeat{ child: Box::new(make_literal("a")),
            lo: 2, hi: usize::MAX, greedy: true });
        assert_eq!(p("a{2}"), Expr::Repeat{ child: Box::new(make_literal("a")),
            lo: 2, hi: 2, greedy: true });
        assert_eq!(p("a{,2}"), Expr::Repeat{ child: Box::new(make_literal("a")),
            lo: 0, hi: 2, greedy: true });

        assert_eq!(p("a{2,42}?"), Expr::Repeat{ child: Box::new(make_literal("a")),
            lo: 2, hi: 42, greedy: false });
        assert_eq!(p("a{2,}?"), Expr::Repeat{ child: Box::new(make_literal("a")),
            lo: 2, hi: usize::MAX, greedy: false });
        assert_eq!(p("a{2}?"), Expr::Repeat{ child: Box::new(make_literal("a")),
            lo: 2, hi: 2, greedy: false });
        assert_eq!(p("a{,2}?"), Expr::Repeat{ child: Box::new(make_literal("a")),
            lo: 0, hi: 2, greedy: false });
    }

    #[test]
    fn delegate_zero() {
        assert_eq!(p("\\b"), Expr::Delegate {
            inner: String::from("\\b"), size: 0
        });
        assert_eq!(p("\\B"), Expr::Delegate {
            inner: String::from("\\B"), size: 0
        });
    }

    #[test]
    fn delegate_named_group() {
        assert_eq!(p("\\p{Greek}"), Expr::Delegate {
            inner: String::from("\\p{Greek}"), size: 1
        });
        assert_eq!(p("\\pL"), Expr::Delegate {
            inner: String::from("\\pL"), size: 1
        });
        assert_eq!(p("\\P{Greek}"), Expr::Delegate {
            inner: String::from("\\P{Greek}"), size: 1
        });
        assert_eq!(p("\\PL"), Expr::Delegate {
            inner: String::from("\\PL"), size: 1
        });
    }

    #[test]
    fn backref() {
        assert_eq!(p("(.)\\1"), Expr::Concat(vec![
            Expr::Group(Box::new(
                Expr::Any { newline: false }
            )),
            Expr::Backref(1),
        ]));
    }

    #[test]
    fn lookaround() {
        assert_eq!(p("(?=a)"), Expr::LookAround(Box::new(make_literal("a")),
            LookAhead));
        assert_eq!(p("(?!a)"), Expr::LookAround(Box::new(make_literal("a")),
            LookAheadNeg));
        assert_eq!(p("(?<=a)"), Expr::LookAround(Box::new(make_literal("a")),
            LookBehind));
        assert_eq!(p("(?<!a)"), Expr::LookAround(Box::new(make_literal("a")),
            LookBehindNeg));
    }

    #[test]
    fn shy_group() {
        assert_eq!(p("(?:ab)c"), Expr::Concat(vec![
            Expr::Concat(vec![
                make_literal("a"),
                make_literal("b"),
            ]),
            make_literal("c"),
        ]));
    }

    #[test]
    fn flag_state() {
        assert_eq!(p("(?s)."), Expr::Any { newline: true });
        assert_eq!(p("(?s:(?-s:.))"), Expr::Any { newline: false });
        assert_eq!(p("(?s:.)."), Expr::Concat(vec![
            Expr::Any { newline: true },
            Expr::Any { newline: false },
        ]));
        assert_eq!(p("(?:(?s).)."), Expr::Concat(vec![
            Expr::Any { newline: true },
            Expr::Any { newline: false },
        ]));
    }

    #[test]
    fn flag_multiline() {
        assert_eq!(p("^"), Expr::StartText);
        assert_eq!(p("(?m:^)"), Expr::StartLine);
        assert_eq!(p("$"), Expr::EndText);
        assert_eq!(p("(?m:$)"), Expr::EndLine);
    }

    #[test]
    fn flag_swap_greed() {
        assert_eq!(p("a*"), p("(?U:a*?)"));
        assert_eq!(p("a*?"), p("(?U:a*)"));
    }

    #[test]
    fn invalid_flags() {
        assert!(Expr::parse("(?").is_err());
        assert!(Expr::parse("(?)").is_err());
        assert!(Expr::parse("(?-)").is_err());
        assert!(Expr::parse("(?-:a)").is_err());
        assert!(Expr::parse("(?q:a)").is_err());
    }

    #[test]
    fn lifetime() {
        assert_eq!(p("\\'[a-zA-Z_][a-zA-Z0-9_]*(?!\\')\\b"),

            Expr::Concat(vec![
                make_literal("'"),
                Expr::Delegate { inner: String::from("[a-zA-Z_]"), size: 1 },
                Expr::Repeat { child: Box::new(
                    Expr::Delegate { inner: String::from("[a-zA-Z0-9_]"), size: 1 }
                ), lo: 0, hi: usize::MAX, greedy: true },
                Expr::LookAround(Box::new(
                    make_literal("'")
                ), LookAheadNeg),
                Expr::Delegate { inner: String::from("\\b"), size: 0 }])

        );
    }

    #[test]
    fn ignore_whitespace() {
        assert_eq!(p("(?x: a )"), p("a"));
        assert_eq!(p("(?x: a # ) bobby tables\n b )"), p("ab"));
        assert_eq!(p("(?x: a | b )"), p("a|b"));
        assert_eq!(p("(?x: ( a b ) )"), p("(ab)"));
        assert_eq!(p("(?x: a + )"), p("a+"));
        assert_eq!(p("(?x: a {2} )"), p("a{2}"));
        assert_eq!(p("(?x: a { 2 } )"), p("a{2}"));
        assert_eq!(p("(?x: a { 2 , } )"), p("a{2,}"));
        assert_eq!(p("(?x: a { , 2 } )"), p("a{,2}"));
        assert_eq!(p("(?x: a { 2 , 3 } )"), p("a{2,3}"));
        assert_eq!(p("(?x: a { 2 , 3 } ? )"), p("a{2,3}?"));
        assert_eq!(p("(?x: ( ? i : . ) )"), p("(?i:.)"));
        assert_eq!(p("(?x: ( ?= a ) )"), p("(?=a)"));
        assert_eq!(p("(?x: [ ^ a - z ] )"), p("[^a-z]"));
        assert_eq!(p("(?x: [ : a s c i i : ] )"), p("[:ascii:]"));
        assert_eq!(p("(?x: [ \\] ] )"), p("[\\]]"));
        assert_eq!(p("(?x: [ \\] \\\\] )"), p("[\\]\\\\]"));
        assert_eq!(p("(?x: a (?-x:#) b )"), p("a#b"));
    }

}
