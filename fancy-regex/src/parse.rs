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
use regex::Regex;

use codepoint_len;
use MAX_RECURSION;
use Expr;
use Error;
use LookAround::*;
use Result;

pub struct Parser<'a> {
    re: &'a str,  // source
    backrefs: BitSet,
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
        }
    }

    fn parse_re(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let (mut ix, child) = try!(self.parse_branch(ix, depth));
        if self.re[ix..].starts_with("|") {
            let mut children = vec![child];
            while self.re[ix..].starts_with("|") {
                ix += 1;
                let (next, child) = try!(self.parse_branch(ix, depth));
                children.push(child);
                ix = next;
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
            children.push(child);
            ix = next;
        }
        if children.len() == 0 {
            Ok((ix, Expr::Empty))
        } else if children.len() == 1 {
            Ok((ix, children.pop().unwrap()))
        } else {
            Ok((ix, Expr::Concat(children)))
        }
    }

    fn parse_piece(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let (mut ix, child) = try!(self.parse_atom(ix, depth));
        if ix < self.re.len() {
            // fail when child is empty?
            let (lo, hi) = match self.re.as_bytes()[ix] {
                b'?' => (0, 1),
                b'*' => (0, usize::MAX),
                b'+' => (1, usize::MAX),
                b'{' => {
                    let (next, lo, hi) = try!(parse_repeat(self.re, ix));
                    ix = next - 1;
                    (lo, hi)
                }
                _ => return Ok((ix, child))
            };
            ix += 1;
            let mut greedy = true;
            if ix < self.re.len() && self.re.as_bytes()[ix] == b'?' {
                greedy = false;
                ix += 1;
            }
            return Ok((ix, Expr::Repeat {
                child: Box::new(child),
                lo: lo,
                hi: hi,
                greedy: greedy,
            }))
        }
        Ok((ix, child))
    }

    fn parse_atom(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        if ix == self.re.len() {
            return Ok((ix, Expr::Empty));
        }
        match self.re.as_bytes()[ix] {
            b'.' => Ok((ix + 1, Expr::Any)),
            b'^' => Ok((ix + 1, Expr::StartText)),
            b'$' => Ok((ix + 1, Expr::EndText)),
            b'(' => self.parse_group(ix, depth),
            b'\\' => self.parse_escape(ix),
            b'+' | b'*' | b'?' | b'|' | b')' | b']' | b'{' | b'}' =>
                Ok((ix, Expr::Empty)),
            b'[' => self.parse_class(ix),
            b => {
                // TODO: probably want to match multiple codepoints
                let next = ix + codepoint_len(b);
                Ok((next, Expr::Literal {
                    val: String::from(&self.re[ix..next])
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
        if is_digit(b)  {
            while end < self.re.len() && is_digit(bytes[end]) {
                end += 1;
            }
            let group = usize::from_str(&self.re[ix + 1 .. end]).unwrap();
            self.backrefs.insert(group);
            return Ok((end, Expr::Backref(group)));
        } else if b == b'A' || b == b'z' || b == b'b' || b == b'B' {
            size = 0;
        } else if (b | 32) == b'd' || (b | 32) == b's' || (b | 32) == b'w' {
            // size = 1
        } else if (b | 32) == b'h' {
            let s = if b == b'h' {
                "[0-9A-Za-z]"
            } else {
                "^[0-9A-Za-z]"
            };
            let inner = String::from(s);
            return Ok((end, Expr::Delegate { inner: inner, size: size }));
        } else if b == b'x' {
            if end + 2 > self.re.len() {
                return Err(Error::InvalidHex);
            }
            let b = bytes[end];
            let s = if is_hex_digit(b) && is_hex_digit(bytes[end + 1]) {
                let start = end;
                end += 2;
                &self.re[start..end]
            } else if b == b'{' {
                let starthex = end + 1;
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
                    end = endhex + 1;
                }
                &self.re[starthex .. endhex]
            } else {
                return Err(Error::InvalidHex);
            };
            let codepoint = u32::from_str_radix(s, 16).unwrap();
            if let Some(c) = ::std::char::from_u32(codepoint) {
                let mut inner = String::with_capacity(4);
                inner.push(c);
                return Ok((end, Expr::Literal { val: inner }));
            } else {
                return Err(Error::InvalidCodepointValue);
            }
        } else if (b | 32) == b'p' {
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
        } else if b'!' <= b && b <= b'~' {
            let inner = String::from(&self.re[ix + 1..end]);
            return Ok((end, Expr::Literal { val: inner }));
        }
        // what to do with characters outside printable ASCII?
        let inner = String::from(&self.re[ix..end]);
        Ok((end, Expr::Delegate { inner: inner, size: size }))
    }

    fn parse_class(&self, ix: usize) -> Result<(usize, Expr)> {
        lazy_static! {
            static ref CLASS: Regex = Regex::new(
                r"^\[(\\.|[^\\\]])+\]"
            ).unwrap();
        }
        if let Some((_, len)) = CLASS.find(&self.re[ix..]) {
            let end = ix + len;
            let inner = String::from(&self.re[ix..end]);
            Ok((end, Expr::Delegate { inner: inner, size: 1 }))
        } else {
            Err(Error::InvalidClass)
        }
    }

    fn parse_group(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let depth = depth + 1;
        if depth >= MAX_RECURSION {
            return Err(Error::RecursionExceeded);
        }
        let ix = ix + 1;
        let (la, skip) = if self.re[ix..].starts_with("?=") {
            (Some(LookAhead), 2)
        } else if self.re[ix..].starts_with("?!") {
            (Some(LookAheadNeg), 2)
        } else if self.re[ix..].starts_with("?<=") {
            (Some(LookBehind), 3)
        } else if self.re[ix..].starts_with("?<!") {
            (Some(LookBehindNeg), 3)
        } else if self.re[ix..].starts_with("?:") {
            // TODO: shy group is just a special case of flag parsing
            (None, 2)
        } else {
            (None, 0)
        };
        let ix = ix + skip;
        let (ix, child) = try!(self.parse_re(ix, depth));
        if ix == self.re.len() {
            return Err(Error::UnclosedOpenParen);
        } else if self.re.as_bytes()[ix] != b')' {
            return Err(Error::ParseError);
        };
        let result = match la {
            Some(la) => Expr::LookAround(Box::new(child), la),
            None if skip == 0 => Expr::Group(Box::new(child)),
            None => child
        };
        Ok((ix + 1, result))
    }
}

// ix, lo, hi
fn parse_repeat(s: &str, ix: usize) -> Result<(usize, usize, usize)> {
    lazy_static! {
        static ref REPEAT: Regex = Regex::new(
            "^\\{(\\d*)(,(\\d+)?)?\\}"
        ).unwrap();
    }
    if let Some(caps) = REPEAT.captures(&s[ix..]) {
        let ix = ix + caps.pos(0).unwrap().1;
        let lo = usize::from_str(caps.at(1).unwrap()).unwrap_or(0);
        if let Some(hi_str) = caps.at(3) {
            if let Ok(hi) = usize::from_str(hi_str) {
                return Ok((ix, lo, hi));
            } else {
                return Err(Error::InvalidRepeat);
            }
        } else if caps.at(2).is_some() {
            return Ok((ix, lo, usize::MAX));
        } else {
            return Ok((ix, lo, lo));
        }
    }
    Err(Error::InvalidRepeat)
}

fn is_digit(b: u8) -> bool {
    b'0' <= b && b <= b'9'
}

fn is_hex_digit(b: u8) -> bool {
    is_digit(b) || (b'a' <= (b | 32) && (b | 32) <= b'f')
}
