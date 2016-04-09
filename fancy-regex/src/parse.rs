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
        lazy_static! {
            static ref BACKREF: Regex = Regex::new(
                "^\\\\(\\d+)"
            ).unwrap();
        }
        if ix == self.re.len() {
            return Ok((ix, Expr::Empty));
        }
        match self.re.as_bytes()[ix] {
            b'.' => {
                Ok((ix + 1, Expr::Any))
            }
            b'(' => return self.parse_group(ix, depth),
            b'\\' => {
                if let Some(caps) = BACKREF.captures(&self.re[ix..]) {
                    let group = usize::from_str(caps.at(1).unwrap()).unwrap();
                    let ix = ix + caps.pos(1).unwrap().1;
                    self.backrefs.insert(group);
                    return Ok((ix, Expr::Backref(group)));
                }
                self.parse_delegate(ix)
            }
            b'+' | b'*' | b'?' | b'|' | b')' | b']' | b'{' | b'}' =>
                Ok((ix, Expr::Empty)),
            b'[' | b'^' | b'$' => self.parse_delegate(ix),
            b => {
                // TODO: probably want to match multiple codepoints
                let next = ix + codepoint_len(b);
                Ok((next, Expr::Literal {
                    val: String::from(&self.re[ix..next])
                }))
            }
        }
    }

    fn parse_delegate(&self, ix: usize) -> Result<(usize, Expr)> {
        lazy_static! {
            static ref ZERO_WIDTH: Regex = Regex::new(
                "^(\\^|\\$|\\\\[AzbB])"
            ).unwrap();
            static ref ONE_CHAR: Regex = Regex::new(
                "^(\\\\([!-/:-@\\[-`\\{-~aftnrv]|x[0-9a-fA-F]{2}|x\\{[0-9a-fA-F]{1,6}\\})\
                |\\.|\\[(\\\\.|[^\\\\\\]])+\\]|\\\\[dDsSwW]|\\\\[pP](\\w|\\{\\w+\\}))"
            ).unwrap();
        }
        let (size, x) = if let Some(caps) = ZERO_WIDTH.captures(&self.re[ix..]) {
            (0, caps.at(0).unwrap())
        } else {
            if let Some(caps) = ONE_CHAR.captures(&self.re[ix..]) {
                (1, caps.at(0).unwrap())
            } else {
                return Ok((ix, Expr::Empty));
            }
        };
        Ok((ix + x.len(), Expr::Delegate { inner: String::from(x), size: size }))
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

