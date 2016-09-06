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

//! An implementation of regexes, supporting a relatively rich set of features, including backreferences and look-around.

#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate bit_set;
extern crate memchr;

use std::usize;
use bit_set::BitSet;

// These modules are pub so examples/toy.rs can access them,
// but we'll want to revisit that.
pub mod parse;
pub mod analyze;
pub mod compile;
pub mod vm;

use parse::Parser;
use analyze::Analysis;
use compile::compile;
use vm::Prog;

const MAX_RECURSION: usize = 64;

// the public API

pub type Result<T> = ::std::result::Result<T, Error>;

// We use one Error type for both compile time and run time errors,
// to minimize the boilerplate for callers.
#[derive(Debug)]
pub enum Error {
    // Compile time errors
    ParseError,
    UnclosedOpenParen,
    InvalidRepeat,
    RecursionExceeded,
    LookBehindNotConst,
    TrailingBackslash,
    InvalidEscape,
    UnclosedUnicodeName,
    InvalidHex,
    InvalidCodepointValue,
    InvalidClass,
    UnknownFlag,
    NonUnicodeUnsupported,
    InvalidBackref,
    InnerError(regex::Error),

    // Run time errors
    StackOverflow,
}


pub enum Regex {
    // Do we want to box this? It's pretty big...
    Wrap {
        inner: regex::Regex,
        inner1: Option<Box<regex::Regex>>,
    },
    Impl {
        prog: Prog,
        n_groups: usize,
    }
}

pub enum Captures<'t> {
    Wrap {
        inner: regex::Captures<'t>,

        // starting position, in _from_pos variants
        offset: usize,

        enclosing_groups: usize,
    },
    Impl {
        text: &'t str,
        saves: Vec<usize>,
    }
}

pub struct SubCaptures<'t> {
    caps: &'t Captures<'t>,
    i: usize,
}

impl Regex {
    pub fn new(re: &str) -> Result<Regex> {
        let (raw_e, backrefs) = try!(Expr::parse(re));

        // wrapper to search for re at arbitrary start position,
        // and to capture the match bounds
        let e = Expr::Concat(vec![
            Expr::Repeat {
                child: Box::new(Expr::Any { newline: true }),
                lo: 0, hi: usize::MAX, greedy: false
            },
            Expr::Group(Box::new(
                raw_e
            ))
        ]);

        let a = Analysis::analyze(&e, &backrefs);

        let inner_info = &a.infos[4];  // references inner expr
        if !inner_info.hard {
            // easy case, wrap regex

            // we do our own to_str because escapes are different
            let mut re_cooked = String::new();
            // same as raw_e above, but it was moved, so traverse to find it
            let raw_e = match e {
                Expr::Concat(ref v) =>
                    match v[1] {
                        Expr::Group(ref child) => child,
                        _ => unreachable!()
                    },
                _ => unreachable!()
            };
            raw_e.to_str(&mut re_cooked, 0);
            let inner = try!(compile::compile_inner(&re_cooked));
            let inner1 = if inner_info.looks_left {
                // create regex to handle 1-char look-behind
                let re1 = ["^(?s:.)+?(", re_cooked.as_str(), ")"].concat();
                let compiled = try!(compile::compile_inner(&re1));
                Some(Box::new(compiled))
            } else {
                None
            };
            return Ok(Regex::Wrap {
                inner: inner,
                inner1: inner1,
            });
        }

        let p = try!(compile(&a));
        Ok(Regex::Impl {
            prog: p,
            n_groups: a.n_groups(),
        })
    }

    pub fn is_match(&self, text: &str) -> Result<bool> {
        match *self {
            Regex::Wrap { ref inner, .. } => Ok(inner.is_match(text)),
            Regex::Impl { ref prog, .. } => {
                let result = try!(vm::run(prog, text, 0, 0));
                Ok(result.is_some())
            }
        }
    }

    pub fn find(&self, text: &str) -> Result<Option<(usize, usize)>> {
        match *self {
            Regex::Wrap { ref inner, .. } => Ok(inner.find(text)),
            Regex::Impl { ref prog, .. } => {
                let result = try!(vm::run(prog, text, 0, 0));
                Ok(result.map(|saves| (saves[0], saves[1])))
            }
        }
    }

    pub fn captures<'t>(&self, text: &'t str) -> Result<Option<Captures<'t>>> {
        match *self {
            Regex::Wrap { ref inner, .. } =>
                Ok(inner.captures(text).map(|caps| Captures::Wrap {
                    inner: caps,
                    offset: 0,
                    enclosing_groups: 0,
                })),
            Regex::Impl { ref prog, n_groups } => {
                let result = try!(vm::run(prog, text, 0, 0));
                Ok(result.map(|mut saves| {
                    saves.truncate(n_groups * 2);
                    Captures::Impl {
                        text: text,
                        saves: saves
                    }
                }))
            }
        }
    }

    pub fn captures_from_pos<'t>(&self, text: &'t str, pos: usize) ->
            Result<Option<Captures<'t>>> {
        match *self {
            Regex::Wrap { ref inner, ref inner1 } => {
                if inner1.is_none() || pos == 0 {
                    Ok(inner.captures(&text[pos..]).map(|caps| Captures::Wrap {
                        inner: caps,
                        offset: pos,
                        enclosing_groups: 0,
                    }))
                } else {
                    let ix = prev_codepoint_ix(text, pos);
                    let inner1 = inner1.as_ref().unwrap();
                    Ok(inner1.captures(&text[ix..]).map(|caps| Captures::Wrap {
                        inner: caps,
                        offset: ix,
                        enclosing_groups: 1,
                    }))
                }
            }
            Regex::Impl { ref prog, n_groups } => {
                let result = try!(vm::run(prog, text, pos, 0));
                Ok(result.map(|mut saves| {
                    saves.truncate(n_groups * 2);
                    Captures::Impl {
                        text: text,
                        saves: saves
                    }
                }))
            }
        }
    }

    // for debugging only
    pub fn debug_print(&self) {
        match *self {
            Regex::Wrap { ref inner, .. } => println!("wrapped {:?}", inner),
            Regex::Impl { ref prog, .. } => prog.debug_print()
        }
    }
}

impl<'t> Captures<'t> {
    pub fn pos(&self, i: usize) -> Option<(usize, usize)> {
        match *self {
            Captures::Wrap { ref inner, ref offset, enclosing_groups } => {
                inner.pos(i + enclosing_groups).map((|(lo, hi)|
                    (lo + offset, hi + offset)))
            }
            Captures::Impl { ref saves, .. } => {
                if i >= saves.len() {
                    return None;
                }
                let lo = saves[i * 2];
                if lo == std::usize::MAX {
                    return None;
                }
                let hi = saves[i * 2 + 1];
                Some((lo, hi))
            }
        }
    }

    pub fn at(&self, i: usize) -> Option<&'t str> {
        match *self {
            Captures::Wrap { ref inner, enclosing_groups, .. } => {
                inner.at(i + enclosing_groups)
            }
            Captures::Impl { text, .. } => {
                self.pos(i).map(|(lo, hi)|
                    &text[lo..hi]
                )
            }
        }
    }

    pub fn iter(&'t self) -> SubCaptures<'t> {
        SubCaptures { caps: self, i: 0 }
    }

    pub fn len(&self) -> usize {
        match *self {
            Captures::Wrap { ref inner, enclosing_groups, .. } => {
                inner.len() - enclosing_groups
            }
            Captures::Impl { ref saves, .. } => saves.len() / 2
        }
    }

    pub fn is_empty(&self) -> bool {
        match *self {
            Captures::Wrap { ref inner, enclosing_groups, .. } =>
                inner.len() == enclosing_groups,
            Captures::Impl { ref saves, .. } => saves.is_empty()
        }
    }
}

impl<'t> Iterator for SubCaptures<'t> {
    type Item = Option<&'t str>;

    fn next(&mut self) -> Option<Option<&'t str>> {
        if self.i < self.caps.len() {
            let result = self.caps.at(self.i);
            self.i += 1;
            Some(result)
        } else {
            None
        }
    }
}

// TODO: might be nice to implement ExactSizeIterator etc for SubCaptures

// impl error traits (::std::error::Error, fmt::Display)

// Access to the AST. This is public for now but may change.

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Empty,
    Any { newline: bool },
    StartText,
    EndText,
    StartLine,
    EndLine,
    Literal {
        val: String,
        casei: bool,
    },
    Concat(Vec<Expr>),
    Alt(Vec<Expr>),
    Group(Box<Expr>),
    LookAround(Box<Expr>, LookAround),
    Repeat {
        child: Box<Expr>,
        lo: usize,
        hi: usize,
        greedy: bool,
    },
    Delegate {
        inner: String,
        size: usize,  // TODO: move into analysis result
    },
    Backref(usize),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LookAround {
    LookAhead,
    LookAheadNeg,
    LookBehind,
    LookBehindNeg,
}

// silly to write my own, but this is super-fast for the common 1-digit
// case.
fn push_usize(s: &mut String, x: usize) {
    if x >= 10 {
        push_usize(s, x / 10);
        s.push((b'0' + (x % 10) as u8) as char);
    } else {
        s.push((b'0' + (x as u8)) as char);
    }
}

fn push_quoted(buf: &mut String, s: &str) {
    for c in s.chars() {
        match c {
            '\\' | '.' | '+' | '*' | '?' | '(' | ')' | '|' |
            '[' | ']' | '{' | '}' | '^' | '$' | '#' => buf.push('\\'),
            _ => ()
        }
        buf.push(c);
    }
}

impl Expr {
    pub fn parse(re: &str) -> Result<(Expr, BitSet)> {
        Parser::parse(re)
    }

    pub fn to_str(&self, buf: &mut String, precedence: u8) {
        match *self {
            Expr::Empty => (),
            Expr::Any { newline } => buf.push_str(
                if newline { "(?s:.)" } else { "." }
            ),
            Expr::Literal{ ref val, casei } => {
                if casei { buf.push_str("(?i:"); }
                push_quoted(buf, val);
                if casei { buf.push_str(")"); }
            }
            Expr::StartText => buf.push('^'),
            Expr::EndText => buf.push('$'),
            Expr::StartLine => buf.push_str("(?m:^)"),
            Expr::EndLine => buf.push_str("(?m:$)"),
            Expr::Concat(ref children) => {
                if precedence > 1 {
                    buf.push_str("(?:");
                }
                for child in children {
                    child.to_str(buf, 2);
                }
                if precedence > 1 {
                    buf.push(')')
                }
            }
            Expr::Alt(ref children) => {
                if precedence > 0 {
                    buf.push_str("(?:");
                }
                children[0].to_str(buf, 1);
                for child in &children[1..] {
                    buf.push('|');
                    child.to_str(buf, 1);
                }
                if precedence > 0 {
                    buf.push(')');
                }
            }
            Expr::Group(ref child) => {
                buf.push('(');
                child.to_str(buf, 0);
                buf.push(')');
            }
            Expr::Repeat{ ref child, lo, hi, greedy } => {
                if precedence > 2 {
                    buf.push_str("(?:");
                }
                child.to_str(buf, 3);
                buf.push('{');
                push_usize(buf, lo);
                buf.push(',');
                if hi != usize::MAX {
                    push_usize(buf, hi);
                }
                buf.push('}');
                if !greedy {
                    buf.push('?');
                }
                if precedence > 2 {
                    buf.push(')');
                }
            }
            Expr::Delegate{ ref inner, .. } => {
                // at the moment, delegate nodes are just atoms
                buf.push_str(inner);
            }
            _ => panic!("attempting to format hard expr")
        }
    }
}

// precondition: ix > 0
fn prev_codepoint_ix(s: &str, mut ix: usize) -> usize {
    let bytes = s.as_bytes();
    loop {
        ix -= 1;
        // fancy bit magic for ranges 0..0x80 + 0xc0..
        if (bytes[ix] as i8) >= -0x40 {
            break;
        }
    }
    ix
}

fn codepoint_len(b: u8) -> usize {
    match b {
        b if b < 0x80 => 1,
        b if b < 0xe0 => 2,
        b if b < 0xf0 => 3,
        _ => 4
    }
}

// If this returns false, then there is no possible backref in the re

// Both potential implementations are turned off, because we currently
// always need to do a deeper analysis because of 1-character
// look-behind. If we could call a find_from_pos method of regex::Regex,
// it would make sense to bring this back.
/*
pub fn detect_possible_backref(re: &str) -> bool {
    let mut last = b'\x00';
    for b in re.as_bytes() {
        if b'0' <= *b && *b <= b'9' && last == b'\\' { return true; }
        last = *b;
    }
    false
}

pub fn detect_possible_backref(re: &str) -> bool {
    let mut bytes = re.as_bytes();
    loop {
        match memchr::memchr(b'\\', &bytes[..bytes.len() - 1]) {
            Some(i) => {
                bytes = &bytes[i + 1..];
                let c = bytes[0];
                if b'0' <= c && c <= b'9' { return true; }
            }
            None => return false
        }
    }
}
*/

#[cfg(test)]
mod tests {
    use Expr;
    use parse::make_literal;
    //use detect_possible_backref;

    // tests for to_str

    #[test]
    fn to_str_concat_alt() {
        let mut s = String::new();
        let e = Expr::Concat(vec![
            Expr::Alt(vec![
                make_literal("a"),
                make_literal("b"),
            ]),
            make_literal("c"),
        ]);
        e.to_str(&mut s, 0);
        assert_eq!(s, "(?:a|b)c");
    }

    #[test]
    fn to_str_rep_concat() {
        let mut s = String::new();
        let e = Expr::Repeat{ child: Box::new(
            Expr::Concat(vec![
                make_literal("a"),
                make_literal("b"),
            ])),
            lo: 2, hi: 3, greedy: true
        };
        e.to_str(&mut s, 0);
        assert_eq!(s, "(?:ab){2,3}");
    }

    #[test]
    fn to_str_group_alt() {
        let mut s = String::new();
        let e = Expr::Group(Box::new(
            Expr::Alt(vec![
                make_literal("a"),
                make_literal("b"),
            ])
        ));
        e.to_str(&mut s, 0);
        assert_eq!(s, "(a|b)");
    }

    /*
    #[test]
    fn detect_backref() {
        assert_eq!(detect_possible_backref("a0a1a2"), false);
        assert_eq!(detect_possible_backref("a0a1\\a2"), false);
        assert_eq!(detect_possible_backref("a0a\\1a2"), true);
        assert_eq!(detect_possible_backref("a0a1a2\\"), false);
    }
    */
}
