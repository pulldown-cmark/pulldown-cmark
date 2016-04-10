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

pub struct Regex {
    // TODO: may want make this an enum with an option to be a
    // thin wrapper around regex::Regex, in the non-fancy case.
    prog: Prog,
    // TODO: might have separate prog for supporting starting pos
    n_groups: usize,
}

pub struct Captures<'t> {
    text: &'t str,
    saves: Vec<usize>,
}

impl Regex {
    pub fn new(re: &str) -> Result<Regex> {
        let (raw_e, backrefs) = try!(Expr::parse(re));

        // wrapper to search for re at arbitrary start position,
        // and to capture the match bounds
        let e = Expr::Concat(vec![
            Expr::Repeat {
                child: Box::new(Expr::Any),
                lo: 0, hi: usize::MAX, greedy: false
            },
            Expr::Group(Box::new(
                raw_e
            ))
        ]);

        let a = Analysis::analyze(&e, &backrefs);
        let p = compile(&a);
        Ok(Regex {
            prog: p,
            n_groups: a.n_groups(),
        })
    }

    pub fn is_match(&self, text: &str) -> bool {
        vm::run(&self.prog, text, 0, 0).is_some()
    }

    pub fn find(&self, text: &str) -> Option<(usize, usize)> {
        vm::run(&self.prog, text, 0, 0).map(|saves|
            (saves[0], saves[1])
        )
    }

    pub fn captures<'t>(&self, text: &'t str) -> Option<Captures<'t>> {
        vm::run(&self.prog, text, 0, 0).map(|mut saves| {
            saves.truncate(self.n_groups * 2);
            Captures {
                text: text,
                saves: saves
            }
        })
    }

    // for debugging only
    pub fn debug_print(&self) {
        self.prog.debug_print();
    }
}

impl<'t> Captures<'t> {
    pub fn pos(&self, i: usize) -> Option<(usize, usize)> {
        if i >= self.saves.len() {
            return None;
        }
        let lo = self.saves[i * 2];
        if lo == std::usize::MAX {
            return None;
        }
        let hi = self.saves[i * 2 + 1];
        Some((lo, hi))
    }

    pub fn at(&self, i: usize) -> Option<&'t str> {
        self.pos(i).map(|(lo, hi)|
            &self.text[lo..hi]
        )
    }

    // TODO: I don't think this matches regex-rs semantics; verify
    pub fn len(&self) -> usize {
        self.saves.len() / 2
    }

    // TODO: as above
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

}

#[derive(Debug)]
pub enum Error {
    ParseError,
    UnclosedOpenParen,
    InvalidRepeat,
    RecursionExceeded,
}

// impl error traits (::std::error::Error, fmt::Display)

// Access to the AST. This is public for now but may change.

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Empty,
    Any,  // nl sensitivity
    Literal {
        val: String,
        // case insensitive flag
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

#[derive(Debug, PartialEq, Eq)]
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

impl Expr {
    pub fn parse(re: &str) -> Result<(Expr, BitSet)> {
        Parser::parse(re)
    }

    pub fn to_str(&self, buf: &mut String, precedence: u8) {
        match *self {
            Expr::Empty => (),
            Expr::Any => buf.push('.'),
            Expr::Literal{ ref val } => buf.push_str(val),  // TODO:  quoting?
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

fn codepoint_len(b: u8) -> usize {
    match b {
        b if b < 0x80 => 1,
        b if b < 0xe0 => 2,
        b if b < 0xf0 => 3,
        _ => 4
    }
}

// if this returns false, then there is no possible backref in the re

/*
pub fn detect_possible_backref(re: &str) -> bool {
    let mut last = b'\x00';
    for b in re.as_bytes() {
        if b'0' <= *b && *b <= b'9' && last == b'\\' { return true; }
        last = *b;
    }
    false
}
*/

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

#[cfg(test)]
mod tests {
    use Expr;
    use LookAround::*;
    use detect_possible_backref;
    use std::usize;

    fn p(s: &str) -> Expr { Expr::parse(s).unwrap().0 }

    #[test]
    fn any() {
        assert_eq!(p("."), Expr::Any);
    }

    #[test]
    fn literal() {
        assert_eq!(p("a"), Expr::Literal { val: String::from("a") });
    }

    #[test]
    fn concat() {
        assert_eq!(p("ab"), Expr::Concat(vec![
            Expr::Literal { val: String::from("a") },
            Expr::Literal { val: String::from("b") },
        ]));
    }

    #[test]
    fn alt() {
        assert_eq!(p("a|b"), Expr::Alt(vec![
            Expr::Literal { val: String::from("a") },
            Expr::Literal { val: String::from("b") },
        ]));
    }

    #[test]
    fn group() {
        assert_eq!(p("(a)"), Expr::Group(Box::new(
            Expr::Literal { val: String::from("a") },
        )));
    }

    #[test]
    fn repeat() {
        assert_eq!(p("a{2,42}"), Expr::Repeat{ child: Box::new(
            Expr::Literal { val: String::from("a") },
        ), lo: 2, hi: 42, greedy: true });
        assert_eq!(p("a{2,}"), Expr::Repeat{ child: Box::new(
            Expr::Literal { val: String::from("a") },
        ), lo: 2, hi: usize::MAX, greedy: true });
        assert_eq!(p("a{2}"), Expr::Repeat{ child: Box::new(
            Expr::Literal { val: String::from("a") },
        ), lo: 2, hi: 2, greedy: true });
        assert_eq!(p("a{,2}"), Expr::Repeat{ child: Box::new(
            Expr::Literal { val: String::from("a") },
        ), lo: 0, hi: 2, greedy: true });

        assert_eq!(p("a{2,42}?"), Expr::Repeat{ child: Box::new(
            Expr::Literal { val: String::from("a") },
        ), lo: 2, hi: 42, greedy: false });
        assert_eq!(p("a{2,}?"), Expr::Repeat{ child: Box::new(
            Expr::Literal { val: String::from("a") },
        ), lo: 2, hi: usize::MAX, greedy: false });
        assert_eq!(p("a{2}?"), Expr::Repeat{ child: Box::new(
            Expr::Literal { val: String::from("a") },
        ), lo: 2, hi: 2, greedy: false });
        assert_eq!(p("a{,2}?"), Expr::Repeat{ child: Box::new(
            Expr::Literal { val: String::from("a") },
        ), lo: 0, hi: 2, greedy: false });
    }

    #[test]
    fn delegate_zero() {
        assert_eq!(p("\\b"), Expr::Delegate {
            inner: String::from("\\b"), size: 0
        });
    }

    #[test]
    fn delegate_named_group() {
        assert_eq!(p("\\P{Greek}"), Expr::Delegate {
            inner: String::from("\\P{Greek}"), size: 1
        });
    }

    #[test]
    fn backref() {
        assert_eq!(p("\\42"), Expr::Backref(42));
    }

    #[test]
    fn lookaround() {
        assert_eq!(p("(?=a)"), Expr::LookAround(Box::new(
            Expr::Literal { val: String::from("a") },
        ), LookAhead));
        assert_eq!(p("(?!a)"), Expr::LookAround(Box::new(
            Expr::Literal { val: String::from("a") },
        ), LookAheadNeg));
        assert_eq!(p("(?<=a)"), Expr::LookAround(Box::new(
            Expr::Literal { val: String::from("a") },
        ), LookBehind));
        assert_eq!(p("(?<!a)"), Expr::LookAround(Box::new(
            Expr::Literal { val: String::from("a") },
        ), LookBehindNeg));
    }

    #[test]
    fn shy_group() {
        assert_eq!(p("(?:ab)c"), Expr::Concat(vec![
            Expr::Concat(vec![
                Expr::Literal { val: String::from("a") },
                Expr::Literal { val: String::from("b") },
            ]),
            Expr::Literal { val: String::from("c") },
        ]));
    }

    #[test]
    fn lifetime() {
        assert_eq!(p("\\'[a-zA-Z_][a-zA-Z0-9_]*(?!\\')\\b"),

            Expr::Concat(vec![
                Expr::Delegate { inner: String::from("\\\'"), size: 1 },
                Expr::Delegate { inner: String::from("[a-zA-Z_]"), size: 1 },
                Expr::Repeat { child: Box::new(
                    Expr::Delegate { inner: String::from("[a-zA-Z0-9_]"), size: 1 }
                ), lo: 0, hi: usize::MAX, greedy: true },
                Expr::LookAround(Box::new(
                    Expr::Delegate { inner: String::from("\\\'"), size: 1 }
                ), LookAheadNeg),
                Expr::Delegate { inner: String::from("\\b"), size: 0 }]));

    }

    // tests for to_str

    #[test]
    fn to_str_concat_alt() {
        let mut s = String::new();
        let e = Expr::Concat(vec![
            Expr::Alt(vec![
                Expr::Literal { val: String::from("a") },
                Expr::Literal { val: String::from("b") },
            ]),
            Expr::Literal { val: String::from("c") },
        ]);
        e.to_str(&mut s, 0);
        assert_eq!(s, "(?:a|b)c");
    }

    #[test]
    fn to_str_rep_concat() {
        let mut s = String::new();
        let e = Expr::Repeat{ child: Box::new(
            Expr::Concat(vec![
                Expr::Literal { val: String::from("a") },
                Expr::Literal { val: String::from("b") },
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
                Expr::Literal { val: String::from("a") },
                Expr::Literal { val: String::from("b") },
            ])
        ));
        e.to_str(&mut s, 0);
        assert_eq!(s, "(a|b)");
    }

    #[test]
    fn detect_backref() {
        assert_eq!(detect_possible_backref("a0a1a2"), false);
        assert_eq!(detect_possible_backref("a0a1\\a2"), false);
        assert_eq!(detect_possible_backref("a0a\\1a2"), true);
        assert_eq!(detect_possible_backref("a0a1a2\\"), false);
    }
}
