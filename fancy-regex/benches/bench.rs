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

#![feature(test)]

extern crate test;
extern crate regex;

extern crate fancy_regex;

#[cfg(test)]
mod bench {

    use test::Bencher;
    use regex::Regex;

	use fancy_regex::Expr;
    use fancy_regex::analyze::Analysis;
    use fancy_regex::compile::compile;
    use fancy_regex::vm;

    #[bench]
    fn lifetime_re(b: &mut Bencher) {
        b.iter(|| Expr::parse("\\'[a-zA-Z_][a-zA-Z0-9_]*(?!\\')\\b"));
    }

    #[bench]
    fn literal_re(b: &mut Bencher) {
        b.iter(|| Expr::parse("^(\\\\([!-/:-@\\[-`\\{-~aftnrv]|[0-7]{1,3}|x[0-9a-fA-F]{2}|x\\{[0-9a-fA-F]{1,6}\\})"));
    }

    #[bench]
    fn misc(b: &mut Bencher) {
        b.iter(|| Expr::parse("^\\p{L}|\\p{N}|\\s|.|\\d"));
    }

    #[bench]
    fn literal_re_analyze(b: &mut Bencher) {
        let (e, br) = Expr::parse("^\\\\([!-/:-@\\[-`\\{-~aftnrv]|[0-7]{1,3}|x[0-9a-fA-F]{2}|x\\{[0-9a-fA-F]{1,6}\\})").unwrap();
        b.iter(|| Analysis::analyze(&e, &br));
    }

    #[bench]
    fn literal_re_regex(b: &mut Bencher) {
        b.iter(|| Regex::new("^\\\\([!-/:-@\\[-`\\{-~aftnrv]|[0-7]{1,3}|x[0-9a-fA-F]{2}|x\\{[0-9a-fA-F]{1,6}\\})"));
    }

    /*
    #[bench]
    fn detect_backref(b: &mut Bencher) {
        b.iter(|| detect_possible_backref("^\\\\([!-/:-@\\[-`\\{-~aftnrv]|[0-7]{1,3}|x[0-9a-fA-F]{2}|x\\{[0-9a-fA-F]{1,6}\\})"));
    }
    */

    #[bench]
    fn run_backtrack(b: &mut Bencher) {
        let (e, br) = Expr::parse("^.*?(([ab]+)\\1b)").unwrap();
        let a = Analysis::analyze(&e, &br);
        let p = compile(&a).unwrap();
        b.iter(|| vm::run(&p, "babab", 0, 0))
    }

    // The following regex is a pathological case for backtracking
    // implementations, see README.md:
    #[bench]
    fn run_tricky(b: &mut Bencher) {
        let (e, br) = Expr::parse("(a|b|ab)*bc").unwrap();
        let a = Analysis::analyze(&e, &br);
        let p = compile(&a).unwrap();
        let mut s = String::new();
        for _ in 0..28 {
            s.push_str("ab");
        }
        s.push_str("ac");
        b.iter(|| vm::run(&p, &s, 0, 0))
    }

}
