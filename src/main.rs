// Copyright 2015 Google Inc. All rights reserved.
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

//! Command line tool to exercise pulldown-cmark.

extern crate getopts;

extern crate pulldown_cmark;

use pulldown_cmark::html;
use pulldown_cmark::Options;
use pulldown_cmark::Parser;

use std::env;
use std::fs::File;
use std::io::{self, Read, Write};
use std::mem;
use std::path::Path;

fn render_html(text: &str, opts: Options) -> String {
    let mut s = String::with_capacity(text.len() * 3 / 2);
    let p = Parser::new_ext(text, opts);
    html::push_html(&mut s, p);
    s
}

fn dry_run(text: &str, opts: Options) {
    let p = Parser::new_ext(text, opts);
    let count = p.count();
    println!("{} events", count);
}

fn print_events(text: &str, opts: Options) {
    let mut p = Parser::new_ext(text, opts);
    loop {
        print!("{}: ", p.get_offset());
        if let Some(event) = p.next() {
            println!("{:?}", event);
        } else {
            break;
        }
    }
    println!("EOF");
}

fn read_file(filename: &str) -> String {
    let path = Path::new(filename);
    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", path.display(), why),
        Ok(file) => file,
    };
    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't open {}: {}", path.display(), why),
        Ok(_) => s,
    }
}

// Tests in the spec (v0.26) are of the form:
//
// ```````````````````````````````` example
// <markdown input>
// .
// <expected output>
// ````````````````````````````````
struct Spec<'a> {
    spec: &'a str,
    test_n: usize,
}

impl<'a> Spec<'a> {
    pub fn new(spec: &'a str) -> Self {
        Spec { spec, test_n: 0 }
    }
}

struct TestCase<'a> {
    n: usize,
    input: &'a str,
    expected: &'a str,
}

impl<'a> TestCase<'a> {
    pub fn new(n: usize, input: &'a str, expected: &'a str) -> Self {
        TestCase { n, input, expected }
    }
}

impl<'a> Iterator for Spec<'a> {
    type Item = TestCase<'a>;

    fn next(&mut self) -> Option<TestCase<'a>> {
        let spec = self.spec;

        let i_start = match self
            .spec
            .find("```````````````````````````````` example\n")
            .map(|pos| pos + 41)
        {
            Some(pos) => pos,
            None => return None,
        };

        let i_end = match self.spec[i_start..]
            .find("\n.\n")
            .map(|pos| (pos + 1) + i_start)
        {
            Some(pos) => pos,
            None => return None,
        };

        let e_end = match self.spec[i_end + 2..]
            .find("````````````````````````````````\n")
            .map(|pos| pos + i_end + 2)
        {
            Some(pos) => pos,
            None => return None,
        };

        self.test_n += 1;
        self.spec = &self.spec[e_end + 33..];

        Some(TestCase::new(
            self.test_n,
            &spec[i_start..i_end],
            &spec[i_end + 2..e_end],
        ))
    }
}

fn run_spec(spec_text: &str, args: &[String], opts: Options) {
    let (first, last) = if args.is_empty() {
        (None, None)
    } else {
        let mut iter = args[0].split("..");
        let first = iter.next().and_then(|s| s.parse().ok());
        let last = match iter.next() {
            Some(s) => s.parse().ok(),
            None => first,
        };
        (first, last)
    };

    let spec = Spec::new(spec_text);
    let mut tests_failed = 0;
    let mut tests_run = 0;
    let mut fail_report = String::new();

    for test in spec {
        if first.map(|fst| test.n < fst).unwrap_or(false) {
            continue;
        }
        if last.map(|lst| test.n > lst).unwrap_or(false) {
            break;
        }

        if test.n % 10 == 1 {
            if test.n % 40 == 1 {
                if test.n > 1 {
                    println!();
                }
            } else {
                print!(" ");
            }
            print!("[{:3}]", test.n);
        } else if test.n % 10 == 6 {
            print!(" ");
        }

        let our_html = render_html(&test.input, opts);

        if our_html == test.expected.replace("→", "\t") {
            print!(".");
        } else {
            if tests_failed == 0 {
                fail_report = format!(
                    "\nFAIL {}:\n\n---input---\n{:?}\n\n---wanted---\n{:?}\n\n---got---\n{:?}\n",
                    test.n, test.input, test.expected, our_html
                );
            }
            print!("X");
            tests_failed += 1;
        }

        let _ = io::stdout().flush();
        tests_run += 1;
    }

    println!("\n{}/{} tests passed", tests_run - tests_failed, tests_run);
    print!("{}", fail_report);
}

fn brief(program: &str) -> String {
    format!(
        "Usage: {} [options]\n\n{}",
        program, "Reads markdown from standard input and emits HTML.",
    )
}

pub fn main() {
    let args: Vec<_> = env::args().collect();
    let mut opts = getopts::Options::new();
    opts.optflag("h", "help", "this help message");
    opts.optflag("d", "dry-run", "dry run, produce no output");
    opts.optflag("e", "events", "print event sequence instead of rendering");
    opts.optflag("T", "enable-tables", "enable GitHub-style tables");
    opts.optflag("F", "enable-footnotes", "enable Hoedown-style footnotes");
    opts.optflag(
        "S",
        "enable-strikethrough",
        "enable GitHub-style strikethrough",
    );
    opts.optflag("L", "enable-tasklists", "enable GitHub-style task lists");
    opts.optopt("s", "spec", "run tests from spec file", "FILE");
    opts.optopt("b", "bench", "run benchmark", "FILE");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            eprintln!("{}\n{}", f, opts.usage(&brief(&args[0])));
            std::process::exit(1);
        }
    };
    if matches.opt_present("help") {
        println!("{}", opts.usage(&brief(&args[0])));
        return;
    }
    let mut opts = Options::empty();
    if matches.opt_present("enable-tables") {
        opts.insert(Options::ENABLE_TABLES);
    }
    if matches.opt_present("enable-footnotes") {
        opts.insert(Options::ENABLE_FOOTNOTES);
    }
    if matches.opt_present("enable-strikethrough") {
        opts.insert(Options::ENABLE_STRIKETHROUGH);
    }
    if matches.opt_present("enable-tasklists") {
        opts.insert(Options::ENABLE_TASKLISTS);
    }
    if let Some(filename) = matches.opt_str("spec") {
        run_spec(
            &read_file(&filename).replace("→", "\t"),
            &matches.free,
            opts,
        );
    } else if let Some(filename) = matches.opt_str("bench") {
        let inp = read_file(&filename);
        for _ in 0..1000 {
            let _ = render_html(&inp, opts);
        }
    } else {
        let mut input = String::new();
        if let Err(why) = io::stdin().lock().read_to_string(&mut input) {
            panic!("couldn't read from stdin: {}", why)
        }
        if matches.opt_present("events") {
            print_events(&input, opts);
        } else if matches.opt_present("dry-run") {
            dry_run(&input, opts);
        } else {
            let mut p = Parser::new_ext(&input, opts);
            let stdio = io::stdout();
            let buffer = std::io::BufWriter::with_capacity(1024 * 1024, stdio.lock());
            html::write_html(buffer, &mut p).unwrap();
            // Since the program will now terminate and the memory will be returned
            // to the operating system anyway, there is no point in tidely cleaning
            // up all the datastructures we have used. We shouldn't do this if we'd
            // do other things after this, because this is basically intentionally
            // leaking data. Skipping cleanup let's us return a bit (~5%) faster.
            mem::forget(p);
        }
    }
}
