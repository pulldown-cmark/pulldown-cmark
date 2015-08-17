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

use pulldown_cmark::Parser;
use pulldown_cmark::{Options, OPTION_ENABLE_TABLES, OPTION_ENABLE_FOOTNOTES};
use pulldown_cmark::html;

use std::env;
use std::io;
use std::io::{Read, Write};
use std::path::Path;
use std::fs::File;

fn render_html(text: &str, opts: Options) -> String {
    let mut s = String::with_capacity(text.len() * 3 / 2);
    let p = Parser::new_ext(&text, opts);
    html::push_html(&mut s, p);
    s
}

fn dry_run(text:&str, opts: Options) {
    let p = Parser::new_ext(&text, opts);
    /*
    let events = p.collect::<Vec<_>>();
    let count = events.len();
    */
    let count = p.count();
    println!("{} events", count);
}

fn print_events(text: &str, opts: Options) {
    let mut p = Parser::new_ext(&text, opts);
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
        Ok(file) => file
    };
    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't open {}: {}", path.display(), why),
        Ok(_) => s
    }
}

fn find_test_delim(text: &str) -> Option<usize> {
    if text.starts_with(".\n") { Some(0) }
    else { text.find("\n.\n").map(|pos| pos + 1) }
}

fn run_spec(spec_text: &str, args: &[String], opts: Options) {
    //println!("spec length={}, args={:?}", spec_text.len(), args);
    let (first, last) = if args.is_empty() {
        (None, None)
    } else {
        let mut iter = args[0].split("..");
        let first = iter.next().and_then(|s| s.parse().ok());
        let last = match iter.next() {
            Some(s) => s.parse().ok(),
            None => first
        };
        (first, last)
    };

    let mut test_number = 1;
    let mut tests_failed = 0;
    let mut tests_run = 0;
    let mut line_count = 0;
    let mut tail = spec_text;
    loop {
        let rest = match find_test_delim(tail) {
            Some(pos) => &tail[pos + 2 ..],
            None => break
        };
        let (source, rest) = match find_test_delim(rest) {
            Some(pos) => (&rest[..pos], &rest[pos + 2 ..]),
            None => break
        };
        let (html, rest) = match find_test_delim(rest) {
            Some(pos) => (&rest[.. pos], &rest[pos + 2 ..]),
            None => break
        };
        if first.map(|fst| fst <= test_number).unwrap_or(true) &&
                last.map(|lst| test_number <= lst).unwrap_or(true) {
            if tests_run == 0 || line_count == 0 || (test_number % 10 == 0) {
                if line_count > 30 {
                    println!("");
                    line_count = 0;
                } else if line_count > 0 {
                    print!(" ");
                }
                print!("[{}]", test_number);
            } else if line_count > 0 && (test_number % 10) == 5 {
                print!(" ");
            }
            let our_html = render_html(&source.replace("→", "\t").replace("\n", "\r\n"), opts);
            if our_html == html {
                print!(".");
            } else {
                if tests_failed == 0 {
                    print!("FAIL {}:\n---input---\n{}\n---wanted---\n{}\n---got---\n{}",
                        test_number, source, html, our_html);
                } else {
                    print!("X");
                }
                tests_failed += 1;
            }
            let _ = io::stdout().flush();
            tests_run += 1;
            line_count += 1;
        }
        tail = rest;
        test_number += 1;
    }
    println!("\n{}/{} tests passed", tests_run - tests_failed, tests_run)
}

pub fn main() {
    let args: Vec<_> = env::args().collect();
    let mut opts = getopts::Options::new();
    opts.optflag("d", "dry-run", "dry run, produce no output");
    opts.optflag("e", "events", "print event sequence instead of rendering");
    opts.optflag("T", "enable-tables", "enable GitHub-style tables");
    opts.optflag("F", "enable-footnotes", "enable Hoedown-style footnotes");
    opts.optopt("s", "spec", "run tests from spec file", "FILE");
    opts.optopt("b", "bench", "run benchmark", "FILE");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string())
    };
    let mut opts = Options::empty();
    if matches.opt_present("enable-tables") {
        opts.insert(OPTION_ENABLE_TABLES);
    }
    if matches.opt_present("enable-footnotes") {
        opts.insert(OPTION_ENABLE_FOOTNOTES);
    }
    if let Some(filename) = matches.opt_str("spec") {
        run_spec(&read_file(&filename), &matches.free, opts);
    } else if let Some(filename) = matches.opt_str("bench") {
        let inp = read_file(&filename);
        for _ in 0..1000 {
            let _ = render_html(&inp, opts);
        }
    } else {
        let mut input = String::new();
        match io::stdin().read_to_string(&mut input) {
            Err(why) => panic!("couldn't read from stdin: {}", why),
            Ok(_) => ()
        }
        if matches.opt_present("events") {
            print_events(&input, opts);
        } else if matches.opt_present("dry-run") {
            dry_run(&input, opts);
        } else {
            print!("{}", render_html(&input, opts));
        }
    }
}
