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

//! A simple test app for exercising and debugging the regex engine.

extern crate fancy_regex;

use fancy_regex::*;
use fancy_regex::analyze::Analysis;
use fancy_regex::compile::compile;
use std::env;
use std::str::FromStr;

fn main() {
    let mut args = env::args().skip(1);
    if let Some(cmd) = args.next() {
        if cmd == "parse" {
            if let Some(re) = args.next() {
                let e = Expr::parse(&re);
                println!("{:?}", e);
            }
        } else if cmd == "analyze" {
            if let Some(re) = args.next() {
                let (e, backrefs) = Expr::parse(&re).unwrap();
                let a = Analysis::analyze(&e, &backrefs);
                println!("{:?}", a);
            }
        } else if cmd == "compile" {
            if let Some(re) = args.next() {
                let r = Regex::new(&re).unwrap();
                r.debug_print();
            }
        } else if cmd == "run" {
            let re = args.next().expect("expected regexp argument");
            let r = Regex::new(&re).unwrap();
            let text = args.next().expect("expected text argument");
            let mut pos = 0;
            if let Some(pos_str) = args.next() {
                pos = usize::from_str(&pos_str).unwrap();
            }
            if let Some(caps) = r.captures_from_pos(&text, pos).unwrap() {
                print!("captures:");
                for i in 0..caps.len() {
                    print!(" {}:", i);
                    if let Some((lo, hi)) = caps.pos(i) {
                        print!("[{}..{}] \"{}\"", lo, hi, caps.at(i).unwrap());
                    } else {
                        print!("_");
                    }
                }
                println!("");
                for cap in caps.iter() {
                    println!("iterate {:?}", cap);
                }
            }
        } else if cmd == "trace" {
            if let Some(re) = args.next() {
                let (e, backrefs) = Expr::parse(&re).unwrap();
                let a = Analysis::analyze(&e, &backrefs);
                let p = compile(&a).unwrap();
                if let Some(s) = args.next() {
                    vm::run(&p, &s, 0, vm::OPTION_TRACE).unwrap();
                }
            }
        } else {
            println!("commands: parse <expr>, compile <expr>, run <expr> <input>");
        }
    }
}
