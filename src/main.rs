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

#![forbid(unsafe_code)]

use pulldown_cmark::{html, Options, Parser};

use std::env;
use std::io::{self, Read};
use std::mem;

fn dry_run(text: &str, opts: Options) {
    let p = Parser::new_ext(text, opts);
    let count = p.count();
    println!("{} events", count);
}

fn print_events(text: &str, opts: Options) {
    let parser = Parser::new_ext(text, opts).into_offset_iter();
    for (event, range) in parser {
        println!("{:?}: {:?}", range, event);
    }
    println!("EOF");
}

fn brief(program: &str) -> String {
    format!(
        "Usage: {} [options]\n\n{}",
        program, "Reads markdown from standard input and emits HTML.",
    )
}

pub fn main() -> std::io::Result<()> {
    let args: Vec<_> = env::args().collect();
    let mut cli = getopts::Options::new();
    cli.optflag("h", "help", "this help message");
    cli.optflag("d", "dry-run", "dry run, produce no output");
    cli.optflag("e", "events", "print event sequence instead of rendering");
    cli.optflag("T", "enable-tables", "enable GitHub-style tables");
    cli.optflag(
        "F",
        "enable-footnotes",
        "enable Hoedown-style footnotes  (mutually exclusive with enable-standard-footnotes)",
    );
    cli.optflag(
        "N",
        "enable-standard-footnotes",
        "enable pandoc/GitHub-style (endnote) footnotes (mutually exclusive with enable-footnotes)",
    );
    cli.optflag(
        "S",
        "enable-strikethrough",
        "enable GitHub-style strikethrough",
    );
    cli.optflag("L", "enable-tasklists", "enable GitHub-style task lists");
    cli.optflag("P", "enable-smart-punctuation", "enable smart punctuation");
    cli.optflag(
        "H",
        "enable-heading-attributes",
        "enable heading attributes",
    );

    let matches = match cli.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            eprintln!("{}\n{}", f, cli.usage(&brief(&args[0])));
            std::process::exit(1);
        }
    };
    if matches.opt_present("help") {
        println!("{}", cli.usage(&brief(&args[0])));
        return Ok(());
    }
    let mut opts = Options::empty();
    if matches.opt_present("enable-tables") {
        opts.insert(Options::ENABLE_TABLES);
    }
    match (
        matches.opt_present("enable-footnotes"),
        matches.opt_present("enable-standard-footnotes"),
    ) {
        (false, false) => {}
        (true, false) => opts.insert(Options::ENABLE_FOOTNOTES),
        (false, true) => opts.insert(Options::ENABLE_STANDARD_FOOTNOTES),
        (true, true) => {
            eprintln!(
                "enable-footnotes and enable-standard-footnotes are mutually exclusive\n{usage}",
                usage = cli.usage(&brief(&args[0]))
            );
            std::process::exit(1);
        }
    };
    if matches.opt_present("enable-strikethrough") {
        opts.insert(Options::ENABLE_STRIKETHROUGH);
    }
    if matches.opt_present("enable-tasklists") {
        opts.insert(Options::ENABLE_TASKLISTS);
    }
    if matches.opt_present("enable-smart-punctuation") {
        opts.insert(Options::ENABLE_SMART_PUNCTUATION);
    }
    if matches.opt_present("enable-heading-attributes") {
        opts.insert(Options::ENABLE_HEADING_ATTRIBUTES);
    }

    let mut input = String::new();
    io::stdin().lock().read_to_string(&mut input)?;
    if matches.opt_present("events") {
        print_events(&input, opts);
    } else if matches.opt_present("dry-run") {
        dry_run(&input, opts);
    } else {
        let mut p = Parser::new_ext(&input, opts);
        let stdio = io::stdout();
        let buffer = std::io::BufWriter::with_capacity(1024 * 1024, stdio.lock());
        html::write_html_with_options(buffer, &mut p, opts)?;
        // Since the program will now terminate and the memory will be returned
        // to the operating system anyway, there is no point in tidily cleaning
        // up all the datastructures we have used. We shouldn't do this if we'd
        // do other things after this, because this is basically intentionally
        // leaking data. Skipping cleanup lets us return a bit (~5%) faster.
        mem::forget(p);
    }
    Ok(())
}
