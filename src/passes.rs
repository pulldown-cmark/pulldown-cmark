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

//! Main public pull parse interface, running two passes over input.

use parse::{RawParser, Event, Tag, Options, OPTION_FIRST_PASS};
use std::collections::HashSet;

pub struct Parser<'a> {
    inner: RawParser<'a>,
    loose_lists: HashSet<usize>,
    loose_stack: Vec<bool>,
}

impl<'a> Parser<'a> {
    pub fn new(text: &'a str) -> Parser<'a> {
        Parser::new_ext(text, Options::empty())
    }
    pub fn new_ext(text: &'a str, opts: Options) -> Parser<'a> {
        Self::new_with_broken_link_callback(text, opts, None)
    }

    /// In case the parser encounters any potential links that have a broken
    /// reference (e.g `[foo]` when there is no `[foo]: ` entry at the bottom)
    /// the provided callback will be called with the reference name,
    /// and the returned pair will be used as the link name and title if not
    /// None.
    pub fn new_with_broken_link_callback(text: &'a str, mut opts: Options,
            callback: Option<&'a Fn(&str, &str) -> Option<(String, String)>>)
            -> Parser<'a>  {
        opts.remove(OPTION_FIRST_PASS);
        // first pass, collecting info
        let first_opts = opts | OPTION_FIRST_PASS;
        let mut first = RawParser::new(text, first_opts);
        while first.next().is_some() { }
        let info = first.get_info();

        // second pass
        let loose_lists = info.loose_lists;
        let second = RawParser::new_with_links_and_callback(text, opts, info.links, callback);

        //println!("loose lists: {:?}", info.loose_lists);
        Parser {
            inner: second,
            loose_lists: loose_lists,
            loose_stack: Vec::new(),
        }
    }

    pub fn get_offset(&self) -> usize {
        self.inner.get_offset()
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Event<'a>> {
        loop {
            match self.inner.next() {
                Some(event) => {
                    match event {
                        Event::Start(Tag::List(_)) => {
                            let offset = self.inner.get_offset();
                            let is_loose = self.loose_lists.contains(&offset);
                            self.loose_stack.push(is_loose);
                        }
                        Event::Start(Tag::BlockQuote) => {
                            self.loose_stack.push(true);
                        }
                        Event::Start(Tag::Paragraph) | Event::End(Tag::Paragraph) => {
                            if let Some(&false) = self.loose_stack.last() {
                                continue;
                            }
                        }
                        Event::End(Tag::List(_)) | Event::End(Tag::BlockQuote) => {
                            let _ = self.loose_stack.pop();
                        }
                        _ => ()
                    }
                    return Some(event)
                }
                None => return None
            }
        }
    }
}

// Note: there are currently no tests here, because so far there's been adequate coverage
// using the test cases from the CommonMark spec. Those can be run using:
//
//     cargo run -- -s spec.text
//
// where spec.txt is from <https://github.com/jgm/cmark/blob/master/test/spec.txt>.
//
// There are more cases that should be tested beyond that spec, and those should be added here.
