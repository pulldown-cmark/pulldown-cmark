// Copyright 2015 Google Inc. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! Main public pull parse interface, running two passes over input.

use parse::{RawParser, Event, Tag, Options};
use std::collections::HashSet;

pub struct Parser<'a> {
    inner: RawParser<'a>,
    loose_lists: HashSet<usize>,
    loose_stack: Vec<bool>,
}

impl<'a> Parser<'a> {
    pub fn new(text: &'a str) -> Parser<'a> {
        // first pass, collecting info
        let mut opts = Options::new();
        opts.set_first_pass();
        let mut first = RawParser::new(text, opts);
        while first.next().is_some() { }
        let info = first.get_info();

        // second pass
        let loose_lists = info.loose_lists;
        let opts = Options::new();
        let second = RawParser::new_with_links(text, opts, info.links);

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

# Note: there are currently no tests here, because so far there's been adequate coverage
# using the test cases from the CommonMark spec. Those can be run using:
#
#     cargo run -- -s spec.text
#
# where spec.txt is from <https://github.com/jgm/cmark/blob/master/test/spec.txt>.
#
# There are more cases that should be tested beyond that spec, and those should be added here.
