
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

//! HTML renderer that takes an iterator of events as input.

use std::fmt::Write;

use parse::{Event, Tag};
use parse::Event::{Start, End, Text, Html, InlineHtml, SoftBreak, HardBreak};
use escape::{escape_html, escape_href};

struct Ctx<'b, I> {
    iter: I,
    buf: &'b mut String,
}

impl<'a, 'b, I: Iterator<Item=Event<'a>>> Ctx<'b, I> {
    fn fresh_line(&mut self) {
        if !(self.buf.is_empty() || self.buf.ends_with('\n')) {
            self.buf.push('\n');
        }
    }

    pub fn run(&mut self) {
        loop {
            match self.iter.next() {
                Some(event) => {
                    match event {
                        Start(tag) => self.start_tag(tag),
                        End(tag) => self.end_tag(tag),
                        Text(text) => escape_html(self.buf, &text, false),
                        Html(html) => self.buf.push_str(&html),
                        InlineHtml(html) => self.buf.push_str(&html),
                        SoftBreak => self.buf.push('\n'),
                        HardBreak => self.buf.push_str("<br />\n")
                    }
                }
                None => break
            }
        }
    }

    fn start_tag(&mut self, tag: Tag) {
        match tag {
            Tag::Paragraph =>  {
                self.fresh_line();
                self.buf.push_str("<p>");
            }
            Tag::Rule => {
                self.fresh_line();
                self.buf.push_str("<hr />\n")
            }
            Tag::Header(level) => {
                self.fresh_line();
                self.buf.push_str("<h");
                self.buf.push((b'0' + level as u8) as char);
                self.buf.push('>');
            }
            Tag::BlockQuote => {
                self.fresh_line();
                self.buf.push_str("<blockquote>\n");
            }
            Tag::CodeBlock(info) => {
                self.fresh_line();
                let lang = info.split(' ').next().unwrap();
                if lang.is_empty() {
                    self.buf.push_str("<pre><code>");
                } else {
                    self.buf.push_str("<pre><code class=\"language-");
                    escape_html(self.buf, lang, false);
                    self.buf.push_str("\">");
                }
            }
            Tag::List(Some(1)) => {
                self.fresh_line();
                self.buf.push_str("<ol>\n");
            }
            Tag::List(Some(start)) => {
                self.fresh_line();
                let _ = write!(self.buf, "<ol start=\"{}\">\n", start);
            }
            Tag::List(None) => {
                self.fresh_line();
                self.buf.push_str("<ul>\n");
            }
            Tag::Item => {
                self.fresh_line();
                self.buf.push_str("<li>");
            }
            Tag::Emphasis => self.buf.push_str("<em>"),
            Tag::Strong => self.buf.push_str("<strong>"),
            Tag::Code => self.buf.push_str("<code>"),
            Tag::Link(dest, title) => {
                self.buf.push_str("<a href=\"");
                escape_href(self.buf, &dest);
                if !title.is_empty() {
                    self.buf.push_str("\" title=\"");
                    escape_html(self.buf, &title, false);
                }
                self.buf.push_str("\">");
            }
            Tag::Image(dest, title) => {
                self.buf.push_str("<img src=\"");
                escape_href(self.buf, &dest);
                self.buf.push_str("\" alt=\"");
                self.raw_text();
                if !title.is_empty() {
                    self.buf.push_str("\" title=\"");
                    escape_html(self.buf, &title, false);
                }
                self.buf.push_str("\" />")
            }
        }
    }

    fn end_tag(&mut self, tag: Tag) {
        match tag {
            Tag::Paragraph => self.buf.push_str("</p>\n"),
            Tag::Rule => (),
            Tag::Header(level) => {
                self.buf.push_str("</h");
                self.buf.push((b'0' + level as u8) as char);
                self.buf.push_str(">\n");
            }
            Tag::BlockQuote => self.buf.push_str("</blockquote>\n"),
            Tag::CodeBlock(_) => self.buf.push_str("</code></pre>\n"),
            Tag::List(Some(_)) => self.buf.push_str("</ol>\n"),
            Tag::List(None) => self.buf.push_str("</ul>\n"),
            Tag::Item => self.buf.push_str("</li>\n"),
            Tag::Emphasis => self.buf.push_str("</em>"),
            Tag::Strong => self.buf.push_str("</strong>"),
            Tag::Code => self.buf.push_str("</code>"),
            Tag::Link(_, _) => self.buf.push_str("</a>"),
            Tag::Image(_, _) => ()  // shouldn't happen, handled in start
        }
    }

    // run raw text, consuming end tag
    fn raw_text(&mut self) {
        let mut nest = 0;
        loop {
            match self.iter.next() {
                Some(event) => {
                    match event {
                        Start(_) => nest += 1,
                        End(_) => {
                            if nest == 0 { break; }
                            nest -= 1;
                        }
                        Text(text) => escape_html(self.buf, &text, false),
                        Html(_) => (),
                        InlineHtml(html) => escape_html(self.buf, &html, false),
                        SoftBreak | HardBreak => self.buf.push(' '),
                    }
                }
                None => break
            }
        }
    }
}

pub fn push_html<'a, I: Iterator<Item=Event<'a>>>(buf: &mut String, iter: I) {
    let mut ctx = Ctx {
        iter: iter,
        buf: buf,
    };
    ctx.run();
}
