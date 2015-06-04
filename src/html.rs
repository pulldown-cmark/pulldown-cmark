
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

use std::fmt::{self, Write};

use parse::{Event, Tag};
use parse::Event::{Start, End, Text, Html, InlineHtml, SoftBreak, HardBreak};
use escape::{escape_html, escape_href};

struct Ctx<'b, I, W: 'b> {
    iter: I,
    buf: FreshLineAdapter<'b, W>,
}

struct FreshLineAdapter<'b, W: 'b> {
    writer: &'b mut W,
    has_fresh_line: bool,
}

impl<'b, W> Write for FreshLineAdapter<'b, W> where W: Write + 'b {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        if s.len() > 0 {
            self.has_fresh_line = s.as_bytes()[s.len()-1] == b'\n';
            self.writer.write_str(s)
        } else {
            Ok(())
        }
    }
    #[inline]
    fn write_char(&mut self, c: char) -> fmt::Result {
        self.has_fresh_line = c == '\n';
        self.writer.write_char(c)
    }
}

impl<'a, 'b, I: Iterator<Item=Event<'a>>, W: Write> Ctx<'b, I, W> {
    fn fresh_line(&mut self) -> fmt::Result {
        if !self.buf.has_fresh_line {
            self.buf.write_char('\n')
        } else {
            Ok(())
        }
    }

    pub fn run(&mut self) -> fmt::Result {
        loop {
            match self.iter.next() {
                Some(event) => {
                    try!(match event {
                        Start(tag) => self.start_tag(tag),
                        End(tag) => self.end_tag(tag),
                        Text(text) => escape_html(&mut self.buf, &text, false),
                        Html(html) => self.buf.write_str(&html),
                        InlineHtml(html) => self.buf.write_str(&html),
                        SoftBreak => self.buf.write_char('\n'),
                        HardBreak => self.buf.write_str("<br />\n")
                    });
                }
                None => break
            }
        }
        Ok(())
    }

    fn start_tag(&mut self, tag: Tag) -> fmt::Result {
        match tag {
            Tag::Paragraph =>  {
                try!(self.fresh_line());
                self.buf.write_str("<p>")
            }
            Tag::Rule => {
                try!(self.fresh_line());
                self.buf.write_str("<hr />\n")
            }
            Tag::Header(level) => {
                try!(self.fresh_line());
                try!(self.buf.write_str("<h"));
                try!(self.buf.write_char((b'0' + level as u8) as char));
                self.buf.write_char('>')
            }
            Tag::BlockQuote => {
                try!(self.fresh_line());
                self.buf.write_str("<blockquote>\n")
            }
            Tag::CodeBlock(info) => {
                try!(self.fresh_line());
                let lang = info.split(' ').next().unwrap();
                if lang.is_empty() {
                    self.buf.write_str("<pre><code>")
                } else {
                    try!(self.buf.write_str("<pre><code class=\"language-"));
                    try!(escape_html(&mut self.buf, lang, false));
                    self.buf.write_str("\">")
                }
            }
            Tag::List(Some(1)) => {
                try!(self.fresh_line());
                self.buf.write_str("<ol>\n")
            }
            Tag::List(Some(start)) => {
                try!(self.fresh_line());
                write!(self.buf, "<ol start=\"{}\">\n", start)
            }
            Tag::List(None) => {
                try!(self.fresh_line());
                self.buf.write_str("<ul>\n")
            }
            Tag::Item => {
                try!(self.fresh_line());
                self.buf.write_str("<li>")
            }
            Tag::Emphasis => self.buf.write_str("<em>"),
            Tag::Strong => self.buf.write_str("<strong>"),
            Tag::Code => self.buf.write_str("<code>"),
            Tag::Link(dest, title) => {
                try!(self.buf.write_str("<a href=\""));
                try!(escape_href(&mut self.buf, &dest));
                if !title.is_empty() {
                    try!(self.buf.write_str("\" title=\""));
                    try!(escape_html(&mut self.buf, &title, false));
                }
                self.buf.write_str("\">")
            }
            Tag::Image(dest, title) => {
                try!(self.buf.write_str("<img src=\""));
                try!(escape_href(&mut self.buf, &dest));
                try!(self.buf.write_str("\" alt=\""));
                try!(self.raw_text());
                if !title.is_empty() {
                    try!(self.buf.write_str("\" title=\""));
                    try!(escape_html(&mut self.buf, &title, false));
                }
                self.buf.write_str("\" />")
            }
        }
    }

    fn end_tag(&mut self, tag: Tag) -> fmt::Result {
        match tag {
            Tag::Paragraph => self.buf.write_str("</p>\n"),
            Tag::Rule => Ok(()),
            Tag::Header(level) => {
                try!(self.buf.write_str("</h"));
                try!(self.buf.write_char((b'0' + level as u8) as char));
                self.buf.write_str(">\n")
            }
            Tag::BlockQuote => self.buf.write_str("</blockquote>\n"),
            Tag::CodeBlock(_) => self.buf.write_str("</code></pre>\n"),
            Tag::List(Some(_)) => self.buf.write_str("</ol>\n"),
            Tag::List(None) => self.buf.write_str("</ul>\n"),
            Tag::Item => self.buf.write_str("</li>\n"),
            Tag::Emphasis => self.buf.write_str("</em>"),
            Tag::Strong => self.buf.write_str("</strong>"),
            Tag::Code => self.buf.write_str("</code>"),
            Tag::Link(_, _) => self.buf.write_str("</a>"),
            Tag::Image(_, _) => Ok(())  // shouldn't happen, handled in start
        }
    }

    // run raw text, consuming end tag
    fn raw_text(&mut self) -> fmt::Result {
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
                        Text(text) => try!(escape_html(&mut self.buf, &text, false)),
                        Html(_) => (),
                        InlineHtml(html) => try!(escape_html(&mut self.buf, &html, false)),
                        SoftBreak | HardBreak => try!(self.buf.write_char(' ')),
                    }
                }
                None => break
            }
        }
        Ok(())
    }
}

pub fn push_html<'a, I: Iterator<Item=Event<'a>>, W: Write>(buf: &mut W, iter: I) -> fmt::Result {
    let mut ctx = Ctx {
        iter: iter,
        buf: FreshLineAdapter {
            writer: buf,
            has_fresh_line: true,
        },
    };
    ctx.run()
}
