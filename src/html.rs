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

//! HTML renderer that takes an iterator of events as input.

use std::collections::HashMap;
use std::io::{self, Write};

use crate::parse::{LinkType, Event, Tag, Alignment};
use crate::parse::Event::*;
use crate::strings::CowStr;
use crate::escape::{escape_html, escape_href};

enum TableState {
    Head,
    Body,
}

struct HtmlWriter<'a, I, W>
where
    W: Write,
{
    /// Iterator supplying events.
    iter: I,

    /// Writer to write to.
    writer: W,

    /// Whether or not the last write wrote a newline.
    end_newline: bool,

    table_state: TableState,
    table_alignments: Vec<Alignment>,
    table_cell_index: usize,
    numbers: HashMap<CowStr<'a>, usize>,
}

impl<'a, I, W> HtmlWriter<'a, I, W>
where
    I: Iterator<Item = Event<'a>>,
    W: Write,
{
    /// Writes a new line.
    fn write_newline(&mut self) -> io::Result<()> {
        self.end_newline = true;
        self.writer.write_all(&[b'\n'])
    }

    /// Writes a buffer, and tracks whether or not a newline was written.
    fn write(&mut self, bytes: &[u8], write_newline: bool) -> io::Result<()> {
        self.writer.write_all(bytes)?;

        if write_newline {
            self.write_newline()
        } else {
            if bytes.len() > 0 {
                self.end_newline = bytes[bytes.len() - 1] == b'\n';
            }
            Ok(())
        }
    }

    /// Writes a newline if data was already written to the output stream,
    /// and the previous line did not end with a newline.
    fn fresh_line(&mut self) -> io::Result<()> {
        if !self.end_newline {
            self.write_newline()
        } else {
            Ok(())
        }
    }

    pub fn run(mut self) -> io::Result<()> {
        while let Some(event) = self.iter.next() {
            match event {
                Start(tag) => {
                    self.start_tag(tag)?;
                }
                End(tag) => {
                    self.end_tag(tag)?;
                }
                Text(text) => {
                    escape_html(&mut self.writer, &text, false)?;
                    self.end_newline = text.ends_with('\n');
                }
                Html(html) | InlineHtml(html) => {
                    self.write(html.as_bytes(), false)?;
                }
                SoftBreak => {
                    self.write_newline()?;
                }
                HardBreak => {
                    self.write(b"<br />", true)?;
                }
                FootnoteReference(name) => {
                    let len = self.numbers.len() + 1;
                    self.write(b"<sup class=\"footnote-reference\"><a href=\"#", false)?;
                    escape_html(&mut self.writer, &name, false)?;
                    self.write(b"\">", false)?;
                    let number = *self.numbers.entry(name).or_insert(len);
                    self.write(format!("{}", number).as_bytes(), false)?;
                    self.write(b"</a></sup>", false)?;
                }
            }
        }
        Ok(())
    }

    /// Writes the start of an HTML tag.
    fn start_tag(&mut self, tag: Tag<'a>) -> io::Result<()> {
        match tag {
            Tag::Paragraph => {
                self.fresh_line()?;
                self.write(b"<p>", false)
            }
            Tag::Rule => {
                self.fresh_line()?;
                self.write(b"<hr />", true)
            }
            Tag::Header(level) => {
                self.fresh_line()?;
                self.write(b"<h", false)?;
                self.write(&[(b'0' + level as u8)], false)?;
                self.write(b">", false)
            }
            Tag::Table(alignments) => {
                self.table_alignments = alignments;
                self.write(b"<table>", false)
            }
            Tag::TableHead => {
                self.table_state = TableState::Head;
                self.table_cell_index = 0;
                self.write(b"<thead><tr>", false)
            }
            Tag::TableRow => {
                self.table_cell_index = 0;
                self.write(b"<tr>", false)
            }
            Tag::TableCell => {
                match self.table_state {
                    TableState::Head => {
                        self.write(b"<th", false)?;
                    }
                    TableState::Body => {
                        self.write(b"<td", false)?;
                    }
                }
                match self.table_alignments.get(self.table_cell_index) {
                    Some(&Alignment::Left) => {
                        self.write(b" align=\"left\"", false)?;
                    }
                    Some(&Alignment::Center) => {
                        self.write(b" align=\"center\"", false)?;
                    }
                    Some(&Alignment::Right) => {
                        self.write(b" align=\"right\"", false)?;
                    }
                    _ => (),
                }
                self.write(b">", false)
            }
            Tag::BlockQuote => {
                self.fresh_line()?;
                self.write(b"<blockquote>", true)
            }
            Tag::CodeBlock(info) => {
                self.fresh_line()?;
                let lang = info.split(' ').next().unwrap();
                if lang.is_empty() {
                    self.write(b"<pre><code>", false)
                } else {
                    self.write(b"<pre><code class=\"language-", false)?;
                    escape_html(&mut self.writer, lang, false)?;
                    self.write(b"\">", false)
                }
            }
            Tag::List(Some(1)) => {
                self.fresh_line()?;
                self.write(b"<ol>", true)
            }
            Tag::List(Some(start)) => {
                self.fresh_line()?;
                self.write(b"<ol start=\"", false)?;
                self.write(format!("{}", start).as_bytes(), false)?;
                self.write(b"\">", true)
            }
            Tag::List(None) => {
                self.fresh_line()?;
                self.write(b"<ul>", true)
            }
            Tag::Item => {
                self.fresh_line()?;
                self.write(b"<li>", false)
            }
            Tag::Emphasis => self.write(b"<em>", false),
            Tag::Strong => self.write(b"<strong>", false),
            Tag::Strikethrough => self.write(b"<del>", false),
            Tag::Code => self.write(b"<code>", false),
            Tag::Link(LinkType::Email, dest, title) => {
                self.write(b"<a href=\"mailto:", false)?;
                escape_href(&mut self.writer, &dest)?;
                if !title.is_empty() {
                    self.write(b"\" title=\"", false)?;
                    escape_html(&mut self.writer, &title, false)?;
                }
                self.write(b"\">", false)
            }
            Tag::Link(_link_type, dest, title) => {
                self.write(b"<a href=\"", false)?;
                escape_href(&mut self.writer, &dest)?;
                if !title.is_empty() {
                    self.write(b"\" title=\"", false)?;
                    escape_html(&mut self.writer, &title, false)?;
                }
                self.write(b"\">", false)
            }
            Tag::Image(_link_type, dest, title) => {
                self.write(b"<img src=\"", false)?;
                escape_href(&mut self.writer, &dest)?;
                self.write(b"\" alt=\"", false)?;
                self.raw_text()?;
                if !title.is_empty() {
                    self.write(b"\" title=\"", false)?;
                    escape_html(&mut self.writer, &title, false)?;
                }
                self.write(b"\" />", false)
            }
            Tag::FootnoteDefinition(name) => {
                self.fresh_line()?;
                let len = self.numbers.len() + 1;
                self.write(b"<div class=\"footnote-definition\" id=\"", false)?;
                escape_html(&mut self.writer, &*name, false)?;
                self.write(b"\"><sup class=\"footnote-definition-label\">", false)?;
                let number = *self.numbers.entry(name).or_insert(len);
                self.write(&*format!("{}", number).as_bytes(), false)?;
                self.write(b"</sup>", false)
            }
            Tag::HtmlBlock => Ok(())
        }
    }

    fn end_tag(&mut self, tag: Tag) -> io::Result<()> {
        match tag {
            Tag::Paragraph => {
                self.write(b"</p>", true)?;
            }
            Tag::Rule => (),
            Tag::Header(level) => {
                self.write(b"</h", false)?;
                self.write(&[(b'0' + level as u8)], false)?;
                self.write(b">", true)?;
            }
            Tag::Table(_) => {
                self.write(b"</tbody></table>", true)?;
            }
            Tag::TableHead => {
                self.write(b"</tr></thead><tbody>", true)?;
                self.table_state = TableState::Body;
            }
            Tag::TableRow => {
                self.write(b"</tr>", true)?;
            }
            Tag::TableCell => {
                match self.table_state {
                    TableState::Head => {
                        self.write(b"</th>", false)?;
                    }
                    TableState::Body => {
                        self.write(b"</td>", false)?;
                    }
                }
                self.table_cell_index += 1;
            }
            Tag::BlockQuote => {
                self.write(b"</blockquote>", true)?;
            }
            Tag::CodeBlock(_) => {
                self.write(b"</code></pre>", true)?;
            }
            Tag::List(Some(_)) => {
                self.write(b"</ol>", true)?;
            }
            Tag::List(None) => {
                self.write(b"</ul>", true)?;
            }
            Tag::Item => {
                self.write(b"</li>", true)?;
            }
            Tag::Emphasis => {
                self.write(b"</em>", false)?;
            }
            Tag::Strong => {
                self.write(b"</strong>", false)?;
            }
            Tag::Strikethrough => {
                self.write(b"</del>", false)?;
            }
            Tag::Code => {
                self.write(b"</code>", false)?;
            }
            Tag::Link(_, _, _) => {
                self.write(b"</a>", false)?;
            }
            Tag::Image(_, _, _) => (), // shouldn't happen, handled in start
            Tag::FootnoteDefinition(_) => {
                self.write(b"</div>", true)?;
            }
            Tag::HtmlBlock => {}
        }
        Ok(())
    }

    // run raw text, consuming end tag
    fn raw_text<'c>(&mut self) -> io::Result<()> {
        let mut nest = 0;
        while let Some(event) = self.iter.next() {
            match event {
                Start(_) => nest += 1,
                End(_) => {
                    if nest == 0 {
                        break;
                    }
                    nest -= 1;
                }
                Text(text) => {
                    escape_html(&mut self.writer, &text, false)?;
                    self.end_newline = text.ends_with('\n');
                }
                Html(_) => (),
                InlineHtml(html) => {
                    escape_html(&mut self.writer, &html, false)?;
                    self.end_newline = html.ends_with('\n');
                }
                SoftBreak | HardBreak => {
                    self.write(b" ", false)?;
                }
                FootnoteReference(name) => {
                    let len = self.numbers.len() + 1;
                    let number = *self.numbers.entry(name).or_insert(len);
                    self.write(&*format!("[{}]", number).as_bytes(), false)?;
                }
            }
        }
        Ok(())
    }
}

/// Iterate over an `Iterator` of `Event`s, generate HTML for each `Event`, and
/// push it to a `String`.
///
/// # Examples
///
/// ```
/// use pulldown_cmark::{html, Parser};
///
/// let markdown_str = r#"
/// hello
/// =====
///
/// * alpha
/// * beta
/// "#;
/// let parser = Parser::new(markdown_str);
///
/// let mut html_buf = String::new();
/// html::push_html(&mut html_buf, parser);
///
/// assert_eq!(html_buf, r#"<h1>hello</h1>
/// <ul>
/// <li>alpha</li>
/// <li>beta</li>
/// </ul>
/// "#);
/// ```
pub fn push_html<'a, I>(s: &mut String, iter: I)
where
    I: Iterator<Item = Event<'a>>,
{
    unsafe {
        // we only write utf-8, so this should be OK
        write_html(s.as_mut_vec(), iter).unwrap();
    }
}

/// Iterate over an `Iterator` of `Event`s, generate HTML for each `Event`, and
/// write it out to a writable stream.
///
/// # Examples
///
/// ```
/// use pulldown_cmark::{html, Parser};
/// use std::io::Cursor;
///
/// let markdown_str = r#"
/// hello
/// =====
///
/// * alpha
/// * beta
/// "#;
/// let mut bytes = Vec::new();
/// let parser = Parser::new(markdown_str);
///
/// html::write_html(Cursor::new(&mut bytes), parser);
///
/// assert_eq!(&String::from_utf8_lossy(&bytes)[..], r#"<h1>hello</h1>
/// <ul>
/// <li>alpha</li>
/// <li>beta</li>
/// </ul>
/// "#);
/// ```
pub fn write_html<'a, I, W>(writer: W, iter: I) -> io::Result<()>
where
    I: Iterator<Item = Event<'a>>,
    W: Write,
{
    let writer = HtmlWriter {
        iter: iter,
        writer: writer,
        end_newline: true,
        table_state: TableState::Head,
        table_alignments: vec![],
        table_cell_index: 0,
        numbers: HashMap::new(),
    };
    writer.run()
}
