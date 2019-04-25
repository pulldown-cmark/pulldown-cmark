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
use std::io::{self, Write, ErrorKind};
use std::fmt::{Arguments, Write as FmtWrite};

use crate::parse::{LinkType, Event, Tag, Alignment};
use crate::parse::Event::*;
use crate::strings::CowStr;
use crate::escape::{escape_html, escape_href};

enum TableState {
    Head,
    Body,
}

/// Wrapper for String for which we can implement `StrWrite`.
/// We can't implement it for `String` directly as it could theoretically
/// conflict with the blanket implementation if `std::io::Write` was
/// ever implemented for `String`.
struct StringWrap<'w>(&'w mut String);

struct StrWriteMutRef<'w, W>(&'w mut W);

// TODO: expose this?
pub trait StrWrite {
    fn write_str(&mut self, s: &str) -> io::Result<()>;

    fn write_fmt(&mut self, args: Arguments) -> io::Result<()>;
}

impl<'w> StrWrite for StringWrap<'w> {
    fn write_str(&mut self, s: &str) -> io::Result<()> {
        self.0.push_str(s);
        Ok(())
    }

    fn write_fmt(&mut self, args: Arguments) -> io::Result<()> {
        // FIXME: translate fmt error to io error?
        self.0.write_fmt(args).map_err(|_| ErrorKind::Other.into())
    }
}

impl<W> StrWrite for W
    where W: Write
{
    fn write_str(&mut self, s: &str) -> io::Result<()> {
        self.write_all(s.as_bytes())
    }

    fn write_fmt(&mut self, args: Arguments) -> io::Result<()> {
        self.write_fmt(args)
    }
}

impl<W> StrWrite for StrWriteMutRef<'_, W>
    where W: StrWrite
{
    fn write_str(&mut self, s: &str) -> io::Result<()> {
        self.0.write_str(s)
    }

    fn write_fmt(&mut self, args: Arguments) -> io::Result<()> {
        self.0.write_fmt(args)
    }
}

struct HtmlWriter<'a, I, W> {
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
    W: StrWrite,
{
    /// Writes a new line.
    fn write_newline(&mut self) -> io::Result<()> {
        self.end_newline = true;
        self.writer.write_str("\n")
    }

    fn simple_write(&mut self, s: &str) -> io::Result<()> {
        self.writer.write_str(s)
    }

    /// Writes a buffer, and tracks whether or not a newline was written.
    fn write(&mut self, s: &str, write_newline: bool) -> io::Result<()> {
        self.writer.write_str(s)?;

        if write_newline {
            self.write_newline()
        } else {
            if !s.is_empty() {
                self.end_newline = s.ends_with('\n');
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
                    escape_html(StrWriteMutRef(&mut self.writer), &text)?;
                    self.end_newline = text.ends_with('\n');
                }
                Code(text) => {
                    self.simple_write("<code>")?;
                    escape_html(StrWriteMutRef(&mut self.writer), &text)?;
                    self.simple_write("</code>")?;
                    self.end_newline = false;
                }
                Html(html) | InlineHtml(html) => {
                    self.write(&html, false)?;
                }
                SoftBreak => {
                    self.write_newline()?;
                }
                HardBreak => {
                    self.simple_write("<br />\n")?;
                    self.end_newline = true;
                }
                FootnoteReference(name) => {
                    let len = self.numbers.len() + 1;
                    self.simple_write("<sup class=\"footnote-reference\"><a href=\"#")?;
                    escape_html(StrWriteMutRef(&mut self.writer), &name)?;
                    self.simple_write("\">")?;
                    let number = *self.numbers.entry(name).or_insert(len);
                    write!(&mut self.writer, "{}", number)?;
                    self.simple_write("</a></sup>")?;
                    self.end_newline = false;
                }
                TaskListMarker(true) => {
                    self.simple_write("<input disabled=\"\" type=\"checkbox\" checked=\"\"/>\n")?;
                    self.end_newline = true;
                }
                TaskListMarker(false) => {
                    self.simple_write("<input disabled=\"\" type=\"checkbox\"/>\n")?;
                    self.end_newline = true;
                }
            }
        }
        Ok(())
    }

    /// Writes the start of an HTML tag.
    fn start_tag(&mut self, tag: Tag<'a>) -> io::Result<()> {
        match tag {
            Tag::Paragraph => {
                if self.end_newline {
                    self.write("<p>", false)
                } else {
                    self.write("\n<p>", false)
                }
            }
            Tag::Rule => {
                self.fresh_line()?;
                self.end_newline = true;
                self.simple_write("<hr />\n")
            }
            Tag::Header(level) => {
                self.fresh_line()?;
                self.end_newline = false;
                write!(&mut self.writer, "<h{}>", level)
            }
            Tag::Table(alignments) => {
                self.table_alignments = alignments;
                self.write("<table>", false)
            }
            Tag::TableHead => {
                self.table_state = TableState::Head;
                self.table_cell_index = 0;
                self.write("<thead><tr>", false)
            }
            Tag::TableRow => {
                self.table_cell_index = 0;
                self.write("<tr>", false)
            }
            Tag::TableCell => {
                match self.table_state {
                    TableState::Head => {
                        self.write("<th", false)?;
                    }
                    TableState::Body => {
                        self.write("<td", false)?;
                    }
                }
                match self.table_alignments.get(self.table_cell_index) {
                    Some(&Alignment::Left) => {
                        self.write(" align=\"left\"", false)?;
                    }
                    Some(&Alignment::Center) => {
                        self.write(" align=\"center\"", false)?;
                    }
                    Some(&Alignment::Right) => {
                        self.write(" align=\"right\"", false)?;
                    }
                    _ => (),
                }
                self.write(">", false)
            }
            Tag::BlockQuote => {
                self.fresh_line()?;
                self.end_newline = true;
                self.simple_write("<blockquote>\n")
            }
            Tag::CodeBlock(info) => {
                self.fresh_line()?;
                let lang = info.split(' ').next().unwrap();
                if lang.is_empty() {
                    self.write("<pre><code>", false)
                } else {
                    self.write("<pre><code class=\"language-", false)?;
                    escape_html(StrWriteMutRef(&mut self.writer), lang)?;
                    self.write("\">", false)
                }
            }
            Tag::List(Some(1)) => {
                self.fresh_line()?;
                self.end_newline = true;
                self.simple_write("<ol>\n")
            }
            Tag::List(Some(start)) => {
                self.fresh_line()?;
                self.write("<ol start=\"", false)?;
                write!(&mut self.writer, "{}", start)?;
                self.write("\">", true)
            }
            Tag::List(None) => {
                self.fresh_line()?;
                self.end_newline = true;
                self.simple_write("<ul>\n")
            }
            Tag::Item => {
                if self.end_newline {
                    self.write("<li>", false)
                } else {
                    self.write("\n<li>", false)
                }
            }
            Tag::Emphasis => self.write("<em>", false),
            Tag::Strong => self.write("<strong>", false),
            Tag::Strikethrough => self.write("<del>", false),
            Tag::Link(LinkType::Email, dest, title) => {
                self.write("<a href=\"mailto:", false)?;
                escape_href(StrWriteMutRef(&mut self.writer), &dest)?;
                if !title.is_empty() {
                    self.write("\" title=\"", false)?;
                    escape_html(StrWriteMutRef(&mut self.writer), &title)?;
                }
                self.write("\">", false)
            }
            Tag::Link(_link_type, dest, title) => {
                self.write("<a href=\"", false)?;
                escape_href(StrWriteMutRef(&mut self.writer), &dest)?;
                if !title.is_empty() {
                    self.write("\" title=\"", false)?;
                    escape_html(StrWriteMutRef(&mut self.writer), &title)?;
                }
                self.write("\">", false)
            }
            Tag::Image(_link_type, dest, title) => {
                self.write("<img src=\"", false)?;
                escape_href(StrWriteMutRef(&mut self.writer), &dest)?;
                self.write("\" alt=\"", false)?;
                self.raw_text()?;
                if !title.is_empty() {
                    self.write("\" title=\"", false)?;
                    escape_html(StrWriteMutRef(&mut self.writer), &title)?;
                }
                self.write("\" />", false)
            }
            Tag::FootnoteDefinition(name) => {
                self.fresh_line()?;
                let len = self.numbers.len() + 1;
                self.write("<div class=\"footnote-definition\" id=\"", false)?;
                escape_html(StrWriteMutRef(&mut self.writer), &*name)?;
                self.write("\"><sup class=\"footnote-definition-label\">", false)?;
                let number = *self.numbers.entry(name).or_insert(len);
                write!(&mut self.writer, "{}", number)?;
                self.write("</sup>", false)
            }
            Tag::HtmlBlock => Ok(())
        }
    }

    fn end_tag(&mut self, tag: Tag) -> io::Result<()> {
        match tag {
            Tag::Paragraph => {
                self.end_newline = true;
                self.simple_write("</p>\n")?;
            }
            Tag::Rule => (),
            Tag::Header(level) => {
                self.simple_write("</h")?;
                write!(&mut self.writer, "{}", level)?;
                self.end_newline = true;
                self.simple_write(">\n")?;
            }
            Tag::Table(_) => {
                self.end_newline = true;
                self.simple_write("</tbody></table>\n")?;
            }
            Tag::TableHead => {
                self.end_newline = true;
                self.simple_write("</tr></thead><tbody>\n")?;
                self.table_state = TableState::Body;
            }
            Tag::TableRow => {
                self.end_newline = true;
                self.simple_write("</tr>\n")?;
            }
            Tag::TableCell => {
                match self.table_state {
                    TableState::Head => {
                        self.write("</th>", false)?;
                    }
                    TableState::Body => {
                        self.write("</td>", false)?;
                    }
                }
                self.table_cell_index += 1;
            }
            Tag::BlockQuote => {
                self.end_newline = true;
                self.simple_write("</blockquote>\n")?;
            }
            Tag::CodeBlock(_) => {
                self.end_newline = true;
                self.simple_write("</code></pre>\n")?;
            }
            Tag::List(Some(_)) => {
                self.end_newline = true;
                self.simple_write("</ol>\n")?;
            }
            Tag::List(None) => {
                self.end_newline = true;
                self.simple_write("</ul>\n")?;
            }
            Tag::Item => {
                self.end_newline = true;
                self.simple_write("</li>\n")?;
            }
            Tag::Emphasis => {
                self.write("</em>", false)?;
            }
            Tag::Strong => {
                self.write("</strong>", false)?;
            }
            Tag::Strikethrough => {
                self.write("</del>", false)?;
            }
            Tag::Link(_, _, _) => {
                self.write("</a>", false)?;
            }
            Tag::Image(_, _, _) => (), // shouldn't happen, handled in start
            Tag::FootnoteDefinition(_) => {
                self.end_newline = true;
                self.simple_write("</div>\n")?;
            }
            Tag::HtmlBlock => {}
        }
        Ok(())
    }

    // run raw text, consuming end tag
    fn raw_text(&mut self) -> io::Result<()> {
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
                Html(_) => (),
                InlineHtml(text) | Code(text) | Text(text) => {
                    escape_html(StrWriteMutRef(&mut self.writer), &text)?;
                    self.end_newline = text.ends_with('\n');
                }
                SoftBreak | HardBreak => {
                    self.write(" ", false)?;
                }
                FootnoteReference(name) => {
                    let len = self.numbers.len() + 1;
                    let number = *self.numbers.entry(name).or_insert(len);
                    write!(&mut self.writer, "[{}]", number)?;
                }
                TaskListMarker(true) => self.write("[x]", false)?,
                TaskListMarker(false) => self.write("[ ]", false)?,
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
    write_html(StringWrap(s), iter).unwrap();
}

/// Iterate over an `Iterator` of `Event`s, generate HTML for each `Event`, and
/// write it out to a writable stream.
///
/// **Note**: using this function with an unbuffered writer like a file or socket
/// will result in poor performance. Wrap these in a
/// [`BufWriter`](https://doc.rust-lang.org/std/io/struct.BufWriter.html) to
/// prevent unnecessary slowdowns.
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
    W: StrWrite,
{
    let writer = HtmlWriter {
        iter,
        writer,
        end_newline: true,
        table_state: TableState::Head,
        table_alignments: vec![],
        table_cell_index: 0,
        numbers: HashMap::new(),
    };
    writer.run()
}
