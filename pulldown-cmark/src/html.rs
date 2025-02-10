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

use crate::strings::CowStr;
use crate::Event::*;
use crate::{Alignment, BlockQuoteKind, CodeBlockKind, Event, LinkType, Tag, TagEnd};
use pulldown_cmark_escape::{
    escape_href, escape_html, escape_html_body_text, FmtWriter, IoWriter, StrWrite,
};

enum TableState {
    Head,
    Body,
}

struct HtmlWriter<'a, I, W> {
    /// Iterator supplying events.
    iter: I,

    /// Writer to write to.
    writer: W,

    /// Whether or not the last write wrote a newline.
    end_newline: bool,

    /// Whether if inside a metadata block (text should not be written)
    in_non_writing_block: bool,

    table_state: TableState,
    table_alignments: Vec<Alignment>,
    table_cell_index: usize,
    numbers: HashMap<CowStr<'a>, usize>,
}

impl<'a, I, W, BQK> HtmlWriter<'a, I, W>
where
    I: Iterator<Item = Event<'a, BQK>>,
    W: StrWrite,
    BQK: ToClass,
{
    fn new(iter: I, writer: W) -> Self {
        Self {
            iter,
            writer,
            end_newline: true,
            in_non_writing_block: false,
            table_state: TableState::Head,
            table_alignments: vec![],
            table_cell_index: 0,
            numbers: HashMap::new(),
        }
    }

    /// Writes a new line.
    #[inline]
    fn write_newline(&mut self) -> Result<(), W::Error> {
        self.end_newline = true;
        self.writer.write_str("\n")
    }

    /// Writes a buffer, and tracks whether or not a newline was written.
    #[inline]
    fn write(&mut self, s: &str) -> Result<(), W::Error> {
        self.writer.write_str(s)?;

        if !s.is_empty() {
            self.end_newline = s.ends_with('\n');
        }
        Ok(())
    }

    fn run(mut self) -> Result<(), W::Error> {
        while let Some(event) = self.iter.next() {
            match event {
                Start(tag) => {
                    self.start_tag(tag)?;
                }
                End(tag) => {
                    self.end_tag(tag)?;
                }
                Text(text) => {
                    if !self.in_non_writing_block {
                        escape_html_body_text(&mut self.writer, &text)?;
                        self.end_newline = text.ends_with('\n');
                    }
                }
                Code(text) => {
                    self.write("<code>")?;
                    escape_html_body_text(&mut self.writer, &text)?;
                    self.write("</code>")?;
                }
                InlineMath(text) => {
                    self.write(r#"<span class="math math-inline">"#)?;
                    escape_html(&mut self.writer, &text)?;
                    self.write("</span>")?;
                }
                DisplayMath(text) => {
                    self.write(r#"<span class="math math-display">"#)?;
                    escape_html(&mut self.writer, &text)?;
                    self.write("</span>")?;
                }
                Html(html) | InlineHtml(html) => {
                    self.write(&html)?;
                }
                SoftBreak => {
                    self.write_newline()?;
                }
                HardBreak => {
                    self.write("<br />\n")?;
                }
                Rule => {
                    if self.end_newline {
                        self.write("<hr />\n")?;
                    } else {
                        self.write("\n<hr />\n")?;
                    }
                }
                FootnoteReference(name) => {
                    let len = self.numbers.len() + 1;
                    self.write("<sup class=\"footnote-reference\"><a href=\"#")?;
                    escape_html(&mut self.writer, &name)?;
                    self.write("\">")?;
                    let number = *self.numbers.entry(name).or_insert(len);
                    write!(&mut self.writer, "{}", number)?;
                    self.write("</a></sup>")?;
                }
                TaskListMarker(true) => {
                    self.write("<input disabled=\"\" type=\"checkbox\" checked=\"\"/>\n")?;
                }
                TaskListMarker(false) => {
                    self.write("<input disabled=\"\" type=\"checkbox\"/>\n")?;
                }
            }
        }
        Ok(())
    }

    /// Writes the start of an HTML tag.
    fn start_tag(&mut self, tag: Tag<'a, BQK>) -> Result<(), W::Error> {
        match tag {
            Tag::HtmlBlock => Ok(()),
            Tag::Paragraph => {
                if self.end_newline {
                    self.write("<p>")
                } else {
                    self.write("\n<p>")
                }
            }
            Tag::Heading {
                level,
                id,
                classes,
                attrs,
            } => {
                if self.end_newline {
                    self.write("<")?;
                } else {
                    self.write("\n<")?;
                }
                write!(&mut self.writer, "{}", level)?;
                if let Some(id) = id {
                    self.write(" id=\"")?;
                    escape_html(&mut self.writer, &id)?;
                    self.write("\"")?;
                }
                let mut classes = classes.iter();
                if let Some(class) = classes.next() {
                    self.write(" class=\"")?;
                    escape_html(&mut self.writer, class)?;
                    for class in classes {
                        self.write(" ")?;
                        escape_html(&mut self.writer, class)?;
                    }
                    self.write("\"")?;
                }
                for (attr, value) in attrs {
                    self.write(" ")?;
                    escape_html(&mut self.writer, &attr)?;
                    if let Some(val) = value {
                        self.write("=\"")?;
                        escape_html(&mut self.writer, &val)?;
                        self.write("\"")?;
                    } else {
                        self.write("=\"\"")?;
                    }
                }
                self.write(">")
            }
            Tag::Table(alignments) => {
                self.table_alignments = alignments;
                self.write("<table>")
            }
            Tag::TableHead => {
                self.table_state = TableState::Head;
                self.table_cell_index = 0;
                self.write("<thead><tr>")
            }
            Tag::TableRow => {
                self.table_cell_index = 0;
                self.write("<tr>")
            }
            Tag::TableCell => {
                match self.table_state {
                    TableState::Head => {
                        self.write("<th")?;
                    }
                    TableState::Body => {
                        self.write("<td")?;
                    }
                }
                match self.table_alignments.get(self.table_cell_index) {
                    Some(&Alignment::Left) => self.write(" style=\"text-align: left\">"),
                    Some(&Alignment::Center) => self.write(" style=\"text-align: center\">"),
                    Some(&Alignment::Right) => self.write(" style=\"text-align: right\">"),
                    _ => self.write(">"),
                }
            }
            Tag::BlockQuote(kind) => {
                let class_str;
                let class_str = if let Some(kind) = kind.as_ref() {
                    class_str = kind.to_class();
                    Some(&class_str[..])
                } else {
                    None
                };
                let tag;
                self.write(match (class_str, self.end_newline) {
                    (Some(""), false) => "\n<div class=\"markdown-admonition\">\n",
                    (Some(""), true) => "<div class=\"markdown-admonition\">\n",
                    (Some(class_str), false) => {
                        tag = format!("\n<div class=\"markdown-admonition {class_str}\">\n");
                        &tag
                    }
                    (Some(class_str), true) => {
                        tag = format!("<div class=\"markdown-admonition {class_str}\">\n");
                        &tag
                    }
                    (None, false) => "\n<blockquote>\n",
                    (None, true) => "<blockquote>\n",
                })
            }
            Tag::CodeBlock(info) => {
                if !self.end_newline {
                    self.write_newline()?;
                }
                match info {
                    CodeBlockKind::Fenced(info) => {
                        let lang = info.split(' ').next().unwrap();
                        if lang.is_empty() {
                            self.write("<pre><code>")
                        } else {
                            self.write("<pre><code class=\"language-")?;
                            escape_html(&mut self.writer, lang)?;
                            self.write("\">")
                        }
                    }
                    CodeBlockKind::Indented => self.write("<pre><code>"),
                }
            }
            Tag::List(Some(1)) => {
                if self.end_newline {
                    self.write("<ol>\n")
                } else {
                    self.write("\n<ol>\n")
                }
            }
            Tag::List(Some(start)) => {
                if self.end_newline {
                    self.write("<ol start=\"")?;
                } else {
                    self.write("\n<ol start=\"")?;
                }
                write!(&mut self.writer, "{}", start)?;
                self.write("\">\n")
            }
            Tag::List(None) => {
                if self.end_newline {
                    self.write("<ul>\n")
                } else {
                    self.write("\n<ul>\n")
                }
            }
            Tag::Item => {
                if self.end_newline {
                    self.write("<li>")
                } else {
                    self.write("\n<li>")
                }
            }
            Tag::DefinitionList => {
                if self.end_newline {
                    self.write("<dl>\n")
                } else {
                    self.write("\n<dl>\n")
                }
            }
            Tag::DefinitionListTitle => {
                if self.end_newline {
                    self.write("<dt>")
                } else {
                    self.write("\n<dt>")
                }
            }
            Tag::DefinitionListDefinition => {
                if self.end_newline {
                    self.write("<dd>")
                } else {
                    self.write("\n<dd>")
                }
            }
            Tag::Subscript => self.write("<sub>"),
            Tag::Superscript => self.write("<sup>"),
            Tag::Emphasis => self.write("<em>"),
            Tag::Strong => self.write("<strong>"),
            Tag::Strikethrough => self.write("<del>"),
            Tag::Link {
                link_type: LinkType::Email,
                dest_url,
                title,
                id: _,
            } => {
                self.write("<a href=\"mailto:")?;
                escape_href(&mut self.writer, &dest_url)?;
                if !title.is_empty() {
                    self.write("\" title=\"")?;
                    escape_html(&mut self.writer, &title)?;
                }
                self.write("\">")
            }
            Tag::Link {
                link_type: _,
                dest_url,
                title,
                id: _,
            } => {
                self.write("<a href=\"")?;
                escape_href(&mut self.writer, &dest_url)?;
                if !title.is_empty() {
                    self.write("\" title=\"")?;
                    escape_html(&mut self.writer, &title)?;
                }
                self.write("\">")
            }
            Tag::Image {
                link_type: _,
                dest_url,
                title,
                id: _,
            } => {
                self.write("<img src=\"")?;
                escape_href(&mut self.writer, &dest_url)?;
                self.write("\" alt=\"")?;
                self.raw_text()?;
                if !title.is_empty() {
                    self.write("\" title=\"")?;
                    escape_html(&mut self.writer, &title)?;
                }
                self.write("\" />")
            }
            Tag::FootnoteDefinition(name) => {
                if self.end_newline {
                    self.write("<div class=\"footnote-definition\" id=\"")?;
                } else {
                    self.write("\n<div class=\"footnote-definition\" id=\"")?;
                }
                escape_html(&mut self.writer, &name)?;
                self.write("\"><sup class=\"footnote-definition-label\">")?;
                let len = self.numbers.len() + 1;
                let number = *self.numbers.entry(name).or_insert(len);
                write!(&mut self.writer, "{}", number)?;
                self.write("</sup>")
            }
            Tag::MetadataBlock(_) => {
                self.in_non_writing_block = true;
                Ok(())
            }
        }
    }

    fn end_tag(&mut self, tag: TagEnd<BQK>) -> Result<(), W::Error> {
        match tag {
            TagEnd::HtmlBlock => {}
            TagEnd::Paragraph => {
                self.write("</p>\n")?;
            }
            TagEnd::Heading(level) => {
                self.write("</")?;
                write!(&mut self.writer, "{}", level)?;
                self.write(">\n")?;
            }
            TagEnd::Table => {
                self.write("</tbody></table>\n")?;
            }
            TagEnd::TableHead => {
                self.write("</tr></thead><tbody>\n")?;
                self.table_state = TableState::Body;
            }
            TagEnd::TableRow => {
                self.write("</tr>\n")?;
            }
            TagEnd::TableCell => {
                match self.table_state {
                    TableState::Head => {
                        self.write("</th>")?;
                    }
                    TableState::Body => {
                        self.write("</td>")?;
                    }
                }
                self.table_cell_index += 1;
            }
            TagEnd::BlockQuote(kind) => {
                self.write(if kind.is_some() {
                    "</div>\n"
                } else {
                    "</blockquote>\n"
                })?;
            }
            TagEnd::CodeBlock => {
                self.write("</code></pre>\n")?;
            }
            TagEnd::List(true) => {
                self.write("</ol>\n")?;
            }
            TagEnd::List(false) => {
                self.write("</ul>\n")?;
            }
            TagEnd::Item => {
                self.write("</li>\n")?;
            }
            TagEnd::DefinitionList => {
                self.write("</dl>\n")?;
            }
            TagEnd::DefinitionListTitle => {
                self.write("</dt>\n")?;
            }
            TagEnd::DefinitionListDefinition => {
                self.write("</dd>\n")?;
            }
            TagEnd::Emphasis => {
                self.write("</em>")?;
            }
            TagEnd::Superscript => {
                self.write("</sup>")?;
            }
            TagEnd::Subscript => {
                self.write("</sub>")?;
            }
            TagEnd::Strong => {
                self.write("</strong>")?;
            }
            TagEnd::Strikethrough => {
                self.write("</del>")?;
            }
            TagEnd::Link => {
                self.write("</a>")?;
            }
            TagEnd::Image => (), // shouldn't happen, handled in start
            TagEnd::FootnoteDefinition => {
                self.write("</div>\n")?;
            }
            TagEnd::MetadataBlock(_) => {
                self.in_non_writing_block = false;
            }
        }
        Ok(())
    }

    // run raw text, consuming end tag
    fn raw_text(&mut self) -> Result<(), W::Error> {
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
                Html(_) => {}
                InlineHtml(text) | Code(text) | Text(text) => {
                    // Don't use escape_html_body_text here.
                    // The output of this function is used in the `alt` attribute.
                    escape_html(&mut self.writer, &text)?;
                    self.end_newline = text.ends_with('\n');
                }
                InlineMath(text) => {
                    self.write("$")?;
                    escape_html(&mut self.writer, &text)?;
                    self.write("$")?;
                }
                DisplayMath(text) => {
                    self.write("$$")?;
                    escape_html(&mut self.writer, &text)?;
                    self.write("$$")?;
                }
                SoftBreak | HardBreak | Rule => {
                    self.write(" ")?;
                }
                FootnoteReference(name) => {
                    let len = self.numbers.len() + 1;
                    let number = *self.numbers.entry(name).or_insert(len);
                    write!(&mut self.writer, "[{}]", number)?;
                }
                TaskListMarker(true) => self.write("[x]")?,
                TaskListMarker(false) => self.write("[ ]")?,
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
pub fn push_html<'a, I, AD: ToClass>(s: &mut String, iter: I)
where
    I: Iterator<Item = Event<'a, AD>>,
{
    write_html_fmt(s, iter).unwrap()
}

/// Iterate over an `Iterator` of `Event`s, generate HTML for each `Event`, and
/// write it out to an I/O stream.
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
/// html::write_html_io(Cursor::new(&mut bytes), parser);
///
/// assert_eq!(&String::from_utf8_lossy(&bytes)[..], r#"<h1>hello</h1>
/// <ul>
/// <li>alpha</li>
/// <li>beta</li>
/// </ul>
/// "#);
/// ```
pub fn write_html_io<'a, I, W, AD: ToClass>(writer: W, iter: I) -> std::io::Result<()>
where
    I: Iterator<Item = Event<'a, AD>>,
    W: std::io::Write,
{
    HtmlWriter::new(iter, IoWriter(writer)).run()
}

/// Iterate over an `Iterator` of `Event`s, generate HTML for each `Event`, and
/// write it into Unicode-accepting buffer or stream.
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
/// let mut buf = String::new();
/// let parser = Parser::new(markdown_str);
///
/// html::write_html_fmt(&mut buf, parser);
///
/// assert_eq!(buf, r#"<h1>hello</h1>
/// <ul>
/// <li>alpha</li>
/// <li>beta</li>
/// </ul>
/// "#);
/// ```
pub fn write_html_fmt<'a, I, W, AD: ToClass>(writer: W, iter: I) -> std::fmt::Result
where
    I: Iterator<Item = Event<'a, AD>>,
    W: std::fmt::Write,
{
    HtmlWriter::new(iter, FmtWriter(writer)).run()
}

/// Return an HTML class, if any, for an admonition.
///
/// This is a convenience function that lets you use the built-in HTML renderer.
/// If your needs are more complex, you'll have to process the event stream instead.
///
/// # Example
///
/// ```rust
/// use pulldown_cmark::{html, Parser, Options, AdmonitionTagCallback, DefaultBrokenLinkCallback, Tag, TagEnd, TextMergeStream, Event, CowStr};
/// use pulldown_cmark_escape::{escape_href, escape_html};
/// #[derive(Clone, Debug, Copy, Eq, PartialEq)]
/// enum Admonition<'input> {
///     /// `"[!NOTE]"`
///     Note,
///     /// `"[!TIP]"`
///     Tip,
///     /// `"[!IMPORTANT]"`
///     Important,
///     /// `"[!WARNING]"`
///     Warning,
///     /// `"[!REVIEW]"`
///     Review,
///     /// `"[!CAUTION]"`
///     Caution,
///     /// `"[!VIDEO=url]"`
///     Video(&'input str),
///     /// `"[!DIV" (space ("CLASS=" attrvalue)|("ID=" attrvalue)|("." attrvalue)|("#" attrvalue))* "]"`
///     /// space = `" "+`
///     /// attrvalue = `("\"" anything_but_quote* "\"")|anything_but_space*`
///     Div(&'input str, &'input str),
/// }
/// impl<'input> html::ToClass for Admonition<'input> {
///     fn to_class<'a>(&'a self) -> CowStr<'static> {
///         CowStr::Borrowed(match *self {
///             Admonition::Note => "markdown-note",
///             Admonition::Tip => "markdown-tip",
///             Admonition::Important => "markdown-important",
///             Admonition::Warning => "markdown-warning",
///             Admonition::Review => "markdown-review",
///             Admonition::Caution => "markdown-caution",
///             Admonition::Video(_) => unreachable!("removed by event filter"),
///             Admonition::Div(_, _) => unreachable!("removed by event filter"),
///         })
///     }
/// }
/// struct AdmonitionHandler;
/// impl<'input> AdmonitionTagCallback<'input> for AdmonitionHandler {
///     type DataKind = Admonition<'input>;
///     fn handle_admonition_tag(&mut self, input: &'input str) -> Option<Self::DataKind> {
///         Some(if input.len() >= 6 && input[..6].eq_ignore_ascii_case("video=") {
///             Admonition::Video(&input[6..])
///         } else if input.len() >= 3 && input[..3].eq_ignore_ascii_case("div") {
///             let mut id = "";
///             let mut class = "";
///             let mut input = &input[3..];
///             while input != "" {
///                 fn attribute(mut input: &str) -> (&str, &str) {
///                     let attr;
///                     if input.starts_with('"') {
///                         input = &input[1..];
///                         let mut end = input.bytes().position(|c| c == b'"').unwrap_or(input.len());
///                         attr = &input[..end];
///                         if input.as_bytes().get(end) == Some(&b'"') {
///                             end += 1;
///                         }
///                         input = &input[end..];
///                     } else {
///                         let end = input.bytes().position(|c| c == b' ').unwrap_or(input.len());
///                         attr = &input[..end];
///                         input = &input[end..];
///                     }
///                     (attr, input)
///                 }
///                 if input.as_bytes().get(0) == Some(&b' ') {
///                     input = &input[1..];
///                 } else {
///                     return None;
///                 }
///                 match input.as_bytes() {
///                     [b' ', ..] => {},
///                     [b'i' | b'I', b'd' | b'D', b'=', ..] => {
///                         (id, input) = attribute(&input[3..]);
///                     },
///                     [b'#', ..] => {
///                         (id, input) = attribute(&input[1..]);
///                     },
///                     [b'c' | b'C', b'l' | b'L', b'a' | b'A', b's' | b'S', b's' | b'S', b'=', ..] => {
///                         (class, input) = attribute(&input[6..]);
///                     },
///                     [b'.', ..] => {
///                         (class, input) = attribute(&input[1..]);
///                     },
///                     _ => return None,
///                 }
///             }
///             Admonition::Div(id, class)
///         } else if input.eq_ignore_ascii_case("note") {
///             Admonition::Note
///         } else if input.eq_ignore_ascii_case("tip") {
///             Admonition::Tip
///         } else if input.eq_ignore_ascii_case("important") {
///             Admonition::Important
///         } else if input.eq_ignore_ascii_case("warning") {
///             Admonition::Warning
///         } else if input.eq_ignore_ascii_case("caution") {
///             Admonition::Caution
///         } else if input.eq_ignore_ascii_case("review") {
///             Admonition::Review
///         } else {
///             return None;
///         })
///     }
/// }
/// let parser = Parser::new_with_callbacks(
///     "> [!video=url]\n\n> [!note]\n>be aware\n\n> [!div id=nn class=xx]\n> internals\n\n> [!div unsupported=attr]\n> internals",
///     Options::ENABLE_BLOCK_QUOTE_ADMONITIONS,
///     None::<DefaultBrokenLinkCallback>,
///     AdmonitionHandler,
/// ).map(|event| match event {
///     Event::Start(Tag::BlockQuote(Some(Admonition::Video(url)))) => {
///         let mut tag = String::from("<video src=\"");
///         escape_href(&mut tag, url);
///         tag += "\">\n";
///         Event::Html(CowStr::from(tag))
///     }
///     Event::End(TagEnd::BlockQuote(Some(Admonition::Video(_)))) => {
///         Event::Html(CowStr::Borrowed("</video>\n"))
///     }
///     Event::Start(Tag::BlockQuote(Some(Admonition::Div(id, class)))) => {
///         let mut tag = String::from("<div id=\"");
///         escape_html(&mut tag, id);
///         tag += "\" class=\"";
///         escape_html(&mut tag, class);
///         tag += "\">\n";
///         Event::Html(CowStr::from(tag))
///     }
///     Event::End(TagEnd::BlockQuote(Some(Admonition::Div(_, _)))) => {
///         Event::Html(CowStr::Borrowed("</div>\n"))
///     }
///     event => event,
/// });
/// let mut result = String::new();
/// html::push_html(&mut result, parser);
/// assert_eq!(result, r##"<video src="url">
/// </video>
/// <div class="markdown-admonition markdown-note">
/// <p>be aware</p>
/// </div>
/// <div id="nn" class="xx">
/// <p>internals</p>
/// </div>
/// <blockquote>
/// <p>[!div unsupported=attr]
/// internals</p>
/// </blockquote>
/// "##);
/// ```
pub trait ToClass {
    /// <div class="warning">
    ///
    /// Invalid admonition tags should be rejected earlier, in the
    /// [`AdmonitionTagCallback`], by returning None. Once this
    /// function gets called, the admonition has already been parsed.
    ///
    /// </div>
    ///
    /// [`AdmonitionTagCallback`]: crate::AdmonitionTagCallback
    fn to_class<'a>(&'a self) -> CowStr<'a>;
}

impl ToClass for str {
    fn to_class<'a>(&'a self) -> CowStr<'a> {
        CowStr::Borrowed(self)
    }
}

impl ToClass for String {
    fn to_class<'a>(&'a self) -> CowStr<'a> {
        CowStr::Borrowed(&self[..])
    }
}

impl ToClass for Box<str> {
    fn to_class<'a>(&'a self) -> CowStr<'a> {
        CowStr::Borrowed(&self[..])
    }
}

impl<'input> ToClass for CowStr<'input> {
    fn to_class<'a>(&'a self) -> CowStr<'a> {
        CowStr::Borrowed(&self[..])
    }
}

impl ToClass for crate::BlockQuoteKind {
    fn to_class(&'_ self) -> CowStr<'static> {
        CowStr::Borrowed(match *self {
            BlockQuoteKind::Note => "markdown-alert-note",
            BlockQuoteKind::Tip => "markdown-alert-tip",
            BlockQuoteKind::Important => "markdown-alert-important",
            BlockQuoteKind::Warning => "markdown-alert-warning",
            BlockQuoteKind::Caution => "markdown-alert-caution",
        })
    }
}
