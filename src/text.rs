//! Text renderer that takes an iterator of events as input.

use std::io::{self, Write};

use crate::escape::{escape_href, StrWrite, WriteWrapper};
use crate::Event::*;
use crate::{Event, Tag, TagEnd};

struct TextWriter<I, W> {
    /// Iterator supplying events.
    iter: I,

    /// Writer to write to.
    writer: W,

    /// Whether if inside a block  where text should not be written
    in_non_writing_block: bool,

    /// How many characters on the current line
    chars_on_line: usize,
}

impl<'a, I, W> TextWriter<I, W>
where
    I: Iterator<Item = Event<'a>>,
    W: StrWrite,
{
    fn new(iter: I, writer: W) -> Self {
        Self {
            iter,
            writer,
            in_non_writing_block: false,
            chars_on_line: 0,
        }
    }

    /// Writes a new line.
    #[inline(always)]
    fn write_newline(&mut self) -> io::Result<()> {
        self.writer.write_str("\n")?;
        self.chars_on_line = 0;
        Ok(())
    }

    /// Writes a buffer.
    #[inline(always)]
    fn write(&mut self, s: &str) -> io::Result<()> {
        self.writer.write_str(s)?;
        if s.ends_with('\n') {
            self.chars_on_line = 0;
        } else {
            self.chars_on_line += s.len();
        }
        Ok(())
    }

    /// Write a separator, if there is text on the line
    #[inline(always)]
    fn write_separator(&mut self) -> io::Result<()> {
        if self.chars_on_line != 0 {
            self.writer.write_str(" ")?;
            self.chars_on_line += 1;
        }
        Ok(())
    }

    fn run(mut self) -> io::Result<()> {
        while let Some(event) = self.iter.next() {
            match event {
                Start(tag) => match tag {
                    Tag::Link {
                        dest_url, title, ..
                    } => {
                        self.write(&title)?;
                        self.write(": ")?;
                        escape_href(&mut self.writer, &dest_url)?;
                        self.in_non_writing_block = true;
                    }
                    Tag::FootnoteDefinition(_name) => self.in_non_writing_block = true,
                    Tag::MetadataBlock(_kind) => self.in_non_writing_block = true,
                    _ => {}
                },
                End(tag) => match tag {
                    TagEnd::Paragraph
                    | TagEnd::Heading(_)
                    | TagEnd::BlockQuote
                    | TagEnd::Item
                    | TagEnd::Table
                    | TagEnd::TableHead
                    | TagEnd::TableRow => self.write_newline()?,
                    TagEnd::Link | TagEnd::FootnoteDefinition => self.in_non_writing_block = false,
                    TagEnd::MetadataBlock(_kind) => self.in_non_writing_block = false,
                    _ => {}
                },
                Text(text) => {
                    if !self.in_non_writing_block {
                        self.write_separator()?;
                        self.write(&text)?;
                    }
                }
                Code(text) => {
                    self.write_separator()?;
                    self.write(&text)?;
                }
                Html(html) | InlineHtml(html) => {
                    self.write_separator()?;
                    self.write(&html)?;
                }
                SoftBreak => {
                    self.write_newline()?;
                }
                HardBreak => {
                    self.write_newline()?;
                }
                Rule => {}
                FootnoteReference(_name) => {}
                TaskListMarker(_bool) => {}
            }
        }
        Ok(())
    }
}

/// Iterate over an `Iterator` of `Event`s, generate text for each `Event`, and
/// push it to a `String`.
///
/// # Examples
///
/// ```
/// use pulldown_cmark::{text, Parser};
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
/// let mut text_buf = String::new();
/// text::push_text(&mut text_buf, parser);
///
/// assert_eq!(text_buf, r#"hello
/// alpha
/// beta
/// "#);
/// ```
pub fn push_text<'a, I>(s: &mut String, iter: I)
where
    I: Iterator<Item = Event<'a>>,
{
    TextWriter::new(iter, s).run().unwrap();
}

/// Iterate over an `Iterator` of `Event`s, generate text for each `Event`, and
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
/// use pulldown_cmark::{text, Parser};
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
/// let _ = text::write_text(Cursor::new(&mut bytes), parser);
///
/// assert_eq!(&String::from_utf8_lossy(&bytes)[..], r#"hello
/// alpha
/// beta
/// "#);
/// ```
pub fn write_text<'a, I, W>(writer: W, iter: I) -> io::Result<()>
where
    I: Iterator<Item = Event<'a>>,
    W: Write,
{
    TextWriter::new(iter, WriteWrapper(writer)).run()
}
