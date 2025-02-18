mod default;
mod element_renderer;
mod event_processor;

use crate::Event;
pub use default::HtmlWriter;
pub use element_renderer::{HtmlElementRenderer, TableState};
pub use event_processor::HtmlEventProcessor;

pub use pulldown_cmark_escape::StrWrite;

use pulldown_cmark_escape::{FmtWriter, IoWriter};

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
pub fn write_html_io<'a, I, W>(writer: W, iter: I) -> std::io::Result<()>
where
    I: Iterator<Item = Event<'a>>,
    W: std::io::Write,
{
    let renderer = HtmlWriter::new(IoWriter(writer));
    let processor = HtmlEventProcessor::new(iter, renderer);
    processor.run()
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
pub fn write_html_fmt<'a, I, W>(writer: W, iter: I) -> std::fmt::Result
where
    I: Iterator<Item = Event<'a>>,
    W: std::fmt::Write,
{
    let renderer = HtmlWriter::new(FmtWriter(writer));
    let processor = HtmlEventProcessor::new(iter, renderer);
    processor.run()
}
