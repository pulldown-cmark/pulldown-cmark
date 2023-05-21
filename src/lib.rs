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

//! Pull parser for [CommonMark](https://commonmark.org). This crate provides a [Parser](struct.Parser.html) struct
//! which is an iterator over [Event](enum.Event.html)s. This iterator can be used
//! directly, or to output HTML using the [HTML module](html/index.html).
//!
//! By default, only CommonMark features are enabled. To use extensions like tables,
//! footnotes or task lists, enable them by setting the corresponding flags in the
//! [Options](struct.Options.html) struct.
//!
//! # Example
//! ```rust
//! use pulldown_cmark::{Parser, Options, html};
//!
//! let markdown_input = "Hello world, this is a ~~complicated~~ *very simple* example.";
//!
//! // Set up options and parser. Strikethroughs are not part of the CommonMark standard
//! // and we therefore must enable it explicitly.
//! let mut options = Options::empty();
//! options.insert(Options::ENABLE_STRIKETHROUGH);
//! let parser = Parser::new_ext(markdown_input, options);
//!
//! // Write to String buffer.
//! let mut html_output = String::new();
//! html::push_html(&mut html_output, parser);
//!
//! // Check that the output is what we expected.
//! let expected_html = "<p>Hello world, this is a <del>complicated</del> <em>very simple</em> example.</p>\n";
//! assert_eq!(expected_html, &html_output);
//! ```

// When compiled for the rustc compiler itself we want to make sure that this is
// an unstable crate.
#![cfg_attr(rustbuild, feature(staged_api, rustc_private))]
#![cfg_attr(rustbuild, unstable(feature = "rustc_private", issue = "27812"))]
// Forbid unsafe code unless the SIMD feature is enabled.
#![cfg_attr(not(feature = "simd"), forbid(unsafe_code))]
#![warn(missing_debug_implementations)]

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub mod html;

mod entities;
pub mod escape;
mod firstpass;
mod linklabel;
mod parse;
mod puncttable;
mod scanners;
mod strings;
mod tree;

use std::{convert::TryFrom, fmt::Display};

pub use crate::parse::{BrokenLink, BrokenLinkCallback, LinkDef, OffsetIter, Parser, RefDefs};
pub use crate::strings::{CowStr, InlineStr};

/// Codeblock kind.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum CodeBlockKind<'a> {
    Indented,
    /// The value contained in the tag describes the language of the code, which may be empty.
    #[cfg_attr(feature = "serde", serde(borrow))]
    Fenced(CowStr<'a>),
}

impl<'a> CodeBlockKind<'a> {
    pub fn is_indented(&self) -> bool {
        matches!(*self, CodeBlockKind::Indented)
    }

    pub fn is_fenced(&self) -> bool {
        matches!(*self, CodeBlockKind::Fenced(_))
    }
}

/// Tags for elements that can contain other elements.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Tag<'a> {
    /// A paragraph of text and other inline elements.
    Paragraph,

    /// A heading. The first field indicates the level of the heading,
    /// the second the fragment identifier, and the third the classes.
    Heading(HeadingLevel, Option<&'a str>, Vec<&'a str>),

    BlockQuote,
    /// A code block.
    CodeBlock(CodeBlockKind<'a>),

    /// A list. If the list is ordered the field indicates the number of the first item.
    /// Contains only list items.
    List(Option<u64>), // TODO: add delim and tight for ast (not needed for html)
    /// A list item.
    Item,
    /// A footnote definition. The value contained is the footnote's label by which it can
    /// be referred to.
    #[cfg_attr(feature = "serde", serde(borrow))]
    FootnoteDefinition(CowStr<'a>),

    /// A table. Contains a vector describing the text-alignment for each of its columns.
    Table(Vec<Alignment>),
    /// A table header. Contains only `TableCell`s. Note that the table body starts immediately
    /// after the closure of the `TableHead` tag. There is no `TableBody` tag.
    TableHead,
    /// A table row. Is used both for header rows as body rows. Contains only `TableCell`s.
    TableRow,
    TableCell,

    // span-level tags
    Emphasis,
    Strong,
    Strikethrough,

    /// A link. The first field is the link type, the second the destination URL and the third is a title.
    Link(LinkType, CowStr<'a>, CowStr<'a>),

    /// An image. The first field is the link type, the second the destination URL and the third is a title.
    Image(LinkType, CowStr<'a>, CowStr<'a>),
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum HeadingLevel {
    H1 = 1,
    H2,
    H3,
    H4,
    H5,
    H6,
}

impl Display for HeadingLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::H1 => write!(f, "h1"),
            Self::H2 => write!(f, "h2"),
            Self::H3 => write!(f, "h3"),
            Self::H4 => write!(f, "h4"),
            Self::H5 => write!(f, "h5"),
            Self::H6 => write!(f, "h6"),
        }
    }
}

/// Returned when trying to convert a `usize` into a `Heading` but it fails
/// because the usize isn't a valid heading level
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct InvalidHeadingLevel(usize);

impl TryFrom<usize> for HeadingLevel {
    type Error = InvalidHeadingLevel;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::H1),
            2 => Ok(Self::H2),
            3 => Ok(Self::H3),
            4 => Ok(Self::H4),
            5 => Ok(Self::H5),
            6 => Ok(Self::H6),
            _ => Err(InvalidHeadingLevel(value)),
        }
    }
}

/// Type specifier for inline links. See [the Tag::Link](enum.Tag.html#variant.Link) for more information.
#[derive(Clone, Debug, PartialEq, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum LinkType {
    /// Inline link like `[foo](bar)`
    Inline,
    /// Reference link like `[foo][bar]`
    Reference,
    /// Reference without destination in the document, but resolved by the broken_link_callback
    ReferenceUnknown,
    /// Collapsed link like `[foo][]`
    Collapsed,
    /// Collapsed link without destination in the document, but resolved by the broken_link_callback
    CollapsedUnknown,
    /// Shortcut link like `[foo]`
    Shortcut,
    /// Shortcut without destination in the document, but resolved by the broken_link_callback
    ShortcutUnknown,
    /// Autolink like `<http://foo.bar/baz>`
    Autolink,
    /// Email address in autolink like `<john@example.org>`
    Email,
}

impl LinkType {
    fn to_unknown(self) -> Self {
        match self {
            LinkType::Reference => LinkType::ReferenceUnknown,
            LinkType::Collapsed => LinkType::CollapsedUnknown,
            LinkType::Shortcut => LinkType::ShortcutUnknown,
            _ => unreachable!(),
        }
    }
}

/// Markdown events that are generated in a preorder traversal of the document
/// tree, with additional `End` events whenever all of an inner node's children
/// have been visited.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Event<'a> {
    /// Start of a tagged element. Events that are yielded after this event
    /// and before its corresponding `End` event are inside this element.
    /// Start and end events are guaranteed to be balanced.
    #[cfg_attr(feature = "serde", serde(borrow))]
    Start(Tag<'a>),
    /// End of a tagged element.
    #[cfg_attr(feature = "serde", serde(borrow))]
    End(Tag<'a>),
    /// A text node.
    #[cfg_attr(feature = "serde", serde(borrow))]
    Text(CowStr<'a>),
    /// An inline code node.
    #[cfg_attr(feature = "serde", serde(borrow))]
    Code(CowStr<'a>),
    /// An HTML node.
    #[cfg_attr(feature = "serde", serde(borrow))]
    Html(CowStr<'a>),
    /// A reference to a footnote with given label, which may or may not be defined
    /// by an event with a `Tag::FootnoteDefinition` tag. Definitions and references to them may
    /// occur in any order.
    #[cfg_attr(feature = "serde", serde(borrow))]
    FootnoteReference(CowStr<'a>),
    /// A soft line break.
    SoftBreak,
    /// A hard line break.
    HardBreak,
    /// A horizontal ruler.
    Rule,
    /// A task list marker, rendered as a checkbox in HTML. Contains a true when it is checked.
    TaskListMarker(bool),
}

/// Table column text alignment.
#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]

pub enum Alignment {
    /// Default text alignment.
    None,
    Left,
    Center,
    Right,
}

bitflags::bitflags! {
    /// Option struct containing flags for enabling extra features
    /// that are not part of the CommonMark spec.
    pub struct Options: u32 {
        const ENABLE_TABLES = 1 << 1;
        const ENABLE_FOOTNOTES = 1 << 2;
        const ENABLE_STRIKETHROUGH = 1 << 3;
        const ENABLE_TASKLISTS = 1 << 4;
        const ENABLE_SMART_PUNCTUATION = 1 << 5;
        /// Extension to allow headings to have ID and classes.
        ///
        /// `# text { #id .class1 .class2 }` is interpreted as a level 1 heading
        /// with the content `text`, ID `id`, and classes `class1` and `class2`.
        /// Note that attributes (ID and classes) should be space-separated.
        const ENABLE_HEADING_ATTRIBUTES = 1 << 6;
    }
}
