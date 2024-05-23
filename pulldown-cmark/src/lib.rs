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
//! use pulldown_cmark::{Parser, Options};
//!
//! let markdown_input = "Hello world, this is a ~~complicated~~ *very simple* example.";
//!
//! // Set up options and parser. Strikethroughs are not part of the CommonMark standard
//! // and we therefore must enable it explicitly.
//! let mut options = Options::empty();
//! options.insert(Options::ENABLE_STRIKETHROUGH);
//! let parser = Parser::new_ext(markdown_input, options);
//!
//! # #[cfg(feature = "html")] {
//! // Write to String buffer.
//! let mut html_output = String::new();
//! pulldown_cmark::html::push_html(&mut html_output, parser);
//!
//! // Check that the output is what we expected.
//! let expected_html = "<p>Hello world, this is a <del>complicated</del> <em>very simple</em> example.</p>\n";
//! assert_eq!(expected_html, &html_output);
//! # }
//! ```
//!
//! Note that consecutive text events can happen due to the manner in which the
//! parser evaluates the source. A utility `TextMergeStream` exists to improve
//! the comfort of iterating the events:
//!
//! ```rust
//! use pulldown_cmark::{Event, Parser, TextMergeStream};
//!
//! let markdown_input = "Hello world, this is a ~~complicated~~ *very simple* example.";
//!
//! let iterator = TextMergeStream::new(Parser::new(markdown_input));
//!
//! for event in iterator {
//!     match event {
//!         Event::Text(text) => println!("{}", text),
//!         _ => {}
//!     }
//! }
//! ```
//!

// When compiled for the rustc compiler itself we want to make sure that this is
// an unstable crate.
#![cfg_attr(rustbuild, feature(staged_api, rustc_private))]
#![cfg_attr(rustbuild, unstable(feature = "rustc_private", issue = "27812"))]
// Forbid unsafe code unless the SIMD feature is enabled.
#![cfg_attr(not(feature = "simd"), forbid(unsafe_code))]
#![warn(missing_debug_implementations)]

#[cfg(feature = "html")]
pub mod html;

pub mod utils;

pub use pulldown_cmark_lib::{
    BrokenLink, BrokenLinkCallback, DefaultBrokenLinkCallback, OffsetIter, Parser, RefDefs,
};
pub use pulldown_cmark_lib::{CowStr, InlineStr};
pub use crate::utils::*;

pub use pulldown_cmark_lib::CodeBlockKind;
pub use pulldown_cmark_lib::BlockQuoteKind;
pub use pulldown_cmark_lib::MetadataBlockKind;
pub use pulldown_cmark_lib::Tag;
pub use pulldown_cmark_lib::TagEnd;
pub use pulldown_cmark_lib::HeadingLevel;
pub use pulldown_cmark_lib::InvalidHeadingLevel;
pub use pulldown_cmark_lib::LinkType;
pub use pulldown_cmark_lib::Event;
pub use pulldown_cmark_lib::Alignment;
pub use pulldown_cmark_lib::Options;
