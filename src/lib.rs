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

pub mod html;

#[macro_use]
extern crate bitflags;
extern crate unicase;

mod entities;
pub mod escape;
mod linklabel;
mod parse;
mod puncttable;
mod scanners;
mod strings;
mod tree;

#[cfg(all(target_arch = "x86_64", feature = "simd"))]
mod simd;

pub use crate::parse::{
    Alignment, BrokenLink, CodeBlockKind, Event, LinkType, OffsetIter, Options, Parser, Tag,
};
pub use crate::strings::{CowStr, InlineStr};
