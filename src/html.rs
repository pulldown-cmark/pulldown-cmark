
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
use parse::Event::{Start, End, Text, SoftBreak, HardBreak};
use escape::{escape_html, escape_href};

fn fresh_line(buf: &mut String) {
	if !(buf.is_empty() || buf.ends_with('\n')) {
		buf.push('\n');
	}
}

pub fn push_html<'a, I: Iterator<Item=Event<'a>>>(buf: &mut String, iter: I) {
	for event in iter {
		match event {
			Start(tag) => start_tag(buf, tag),
			End(tag) => end_tag(buf, tag),
			Text(text) => escape_html(buf, &text, false),
			SoftBreak => buf.push('\n'),
			HardBreak => buf.push_str("<br />\n")
		}
	}
}

fn start_tag(buf: &mut String, tag: Tag) {
	match tag {
		Tag::Paragraph =>  {
			fresh_line(buf);
			buf.push_str("<p>");
		}
		Tag::Rule => {
			fresh_line(buf);
			buf.push_str("<hr />\n")
		}
		Tag::Header(level) => {
			fresh_line(buf);
			buf.push_str("<h");
			buf.push((b'0' + level as u8) as char);
			buf.push('>');
		}
		Tag::BlockQuote => {
			fresh_line(buf);
			buf.push_str("<blockquote>\n");
		}
		Tag::CodeBlock(info) => {
			fresh_line(buf);
			let lang = info.split(' ').next().unwrap();
			if lang.is_empty() {
				buf.push_str("<pre><code>");
			} else {
				buf.push_str("<pre><code class=\"language-");
				escape_html(buf, lang, false);
				buf.push_str("\">");
			}
		}
		Tag::List(Some(1)) => {
			fresh_line(buf);
			buf.push_str("<ol>\n");
		}
		Tag::List(Some(start)) => {
			fresh_line(buf);
			let _ = write!(buf, "<ol start=\"{}\">\n", start);
		}
		Tag::List(None) => {
			fresh_line(buf);
			buf.push_str("<ul>\n");
		}
		Tag::Item => {
			fresh_line(buf);
			buf.push_str("<li>");
		}
		Tag::Emphasis => buf.push_str("<em>"),
		Tag::Strong => buf.push_str("<strong>"),
		Tag::Link(dest, title) => {
			buf.push_str("<a href=\"");
			escape_href(buf, &dest);
			if !title.is_empty() {
				buf.push_str("\" title=\"");
				escape_html(buf, &title, false);
			}
			buf.push_str("\">");
		}
		Tag::Image(dest, _) => {
			buf.push_str("<img src=\"");
			escape_href(buf, &dest);
			buf.push_str("\" alt=\"");
			// TODO: suppress markup for alt text
		}
	}
}

fn end_tag(buf: &mut String, tag: Tag) {
	match tag {
		Tag::Paragraph => buf.push_str("</p>\n"),
		Tag::Rule => (),
		Tag::Header(level) => {
			buf.push_str("</h");
			buf.push((b'0' + level as u8) as char);
			buf.push_str(">\n");
		}
		Tag::BlockQuote => buf.push_str("</blockquote>\n"),
		Tag::CodeBlock(_) => buf.push_str("</code></pre>\n"),
		Tag::List(Some(_)) => buf.push_str("</ol>\n"),
		Tag::List(None) => buf.push_str("</ul>\n"),
		Tag::Item => buf.push_str("</li>\n"),
		Tag::Emphasis => buf.push_str("</em>"),
		Tag::Strong => buf.push_str("</strong>"),
		Tag::Link(_, _) => buf.push_str("</a>"),
		Tag::Image(_, title) => {
			if !title.is_empty() {
				buf.push_str("\" title=\"");
				escape_html(buf, &title, false);
			}
			buf.push_str("\" />")
		}
	}
}
