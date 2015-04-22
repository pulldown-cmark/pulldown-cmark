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
use escape::escape_html;

pub fn push_html<'a, I: Iterator<Item=Event<'a>>>(buf: &mut String, iter: I) {
	for event in iter {
		match event {
			Event::Start(tag) => start_tag(buf, tag),
			Event::End(tag) => end_tag(buf, tag),
			Event::Text(text) => escape_html(buf, text, false),
			Event::Entity(text) => buf.push_str(text),
			Event::LineBreak => buf.push_str("<br />\n")
		}
	}
}

fn start_tag(buf: &mut String, tag: Tag) {
	match tag {
		Tag::Paragraph => buf.push_str("<p>"),
		Tag::Rule => buf.push_str("<hr />\n"),
		Tag::Header(level) => {
			buf.push_str("<h");
			buf.push((b'0' + level as u8) as char);
			buf.push('>');
		}
		Tag::Emphasis => buf.push_str("<em>"),
		Tag::Strong => buf.push_str("<strong>")
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
		Tag::Emphasis => buf.push_str("</em>"),
		Tag::Strong => buf.push_str("</strong>")
	}
}
