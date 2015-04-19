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
	}
}
