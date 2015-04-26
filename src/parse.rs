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

//! Raw parser, for doing a single pass over input.

use scanners::*;
use std::borrow::Cow;
use std::borrow::Cow::Borrowed;

#[derive(PartialEq, Debug)]
enum State {
	StartBlock,
	InContainers,
	Inline,
	CodeLineStart,
	Code,
}

#[derive(Copy, Clone, Debug)]
enum Container {
	BlockQuote,
	List(u8),
	ListItem(usize),
}

pub struct RawParser<'a> {
	text: &'a str,
	off: usize,

	state: State,
	stack: Vec<(Tag<'a>, usize, usize)>,

	containers: Vec<Container>,

	// state for code fences
	fence_char: u8,
	fence_count: usize,
	fence_indent: usize,
}

#[derive(Clone, Debug)]
pub enum Tag<'a> {
	// block-level tags
	Paragraph,
	Rule,
	Header(i32),
	BlockQuote,
	CodeBlock(&'a str),
	List(Option<usize>),  // TODO: add delim and tight for ast (not needed for html)
	Item,

	// span-level tags
	Emphasis,
	Strong,
	Link(Cow<'a, str>, Cow<'a, str>),
	Image(Cow<'a, str>, Cow<'a, str>),
}

pub enum Event<'a> {
	Start(Tag<'a>),
	End(Tag<'a>),
	Text(Cow<'a, str>),
	SoftBreak,
	HardBreak,
}

impl<'a> RawParser<'a> {
	pub fn new(text: &'a str) -> RawParser<'a> {
		let mut ret = RawParser {
			text: text,
			off: if text.starts_with("\u{FEFF}") { 3 } else { 0 },
			state: State::StartBlock,
			stack: Vec::new(),
			containers: Vec::new(),

			fence_char: 0,
			fence_count: 0,
			fence_indent: 0,
		};
		ret.skip_blank_lines();
		ret
	}

	// offset into text representing current parse position, hopefully
	// useful for building source maps
	pub fn get_offset(&self) -> usize {
		self.off
	}

	fn limit(&self) -> usize {
		match self.stack.last() {
			Some(&(_, limit, _)) => limit,
			None => self.text.len()
		}
	}

	// if end is not known, limit should be text.len(), next should be 0
	fn start(&mut self, tag: Tag<'a>, limit: usize, next: usize) -> Event<'a> {
		self.stack.push((tag.clone(), limit, next));
		Event::Start(tag)
	}

	fn end(&mut self) -> Event<'a> {
		let (tag, _, next) = self.stack.pop().unwrap();
		match tag {
			// containers
			Tag::BlockQuote | Tag::List(_) | Tag::Item => {
				let _ = self.containers.pop();
			}

			// block level tags
			Tag::Paragraph | Tag::Header(_) | Tag::Rule | Tag::CodeBlock(_) => {
				self.state = State::StartBlock;
				// TODO: skip blank lines (for cleaner source maps)
			}

			// inline
			_ => ()
		}
		if next != 0 { self.off = next; }

		/*
		if self.stack.is_empty() {
			// TODO maybe: make block ends do this
			self.state = State::StartBlock;
			self.skip_blank_lines();
		}
		*/
		Event::End(tag)
	}

	fn skip_leading_whitespace(&mut self) {
		let limit = self.limit();
		while self.off < limit {
			match self.text.as_bytes()[self.off] {
				b' ' | b'\t' | b'\x0b' ... b'\r' => self.off += 1,
				_ => break
			}
		}
	}

	fn skip_inline_linestart(&mut self) {
		self.skip_leading_whitespace();
	}

	fn skip_code_linestart(&mut self) {
		let (n, _) = calc_indent(&self.text[self.off ..], self.fence_indent);
		// TODO: handle case where tab character takes us past fence indent
		self.off += n;
	}

	fn skip_blank_lines(&mut self) {
		loop {
			let ret = scan_blank_line(&self.text[self.off..]);
			if ret == 0 {
				break;
			}
			self.off += ret;
		}
	}

	// Scan markers and indentation for current container stack
	fn scan_containers(&mut self, lazy: bool) -> (usize, bool) {
		let tail = &self.text[self.off ..];
		let mut i = 0;
		for container in self.containers.iter() {
			match *container {
				Container::BlockQuote => {
					let n = scan_blockquote_start(&tail[i..]);
					if n == 0 {
						return (i, lazy);
					} else {
						i += n;
					}
				}
				Container::List(_) => (),
				Container::ListItem(indent) => {
					let (n, actual) = calc_indent(&tail[i..], indent);
					if actual < indent {
						return (i, lazy || scan_eol(&tail[i + n .. ]).1);
					} else {
						i += n;
					}
				}
			}
		}
		(i, true)
	}

	fn start_block(&mut self) -> Option<Event<'a>> {
		let size = self.text.len();
		while self.off < size {
			if self.state != State::InContainers {
				let (n, scanned) = self.scan_containers(false);
				if !scanned {
					return Some(self.end());
				}
				self.off += n;
				self.state = State::InContainers;
			}

			let n = scan_blank_line(&self.text[self.off..]);
			if n != 0 {
				self.off += n;
				self.state = State::StartBlock;
				// two blank lines close a list
				if let Some(&Container::List(_)) = self.containers.last() {
					let (n_containers, scanned) = self.scan_containers(false);
					if scanned {
						let n_line2 = scan_blank_line(&self.text[self.off + n_containers ..]);
						if n_line2 != 0 {
							let off = self.off + n_containers + n_line2;
							let ret = Some(self.end());
							self.off = off;
							return ret;
						}
					}
				}
				continue;
			}

			// must be before list item because ambiguous
			let n = scan_hrule(&self.text[self.off..]);
			if n != 0 {
				// see below
				if let Some(&Container::List(_)) = self.containers.last() {
					return Some(self.end());
				}
				self.off += n;
				return Some(self.start_hrule());
			}

			let (n, c, start, indent) = scan_listitem(&self.text[self.off ..]);
			if n != 0 {
				return Some(self.start_listitem(n, c, start, indent));
			}

			// not a list item, so if we're in a list, close it
			if let Some(&Container::List(_)) = self.containers.last() {
				return Some(self.end());
			}

			let (n, level) = scan_atx_header(&self.text[self.off ..]);
			if n != 0 {
				self.off += n;
				return Some(self.start_atx_header(level));
			}

			let (n, ch, count, indent) = scan_code_fence(&self.text[self.off ..]);
			if n != 0 {
				return Some(self.start_code_fence(n, ch, count, indent));
			}

			if calc_indent(&self.text[self.off ..], 4).1 == 4 {
				return Some(self.start_indented_code());
			}

			let n = scan_blockquote_start(&self.text[self.off ..]);
			if n != 0 {
				self.off += n;
				self.containers.push(Container::BlockQuote);
				return Some(self.start(Tag::BlockQuote, self.text.len(), 0));
			}

			return Some(self.start_paragraph());
		}
		None
	}

	// can start a paragraph or a setext header, as they start similarly
	fn start_paragraph(&mut self) -> Event<'a> {
		self.skip_leading_whitespace();

		let mut i = self.off + scan_nextline(&self.text[self.off..]);

		if let (n, true) = self.scan_containers(false) {
			i += n;
			let (n, level) = scan_setext_header(&self.text[i..]);
			if n != 0 {
				let next = i + n;
				while i > self.off && is_ascii_whitespace(self.text.as_bytes()[i - 1]) {
					i -= 1;
				}
				self.state = State::Inline;
				return self.start(Tag::Header(level), i, next);
			}
		}

		let size = self.text.len();
		// let the block closing logic in char_newline take care of computing the end
		self.state = State::Inline;
		self.start(Tag::Paragraph, size, 0)
	}

	fn start_hrule(&mut self) -> Event<'a> {
		let limit = self.off;  // body of hrule is empty
		self.state = State::Inline;  // handy state for producing correct end tag
		self.start(Tag::Rule, limit, limit)
	}

	fn start_atx_header(&mut self, level: i32) -> Event<'a> {
		self.skip_leading_whitespace();

		let tail = &self.text[self.off..];
		let next = scan_nextline(tail);
		let mut limit = next;
		while limit > 0 && is_ascii_whitespace(tail.as_bytes()[limit - 1]) {
			limit -= 1;
		}
		let mut end = limit;
		while end > 0 && tail.as_bytes()[end - 1] == b'#' {
			end -= 1;
		}
		if end == 0 {
			limit = end;
		} else if is_ascii_whitespace(tail.as_bytes()[end - 1]) {
			limit = end - 1;
		}
		let limit = limit + self.off;
		let next = next + self.off;
		self.state = State::Inline;
		self.start(Tag::Header(level), limit, next)
	}

	fn start_indented_code(&mut self) -> Event<'a> {
		self.fence_char = b'\0';
		self.fence_indent = 4;
		let size = self.text.len();
		self.state = State::Code;
		self.skip_code_linestart();
		self.start(Tag::CodeBlock(""), size, 0)
	}

	fn start_listitem(&mut self, n: usize, c: u8, start: usize, indent: usize) -> Event<'a> {
		match self.containers.last() {
			Some(&Container::List(c2)) => {
				if c != c2 {
					// mismatched list type or delimeter
					return self.end();
				}
				self.off += n;
				//self.state = State::Inline;  // TODO: don't do this if loose
				self.containers.push(Container::ListItem(indent));
				self.start(Tag::Item, self.text.len(), 0)
			}
			_ => {
				self.containers.push(Container::List(c));
				// arguably this should be done in the scanner, pass the option
				let startopt = if c == b'.' || c == b')' { Some(start) } else { None };
				self.start(Tag::List(startopt), self.text.len(), 0)
			}
		}
	}

	fn start_code_fence(&mut self, n: usize, ch: u8, count: usize, indent: usize) -> Event<'a> {
		self.fence_char = ch;
		self.fence_count = count;
		self.fence_indent = indent;
		let beg_info = self.off + n;
		let next_line = beg_info + scan_nextline(&self.text[beg_info..]);
		self.off = next_line;
		let info = &self.text[beg_info..next_line].trim();
		let size = self.text.len();
		self.state = State::CodeLineStart;
		self.start(Tag::CodeBlock(info), size, 0)
	}

	fn next_code_line_start(&mut self) -> Event<'a> {
		let off = match self.scan_containers(false) {
			(_, false) => {
				return self.end();
			}
			(n, _) => self.off + n
		};
		if self.is_code_block_end(off) {
			let ret = self.end();
			if self.fence_char != b'\0' {
				self.off = off + scan_nextline(&self.text[off..]);
			}
			ret
		} else {
			self.off = off;
			self.skip_code_linestart();
			self.state = State::Code;
			self.next_code()
		}
	}

	fn next_code(&mut self) -> Event<'a> {
		let size = self.text.len();
		let beg = self.off;
		let mut i = beg;
		while i < size {
			let c = self.text.as_bytes()[i];
			if c < b' ' {
				match c {
					b'\n' => {
						i += 1;
						self.state = State::CodeLineStart;
						break;
					}
					b'\t' => {
						if i > beg { break; }
						return self.char_tab();
					}
					// TODO: \r
					_ => ()
				}
			}
			i += 1;
		}
		self.off = i;
		Event::Text(Borrowed(&self.text[beg..i]))
	}

	fn is_code_block_end(&self, loc: usize) -> bool {
		let tail = &self.text[loc..];
		if self.fence_char == b'\0' {
			// indented code block
			let (_, spaces) = calc_indent(tail, 4);
			// TODO: handle blank lines specially
			spaces < 4
		} else {
			let (n, c, count, _) = scan_code_fence(tail);
			if c != self.fence_char || count < self.fence_count {
				return false;
			}
			if n < tail.len() && scan_blank_line(&tail[n..]) == 0 {
				// Closing code fences cannot have info strings
				return false;
			}
			return true;
		}
	}

	// determine whether the line starting at loc ends the block
	fn is_inline_block_end(&self, loc: usize) -> bool {
		let tail = &self.text[loc..];
		tail.is_empty() ||
				scan_blank_line(tail) != 0 ||
				scan_hrule(tail) != 0 ||
				scan_atx_header(tail).0 != 0 ||
				scan_code_fence(tail).0 != 0 ||
				scan_blockquote_start(tail) != 0 ||
				scan_listitem(tail).0 != 0
	}

	fn next_inline(&mut self) -> Event<'a> {
		let beg = self.off;
		let mut i = beg;
		let limit = self.limit();
		while i < limit {
			let c = self.text.as_bytes()[i];
			if self.is_active_char(c) {
				if i > beg {
					self.off = i;
					return Event::Text(Borrowed(&self.text[beg..i]));
				}
				if let Some(event) = self.active_char(c) {
					return event;
				}
			}
			i += 1;
		}
		if i > beg {
			self.off = i;
			Event::Text(Borrowed(&self.text[beg..i]))
		} else {
			self.end()
		}
	}

	fn is_active_char(&self, c: u8) -> bool {
		c == b'\t' || c == b'\n' || c == b'\r' || c == b'_' || c == b'\\' || c == b'&' ||
				c == b'_' || c == b'*' || c == b'[' || c == b'!'
	}

	fn active_char(&mut self, c: u8) -> Option<Event<'a>> {
		match c {
			b'\t' => Some(self.char_tab()),
			b'\n' => self.char_newline(),
			b'\r' => self.char_return(),
			b'\\' => self.char_backslash(),
			b'&' => self.char_entity(),
			b'_' => self.char_emphasis(),
			b'*' => self.char_emphasis(),
			b'[' | b'!' => self.char_link(),
			_ => None
		}
	}

	// expand tab in content (used for code and inline)
	// scan backward to find offset, counting unicode code points
	fn char_tab(&mut self) -> Event<'a> {
		let mut count = 0;
		let mut i = self.off;
		while i > 0 {
			i -= 1;
			let c = self.text.as_bytes()[i];
			if c == b'\t' || c == b'\n' {
				break;
			} else if (c & 0xc0) != 0x80 {
				count += 1;
			}
		}
		self.off += 1;
		Event::Text(Borrowed(&"    "[(count % 4) ..]))
	}

	// newline can be a bunch of cases, including closing a block
	fn char_newline(&mut self) -> Option<Event<'a>> {
		self.off += 1;
		let start = if let (n, true) = self.scan_containers(true) {
			self.off + n
		} else {
			return Some(self.end());
		};
		if self.is_inline_block_end(start) {
			//println!("off {} stack {:?} containers {:?}", self.off, self.stack, self.containers);
			Some(self.end())
		} else {
			self.off = start;
			self.skip_inline_linestart();
			Some(Event::SoftBreak)
		}
	}

	fn char_return(&mut self) -> Option<Event<'a>> {
		if self.text[self.off + 1..].starts_with('\n') {
			self.off += 1;
			Some(Event::Text(Borrowed("")))
		} else {
			None
		}
	}

	fn char_backslash(&mut self) -> Option<Event<'a>> {
		let limit = self.limit();
		if self.off + 1 < limit {
			let c = self.text.as_bytes()[self.off + 1];
			if c == b'\n' {
				if self.is_inline_block_end(self.off + 2) { return None; }
				self.off += 2;
				return Some(Event::HardBreak);
			} else if c == b'\r' && self.text[self.off + 2..limit].starts_with('\n') {
				if self.is_inline_block_end(self.off + 3) { return None; }
				self.off += 3;
				return Some(Event::HardBreak);
			}
			if is_ascii_punctuation(c) {
				self.off += 2;
				return Some(Event::Text(Borrowed(&self.text[self.off - 1 .. self.off])));
			}
		}
		None
	}

	fn char_entity(&mut self) -> Option<Event<'a>> {
		match scan_entity(&self.text[self.off ..]) {
			(n, Some(value)) => {
				self.off += n;
				Some(Event::Text(value))
			}
			_ => None
		}
	}

	fn char_emphasis(&mut self) -> Option<Event<'a>> {
		// can see to left for flanking info, but not past limit
		let data = &self.text[.. self.limit()];

		let c = data.as_bytes()[self.off];
		let (n, can_open, _can_close) = compute_open_close(data, self.off, c);
		if !can_open {
			return None;
		}
		let mut stack = vec![n];  // TODO performance: don't allocate
		let mut i = self.off + n;
		while i < data.len() {
			let c2 = data.as_bytes()[i];
			if c2 == b'\n' && !is_escaped(data, i) {
				if self.is_inline_block_end(i + 1) {
					return None
				} else {
					i += 1;
				}
			} else if c2 == c && !is_escaped(data, i) {
				let (mut n2, can_open, can_close) = compute_open_close(data, i, c);
				if can_close {
					loop {
						let ntos = stack.pop().unwrap();
						if ntos > n2 {
							stack.push(ntos - n2);
							break;
						}
						if stack.is_empty() {
							let npop = if ntos < n2 { ntos } else { n2 };
							if npop == 1 {
								self.off += 1;
								return Some(self.start(Tag::Emphasis, i, i + 1));
							} else {
								self.off += 2;
								let next = i + npop;
								return Some(self.start(Tag::Strong, next - 2, next));
							}
						} else {
							i += ntos;
							n2 -= ntos;
						}
					}
				} else if can_open {
					stack.push(n2);
				}
				i += n2;
			} else {
				i += 1;
			}
		}
		None
	}

	fn char_link(&mut self) -> Option<Event<'a>> {
		let beg = self.off;
		let tail = &self.text[beg .. self.limit()];
		let size = tail.len();

		// scan link text
		let (mut i, is_image) = if tail.as_bytes()[0] == b'!' {
			if size < 2 || tail.as_bytes()[1] != b'[' { return None; }
			(2, true)
		} else {
			(1, false)
		};
		let text_beg = i;
		let mut nest = 1;
		while i < size {
			match tail.as_bytes()[i] {
				b'\n' => { return None; }  // TODO: handle multiline (check block boundary)
				b'[' => nest += 1,
				b']' => {
					nest -= 1;
					if nest == 0 {
						break;
					}
				}
				b'\\' => i += 1,
				// TODO: ` etc
				_ => ()
			}
			i += 1;
		}
		if i >= size { return None; }
		let text_end = i;
		i += 1;  // skip closing ]

		// scan dest
		if !tail[i..].starts_with("(") { return None; }
		i += 1;
		while i < size && is_ascii_whitespace(tail.as_bytes()[i]) {
			// todo: check block boundary on newline
			i += 1;
		}
		if i >= size { return None; }

		let pointy = tail.as_bytes()[i] == b'<';
		if pointy { i += 1; }
		let dest_beg = i;
		let mut in_parens = false;
		while i < size {
			match tail.as_bytes()[i] {
				b'\n' => break,
				b' ' => {
					if !pointy && !in_parens { break; }
				}
				b'(' => {
					if !pointy {
						if in_parens { return None; }
						in_parens = true;
					}
				}
				b')' => {
					if !pointy {
						if !in_parens { break; }
						in_parens = false;
					}
				}
				b'>' => {
					if pointy { break; }
				}
				b'\\' => i += 1,
				_ => ()
			}
			i += 1;
		}
		let dest_end = i;
		if pointy {
			if i >= size || tail.as_bytes()[i] != b'>' { return None; }
			i += 1;
		}
		let dest = unescape(&tail[dest_beg..dest_end]);
		while i < size && is_ascii_whitespace(tail.as_bytes()[i]) {
			// todo: check block boundary on newline
			i += 1;
		}
		if i == size { return None; }

		// scan title
		let title = if tail.as_bytes()[i] == b')' {
			Borrowed("")
		} else {
			let titleclose = match tail.as_bytes()[i] {
				b'(' => b')',
				b'\'' => b'\'',
				b'\"' => b'\"',
				_ => return None
			};
			i += 1;
			let title_beg = i;
			while i < size {
				let c = tail.as_bytes()[i];
				if c == titleclose {
					break;
				} else if c == b'\\' {
					i += 2;  // may be > size
				} else {
					i += 1;
				}
			}
			if i >= size { return None; }
			let title_end = i;
			i += 1;
			unescape(tail[title_beg..title_end].trim_right())
		};
		while i < size && is_ascii_whitespace(tail.as_bytes()[i]) {
			// todo: check block boundary on newline
			i += 1;
		}
		if i == size || tail.as_bytes()[i] != b')' { return None; }
		i += 1;
		self.off = beg + text_beg;
		if is_image {
			Some(self.start(Tag::Image(dest, title), beg + text_end, beg + i))
		} else {
			Some(self.start(Tag::Link(dest, title), beg + text_end, beg + i))
		}
	}
}

impl<'a> Iterator for RawParser<'a> {
	type Item = Event<'a>;

	fn next(&mut self) -> Option<Event<'a>> {
		//println!("off {} {:?}, stack {:?} containers {:?}",
		//		self.off, self.state, self.stack, self.containers);
		if self.off < self.text.len() {
			match self.state {
				State::StartBlock | State::InContainers => {
					let ret = self.start_block();
					if ret.is_some() {
						return ret;
					}
				}
				State::Inline => return Some(self.next_inline()),
				State::CodeLineStart => return Some(self.next_code_line_start()),
				State::Code => return Some(self.next_code())
			}
		}
		match self.stack.pop() {
			Some((tag, _, _)) => Some(Event::End(tag)),
			None => None
		}
	}
}
