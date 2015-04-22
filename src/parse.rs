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

//! Main pull parser object.

use entities::is_valid_entity;

pub struct Parser<'a> {
	text: &'a str,
	off: usize,

	stack: Vec<(Tag, usize, usize)>,
}

#[derive(Copy, Clone, Debug)]
pub enum Tag {
	// block-level tags
	Paragraph,
	Rule,
	Header(i32),

	// span-level tags
	Emphasis,
	Strong,
}

pub enum Event<'a> {
	Start(Tag),
	End(Tag),
	Text(&'a str),  // should probably be a Cow
	Entity(&'a str),
	SoftBreak,
	HardBreak,
}

// sorted for binary_search
const ASCII_PUNCTUATION: &'static [u8] = b"!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";

fn scan_blank_line(text: &str) -> usize {
	let mut i = 0;
	while i < text.len() {
		match text.as_bytes()[i] {
			b'\n' => return i + 1,
			b' ' | b'\t' | b'\r' => i += 1,
			_ => return 0
		}
	}
	i
}

fn scan_newline(s: &str) -> usize {
	match s.find('\n') {
		Some(x) => x + 1,
		None => s.len()
	}
}

// returned pair is (number of bytes, number of spaces)
fn calc_indent(text: &str, max: usize) -> (usize, usize) {
	let mut i = 0;
	let mut spaces = 0;
	while i < text.len() && spaces < max {
		match text.as_bytes()[i] {
			b' ' => spaces += 1,
			b'\t' => {
				let new_spaces = spaces + 4 - (spaces & 3);
				if new_spaces > max {
					break;
				}
				spaces = new_spaces;
			},
			_ => break
		}
		i += 1;
	}
	(i, spaces)
}

// return size of line containing hrule, including trailing newline, or 0
fn scan_hrule(data: &str) -> usize {
	let size = data.len();
	let mut i = calc_indent(data, 3).0;
	if i + 2 >= size { return 0; }
	let c = data.as_bytes()[i];
	if !(c == b'*' || c == b'-' || c == b'_') { return 0; }
	let mut n = 0;
	while i < size {
		let c2 = data.as_bytes()[i];
		if c2 == b'\n' {
			i += 1;
			break;
		} else if c2 == c {
			n += 1;
		} else if c2 != b' ' {
			return 0;
		}
		i += 1;  // all possible characters at this point are ASCII
	}
	if n >= 3 { i } else { 0 }
}

// returns number of bytes in prefix and level
fn scan_atx_header(data: &str) -> (usize, i32) {
	let size = data.len();
	let (start, _) = calc_indent(data, 3);
	let data = data.as_bytes();
	let mut i = start;
	while i < size {
		if data[i] != b'#' {
			break;
		}
		i += 1;
	}
	let level = i - start;
	if level >= 1 && level <= 6 {
		if i <size {
			match data[i] {
				b' ' | b'\t' ... b'\r' => (),
				_ => return (0, 0)
			}
		}
		(i, level as i32)
	} else {
		(0, 0)
	}
}

// returns number of bytes in line and level
fn scan_setext_header(data: &str) -> (usize, i32) {
	let size = data.len();
	let (mut i, _) = calc_indent(data, 3);
	if i == size { return (0, 0); }
	let c = data.as_bytes()[i];
	if !(c == b'-' || c == b'=') { return (0, 0); }
	i += 1;
	while i < size && data.as_bytes()[i] == c {
		i += 1;
	}
	while i < size {
		match data.as_bytes()[i] {
			b'\n' => break,
			b' ' | b'\t' ... b'\r' => i += 1,
			_ => return (0, 0)
		}
	}
	let level = if c == b'=' { 1 } else { 2 };
	(i, level)
}

// return whether delimeter run can open or close
fn compute_open_close(data: &str, loc: usize, c: u8) -> (usize, bool, bool) {
	// TODO: handle Unicode, not just ASCII
	let size = data.len();
	let c = data.as_bytes()[loc];
	let mut end = loc + 1;
	while end < size && data.as_bytes()[end] == c {
		end += 1;
	}
	let mut beg = loc;
	while beg > 0 && data.as_bytes()[beg - 1] == c {
		beg -= 1;
	}
	let (white_before, punc_before) = if beg == 0 {
		(true, false)
	} else {
		let c = data.as_bytes()[beg - 1];
		(is_ascii_whitespace(c), is_ascii_punctuation(c))
	};
	let (white_after, punc_after) = if end == size {
		(true, false)
	} else {
		let c = data.as_bytes()[end];
		(is_ascii_whitespace(c), is_ascii_punctuation(c))
	};
	let left_flanking = !white_after && (!punc_after || white_before || punc_before);
	let right_flanking = !white_before && (!punc_before || white_after || punc_after);
	let (can_open, can_close) = match c {
		b'*' => (left_flanking, right_flanking),
		b'_' => (left_flanking && !right_flanking, right_flanking && !left_flanking),
		_ => (false, false)
	};
	(end - loc, can_open, can_close)
}

fn is_ascii_whitespace(c: u8) -> bool {
	(c >= 0x09 && c <= 0x0d) || c == b' '
}

fn is_ascii_alphanumeric(c: u8) -> bool {
	match c {
		b'0' ... b'9' | b'a' ... b'z' | b'A' ... b'Z' => true,
		_ => false
	}
}

fn is_hexdigit(c: u8) -> bool {
	match c {
		b'0' ... b'9' | b'a' ... b'f' | b'A' ... b'F' => true,
		_ => false
	}
}

fn is_digit(c: u8) -> bool {
	b'0' <= c && c <= b'9'
}

fn is_ascii_punctuation(c: u8) -> bool {
	ASCII_PUNCTUATION.binary_search(&c).is_ok()
}

// doesn't bother to check data[0] == '&'
fn scan_entity(data: &str) -> usize {
	let size = data.len();
	let bytes = data.as_bytes();
	let mut end = 1;
	if end < size && bytes[end] == b'#' {
		end += 1;
		if end < size && (bytes[end] == b'x' || bytes[end] == b'X') {
			end += 1;
			while end < size && is_hexdigit(bytes[end]) {
				end += 1;
			}
			if end < size && end > 3 && end < 12 && bytes[end] == b';' {
				return end + 1;
			}
		} else {
			while end < size && is_digit(bytes[end]) {
				end += 1;
			}
			if end < size && end > 2 && end < 11 && bytes[end] == b';' {
				return end + 1;
			}
		}
		return 0;
	}
	while end < size && is_ascii_alphanumeric(data.as_bytes()[end]) {
		end += 1;
	}
	if end < size && bytes[end] == b';' && is_valid_entity(&data[1..end]) {
		return end + 1;  // real entity
	}
	return 0;
}

fn is_escaped(data: &str, loc: usize) -> bool {
	let mut i = loc;
	while i >= 1 && data.as_bytes()[i - 1] == b'\\' {
		i -= 1;
	}
	((loc - i) & 1) != 0
}

impl<'a> Parser<'a> {
	pub fn new(text: &'a str) -> Parser<'a> {
		let mut ret = Parser {
			text: text,
			off: if text.starts_with("\u{FEFF}") { 3 } else { 0 },
			stack: Vec::new(),
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

	fn start(&mut self, tag: Tag, limit: usize, next: usize) -> Event<'a> {
		self.stack.push((tag, limit, next));
		Event::Start(tag)
	}

	fn end(&mut self) -> Event<'a> {
		let (tag, _, next) = self.stack.pop().unwrap();
		self.off = next;
		if self.stack.is_empty() {
			// TODO maybe: make block ends do this
			self.skip_blank_lines();
		}
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

	fn skip_blank_lines(&mut self) {
		loop {
			let ret = scan_blank_line(&self.text[self.off..]);
			if ret == 0 {
				break;
			}
			self.off += ret;
		}
	}

	fn next_block(&mut self) -> Option<Event<'a>> {
		if self.off < self.text.len() {
			// each branch here starts a block by:
			// 1. scanning for the end of the block and computing limit
			// 2. pushing a block tag on the stack and returning its start event

			let n = scan_hrule(&self.text[self.off..]);
			if n != 0 {
				self.off += n;
				return Some(self.start_hrule())
			}

			let (n, level) = scan_atx_header(&self.text[self.off..]);
			if n != 0 {
				self.off += n;
				return Some(self.start_atx_header(level))
			}

			Some(self.start_paragraph())
		} else {
			None
		}
	}

	// can start a paragraph or a setext header, as they start similarly
	fn start_paragraph(&mut self) -> Event<'a> {
		self.skip_leading_whitespace();

		// don't need to scan for paragraph interrupts on first line
		let mut i = self.off + scan_newline(&self.text[self.off..]);

		let (n, level) = scan_setext_header(&self.text[i..]);
		if n != 0 {
			let next = i + n;
			while i > self.off && is_ascii_whitespace(self.text.as_bytes()[i - 1]) {
				i -= 1;
			}
			return self.start(Tag::Header(level), i, next);
		}

		let size = self.text.len();
		// let the block closing logic in char_newline take care of computing the end
		self.start(Tag::Paragraph, size, size)
	}

	fn start_hrule(&mut self) -> Event<'a> {
		let limit = self.off;  // body of hrule is empty
		self.start(Tag::Rule, limit, limit)
	}

	fn start_atx_header(&mut self, level: i32) -> Event<'a> {
		self.skip_leading_whitespace();

		let tail = &self.text[self.off..];
		let next = scan_newline(tail);
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
		self.start(Tag::Header(level), limit, next)
	}

	// determine whether the given ends the block
	fn is_block_end(&self, loc: usize) -> bool {
		let tail = &self.text[loc..];
		tail.is_empty() ||
				scan_blank_line(tail) != 0 ||
				scan_hrule(tail) != 0 ||
				scan_atx_header(tail).0 != 0
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
					return Event::Text(&self.text[beg..i]);
				}
				if let Some(event) = self.active_char(c) {
					return event;
				}
			}
			i += 1;
		}
		if i > beg {
			self.off = i;
			Event::Text(&self.text[beg..i])
		} else {
			self.end()
		}
	}

	fn is_active_char(&self, c: u8) -> bool {
		c == b'\t' || c == b'\n' || c == b'\r' || c == b'_' || c == b'\\' || c == b'&' ||
				c == b'_' || c == b'*'
	}

	fn active_char(&mut self, c: u8) -> Option<Event<'a>> {
		match c {
			b'\t' => self.char_tab(),
			b'\n' => self.char_newline(),
			b'\r' => self.char_return(),
			b'\\' => self.char_backslash(),
			b'&' => self.char_entity(),
			b'_' => self.char_emphasis(),
			b'*' => self.char_emphasis(),
			_ => None
		}
	}

	// expand tab in inline content
	// scan backward to find offset, counting unicode code points
	fn char_tab(&mut self) -> Option<Event<'a>> {
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
		Some(Event::Text(&"    "[(count % 4) ..]))
	}

	// newline can be a bunch of cases, including closing a block
	fn char_newline(&mut self) -> Option<Event<'a>> {
		let next_off = self.off + 1;
		if self.is_block_end(next_off) {
			let (tag, limit, next) = self.stack.pop().unwrap();
			self.off = next_off;
			self.skip_blank_lines();
			Some(Event::End(tag))
		} else {
			self.off += 1;
			Some(Event::SoftBreak)
		}
	}

	fn char_return(&mut self) -> Option<Event<'a>> {
		if self.text[self.off + 1..].starts_with('\n') {
			self.off += 1;
			Some(Event::Text(""))
		} else {
			None
		}
	}

	fn char_backslash(&mut self) -> Option<Event<'a>> {
		let limit = self.limit();
		if self.off + 1 < limit {
			let c = self.text.as_bytes()[self.off + 1];
			if c == b'\n' {
				// TODO: make sure backslash at end of para is handled ok
				if self.is_block_end(self.off + 2) { return None }
				self.off += 2;
				return Some(Event::HardBreak);
			} else if c == b'\r' && self.text[self.off + 2..limit].starts_with('\n') {
				if self.is_block_end(self.off + 3) { return None }
				self.off += 3;
				return Some(Event::HardBreak);
			}
			if is_ascii_punctuation(c) {
				self.off += 2;
				return Some(Event::Text(&self.text[self.off - 1 .. self.off]));
			}
		}
		None
	}

	fn char_entity(&mut self) -> Option<Event<'a>> {
		let beg = self.off;
		let ret = scan_entity(&self.text[beg..]);
		if ret != 0 {
			self.off += ret;
			Some(Event::Entity(&self.text[beg .. self.off]))
		} else {
			None
		}
	}

	fn char_emphasis(&mut self) -> Option<Event<'a>> {
		// can see to left for flanking info, but not past limit
		let data = &self.text[.. self.limit()];

		let c = data.as_bytes()[self.off];
		let (n, can_open, can_close) = compute_open_close(data, self.off, c);
		if !can_open {
			return None;
		}
		let mut stack = vec![n];  // TODO performance: don't allocate
		let mut i = self.off + n;
		while i < data.len() {
			let c2 = data.as_bytes()[i];
			if c2 == b'\n' && !is_escaped(data, i) {
				if self.is_block_end(i + 1) {
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
}

impl<'a> Iterator for Parser<'a> {
	type Item = Event<'a>;

	fn next(&mut self) -> Option<Event<'a>> {
		if self.off < self.text.len() {
			match self.stack.last() {
				Some(_) => return Some(self.next_inline()),
				None => {
					let ret = self.next_block();
					if ret.is_some() { return ret; }
				}
			}
		}
		match self.stack.pop() {
			Some((tag, _, _)) => Some(Event::End(tag)),
			None => None
		}
	}
}
