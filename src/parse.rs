
pub struct Parser<'a> {
	text: &'a str,
	off: usize,
	limit: usize,  // limit of the current block

	// perhaps the limit should be stored on the stack with the tag
	stack: Vec<Tag>,
}

#[derive(Copy, Debug)]
pub enum Tag {
	Paragraph,
	Rule
}

pub enum Event<'a> {
	Start(Tag),
	End(Tag),
	Text(&'a str),  // should probably be a Cow
}

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

impl<'a> Parser<'a> {
	pub fn new(text: &'a str) -> Parser<'a> {
		Parser {
			text: text,
			off: 0,
			limit: 0,
			stack: Vec::new(),
		}
	}

	// offset into text representing current parse position, hopefully
	// useful for building source maps
	pub fn get_offset(&self) -> usize {
		self.off
	}

	fn start(&mut self, tag: Tag) -> Event<'a> {
		self.stack.push(tag);
		Event::Start(tag)
	}

	fn end(&mut self) -> Event<'a> {
		let ret = Event::End(self.stack.pop().unwrap());
		if self.stack.is_empty() {
			// skip blank lines
			loop {
				let ret = scan_blank_line(&self.text[self.off..]);
				if ret == 0 {
					break;
				}
				self.off += ret;
			}
		}
		ret
	}

	fn next_block(&mut self) -> Option<Event<'a>> {
		if self.off < self.text.len() {
			// each branch here starts a block by:
			// 1. scanning for the end of the block and setting limit
			// 2. pushing a block tag on the stack and returning its start event

			let ret = scan_hrule(&self.text[self.off..]);
			if ret != 0 {
				self.off += ret;
				return Some(self.start_hrule())
			}

			Some(self.start_paragraph())
		} else {
			None
		}
	}

	fn start_paragraph(&mut self) -> Event<'a> {
		let mut i = self.off;
		while i < self.text.len() {
			if scan_blank_line(&self.text[i..]) != 0 ||
					scan_hrule(&self.text[i..]) != 0 {
				break;
			}
			i += scan_newline(&self.text[i..]);
		}
		self.limit = i;
		self.start(Tag::Paragraph)
	}

	fn start_hrule(&mut self) -> Event<'a> {
		self.limit = self.off;  // body of rule is empty
		self.start(Tag::Rule)
	}

	fn next_inline(&mut self) -> Event<'a> {
		if self.off < self.limit {
			let beg = self.off;
			self.off = self.limit;
			Event::Text(&self.text[beg .. self.limit])
		} else {
			self.end()
		}
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
			Some(tag) => Some(Event::End(tag)),
			None => None
		}
	}
}
