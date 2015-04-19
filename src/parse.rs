
pub struct Parser<'a> {
	text: &'a str,
	off: usize,
	stack: Vec<Tag>,
}

#[derive(Copy, Debug)]
pub enum Tag {
	Paragraph,
}

pub enum Event<'a> {
	Start(Tag),
	End(Tag),
	Text(&'a str),
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

impl<'a> Parser<'a> {
	pub fn new(text: &'a str) -> Parser<'a> {
		Parser {
			text: text,
			off: 0,
			stack: Vec::new(),
		}
	}

	fn start(&mut self, tag: Tag) -> Event<'a> {
		self.stack.push(tag);
		Event::Start(tag)
	}

	fn next_block(&mut self) -> Option<Event<'a>> {
		loop {
			let ret = scan_blank_line(&self.text[self.off..]);
			if ret == 0 {
				break;
			}
			self.off += ret;
		}
		if self.off < self.text.len() {
			Some(self.start(Tag::Paragraph))
		} else {
			None
		}
	}

	fn next_paragraph(&mut self) -> Event<'a> {
		let beg = self.off;
		let mut i = beg;
		while i < self.text.len() {
			if scan_blank_line(&self.text[i..]) != 0 {
				break;
			}
			i += scan_newline(&self.text[i..]);
		}
		if i > beg {
			self.off = i;
			Event::Text(&self.text[beg..i])
		} else {
			Event::End(self.stack.pop().unwrap())
		}
	}
}

impl<'a> Iterator for Parser<'a> {
	type Item = Event<'a>;

	fn next(&mut self) -> Option<Event<'a>> {
		if self.off < self.text.len() {
			match self.stack.last() {
				Some(&Tag::Paragraph) => return Some(self.next_paragraph()),
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
