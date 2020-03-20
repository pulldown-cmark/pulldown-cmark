use std::marker::PhantomData;

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct ArenaStr {
	offset: usize,
	len: usize,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum CowStr<'a> {
	Source(&'a str),
	Arena(ArenaStr),
}

impl<'a> From<&'a str> for CowStr<'a> {
	fn from(slice: &'a str) -> Self {
		CowStr::Source(slice)
	}
}

impl From<ArenaStr> for CowStr<'_> {
	fn from(slice: ArenaStr) -> Self {
		CowStr::Arena(slice)
	}
}

#[derive(Default)]
pub struct Arena<'a> {
	buf: String,
	marker: PhantomData<&'a str>,
}

impl<'a> Arena<'a> {
	pub fn with_capacity(capacity: usize) -> Self {
		Arena {
			buf: String::with_capacity(capacity),
			marker: PhantomData,
		}
	}

	pub fn alloc_char(&mut self, c: char) -> ArenaStr {
		let offset = self.buf.len();
		let len = c.len_utf8();

		self.buf.push(c);

		ArenaStr {
			offset,
			len,
		}
	}

	pub fn alloc_str(&mut self, slice: &str) -> ArenaStr {
		let offset = self.buf.len();
		let len = slice.len();

		self.buf.push_str(slice);

		ArenaStr {
			offset,
			len,
		}
	}

	pub fn builder(&mut self) -> StrBuilder<'_> {
		StrBuilder {
			offset: self.buf.len(),
			buf: &mut self.buf,
		}
	}

	pub fn get_str(&self, a: ArenaStr) -> &'a str {
		unsafe { &*(&self.buf[a.offset..(a.offset + a.len)] as *const str) }
	}

	pub fn as_str(&self, cow: CowStr<'a>) -> &'a str {
		match cow {
			CowStr::Source(slice) => slice,
			CowStr::Arena(a) => self.get_str(a),
		}
	}
}

pub struct StrBuilder<'a> {
	offset: usize,
	buf: &'a mut String,
}

impl<'a, 'b> From<StrBuilder<'b>> for CowStr<'a> {
	fn from(builder: StrBuilder<'b>) -> Self {
		CowStr::Arena(builder.finish())
	}
}

impl<'a> StrBuilder<'a> {
	pub fn push_str(&mut self, slice: &str) {
		self.buf.push_str(slice);
	}

	pub fn push(&mut self, c: char) {
		self.buf.push(c);
	}

	pub fn len(&self) -> usize {
		self.buf.len() - self.offset
	}

	pub fn finish(self) -> ArenaStr {
		ArenaStr {
			offset: self.offset,
			len: self.len(),
		}
	}

}