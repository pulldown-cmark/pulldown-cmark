use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::ops::Drop;
use std::ptr;

pub struct Arena<'a> {
	ptr: *mut u8,
	capacity: usize,
	offset: usize,
	marker: PhantomData<&'a str>,
}

impl<'a> Arena<'a> {
	#[inline]
	pub fn with_capacity(capacity: usize) -> Self {
		let mut buf = ManuallyDrop::new(Vec::with_capacity(capacity));

		// Ensure we use actual allocated capacity!
		let capacity = buf.capacity();
		let ptr = buf.as_mut_ptr();

		Arena {
			ptr,
			capacity,
			offset: 0,
			marker: PhantomData,
		}
	}

	#[inline]
	pub fn alloc_char(&mut self, c: char) -> &'a str {
		let len = c.len_utf8();

		if self.offset + len > self.capacity {
			panic!("Arena out of bounds!");
		}

		let slice = unsafe {
			ptr::slice_from_raw_parts_mut(
				self.ptr.add(self.offset),
				len,
			)
		};

		self.offset += len;

		c.encode_utf8(unsafe { &mut*slice })
	}

	#[inline]
	pub fn alloc_str(&mut self, slice: &str) -> &'a str {
		if self.offset + slice.len() > self.capacity {
			panic!("Arena out of bounds!");
		}

		let target = unsafe { self.ptr.add(self.offset) };

		unsafe {
			ptr::copy_nonoverlapping(
				slice.as_ptr(),
				target,
				slice.len(),
			);
		}

		self.offset += slice.len();

		unsafe {
			&*(ptr::slice_from_raw_parts(target, slice.len()) as *const str)
		}
	}

	#[inline]
	pub fn builder(&mut self) -> StrBuilder<'a, '_> {
		StrBuilder {
			start: self.offset,
			arena: self,
		}
	}
}

pub struct StrBuilder<'a, 'b> {
	start: usize,
	arena: &'b mut Arena<'a>,
}

impl<'a, 'b> From<StrBuilder<'a, 'b>> for &'a str {
	#[inline]
	fn from(builder: StrBuilder<'a, 'b>) -> Self {
		builder.finish()
	}
}

impl<'a, 'b> StrBuilder<'a, 'b> {
	#[inline]
	pub fn push_str(&mut self, slice: &str) {
		self.arena.alloc_str(slice);
	}

	#[inline]
	pub fn push(&mut self, c: char) {
		self.arena.alloc_char(c);
	}

	#[inline]
	pub fn len(&self) -> usize {
		self.arena.offset - self.start
	}

	#[inline]
	pub fn finish(self) -> &'a str {
		let raw = unsafe {
			ptr::slice_from_raw_parts(
				self.arena.ptr.add(self.start),
				self.len(),
			)
		};

		unsafe { &*(raw as *const str) }
	}
}

impl Drop for Arena<'_> {
	#[inline]
	fn drop(&mut self) {
		unsafe {
			Vec::from_raw_parts(
				self.ptr,
				0,
				self.capacity,
			);
		}
	}
}