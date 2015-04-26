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

//! Main public pull parse interface, running two passes over input.

//use parse;
use parse::{RawParser, Event, Tag};
use std::vec;
use std::iter::IntoIterator;

pub struct Parser<'a> {
	events: vec::IntoIter<(Event<'a>, usize)>,
	offset: usize,
}

impl<'a> Parser<'a> {
	pub fn new(text: &'a str) -> Parser<'a> {
		// first pass, collecting info
		let mut inner = RawParser::new(text);
		let mut event_vec = Vec::new();
		loop {
			match inner.next() {
				Some(event) => event_vec.push((event, inner.get_offset())),
				None => break
			}
		}

		// second pass runs in iterator
		let events = event_vec.into_iter();
		Parser {
			events: events,
			offset: 0,
		}
	}

	pub fn get_offset(&self) -> usize {
		self.offset
	}
}

impl<'a> Iterator for Parser<'a> {
	type Item = Event<'a>;

	fn next(&mut self) -> Option<Event<'a>> {
		match self.events.next() {
			Some((event, offset)) => {
				self.offset = offset;
				Some(event)
			}
			None => None
		}
	}
}
