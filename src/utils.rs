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

//! Utilities for manipulating strings.

use std::cmp;
use std::borrow::Cow;
use std::borrow::Cow::Owned;

fn ascii_tolower(c: u8) -> u8 {
	match c {
		b'A' ... b'Z' => c + b'a' - b'A',
		_ => c
	}
}

// Compare two strings, with case folding for ASCII
pub fn strcasecmp(a: &str, b: &str) -> cmp::Ordering {
	for i in 0..cmp::min(a.len(), b.len()) {
		match ascii_tolower(a.as_bytes()[i]).cmp(&ascii_tolower(b.as_bytes()[i])) {
			cmp::Ordering::Equal => (),
			ordering => return ordering			
		}
	}
	a.len().cmp(&b.len())
}

pub fn cow_append<'a>(a: Cow<'a, str>, b: Cow<'a, str>) -> Cow<'a, str> {
	if a.is_empty() {
		b
	} else if b.is_empty() {
		a
	} else {
		Owned(a.into_owned() + &b)
	}
}
