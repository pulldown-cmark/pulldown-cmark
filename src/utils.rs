// Copyright 2015 Google Inc. All rights reserved.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

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
