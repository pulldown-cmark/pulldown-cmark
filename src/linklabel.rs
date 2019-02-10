// Copyright 2018 Google LLC
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

//! Link label parsing and matching.

use std::borrow::Cow;

use unicase::UniCase;

use crate::scanners::{scan_ch, is_ascii_whitespace, scan_while};

pub type LinkLabel<'a> = UniCase<Cow<'a, str>>;

pub(crate) fn scan_link_label<'a>(text: &'a str) -> Option<(usize, LinkLabel<'a>)> {
    if scan_ch(text, b'[') == 0 {
        return None;
    }
    let mut i = 1;
    let mut only_white_space = true;
    let mut still_borrowed = true;
    let bytes = text.as_bytes();
    // no worries, doesnt allocate until we push things onto it
    let mut label = String::new();

    // TODO: loop over chars instead of bytes to be more utf-8 compatible?
    loop {
        if i >= bytes.len() || i >= 1000 { return None; }
        let mut c = bytes[i];
        match c {
            b'[' => return None,
            b']' => break,
            b'\\' => {
                let _next_byte = *bytes.get(i + 1)?;
                // TODO: does it matter what the next byte is?
                i += 2;
            }
            _ if is_ascii_whitespace(c) => {
                // normalize labels by collapsing whitespaces, including linebreaks
                let whitespaces = scan_while(&text[i..], is_ascii_whitespace);
                if whitespaces > 1 || c != b' ' {
                    if still_borrowed {
                        label.push_str(&text[1..i]);
                    }
                    still_borrowed = false;
                    c = b' ';
                }
                i += whitespaces;
            }
            _ => {
                only_white_space = false;
                i += 1;
            }
        }

        if !still_borrowed {
            label.push(c as char);
        }
    }

    if only_white_space {
        return None;
    }
    if scan_ch(&text[i..], b']') == 0 {
        None
    } else {
        let cow = if still_borrowed {
            text[1..i].into()
        } else {
            label.into()
        };
        Some((i + 1, UniCase::new(cow)))
    }
}
