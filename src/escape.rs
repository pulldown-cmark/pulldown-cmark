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

//! Utility functions for HTML escaping

use std::str::from_utf8;

use crate::entities;
use crate::puncttable::is_ascii_punctuation;

static HREF_SAFE: [u8; 128] = [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1,
        0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
    ];

static HEX_CHARS: &'static [u8] = b"0123456789ABCDEF";

pub fn escape_href(ob: &mut String, s: &str, backslash_escapes: bool) {
    let mut mark = 0;
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < s.len() {
        let c = bytes[i];
        if c >= 0x80 || HREF_SAFE[c as usize] == 0 {
            // character needing escape

            // write partial substring up to mark
            if mark < i {
                ob.push_str(&s[mark..i]);
            }
            match c {
                b'&' => {
                    if let Some((bytes, replacement)) = parse_entity(&s[i..]) {
                        escape_href(ob, replacement, backslash_escapes);
                        mark = i + bytes;
                        i += bytes;
                        continue;
                    }
                    ob.push_str("&amp;");
                },
                b'\\' if backslash_escapes && i + 1 < s.len() && is_ascii_punctuation(bytes[i + 1]) => {
                    // skip \
                }
                b'\'' => {
                    ob.push_str("&#x27;");
                },
                _ => {
                    let mut buf = [0u8; 3];
                    buf[0] = b'%';
                    buf[1] = HEX_CHARS[((c as usize) >> 4) & 0xF];
                    buf[2] = HEX_CHARS[(c as usize) & 0xF];
                    ob.push_str(from_utf8(&buf).unwrap());
                }
            }
            mark = i + 1;  // all escaped characters are ASCII
        }
        i += 1;
    }
    ob.push_str(&s[mark..]);
}

static HTML_ESCAPE_TABLE: [u8; 256] = [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 3,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 5, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];

static HTML_ESCAPES: [&'static str; 7] = [
        "",
        "&quot;",
        "&amp;",
        "&#47;",
        "&lt;",
        "&gt;",
        "\\",
    ];

/// What does secure mean?
pub fn escape_html(ob: &mut String, s: &str, skip_unescape: bool) {
    let size = s.len();
    let bytes = s.as_bytes();
    let mut mark = 0;
    let mut i = 0;

    while i < size {
        match bytes[i..].iter().position(|&c| HTML_ESCAPE_TABLE[c as usize] != 0) {
            Some(pos) => {
                i += pos;
            }
            None => break
        }
        if !skip_unescape {
            // don't unescape inside code blocks and titles
            if let Some((bytes, c)) = parse_unicode_lit(&bytes[i..]) {
                ob.push_str(&s[mark..i]);
                ob.push(c);
                i += bytes;
                mark = i;
                continue;
            }
            if let Some((bytes, replacement)) = parse_entity(&s[i..]) {
                ob.push_str(&s[mark..i]);
                ob.push_str(replacement);
                mark = i + bytes;
                i += bytes;
                continue;
            }
        }
        let mut c = bytes[i];

        if c == b'\\' && !skip_unescape && i + 1 < size && is_ascii_punctuation(bytes[i + 1]) {
            ob.push_str(&s[mark..i]);
            i += 1;
            mark = i;
            c = bytes[i];
        }

        let escape = HTML_ESCAPE_TABLE[c as usize];
        if escape != 0 && c != b'/' {
            ob.push_str(&s[mark..i]);
            ob.push_str(HTML_ESCAPES[escape as usize]);
            mark = i + 1;  // all escaped characters are ASCII
        }
        i += 1;
    }
    ob.push_str(&s[mark..]);
}

fn parse_entity(s: &str) -> Option<(usize, &'static str)> {
    let bytes = s.as_bytes();
    let mut iter = bytes.into_iter();
    if *iter.next()? != b'&' {
        return None;
    }

    // CounterClockwiseContourIntegral is currently the longest entity,
    // but this may change
    let subslice = if bytes.len() > 32 {
        &bytes[1..32]
    } else {
        &bytes[1..]
    };
    let end = 1 + subslice.iter().position(|&b| b == b';')?;
    let replacement = entities::get_entity(&s[1..end])?;

    Some((end + 1, replacement))
}

/// Returns None if s does not start with something of the form &#\d+;
/// Otherwise it returns the number of bytes in s and the unicode character.
fn parse_unicode_lit(bytes: &[u8]) -> Option<(usize, char)> {
    let mut iter = bytes.into_iter().peekable();
    let mut byte_counter = 2;
    let mut val: u32 = 0;
    let mut next;

    if *iter.next()? != b'&' {
        return None;
    }
    if *iter.next()? != b'#' {
        return None;
    }

    let (prefix_bytes, base) = if *iter.peek()? & !0x20 == b'X' {
        let _ = iter.next();
        byte_counter += 1;
        (3, 16)
    } else {
        (2, 10)
    };

    loop {
        next = *iter.next()?;
        byte_counter += 1;
        if next >= b'0' && next <= b'9' {
            val = (next - b'0') as u32 + base * val;
        } else {
            let uppercase = next & !0x20;
            if base == 16 && uppercase >= b'A' && uppercase <= b'F' {
                val = (uppercase - b'A' + 10) as u32 + 16 * val;
            } else {
                break;
            }
        }
    }

    // replace 0 chars by replacement char
    if val == 0 {
        val = 65533;
    }

    if next == b';' && byte_counter - prefix_bytes > 1 {
        Some((byte_counter, std::char::from_u32(val)?))
    } else {
        None
    }
}
