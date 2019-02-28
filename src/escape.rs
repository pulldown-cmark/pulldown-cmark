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

pub fn escape_href(ob: &mut String, s: &str) {
    let mut mark = 0;
    for i in 0..s.len() {
        let c = s.as_bytes()[i];
        if c >= 0x80 || HREF_SAFE[c as usize] == 0 {
            // character needing escape

            // write partial substring up to mark
            if mark < i {
                ob.push_str(&s[mark..i]);
            }
            match c {
                b'&' => {
                    ob.push_str("&amp;");
                },
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
    }
    ob.push_str(&s[mark..]);
}

static HTML_ESCAPE_TABLE: [u8; 256] = [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 3,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 5, 0,
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
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];

static HTML_ESCAPES: [&'static str; 6] = [
        "",
        "&quot;",
        "&amp;",
        "&#47;",
        "&lt;",
        "&gt;"
    ];

pub fn escape_html(ob: &mut String, s: &str, secure: bool) {
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
        let c = bytes[i];
        let escape = HTML_ESCAPE_TABLE[c as usize];
        if escape != 0 && (secure || c != b'/') {
            ob.push_str(&s[mark..i]);
            ob.push_str(HTML_ESCAPES[escape as usize]);
            mark = i + 1;  // all escaped characters are ASCII
        }
        i += 1;
    }
    ob.push_str(&s[mark..]);
}

use core::arch::x86_64::*;
use std::mem::transmute;

fn main() {
    let test_bytes = [b'/', b'"', b'&', b'<', b'>'];

    let mut lower_nibbles = [255u8; 16];
    let mut upper_nibbles = [254u8; 16];

    for &b in &test_bytes {
        lower_nibbles[(b % 16) as usize] = b >> 4;
        upper_nibbles[(b >> 4) as usize] = b >> 4;
    }

    println!("upper nibbles: {:?}", &upper_nibbles);
    println!("lower nibbles: {:?}", &lower_nibbles);

    let upper_vec = unsafe { _mm_loadu_si128(transmute(upper_nibbles.as_ptr())) };
    let lower_vec = unsafe { _mm_loadu_si128(transmute(lower_nibbles.as_ptr())) };

    let mut test_vec: Vec<u8> = "&aXa/aa.a'aa9a<>".as_bytes().into();
    let raw_ptr: *mut _ = unsafe { transmute(test_vec.as_mut_ptr()) };

    unsafe {
        let v = _mm_loadu_si128(raw_ptr as *const _);
        let shuffled_lower = _mm_shuffle_epi8(lower_vec, v);
        let v = _mm_and_si128(_mm_set1_epi8(0x0f), _mm_srli_epi32(v, 4));
        let shuffled_upper = _mm_shuffle_epi8(upper_vec, v);
        let shuffled_total = _mm_cmpeq_epi8(shuffled_lower, shuffled_upper);
        let mut mask = _mm_movemask_epi8(shuffled_total);

        while mask.trailing_zeros() < 32 {
            let ix = mask.trailing_zeros();
            println!("Found a char at index {}", ix);
            mask &= !(1<<ix);
        }
    }
}
