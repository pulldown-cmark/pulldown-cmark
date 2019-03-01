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
use std::arch::x86_64::*;
use std::mem::transmute;

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
    let bytes = s.as_bytes();
    let mut mark = 0;

    scan_simd(bytes, 0, |i| {
        let c = bytes[i];
        let escape = HTML_ESCAPE_TABLE[c as usize];
        if escape != 0 && (secure || c != b'/') {
            ob.push_str(&s[mark..i]);
            ob.push_str(HTML_ESCAPES[escape as usize]);
            mark = i + 1;  // all escaped characters are ASCII
        }
    });

    ob.push_str(&s[mark..]);
}

fn scan_simple<F: FnMut(usize) -> ()>(bytes: &[u8], mut i: usize, mut callback: F) {
    let size = bytes.len();
    while i < size {
        match bytes[i..].iter().position(|&c| HTML_ESCAPE_TABLE[c as usize] != 0) {
            Some(pos) => {
                i += pos;
            }
            None => break
        }
        callback(i);
        i += 1;
    }
}

fn scan_simd<F: FnMut(usize) -> ()>(bytes: &[u8], mut offset: usize, mut callback: F) {
    // hopefully the compiler sees these as constants??
    let lower_vec = unsafe { _mm_set_epi8(
        32, 48, 255u8 as i8, 48, 255u8 as i8,
        255u8 as i8, 255u8 as i8, 255u8 as i8, 255u8 as i8, 32, 
        255u8 as i8, 255u8 as i8, 255u8 as i8, 32, 255u8 as i8, 255u8 as i8
    ) };
    let top_bits_mask = unsafe { _mm_set1_epi8(0xf0u8 as i8) };

    let upperbound = bytes.len().saturating_sub(15);
    while offset < upperbound {
        let mut mask = unsafe {
            let raw_ptr = transmute(bytes[offset..].as_ptr());
            let v = _mm_loadu_si128(raw_ptr);
            let expected_top_bits = _mm_shuffle_epi8(lower_vec, v);
            let top_bit_v = _mm_and_si128(top_bits_mask, v);
            let matches = _mm_cmpeq_epi8(expected_top_bits, top_bit_v);
            _mm_movemask_epi8(matches)
        };

        while mask != 0 {
            let ix = mask.trailing_zeros();
            callback(offset + ix as usize);
            mask ^= mask & -mask;
        }

        offset += 16;
    }

    // we can only do simd if we can atleast 16 bytes at once
    // do final bytes old fashioned way
    scan_simple(&bytes, offset, callback);        
}

#[cfg(test)]
mod html_scan_tests {
    use super::*;
    
    #[test]
    fn scan_simd_works() {
        let mut vec = Vec::new();
        scan_simd("&aXa/aa\"".as_bytes(), 0, |ix| vec.push(ix));
        assert_eq!(vec, vec![0, 4, 7]);
    }
    
    #[test]
    fn combined_scan_works() {
        let mut vec = Vec::new();
        scan_simd("&aXa/aa.a'aa9a<>aab&".as_bytes(), 0, |ix| vec.push(ix));
        assert_eq!(vec, vec![0, 4, 14, 15, 19]);
    }
    
    #[test]
    fn scan_simpl_works() {
        let mut vec = Vec::new();
        scan_simple("&aXa/aa.a'aa9a<>".as_bytes(), 0, |ix| vec.push(ix));
        assert_eq!(vec, vec![0, 4, 14, 15]);
    }

    // only match these bytes, and when we match them, match them 16 times
    #[test]
    fn only_right_bytes_matched() {
        for b in 0..255u8 {
            let right_byte = b == b'&' || b == b'/' || b == b'<' || b == b'>' || b == b'"';
            let vek = vec![b; 16];
            let mut match_count = 0;
            scan_simd(&vek, 0, |_| { match_count += 1; });
            assert!((match_count > 0) == (match_count == 16));
            assert_eq!((match_count == 16), right_byte, "match_count: {}, byte: {}", match_count, b as char);
        }
    }
}
