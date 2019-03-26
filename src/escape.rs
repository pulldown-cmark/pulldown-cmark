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

use std::io::{self, Write};
use std::str::from_utf8;
#[cfg(all(target_arch = "x86_64", feature="simd"))]
use std::arch::x86_64::*;
#[cfg(all(target_arch = "x86_64", feature="simd"))]
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
static AMP_ESCAPE: &'static [u8] = b"&amp;";
static SLASH_ESCAPE: &'static [u8] = b"&#x27;";

pub(crate) fn escape_href<W>(mut w: W, s: &str) -> io::Result<()>
where
    W: Write,
{
    let bytes = s.as_bytes();
    let mut mark = 0;
    for i in 0..bytes.len() {
        let c = bytes[i];
        if c >= 0x80 || HREF_SAFE[c as usize] == 0 {
            // character needing escape

            // write partial substring up to mark
            if mark < i {
                w.write_all(&bytes[mark..i])?;
            }
            match c {
                b'&' => {
                    w.write_all(AMP_ESCAPE)?;
                }
                b'\'' => {
                    w.write_all(SLASH_ESCAPE)?;
                }
                _ => {
                    let mut buf = [0u8; 3];
                    buf[0] = b'%';
                    buf[1] = HEX_CHARS[((c as usize) >> 4) & 0xF];
                    buf[2] = HEX_CHARS[(c as usize) & 0xF];
                    let escaped = from_utf8(&buf).unwrap().as_bytes();
                    w.write_all(escaped)?;
                }
            }
            mark = i + 1; // all escaped characters are ASCII
        }
    }
    w.write_all(&bytes[mark..])?;
    Ok(())
}

static HTML_ESCAPE_TABLE: [u8; 256] = [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 4, 0,
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

static HTML_ESCAPES: [&'static str; 5] = [
        "",
        "&quot;",
        "&amp;",
        "&lt;",
        "&gt;"
    ];

#[cfg(all(target_arch = "x86_64", feature="simd"))]
pub(crate) fn escape_html<W: Write>(mut w: W, s: &str) -> io::Result<()> {
    let bytes = s.as_bytes();
    let mut mark = 0;

    foreach_special_simd(bytes, 0, |i| {
        let replacement = HTML_ESCAPES[HTML_ESCAPE_TABLE[bytes[i] as usize] as usize];
        w.write_all(&bytes[mark..i])?;
        w.write_all(replacement.as_bytes())?;
        mark = i + 1;  // all escaped characters are ASCII
        Ok(())
    })?;
    w.write_all(&bytes[mark..])
}

#[cfg(not(all(target_arch = "x86_64", feature="simd")))]
pub(crate) fn escape_html<W: Write>(w: W, s: &str) -> io::Result<()> {
    escape_html_scalar(w, s)
}

pub(crate) fn escape_html_scalar<W>(mut w: W, s: &str) -> io::Result<()>
where
    W: Write,
{
    let bytes = s.as_bytes();
    let mut mark = 0;
    let mut i = 0;
    while i < s.len() {
        match bytes[i..]
            .iter()
            .position(|&c| HTML_ESCAPE_TABLE[c as usize] != 0)
        {
            Some(pos) => {
                i += pos;
            }
            None => break,
        }
        let c = bytes[i];
        let escape = HTML_ESCAPE_TABLE[c as usize];
        if escape != 0 {
            let escape_seq = HTML_ESCAPES[escape as usize];
            w.write_all(&bytes[mark..i])?;
            w.write_all(escape_seq.as_bytes())?;
            mark = i + 1; // all escaped characters are ASCII
        }
        i += 1;
    }
    w.write_all(&bytes[mark..])
}

/// Calls the given function with the index of every byte in the given byteslice
/// that is either ", &, <, or > and for no other byte.
#[cfg(all(target_arch = "x86_64", feature="simd"))]
fn foreach_special_simd<F>(bytes: &[u8], mut offset: usize, mut callback: F) -> io::Result<()>
    where F: FnMut(usize) -> io::Result<()>
{
    let lower_vec = unsafe { _mm_set_epi8( 0, 62, 0, 60, 0, 0, 0, 0, 0, 38, 0, 0, 0, 34, 0, 127 ) };
    let upperbound = bytes.len().saturating_sub(16);

    // total length less than 16, fall back to scalar code to be super safe.
    if upperbound == 0 {
        while offset < bytes.len() {
            match bytes[offset..]
                .iter()
                .position(|&c| HTML_ESCAPE_TABLE[c as usize] != 0)
            {
                Some(pos) => {
                    callback(offset + pos)?;
                    offset += pos + 1;
                }
                None => return Ok(()),
            }
        }
        return Ok(());
    }

    while offset < upperbound {
        let mut mask = unsafe {
            let raw_ptr = bytes.as_ptr().offset(offset as isize) as *const _;
            let v = _mm_loadu_si128(raw_ptr);
            let expected = _mm_shuffle_epi8(lower_vec, v);
            let matches = _mm_cmpeq_epi8(expected, v);
            _mm_movemask_epi8(matches)
        };

        while mask != 0 {
            let ix = mask.trailing_zeros();
            callback(offset + ix as usize)?;
            mask ^= mask & -mask;
        }

        offset += 16;
    }

    // final iteration - align read with the end of the slice
    let mut mask = unsafe {
        let raw_ptr = transmute(bytes.as_ptr().offset(upperbound as isize));
        let v = _mm_loadu_si128(raw_ptr);
        let expected = _mm_shuffle_epi8(lower_vec, v);
        let matches = _mm_cmpeq_epi8(expected, v);
        _mm_movemask_epi8(matches)
    };

    // shift off the bytes at start we have already scanned
    mask >>= offset - upperbound;

    while mask != 0 {
        let ix = mask.trailing_zeros();
        callback(offset + ix as usize)?;
        mask ^= mask & -mask;
    }
    Ok(())
}

#[cfg(test)]
mod html_scan_tests {    
    #[test]
    #[cfg(all(target_arch = "x86_64", feature="simd"))]
    fn simple() {
        let mut vec = Vec::new();
        super::foreach_special_simd("&aXaaa\"".as_bytes(), 0, |ix| { vec.push(ix); Ok(()) }).unwrap();
        assert_eq!(vec, vec![0, 6]);
    }
    
    #[test]
    #[cfg(all(target_arch = "x86_64", feature="simd"))]
    fn multichunk() {
        let mut vec = Vec::new();
        super::foreach_special_simd("&aXaaaa.a'aa9a<>aab&".as_bytes(), 0, |ix| { vec.push(ix); Ok(()) }).unwrap();
        assert_eq!(vec, vec![0, 14, 15, 19]);
    }

    // only match these bytes, and when we match them, match them 16 times
    #[test]
    #[cfg(all(target_arch = "x86_64", feature="simd"))]
    fn only_right_bytes_matched() {
        for b in 0..255u8 {
            let right_byte = b == b'&' || b == b'<' || b == b'>' || b == b'"';
            let vek = vec![b; 16];
            let mut match_count = 0;
            super::foreach_special_simd(&vek, 0, |_| { match_count += 1; Ok(()) }).unwrap();
            assert!((match_count > 0) == (match_count == 16));
            assert_eq!((match_count == 16), right_byte, "match_count: {}, byte: {:?}", match_count, b as char);
        }
    }
}
