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

use std::io;
use std::str::from_utf8;

use crate::html::StrWrite;

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
static AMP_ESCAPE: &'static str = "&amp;";
static SLASH_ESCAPE: &'static str = "&#x27;";

pub(crate) fn escape_href<W>(mut w: W, s: &str) -> io::Result<()>
where
    W: StrWrite,
{
    let bytes = s.as_bytes();
    let mut mark = 0;
    for i in 0..bytes.len() {
        let c = bytes[i];
        if c >= 0x80 || HREF_SAFE[c as usize] == 0 {
            // character needing escape

            // write partial substring up to mark
            if mark < i {
                w.write_str(&s[mark..i])?;
            }
            match c {
                b'&' => {
                    w.write_str(AMP_ESCAPE)?;
                }
                b'\'' => {
                    w.write_str(SLASH_ESCAPE)?;
                }
                _ => {
                    let mut buf = [0u8; 3];
                    buf[0] = b'%';
                    buf[1] = HEX_CHARS[((c as usize) >> 4) & 0xF];
                    buf[2] = HEX_CHARS[(c as usize) & 0xF];
                    let escaped = from_utf8(&buf).unwrap();
                    w.write_str(escaped)?;
                }
            }
            mark = i + 1; // all escaped characters are ASCII
        }
    }
    w.write_str(&s[mark..])
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

/// Writes the given string to the Write sink, replacing special HTML bytes
/// (<, >, &, ") by escape sequences.
pub(crate) fn escape_html<W: StrWrite>(w: W, s: &str) -> io::Result<()> {
    #[cfg(all(target_arch = "x86_64", feature="simd"))]
    { simd::escape_html(w, s) }
    #[cfg(not(all(target_arch = "x86_64", feature="simd")))]
    { escape_html_scalar(w, s) }
}

fn escape_html_scalar<W: StrWrite>(mut w: W, s: &str) -> io::Result<()> {
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
            w.write_str(&s[mark..i])?;
            w.write_str(escape_seq)?;
            mark = i + 1; // all escaped characters are ASCII
        }
        i += 1;
    }
    w.write_str(&s[mark..])
}

#[cfg(all(target_arch = "x86_64", feature="simd"))]
mod simd {
    use std::arch::x86_64::*;
    use std::io;
    use std::mem::size_of;
    use crate::html::StrWrite;

    const VECTOR_SIZE: usize = size_of::<__m128i>();

    pub(crate) fn escape_html<W: StrWrite>(mut w: W, s: &str) -> io::Result<()> {
        // The SIMD accelerated code uses the PSHUFB instruction, which is part
        // of the SSSE3 instruction set. Further, we can only use this code if
        // the buffer is at least one VECTOR_SIZE in length to prevent reading 
        // out of bounds. If either of these conditions is not met, we fall back
        // to scalar code.
        if is_x86_feature_detected!("ssse3") && s.len() >= VECTOR_SIZE {
            let bytes = s.as_bytes();
            let mut mark = 0;

            unsafe {
                foreach_special_simd(bytes, 0, |i| {
                    let escape_ix = *bytes.get_unchecked(i) as usize;
                    let replacement = super::HTML_ESCAPES[super::HTML_ESCAPE_TABLE[escape_ix] as usize];
                    w.write_str(&s.get_unchecked(mark..i))?;
                    mark = i + 1; // all escaped characters are ASCII
                    w.write_str(replacement)
                })?;
                w.write_str(&s.get_unchecked(mark..))
            }
        } else {
            super::escape_html_scalar(w, s)
        }
    }

    /// Creates the lookup table for use in `compute_mask`.
    const fn create_lookup() -> [u8; 16] {
        let mut table = [0; 16];
        table[(b'<' & 0x0f) as usize] = b'<';
        table[(b'>' & 0x0f) as usize] = b'>';
        table[(b'&' & 0x0f) as usize] = b'&';
        table[(b'"' & 0x0f) as usize] = b'"';
        table[0] = 0b0111_1111;
        table
    }

    #[target_feature(enable = "ssse3")]
    /// Computes a byte mask at given offset in the byte buffer. Its first 16 (least significant)
    /// bits correspond to whether there is an HTML special byte (&, <, ", >) at the 16 bytes
    /// `bytes[offset..]`. For example, the mask `(1 << 3)` states that there is an HTML byte
    /// at `offset + 3`. It is only safe to call this function when
    /// `bytes.len() >= offset + VECTOR_SIZE`.
    unsafe fn compute_mask(bytes: &[u8], offset: usize) -> i32 {
        debug_assert!(bytes.len() >= offset + VECTOR_SIZE);

        let table = create_lookup();
        let lookup = _mm_loadu_si128(table.as_ptr() as *const __m128i);
        let raw_ptr = bytes.as_ptr().offset(offset as isize) as *const __m128i;

        // Load the vector from memory.
        let vector = _mm_loadu_si128(raw_ptr);
        // We take the least significant 4 bits of every byte and use them as indices
        // to map into the lookup vector.
        // Note that shuffle maps bytes with their most significant bit set to lookup[0].
        // Bytes that share their lower nibble with an HTML special byte get mapped to that
        // corresponding special byte. Note that all HTML special bytes have distinct lower
        // nibbles. Other bytes either get mapped to 0 or 127. 
        let expected = _mm_shuffle_epi8(lookup, vector);
        // We compare the original vector to the mapped output. Bytes that shared a lower
        // nibble with an HTML special byte match *only* if they are that special byte. Bytes
        // that have either a 0 lower nibble or their most significant bit set were mapped to
        // 127 and will hence never match. All other bytes have non-zero lower nibbles but
        // were mapped to 0 and will therefore also not match.
        let matches = _mm_cmpeq_epi8(expected, vector);
        
        // Translate matches to a bitmask, where every 1 corresponds to a HTML special character
        // and a 0 is a non-HTML byte.
        _mm_movemask_epi8(matches)
    }

    /// Calls the given function with the index of every byte in the given byteslice
    /// that is either ", &, <, or > and for no other byte.
    /// Make sure to only call this when `bytes.len() >= 16`, undefined behaviour may
    /// occur otherwise.
    #[target_feature(enable = "ssse3")]
    unsafe fn foreach_special_simd<F>(bytes: &[u8], mut offset: usize, mut callback: F) -> io::Result<()>
        where F: FnMut(usize) -> io::Result<()>
    {
        // The strategy here is to walk the byte buffer in chunks of VECTOR_SIZE (16)
        // bytes at a time starting at the given offset. For each chunk, we compute a
        // a bitmask indicating whether the corresponding byte is a HTML special byte.
        // We then iterate over all the 1 bits in this mask and call the callback function
        // with the corresponding index in the buffer.
        // When the number of HTML special bytes in the buffer is relatively low, this
        // allows us to quickly go through the buffer without a lookup and for every
        // single byte.

        debug_assert!(bytes.len() >= VECTOR_SIZE);
        let upperbound = bytes.len() - VECTOR_SIZE;
        while offset < upperbound { 
            let mut mask = compute_mask(bytes, offset);
            while mask != 0 {
                let ix = mask.trailing_zeros();
                callback(offset + ix as usize)?;
                mask ^= mask & -mask;
            }
            offset += VECTOR_SIZE;
        }

        // Final iteration. We align the read with the end of the slice and
        // shift off the bytes at start we have already scanned.
        let mut mask = compute_mask(bytes, upperbound);
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
        fn multichunk() {
            let mut vec = Vec::new();
            unsafe {
                super::foreach_special_simd(
                    "&aXaaaa.a'aa9a<>aab&".as_bytes(),
                    0,
                    |ix| Ok(vec.push(ix))
                ).unwrap();
            }
            assert_eq!(vec, vec![0, 14, 15, 19]);
        }

        // only match these bytes, and when we match them, match them VECTOR_SIZE times
        #[test]
        fn only_right_bytes_matched() {
            for b in 0..255u8 {
                let right_byte = b == b'&' || b == b'<' || b == b'>' || b == b'"';
                let vek = vec![b; super::VECTOR_SIZE];
                let mut match_count = 0;
                unsafe {
                    super::foreach_special_simd(
                        &vek,
                        0,
                        |_| { match_count += 1; Ok(()) }
                    ).unwrap();
                }
                assert!((match_count > 0) == (match_count == super::VECTOR_SIZE));
                assert_eq!((match_count == super::VECTOR_SIZE), right_byte, "match_count: {}, byte: {:?}", match_count, b as char);
            }
        }
    }
}

