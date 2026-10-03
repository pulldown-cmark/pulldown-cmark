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

//! Utility functions for HTML escaping. Only useful when building your own
//! HTML renderer.
#![warn(
    clippy::alloc_instead_of_core,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core
)]
#![cfg_attr(not(feature = "std"), no_std)]
extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

use alloc::string::String;

use core::fmt::{self, Arguments};
use core::str::from_utf8;
#[cfg(feature = "std")]
use std::io::{self, Write};

#[rustfmt::skip]
static HREF_SAFE: [u8; 128] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1,
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0,
];

static HEX_CHARS: &[u8] = b"0123456789ABCDEF";
static AMP_ESCAPE: &str = "&amp;";
static SINGLE_QUOTE_ESCAPE: &str = "&#x27;";

/// This wrapper exists because we can't have both a blanket implementation
/// for all types implementing `Write` and types of the for `&mut W` where
/// `W: StrWrite`. Since we need the latter a lot, we choose to wrap
/// `Write` types.
#[derive(Debug)]
#[cfg(feature = "std")]
pub struct IoWriter<W>(pub W);

/// Trait that allows writing string slices. This is basically an extension
/// of `std::io::Write` in order to include `String`.
pub trait StrWrite {
    type Error;

    fn write_str(&mut self, s: &str) -> Result<(), Self::Error>;
    fn write_fmt(&mut self, args: Arguments) -> Result<(), Self::Error>;
}

#[cfg(feature = "std")]
impl<W> StrWrite for IoWriter<W>
where
    W: Write,
{
    type Error = io::Error;

    #[inline]
    fn write_str(&mut self, s: &str) -> io::Result<()> {
        self.0.write_all(s.as_bytes())
    }

    #[inline]
    fn write_fmt(&mut self, args: Arguments) -> io::Result<()> {
        self.0.write_fmt(args)
    }
}

/// This wrapper exists because we can't have both a blanket implementation
/// for all types implementing `io::Write` and types of the form `&mut W` where
/// `W: StrWrite`. Since we need the latter a lot, we choose to wrap
/// `Write` types.
#[derive(Debug)]
pub struct FmtWriter<W>(pub W);

impl<W> StrWrite for FmtWriter<W>
where
    W: fmt::Write,
{
    type Error = fmt::Error;

    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0.write_str(s)
    }

    #[inline]
    fn write_fmt(&mut self, args: Arguments) -> fmt::Result {
        self.0.write_fmt(args)
    }
}

impl StrWrite for String {
    type Error = fmt::Error;

    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.push_str(s);
        Ok(())
    }

    #[inline]
    fn write_fmt(&mut self, args: Arguments) -> fmt::Result {
        fmt::Write::write_fmt(self, args)
    }
}

impl<W> StrWrite for &'_ mut W
where
    W: StrWrite,
{
    type Error = W::Error;

    #[inline]
    fn write_str(&mut self, s: &str) -> Result<(), Self::Error> {
        (**self).write_str(s)
    }

    #[inline]
    fn write_fmt(&mut self, args: Arguments) -> Result<(), Self::Error> {
        (**self).write_fmt(args)
    }
}

/// Writes an href to the buffer, escaping href unsafe bytes.
pub fn escape_href<W>(mut w: W, s: &str) -> Result<(), W::Error>
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
                    w.write_str(SINGLE_QUOTE_ESCAPE)?;
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

const fn create_html_escape_table(body: bool) -> [u8; 256] {
    let mut table = [0; 256];
    table[b'&' as usize] = 1;
    table[b'<' as usize] = 2;
    table[b'>' as usize] = 3;
    if !body {
        table[b'"' as usize] = 4;
        table[b'\'' as usize] = 5;
    }
    table
}

static HTML_ESCAPE_TABLE: [u8; 256] = create_html_escape_table(false);
static HTML_BODY_TEXT_ESCAPE_TABLE: [u8; 256] = create_html_escape_table(true);

static HTML_ESCAPES: [&str; 6] = ["", "&amp;", "&lt;", "&gt;", "&quot;", "&#39;"];

/// Writes the given string to the Write sink, replacing special HTML bytes
/// (<, >, &, ", ') by escape sequences.
///
/// Use this function to write output to quoted HTML attributes.
/// Since this function doesn't escape spaces, unquoted attributes
/// cannot be used. For example:
///
/// ```rust
/// let mut value = String::new();
/// pulldown_cmark_escape::escape_html(&mut value, "two words")
///     .expect("writing to a string is infallible");
/// // This is okay.
/// let ok = format!("<a title='{value}'>test</a>");
/// // This is not okay.
/// //let not_ok = format!("<a title={value}>test</a>");
/// ````
pub fn escape_html<W: StrWrite>(w: W, s: &str) -> Result<(), W::Error> {
    #[cfg(feature = "simd")]
    {
        simd::escape_html(w, s, &HTML_ESCAPE_TABLE, &simd::HTML_LOOKUP)
    }
    #[cfg(not(feature = "simd"))]
    {
        escape_html_scalar(w, s, &HTML_ESCAPE_TABLE)
    }
}

/// For use in HTML body text, writes the given string to the Write sink,
/// replacing special HTML bytes (<, >, &) by escape sequences.
///
/// <div class="warning">
///
/// This function should be used for escaping text nodes, not attributes.
/// In the below example, the word "foo" is an attribute, and the word
/// "bar" is an text node. The word "bar" could be escaped by this function,
/// but the word "foo" must be escaped using [`escape_html`].
///
/// ```html
/// <span class="foo">bar</span>
/// ```
///
/// If you aren't sure what the difference is, use [`escape_html`].
/// It should always be correct, but will produce larger output.
///
/// </div>
pub fn escape_html_body_text<W: StrWrite>(w: W, s: &str) -> Result<(), W::Error> {
    #[cfg(feature = "simd")]
    {
        simd::escape_html(
            w,
            s,
            &HTML_BODY_TEXT_ESCAPE_TABLE,
            &simd::HTML_BODY_TEXT_LOOKUP,
        )
    }
    #[cfg(not(feature = "simd"))]
    {
        escape_html_scalar(w, s, &HTML_BODY_TEXT_ESCAPE_TABLE)
    }
}

fn escape_html_scalar<W: StrWrite>(
    mut w: W,
    s: &str,
    table: &'static [u8; 256],
) -> Result<(), W::Error> {
    let bytes = s.as_bytes();
    let mut mark = 0;
    let mut i = 0;
    while i < s.len() {
        match bytes[i..].iter().position(|&c| table[c as usize] != 0) {
            Some(pos) => {
                i += pos;
            }
            None => break,
        }
        let c = bytes[i];
        let escape = table[c as usize];
        let escape_seq = HTML_ESCAPES[escape as usize];
        w.write_str(&s[mark..i])?;
        w.write_str(escape_seq)?;
        i += 1;
        mark = i; // all escaped characters are ASCII
    }
    w.write_str(&s[mark..])
}

#[cfg(feature = "simd")]
mod simd {
    use super::{StrWrite, HTML_BODY_TEXT_ESCAPE_TABLE, HTML_ESCAPE_TABLE};
    use fearless_simd::{mask8x16, prelude::*, u8x16, Level};

    const VECTOR_SIZE: usize = 16;

    /// Number of bits per byte lane in the masks returned by [`movemask`].
    #[cfg(target_arch = "aarch64")]
    const LANE_BITS: u32 = 4;
    #[cfg(not(target_arch = "aarch64"))]
    const LANE_BITS: u32 = 1;

    /// Packs a byte mask into a scalar, where a set lane `i` sets bit
    /// `i * LANE_BITS` and all other bits are zero.
    #[inline(always)]
    fn movemask<S: Simd>(simd: S, mask: mask8x16<S>) -> u64 {
        #[cfg(target_arch = "aarch64")]
        {
            // NEON has no movemask instruction and the generic `to_bitmask` needs a
            // horizontal add. Shifting right by four and narrowing (SHRN) packs every
            // lane into a nibble instead, which is a lot cheaper.
            use fearless_simd::{u16x8, u64x2};

            let bytes: u8x16<S> = mask.select(u8x16::splat(simd, 0xff), u8x16::splat(simd, 0));
            let wide: u16x8<S> = bytes.bitcast();
            let narrowed = simd.narrow_u16x8(wide >> 4, wide >> 4);
            let packed: u64x2<S> = narrowed.bitcast();
            packed[0] & 0x1111_1111_1111_1111
        }
        #[cfg(not(target_arch = "aarch64"))]
        {
            let _ = simd;
            mask.to_bitmask()
        }
    }

    /// Shuffle indices that select by the lower nibble of every byte. PSHUFB on
    /// x86 already ignores bits 4 to 6 and zeroes bytes with their most significant
    /// bit set, which is fine for all our lookups, so masking is only needed on
    /// other platforms where larger indices produce zero.
    #[inline(always)]
    fn low_nibble_index<S: Simd>(v: u8x16<S>) -> u8x16<S> {
        if cfg!(any(target_arch = "x86", target_arch = "x86_64")) {
            v
        } else {
            v & 0x0f
        }
    }

    pub(super) fn escape_html<W: StrWrite>(
        w: W,
        s: &str,
        table: &'static [u8; 256],
        lookup: &'static [u8; 16],
    ) -> Result<(), W::Error> {
        // The SIMD accelerated code needs a byte shuffle instruction (PSHUFB on
        // x86, TBL on aarch64). Further, we can only use this code if the buffer
        // is at least one VECTOR_SIZE in length to prevent reading out of bounds.
        // If either of these conditions is not met, we fall back to scalar code.
        if s.len() >= VECTOR_SIZE {
            if let Some(level) = Level::try_detect() {
                #[cfg(target_arch = "aarch64")]
                if let Some(neon) = level.as_neon() {
                    return neon.vectorize(
                        #[inline(always)]
                        || escape_html_simd(neon, w, s, table, lookup),
                    );
                }
                #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
                if let Some(sse4_2) = level.as_sse4_2() {
                    return sse4_2.vectorize(
                        #[inline(always)]
                        || escape_html_simd(sse4_2, w, s, table, lookup),
                    );
                }
                #[cfg(all(target_arch = "wasm32", target_feature = "simd128"))]
                if let Some(wasm) = level.as_wasm_simd128() {
                    return wasm.vectorize(
                        #[inline(always)]
                        || escape_html_simd(wasm, w, s, table, lookup),
                    );
                }
                let _ = level;
            }
        }
        super::escape_html_scalar(w, s, table)
    }

    /// Only call this when `s.len() >= VECTOR_SIZE`, panics otherwise.
    #[inline(always)]
    fn escape_html_simd<S: Simd, W: StrWrite>(
        simd: S,
        mut w: W,
        s: &str,
        table: &'static [u8; 256],
        lookup: &[u8; 16],
    ) -> Result<(), W::Error> {
        let bytes = s.as_bytes();
        let mut mark = 0;

        foreach_special_simd(simd, lookup, bytes, 0, |i| {
            let entry = table[bytes[i] as usize] as usize;
            w.write_str(&s[mark..i])?;
            mark = i + 1; // all escaped characters are ASCII
            if entry == 0 {
                w.write_str(&s[i..mark])
            } else {
                let replacement = super::HTML_ESCAPES[entry];
                w.write_str(replacement)
            }
        })?;
        w.write_str(&s[mark..])
    }

    /// Creates the lookup table for use in `compute_mask`, containing exactly
    /// the bytes that are escaped by the given escape table. Every candidate
    /// byte has a distinct lower nibble.
    const fn create_lookup(escape_table: &[u8; 256]) -> [u8; 16] {
        let mut table = [0; 16];
        let candidates = [b'<', b'>', b'&', b'"', b'\''];
        let mut i = 0;
        while i < candidates.len() {
            let byte = candidates[i];
            if escape_table[byte as usize] != 0 {
                table[(byte & 0x0f) as usize] = byte;
            }
            i += 1;
        }
        table[0] = 0b0111_1111;
        table
    }

    pub(super) static HTML_LOOKUP: [u8; 16] = create_lookup(&HTML_ESCAPE_TABLE);
    pub(super) static HTML_BODY_TEXT_LOOKUP: [u8; 16] = create_lookup(&HTML_BODY_TEXT_ESCAPE_TABLE);

    /// Computes a byte mask at given offset in the byte buffer. Bit `i * LANE_BITS`
    /// corresponds to whether there is an HTML special byte from `lookup` at
    /// `bytes[offset + i]`. For example, the mask `(1 << (3 * LANE_BITS))` states that
    /// there is an HTML byte at `offset + 3`. Panics when
    /// `bytes.len() < offset + VECTOR_SIZE`.
    #[inline(always)]
    fn compute_mask<S: Simd>(simd: S, lookup: &[u8; 16], bytes: &[u8], offset: usize) -> u64 {
        let lookup = u8x16::from_slice(simd, lookup);

        // Load the vector from memory.
        let vector = u8x16::from_slice(simd, &bytes[offset..offset + VECTOR_SIZE]);
        // We take the least significant 4 bits of every byte and use them as indices
        // to map into the lookup vector.
        // Bytes that share their lower nibble with an HTML special byte get mapped to that
        // corresponding special byte. Note that all HTML special bytes have distinct lower
        // nibbles. Other bytes either get mapped to 0 or 127.
        let expected = lookup.swizzle_dyn(low_nibble_index(vector));
        // We compare the original vector to the mapped output. Bytes that shared a lower
        // nibble with an HTML special byte match *only* if they are that special byte. Bytes
        // that have either a 0 lower nibble or their most significant bit set never match,
        // since all lookup values are ASCII and lookup[0] is 127. All other bytes have
        // non-zero lower nibbles but were mapped to 0 and will therefore also not match.
        //
        // Translate matches to a bitmask, where every 1 corresponds to a HTML special character
        // and a 0 is a non-HTML byte.
        movemask(simd, expected.simd_eq(vector))
    }

    /// Calls the given function with the index of every byte in the given byteslice
    /// that is either ", &, <, or > and for no other byte.
    /// Only call this when `bytes.len() >= 16`, panics otherwise.
    #[inline(always)]
    fn foreach_special_simd<S: Simd, E, F>(
        simd: S,
        lookup: &[u8; 16],
        bytes: &[u8],
        mut offset: usize,
        mut callback: F,
    ) -> Result<(), E>
    where
        F: FnMut(usize) -> Result<(), E>,
    {
        // The strategy here is to walk the byte buffer in chunks of VECTOR_SIZE (16)
        // bytes at a time starting at the given offset. For each chunk, we compute a
        // a bitmask indicating whether the corresponding byte is a HTML special byte.
        // We then iterate over all the 1 bits in this mask and call the callback function
        // with the corresponding index in the buffer.
        // When the number of HTML special bytes in the buffer is relatively low, this
        // allows us to quickly go through the buffer without a lookup and for every
        // single byte.

        let upperbound = bytes.len() - VECTOR_SIZE;
        while offset < upperbound {
            let mut mask = compute_mask(simd, lookup, bytes, offset);
            while mask != 0 {
                let ix = mask.trailing_zeros() / LANE_BITS;
                callback(offset + ix as usize)?;
                mask &= mask - 1;
            }
            offset += VECTOR_SIZE;
        }

        // Final iteration. We align the read with the end of the slice and
        // shift off the bytes at start we have already scanned.
        let mut mask = compute_mask(simd, lookup, bytes, upperbound);
        mask >>= (offset - upperbound) * LANE_BITS as usize;
        while mask != 0 {
            let ix = mask.trailing_zeros() / LANE_BITS;
            callback(offset + ix as usize)?;
            mask &= mask - 1;
        }
        Ok(())
    }

    #[cfg(test)]
    mod html_scan_tests {
        use alloc::vec;
        use alloc::vec::Vec;
        use fearless_simd::{dispatch, Level};

        fn special_indices(bytes: &[u8]) -> Vec<usize> {
            let mut vec = Vec::new();
            dispatch!(Level::new(), simd => super::foreach_special_simd(simd, &super::HTML_LOOKUP, bytes, 0, |ix| {
                #[allow(clippy::unit_arg)]
                Ok::<_, core::fmt::Error>(vec.push(ix))
            }))
            .unwrap();
            vec
        }

        #[test]
        fn multichunk() {
            let vec = special_indices("&aXaaaa.a'aa9a<>aab&".as_bytes());
            assert_eq!(vec, vec![0, 9, 14, 15, 19]);
        }

        #[test]
        fn body_text_lookup_skips_quotes() {
            let bytes = "\"'<>&aaaaaaaaaaaaaaaaaaaa\"'".as_bytes();
            let mut vec = Vec::new();
            dispatch!(Level::new(), simd => super::foreach_special_simd(simd, &super::HTML_BODY_TEXT_LOOKUP, bytes, 0, |ix| {
                #[allow(clippy::unit_arg)]
                Ok::<_, core::fmt::Error>(vec.push(ix))
            }))
            .unwrap();
            assert_eq!(vec, vec![2, 3, 4]);
        }

        /// Compares the SIMD implementation to the scalar one on pseudo random
        /// inputs of many lengths, mixing special, ASCII and multi-byte chars.
        #[test]
        fn matches_scalar() {
            use alloc::string::String;

            let alphabet = [
                'a', ' ', '<', '>', '&', '"', '\'', '\0', '\x7f', 'ä', '€', '😀', 'ì', '.',
            ];
            let mut state = 0x2545_f491_4f6c_dd1du64;
            for len in 0..96 {
                for _ in 0..64 {
                    let mut input = String::new();
                    for _ in 0..len {
                        state ^= state << 13;
                        state ^= state >> 7;
                        state ^= state << 17;
                        input.push(alphabet[(state % alphabet.len() as u64) as usize]);
                    }
                    for table in [
                        &super::super::HTML_ESCAPE_TABLE,
                        &super::super::HTML_BODY_TEXT_ESCAPE_TABLE,
                    ] {
                        let lookup = if core::ptr::eq(table, &super::super::HTML_ESCAPE_TABLE) {
                            &super::HTML_LOOKUP
                        } else {
                            &super::HTML_BODY_TEXT_LOOKUP
                        };
                        let mut expected = String::new();
                        super::super::escape_html_scalar(&mut expected, &input, table).unwrap();
                        let mut actual = String::new();
                        super::escape_html(&mut actual, &input, table, lookup).unwrap();
                        assert_eq!(actual, expected, "input: {:?}", input);
                    }
                }
            }
        }

        // only match these bytes, and when we match them, match them VECTOR_SIZE times
        #[test]
        fn only_right_bytes_matched() {
            for b in 0..=255u8 {
                let right_byte = b == b'&' || b == b'<' || b == b'>' || b == b'"' || b == b'\'';
                let vek = vec![b; super::VECTOR_SIZE];
                let match_count = special_indices(&vek).len();
                assert!((match_count > 0) == (match_count == super::VECTOR_SIZE));
                assert_eq!(
                    (match_count == super::VECTOR_SIZE),
                    right_byte,
                    "match_count: {}, byte: {:?}",
                    match_count,
                    b as char
                );
            }
        }
    }
}

#[cfg(test)]
mod test {
    use alloc::string::String;

    pub use super::{escape_href, escape_html, escape_html_body_text};

    #[test]
    fn check_href_escape() {
        let mut s = String::new();
        escape_href(&mut s, "&^_").unwrap();
        assert_eq!(s.as_str(), "&amp;^_");
    }

    #[test]
    fn check_attr_escape() {
        let mut s = String::new();
        escape_html(&mut s, r##"&^"'_"##).unwrap();
        assert_eq!(s.as_str(), "&amp;^&quot;&#39;_");
    }

    #[test]
    fn check_body_escape() {
        let mut s = String::new();
        escape_html_body_text(&mut s, r##"&^"'_"##).unwrap();
        assert_eq!(s.as_str(), r##"&amp;^"'_"##);
    }
}
