//! SIMD byte scanning logic.
//!
//! This module provides functions that allow walking through byteslices, calling
//! provided callback functions on special bytes and their indices using SIMD.
//! The byteset is defined in `compute_lookup`.
//!
//! The idea is to load in a chunk of 16 bytes and perform a lookup into a set of
//! bytes on all the bytes in this chunk simultaneously. We produce a 16 bit bitmask
//! from this and call the callback on every index corresponding to a 1 in this mask
//! before moving on to the next chunk. This allows us to move quickly when there
//! are no or few matches.
//!
//! The table lookup is inspired by this [great overview]. However, since all of the
//! bytes we're interested in are ASCII, we don't quite need the full generality of
//! the universal algorithm and are hence able to skip a few instructions.
//!
//! [great overview]: http://0x80.pl/articles/simd-byte-lookup.html

use crate::parse::{LookupTable, LoopInstruction, Options};
use core::arch::x86_64::*;

pub(crate) const VECTOR_SIZE: usize = std::mem::size_of::<__m128i>();

/// Generates a lookup table containing the bitmaps for our
/// special marker bytes. This is effectively a 128 element 2d bitvector,
/// that can be indexed by a four bit row index (the lower nibble)
/// and a three bit column index (upper nibble).
pub(crate) fn compute_lookup(options: &Options) -> [u8; 16] {
    let mut lookup = [0u8; 16];
    let standard_bytes = [
        b'\n', b'\r', b'*', b'_', b'&', b'\\', b'[', b']', b'<', b'!', b'`',
    ];

    for &byte in &standard_bytes {
        add_lookup_byte(&mut lookup, byte);
    }
    if options.contains(Options::ENABLE_TABLES) {
        add_lookup_byte(&mut lookup, b'|');
    }
    if options.contains(Options::ENABLE_STRIKETHROUGH) {
        add_lookup_byte(&mut lookup, b'~');
    }
    if options.contains(Options::ENABLE_SMART_PUNCTUATION) {
        for &byte in &[b'.', b'-', b'"', b'\''] {
            add_lookup_byte(&mut lookup, byte);
        }
    }

    lookup
}

fn add_lookup_byte(lookup: &mut [u8; 16], byte: u8) {
    lookup[(byte & 0x0f) as usize] |= 1 << (byte >> 4);
}

/// Computes a bit mask for the given byteslice starting from the given index,
/// where the 16 least significant bits indicate (by value of 1) whether or not
/// there is a special character at that byte position. The least significant bit
/// corresponds to `bytes[ix]` and the most significant bit corresponds to
/// `bytes[ix + 15]`.
/// It is only safe to call this function when `bytes.len() >= ix + VECTOR_SIZE`.
#[target_feature(enable = "ssse3")]
#[inline]
unsafe fn compute_mask(lut: &[u8; 16], bytes: &[u8], ix: usize) -> i32 {
    debug_assert!(bytes.len() >= ix + VECTOR_SIZE);

    let bitmap = _mm_loadu_si128(lut.as_ptr() as *const __m128i);
    // Small lookup table to compute single bit bitshifts
    // for 16 bytes at once.
    let bitmask_lookup =
        _mm_setr_epi8(1, 2, 4, 8, 16, 32, 64, -128, -1, -1, -1, -1, -1, -1, -1, -1);

    // Load input from memory.
    let raw_ptr = bytes.as_ptr().add(ix) as *const __m128i;
    let input = _mm_loadu_si128(raw_ptr);
    // Compute the bitmap using the bottom nibble as an index
    // into the lookup table. Note that non-ascii bytes will have
    // their most significant bit set and will map to lookup[0].
    let bitset = _mm_shuffle_epi8(bitmap, input);
    // Compute the high nibbles of the input using a 16-bit rightshift of four
    // and a mask to prevent most-significant bit issues.
    let higher_nibbles = _mm_and_si128(_mm_srli_epi16(input, 4), _mm_set1_epi8(0x0f));
    // Create a bitmask for the bitmap by perform a left shift of the value
    // of the higher nibble. Bytes with their most significant set are mapped
    // to -1 (all ones).
    let bitmask = _mm_shuffle_epi8(bitmask_lookup, higher_nibbles);
    // Test the bit of the bitmap by AND'ing the bitmap and the mask together.
    let tmp = _mm_and_si128(bitset, bitmask);
    // Check whether the result was not null. NEQ is not a SIMD intrinsic,
    // but comparing to the bitmask is logically equivalent. This also prevents us
    // from matching any non-ASCII bytes since none of the bitmaps were all ones
    // (-1).
    let result = _mm_cmpeq_epi8(tmp, bitmask);

    // Return the resulting bitmask.
    _mm_movemask_epi8(result)
}

/// Calls callback on byte indices and their value.
/// Breaks when callback returns LoopInstruction::BreakAtWith(ix, val). And skips the
/// number of bytes in callback return value otherwise.
/// Returns the final index and a possible break value.
pub(crate) fn iterate_special_bytes<F, T>(
    lut: &LookupTable,
    bytes: &[u8],
    ix: usize,
    callback: F,
) -> (usize, Option<T>)
where
    F: FnMut(usize, u8) -> LoopInstruction<Option<T>>,
{
    if is_x86_feature_detected!("ssse3") && bytes.len() >= VECTOR_SIZE {
        unsafe { simd_iterate_special_bytes(&lut.simd, bytes, ix, callback) }
    } else {
        crate::parse::scalar_iterate_special_bytes(&lut.scalar, bytes, ix, callback)
    }
}

/// Calls the callback function for every 1 in the given bitmask with
/// the index `offset + ix`, where `ix` is the position of the 1 in the mask.
/// Returns `Ok(ix)` to continue from index `ix`, `Err((end_ix, opt_val)` to break with
/// final index `end_ix` and optional value `opt_val`.
unsafe fn process_mask<F, T>(
    mut mask: i32,
    bytes: &[u8],
    mut offset: usize,
    callback: &mut F,
) -> Result<usize, (usize, Option<T>)>
where
    F: FnMut(usize, u8) -> LoopInstruction<Option<T>>,
{
    while mask != 0 {
        let mask_ix = mask.trailing_zeros() as usize;
        offset += mask_ix;
        match callback(offset, *bytes.get_unchecked(offset)) {
            LoopInstruction::ContinueAndSkip(skip) => {
                offset += skip + 1;
                mask >>= skip + 1 + mask_ix;
            }
            LoopInstruction::BreakAtWith(ix, val) => return Err((ix, val)),
        }
    }
    Ok(offset)
}

#[target_feature(enable = "ssse3")]
/// Important: only call this function when `bytes.len() >= 16`. Doing
/// so otherwise may exhibit undefined behaviour.
unsafe fn simd_iterate_special_bytes<F, T>(
    lut: &[u8; 16],
    bytes: &[u8],
    mut ix: usize,
    mut callback: F,
) -> (usize, Option<T>)
where
    F: FnMut(usize, u8) -> LoopInstruction<Option<T>>,
{
    debug_assert!(bytes.len() >= VECTOR_SIZE);
    let upperbound = bytes.len() - VECTOR_SIZE;

    while ix < upperbound {
        let mask = compute_mask(lut, bytes, ix);
        let block_start = ix;
        ix = match process_mask(mask, bytes, ix, &mut callback) {
            Ok(ix) => std::cmp::max(ix, VECTOR_SIZE + block_start),
            Err((end_ix, val)) => return (end_ix, val),
        };
    }

    if bytes.len() > ix {
        // shift off the bytes at start we have already scanned
        let mask = compute_mask(lut, bytes, upperbound) >> ix - upperbound;
        if let Err((end_ix, val)) = process_mask(mask, bytes, ix, &mut callback) {
            return (end_ix, val);
        }
    }

    (bytes.len(), None)
}

#[cfg(test)]
mod simd_test {
    use super::{iterate_special_bytes, LoopInstruction};
    use crate::Options;

    fn check_expected_indices(bytes: &[u8], expected: &[usize], skip: usize) {
        let mut opts = Options::empty();
        opts.insert(Options::ENABLE_TABLES);
        opts.insert(Options::ENABLE_FOOTNOTES);
        opts.insert(Options::ENABLE_STRIKETHROUGH);
        opts.insert(Options::ENABLE_TASKLISTS);

        let lut = crate::parse::create_lut(&opts);
        let mut indices = vec![];

        iterate_special_bytes::<_, i32>(&lut, bytes, 0, |ix, _byte_ty| {
            indices.push(ix);
            LoopInstruction::ContinueAndSkip(skip)
        });

        assert_eq!(&indices[..], expected);
    }

    #[test]
    fn simple_no_match() {
        check_expected_indices("abcdef0123456789".as_bytes(), &[], 0);
    }

    #[test]
    fn simple_match() {
        check_expected_indices("*bcd&f0123456789".as_bytes(), &[0, 4], 0);
    }

    #[test]
    fn single_open_fish() {
        check_expected_indices("<".as_bytes(), &[0], 0);
    }

    #[test]
    fn long_match() {
        check_expected_indices("0123456789abcde~*bcd&f0".as_bytes(), &[15, 16, 20], 0);
    }

    #[test]
    fn border_skip() {
        check_expected_indices("0123456789abcde~~~~d&f0".as_bytes(), &[15, 20], 3);
    }

    #[test]
    fn exhaustive_search() {
        let chars = [
            b'\n', b'\r', b'*', b'_', b'~', b'|', b'&', b'\\', b'[', b']', b'<', b'!', b'`',
        ];

        for &c in &chars {
            for i in 0u8..=255 {
                if !chars.contains(&i) {
                    // full match
                    let mut buf = [i; 18];
                    buf[3] = c;
                    buf[6] = c;

                    check_expected_indices(&buf[..], &[3, 6], 0);
                }
            }
        }
    }
}
