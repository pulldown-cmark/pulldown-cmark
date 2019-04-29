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

//! Scanners for fragments of CommonMark syntax

use std::char;
use std::convert::TryInto;

use crate::entities;
use crate::parse::Alignment;
use crate::strings::CowStr;
pub use crate::puncttable::{is_ascii_punctuation, is_punctuation};

use memchr::memchr;

// sorted for binary search
const HTML_TAGS: [&str; 62] = ["address", "article", "aside", "base",
    "basefont", "blockquote", "body", "caption", "center", "col", "colgroup",
    "dd", "details", "dialog", "dir", "div", "dl", "dt", "fieldset",
    "figcaption", "figure", "footer", "form", "frame", "frameset", "h1",
    "h2", "h3", "h4", "h5", "h6", "head", "header", "hr", "html", "iframe",
    "legend", "li", "link", "main", "menu", "menuitem", "nav", "noframes",
    "ol", "optgroup", "option", "p", "param", "section", "source", "summary",
    "table", "tbody", "td", "tfoot", "th", "thead", "title", "tr", "track",
    "ul"];

/// Analysis of the beginning of a line, including indentation and container
/// markers.
#[derive(Clone)]
pub struct LineStart<'a> {
    bytes: &'a [u8],
    tab_start: usize,
    ix: usize,
    spaces_remaining: usize,
    // no thematic breaks can occur before this offset.
    // this prevents scanning over and over up to a certain point
    min_hrule_offset: usize,
}

impl<'a> LineStart<'a> {
    pub fn new(bytes: &[u8]) -> LineStart {
        LineStart {
            bytes,
            tab_start: 0,
            ix: 0,
            spaces_remaining: 0,
            min_hrule_offset: 0,
        }
    }

    /// Try to scan a number of spaces.
    ///
    /// Returns true if all spaces were consumed.
    ///
    /// Note: consumes some spaces even if not successful.
    pub fn scan_space(&mut self, n_space: usize) -> bool {
        self.scan_space_inner(n_space) == 0
    }

    /// Scan a number of spaces up to a maximum.
    ///
    /// Returns number of spaces scanned.
    pub fn scan_space_upto(&mut self, n_space: usize) -> usize {
        n_space - self.scan_space_inner(n_space)
    }

    /// Returns unused remainder of spaces.
    fn scan_space_inner(&mut self, mut n_space: usize) -> usize {
        let n_from_remaining = self.spaces_remaining.min(n_space);
        self.spaces_remaining -= n_from_remaining;
        n_space -= n_from_remaining;
        while n_space > 0 && self.ix < self.bytes.len() {
            match self.bytes[self.ix] {
                b' ' => {
                    self.ix += 1;
                    n_space -= 1;
                }
                b'\t' => {
                    let spaces = 4 - (self.ix - self.tab_start) % 4;
                    self.ix += 1;
                    self.tab_start = self.ix;
                    let n = spaces.min(n_space);
                    n_space -= n;
                    self.spaces_remaining = spaces - n;
                }
                _ => break,
            }
        }
        n_space
    }

    /// Scan all available ASCII whitespace (not including eol).
    pub fn scan_all_space(&mut self) {
        self.spaces_remaining = 0;
        self.ix += self.bytes[self.ix..]
            .iter()
            .take_while(|&&b| b == b' ' || b == b'\t')
            .count();
    }

    /// Determine whether we're at end of line (includes end of file).
    pub fn is_at_eol(&self) -> bool {
        if self.ix >= self.bytes.len() {
            return true;
        }
        let c = self.bytes[self.ix];
        c == b'\r' || c == b'\n'
    }

    fn scan_ch(&mut self, c: u8) -> bool {
        if self.ix < self.bytes.len() && self.bytes[self.ix] == c {
            self.ix += 1;
            true
        } else {
            false
        }
    }

    pub fn scan_blockquote_marker(&mut self) -> bool {
        let save = self.clone();
        let _ = self.scan_space(3);
        if self.scan_ch(b'>') {
            let _ = self.scan_space(1);
            true
        } else {
            *self = save;
            false
        }
    }

    /// Scan a list marker.
    ///
    /// Return value is the character, the start index, and the indent in spaces.
    /// For ordered list markers, the character will be one of b'.' or b')'. For
    /// bullet list markers, it will be one of b'-', b'+', or b'*'.
    pub fn scan_list_marker(&mut self) -> Option<(u8, usize, usize)> {
        let save = self.clone();
        let indent = self.scan_space_upto(3);
        if self.ix < self.bytes.len() {
            let c = self.bytes[self.ix];
            if c == b'-' || c == b'+' || c == b'*' {
                if self.ix >= self.min_hrule_offset {
                    // there could be an hrule here
                    if let Err(min_offset) = scan_hrule(&self.bytes[self.ix..]) {
                        self.min_hrule_offset = min_offset;
                    } else {
                        *self = save;
                        return None;
                    }
                }
                self.ix += 1;
                if self.scan_space(1) || self.is_at_eol() {
                    return self.finish_list_marker(c, 0, indent + 2);
                }
            } else if c >= b'0' && c <= b'9' {
                let start_ix = self.ix;
                let mut ix = self.ix + 1;
                let mut val = u64::from(c - b'0');
                while ix < self.bytes.len() && ix - start_ix < 10 {
                    let c = self.bytes[ix];
                    ix += 1;
                    if c >= b'0' && c <= b'9' {
                        val = val * 10 + u64::from(c - b'0');
                    } else if c == b')' || c == b'.' {
                        self.ix = ix;
                        let val_usize = val as usize;
                        // This will cause some failures on 32 bit arch.
                        // TODO (breaking API change): should be u64, not usize.
                        if val_usize as u64 != val { return None; }                        
                        if self.scan_space(1) || self.is_at_eol() {
                            return self.finish_list_marker(c, val_usize, indent + self.ix - start_ix);
                        }
                    }
                }
            }
        }
        *self = save;
        None
    }

    fn finish_list_marker(&mut self, c: u8, start: usize, mut indent: usize)
        -> Option<(u8, usize, usize)>
    {
        let save = self.clone();

        // skip the rest of the line if it's blank
        if scan_blank_line(&self.bytes[self.ix..]).is_some() {
            return Some((c, start, indent));
        }

        let post_indent = self.scan_space_upto(4);
        if post_indent < 4 {
            indent += post_indent;
        } else {
            *self = save;
        }
        Some((c, start, indent))
    }

    /// Returns Some(is_checked) when a task list marker was found. Resets itself
    /// to original state otherwise.
    pub(crate) fn scan_task_list_marker(&mut self) -> Option<bool> {
        let save = self.clone();
        self.scan_space_upto(3);

        if !self.scan_ch(b'[') {
            *self = save;
            return None;
        }
        let is_checked = match self.bytes.get(self.ix) {
            Some(&c) if is_ascii_whitespace_no_nl(c) => {
                self.ix += 1;
                false
            }
            Some(b'x') | Some(b'X') => {
                self.ix += 1;
                true
            }
            _ => {
                *self = save;
                return None;
            }
        };
        if !self.scan_ch(b']') {
            *self = save;
            return None;
        }
        if !self.bytes.get(self.ix).map(|&b| is_ascii_whitespace_no_nl(b)).unwrap_or(false) {
            *self = save;
            return None;
        }
        Some(is_checked)
    }

    pub fn bytes_scanned(&self) -> usize {
        self.ix
    }

    pub fn remaining_space(&self) -> usize {
        self.spaces_remaining
    }
}

pub fn is_ascii_whitespace(c: u8) -> bool {
    (c >= 0x09 && c <= 0x0d) || c == b' '
}

pub fn is_ascii_whitespace_no_nl(c: u8) -> bool {
    c == b'\t' || c == 0x0b || c == 0x0c || c == b' '
}

pub fn is_ascii_alpha(c: u8) -> bool {
    match c {
        b'a' ... b'z' | b'A' ... b'Z' => true,
        _ => false
    }
}

pub fn is_ascii_alphanumeric(c: u8) -> bool {
    match c {
        b'0' ... b'9' | b'a' ... b'z' | b'A' ... b'Z' => true,
        _ => false
    }
}

pub fn is_ascii_letterdigitdash(c: u8) -> bool {
    c == b'-' || is_ascii_alphanumeric(c)
}

fn is_digit(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

fn is_valid_unquoted_attr_value_char(c: u8) -> bool {
    match c {
        b'\'' | b'"' | b' ' | b'=' | b'>' | b'<' | b'`' | b'\n' | b'\r' => false,
        _ => true
    }
}

// scan a single character
pub fn scan_ch(data: &[u8], c: u8) -> usize {
    if !data.is_empty() && data[0] == c { 1 } else { 0 }
}

pub fn scan_while<F>(data: &[u8], mut f: F) -> usize
        where F: FnMut(u8) -> bool {
    data.iter().take_while(|&&c| f(c)).count()
}

pub fn scan_ch_repeat(data: &[u8], c: u8) -> usize {
    scan_while(data, |x| x == c)
}

// TODO: maybe should scan unicode whitespace too
pub fn scan_whitespace_no_nl(data: &[u8]) -> usize {
    scan_while(data, is_ascii_whitespace_no_nl)
}

pub fn scan_attr_value_chars(data: &[u8]) -> usize {
    scan_while(data, is_valid_unquoted_attr_value_char)
}

pub fn scan_eol(bytes: &[u8]) -> Option<usize> {
    if bytes.is_empty() { return Some(0); }
    match bytes[0] {
        b'\n' => Some(1),
        b'\r' => Some(if bytes.get(1) == Some(&b'\n') { 2 } else { 1 }),
        _ => None
    }
}

pub fn scan_blank_line(bytes: &[u8]) -> Option<usize> {
    let i = scan_whitespace_no_nl(bytes);
    scan_eol(&bytes[i..]).map(|n| i + n)
}

pub fn scan_nextline(bytes: &[u8]) -> usize {
    memchr(b'\n', bytes).map_or(bytes.len(), |x| x + 1)
}

// return: end byte for closing code fence, or None
// if the line is not a closing code fence
pub fn scan_closing_code_fence(bytes: &[u8], fence_char: u8, n_fence_char: usize) -> Option<usize> {
    if bytes.is_empty() { return Some(0); }
    let mut i = 0;
    let num_fence_chars_found = scan_ch_repeat(&bytes[i..], fence_char);
    if num_fence_chars_found < n_fence_char { return None; }
    i += num_fence_chars_found;
    let num_trailing_spaces = scan_ch_repeat(&bytes[i..], b' ');
    i += num_trailing_spaces;
    scan_eol(&bytes[i..]).map(|_| i)
}

// returned pair is (number of bytes, number of spaces)
pub fn calc_indent(text: &[u8], max: usize) -> (usize, usize) {
    let mut spaces = 0;
    let mut offset = 0;

    for (i, &b) in text.iter().enumerate() {
        match b {
            b' ' => {
                spaces += 1;
                if spaces == max {
                    break;
                }
            }
            b'\t' => {
                let new_spaces = spaces + 4 - (spaces & 3);
                if new_spaces > max {
                    break;
                }
                spaces = new_spaces;
            },
            _ => break,
        }
        offset = i;
    }

    (offset, spaces)
}

/// Scan hrule opening sequence.
///
/// Returns Ok(x) when it finds an hrule, where x is the 
/// size of line containing the hrule, including the trailing newline.
/// 
/// Returns Err(x) when it does not find an hrule and x is
/// the offset in data before no hrule can appear.
pub fn scan_hrule(bytes: &[u8]) -> Result<usize, usize> {
    if bytes.len() < 2 { return Err(0); }
    let c = bytes[0];
    if !(c == b'*' || c == b'-' || c == b'_') { return Err(0); }
    let mut n = 0;
    let mut i = 0;

    for (offset, &b) in bytes.iter().enumerate() {
        match b {
            b'\n' | b'\r' => {
                i = offset + scan_eol(&bytes[offset..]).unwrap_or(0);
                break;
            }
            c2 if c2 == c => {
                n += 1;
            }
            b' ' | b'\t' => (),
            _ => return Err(offset)
        }
        i = offset;
    }
    if n >= 3 { Ok(i) } else { Err(i) }
}

/// Scan an ATX heading opening sequence.
///
/// Returns number of bytes in prefix and level.
pub fn scan_atx_heading(data: &[u8]) -> Option<(usize, i32)> {
    let level = scan_ch_repeat(data, b'#');
    if level >= 1 && level <= 6 {
        if let b' ' | b'\t' ... b'\r' = *data.get(level)? {
            return Some((level, level as i32));
        }
    }
    None
}

/// Scan a setext heading underline.
///
/// Returns number of bytes in line (including trailing newline) and level.
pub fn scan_setext_heading(data: &[u8]) -> Option<(usize, i32)> {
    let mut i = 0;
    let c = *data.get(i)?;
    if !(c == b'-' || c == b'=') { return None; }
    i += 1 + scan_ch_repeat(&data[i + 1 ..], c);
    i += scan_blank_line(&data[i..])?;
    let level = if c == b'=' { 1 } else { 2 };
    Some((i, level))
}

// returns number of bytes in line (including trailing
// newline) and column alignments
pub fn scan_table_head(data: &[u8]) -> (usize, Vec<Alignment>) {
    let (mut i, spaces) = calc_indent(data, 4);
    if spaces > 3 || i == data.len() {
        return (0, vec![]);
    }
    let mut cols = vec![];
    let mut active_col = Alignment::None;
    let mut start_col = true;
    if data[i] == b'|' {
        i += 1;
    }
    for c in &data[i..] {
        if let Some(n) = scan_eol(&data[i..]) {
            i += n;
            break;
        }
        match *c {
            b' ' => (),
            b':' => {
                active_col =
                    match (start_col, active_col) {
                        (true, Alignment::None) => Alignment::Left,
                        (false, Alignment::Left) => Alignment::Center,
                        (false, Alignment::None) => Alignment::Right,
                        _ => active_col,
                    };
                start_col = false;
            }
            b'-' => {
                start_col = false;
            },
            b'|' => {
                start_col = true;
                cols.push(active_col);
                active_col = Alignment::None;
            },
            _ => {
                cols = vec![];
                start_col = true;
                break;
            },
        }
        i += 1;
    }

    if !start_col {
        cols.push(active_col);
    }

    (i, cols)
}

/// Scan code fence.
///
/// Returns number of bytes scanned and the char that is repeated to make the code fence.
pub fn scan_code_fence(data: &[u8]) -> Option<(usize, u8)> {
    let c = *data.get(0)?;
    if !(c == b'`' || c == b'~') { return None; }
    let i = 1 + scan_ch_repeat(&data[1..], c);
    if i >= 3 {
        if c == b'`' {
            let suffix = &data[i..];
            let next_line = i + scan_nextline(suffix);
            // FIXME: make sure this is correct
            if suffix[..(next_line - i)].iter().any(|&b| b == b'`') {
                return None;
            }
        }
        Some((i, c))
    } else {
        None
    }
}

pub fn scan_blockquote_start(data: &[u8]) -> Option<usize> {
    if data.starts_with(b"> ") {
        Some(2)
    } else {
        None
    }
}

/// This already assumes the list item has been scanned.
pub fn scan_empty_list(data: &[u8]) -> bool {
    let mut ix = 0;
    for _ in 0..2 {
        if let Some(bytes) = scan_blank_line(&data[ix..]) {
            ix += bytes;
        } else {
            return false;
        }
    }
    true
}

// return number of bytes scanned, delimiter, start index, and indent
pub fn scan_listitem(bytes: &[u8]) -> (usize, u8, usize, usize) {
    if bytes.is_empty() { return (0, 0, 0, 0); }
    let mut c = bytes[0];
    let (w, start) = match c {
        b'-' | b'+' | b'*' => (1, 0),
        b'0' ... b'9' => {
            let (length, start) = parse_decimal(bytes);
            if length >= bytes.len() { return (0, 0, 0, 0); }
            c = bytes[length];
            if !(c == b'.' || c == b')') { return (0, 0, 0, 0); }
            (length + 1, start)
        }
        _ => { return (0, 0, 0, 0); }
    };
    // TODO: replace calc_indent with scan_leading_whitespace, for tab correctness
    let (mut postn, mut postindent) = calc_indent(&bytes[w.. ], 5);
    if postindent == 0 {
        if scan_eol(&bytes[w..]).is_none() {
            return (0, 0, 0, 0);
        }
        postindent += 1;
    } else if postindent > 4 {
        postn = 1;
        postindent = 1;
    }
    if scan_blank_line(&bytes[w..]).is_some() {
        postn = 0;
        postindent = 1;
    }
    (w + postn, c, start, w + postindent)
}

// returns (number of bytes, parsed decimal)
fn parse_decimal(bytes: &[u8]) -> (usize, usize) {
    match bytes.iter()
        .take_while(|&&b| is_digit(b))
        .try_fold((0, 0usize), |(count, acc), c| {
            match acc.checked_mul(10) {
                Some(ten_acc) => Ok((count + 1, ten_acc + usize::from(c - b'0'))),
                // stop early on overflow
                None => Err((count, acc)),
            }
        })
    {
       Ok(p) | Err(p) => p,
    }
}

// returns (number of bytes, parsed decimal)
fn parse_hex(bytes: &[u8]) -> (usize, usize) {
    match bytes.iter()
        .try_fold((0, 0usize), |(count, acc), c| {
            let mut c = *c;
            let digit = if c >= b'0' && c <= b'9' {
                usize::from(c - b'0')
            } else {
                // make lower case
                c |= 0x20;
                if c >= b'a' && c <= b'f' {
                    usize::from(c - b'a' + 10)
                } else {
                    return Err((count, acc));
                }
            };
            match acc.checked_mul(16) {
                Some(sixteen_acc) => Ok((count + 1, sixteen_acc + digit)),
                // stop early on overflow
                None => Err((count, acc)),
            }
        })
    {
       Ok(p) | Err(p) => p,
    }
}

fn char_from_codepoint(input: usize) -> Option<char> {
    let mut codepoint = input.try_into().ok()?;
    if codepoint == 0 {
        codepoint = 0xFFFD;
    }
    char::from_u32(codepoint)
}

// doesn't bother to check data[0] == '&'
pub fn scan_entity(bytes: &[u8]) -> (usize, Option<CowStr<'static>>) {
    let mut end = 1;
    if scan_ch(&bytes[end..], b'#') == 1 {
        end += 1;
        let (bytecount, codepoint) = if end < bytes.len() && bytes[end] | 0x20 == b'x' {
            end += 1;
            parse_hex(&bytes[end..])
        } else {
            parse_decimal(&bytes[end..])
        };
        end += bytecount;
        return if bytecount == 0 || scan_ch(&bytes[end..], b';') == 0 {
            (0, None)
        } else if let Some(c) = char_from_codepoint(codepoint) {
            (end + 1, Some(c.into()))
        } else {
            (0, None)
        };
    }
    end += scan_while(&bytes[end..], is_ascii_alphanumeric);
    if scan_ch(&bytes[end..], b';') == 1 {
        if let Some(value) = entities::get_entity(&bytes[1..end]) {
            return (end + 1, Some(value.into()));
        }
    }
    (0, None)
}

// returns (bytes scanned, title cow)
pub(crate) fn scan_link_title(text: &str, start_ix: usize) -> Option<(usize, CowStr<'_>)> {
    let bytes = text.as_bytes();
    let open = match bytes.get(start_ix) {
        Some(b @ b'\'') | Some(b @ b'\"') | Some(b @ b'(') => *b,
        _ => return None,
    };
    let close = if open == b'(' { b')' } else { open };

    let mut title = String::new();
    let mut mark = start_ix + 1;
    let mut i = start_ix + 1;

    while i < bytes.len() {
        let c = bytes[i];

        if c == close {
            let cow = if mark == 1 {
                (i - start_ix + 1, text[mark..i].into())
            } else {
                title.push_str(&text[mark..i]);
                (i - start_ix + 1, title.into())
            };
            
            return Some(cow);
        }
        if c == open {
            return None;
        }

        // TODO: do b'\r' as well?
        if c == b'&' {
            if let (n, Some(value)) = scan_entity(&bytes[i..]) {
                title.push_str(&text[mark..i]);
                title.push_str(&value);
                i += n;
                mark = i;
                continue;
            }
        }
        if c == b'\\' {
            if i + 1 < bytes.len() && is_ascii_punctuation(bytes[i + 1]) {
                title.push_str(&text[mark..i]);
                i += 1;
                mark = i;
            }
        }

        i += 1;
    }

    None
}

// note: dest returned is raw, still needs to be unescaped
// TODO: check that nested parens are really not allowed for refdefs
pub fn scan_link_dest(data: &str, start_ix: usize, max_next: usize) -> Option<(usize, &str)> {
    let bytes = &data.as_bytes()[start_ix..];
    let mut i = scan_ch(bytes, b'<');
    let pointy = i != 0;
    let dest_beg = i;
    let mut nest = 0;

    while i < bytes.len() {
        match bytes[i] {
            b'\n' | b'\r' => break,
            b' ' if !pointy && nest == 0 => {
                break;
            }
            b'(' if !pointy => {
                if nest > max_next { return None; }
                nest += 1;
            }
            b')' if !pointy => {
                if nest == 0 { break; }
                nest -= 1;
            }
            b'>' if pointy => {
                break;
            }
            b'\\' => i += 1,
            _ => ()
        }
        i += 1;
    }
    let dest_end = i;
    if dest_end > bytes.len() {
        return None;
    }
    if pointy {
        let n = scan_ch(&bytes[i..], b'>');
        if n == 0 { return None; }
        i += n;
    }

    Some((i, &data[(start_ix + dest_beg)..(start_ix + dest_end)]))
}

pub fn scan_attribute_name(data: &[u8]) -> Option<usize> {
    let size = data.len();
    match data.get(0)? {
        c if is_ascii_alpha(*c) => (),
        b'_' | b':' => (),
        _ => {
            return None;
        }
    }
    let mut i = 1;
    while i < size {
        match data[i] {
            c if is_ascii_alphanumeric(c) => i += 1,
            b'_' | b'.' | b':' | b'-' => i += 1,
            _ => break
        }
    }
    Some(i)
}

pub fn scan_attribute_value(data: &[u8]) -> Option<usize> {
    let mut i = 0;
    match *data.get(0)? {
        b'\'' => {
            i += 1;
            i += scan_while(&data[i..], |c| c != b'\'' && c != b'\n' && c != b'\r');
            i += 1;
        },
        b'"' => {
            i += 1;
            i += scan_while(&data[i..], |c| c != b'"' && c != b'\n' && c != b'\r');
            i += 1;
        },
        b' ' | b'=' | b'>' | b'<' | b'`' | b'\n' | b'\r' => { return None; },
        _ => { // unquoted attribute value
            i += scan_attr_value_chars(&data[i..]);
        }
    }
    if i < data.len() {
        Some(i)
    } else {
        None
    }
}

// Remove backslash escapes and resolve entities
pub fn unescape(input: &str) -> CowStr<'_> {
    let mut result = String::new();
    let mut mark = 0;
    let mut i = 0;
    let bytes = input.as_bytes();
    while i < bytes.len() {
        match bytes[i] {
            b'\\' if i + 1 < bytes.len() && is_ascii_punctuation(bytes[i + 1]) => {
                result.push_str(&input[mark..i]);
                mark = i + 1;
                i += 2;
            }
            b'&' => {
                match scan_entity(&bytes[i..]) {
                    (n, Some(value)) => {
                        result.push_str(&input[mark..i]);
                        result.push_str(&value);
                        i += n;
                        mark = i;
                    }
                    _ => i += 1
                }
            }
            b'\r' => {
                result.push_str(&input[mark..i]);
                i += 1;
                mark = i;
            }
            _ => i += 1
        }
    }
    if mark == 0 {
        input.into()
    } else {
        result.push_str(&input[mark..]);
        result.into()
    }
}

pub fn scan_html_block_tag(data: &[u8]) -> (usize, &[u8]) {
    let mut i = scan_ch(data, b'<');
    if i == 0 { return (0, b"") }
    i += scan_ch(&data[i..], b'/');
    let n = scan_while(&data[i..], is_ascii_alphanumeric);
    // TODO: scan attributes and >
    (i + n, &data[i .. i + n])
}

pub fn is_html_tag(tag: &[u8]) -> bool {
    HTML_TAGS.binary_search_by(|probe| {
        let probe_bytes_iter = probe.as_bytes().iter();
        let tag_bytes_iter = tag.iter();

        probe_bytes_iter.zip(tag_bytes_iter)
            .find_map(|(&a, &b)| {
                // We can compare case insensitively because the probes are
                // all lower case alpha strings.
                match a.cmp(&(b | 0x20)) {
                    std::cmp::Ordering::Equal => None,
                    inequality => Some(inequality),
                }
            })
            .unwrap_or_else(|| probe.len().cmp(&tag.len()))
    }).is_ok()
}

pub fn scan_html_type_7(data: &[u8]) -> Option<usize> {
    let mut i = scan_ch(data, b'<');
    if i == 0 {
        return None;
    }

    let close_tag_bytes = scan_ch(&data[i..], b'/');
    i += close_tag_bytes;

    let l = scan_while(&data[i..], is_ascii_alpha);
    if l == 0  {
        return None;
    }
    i += l;
    i += scan_while(&data[i..], is_ascii_letterdigitdash);

    if close_tag_bytes == 0 {
        loop {
            let whitespace = scan_whitespace_no_nl(&data[i..]);
            i += whitespace;
            if let Some(b'/') | Some(b'>') = data.get(i) {
                break;
            }
            if whitespace == 0 { return None; }
            i += scan_attribute(&data[i..])?;
        }
    }

    i += scan_whitespace_no_nl(&data[i..]);

    if close_tag_bytes == 0 {
        i += scan_ch(&data[i..], b'/');
    }

    let c = scan_ch(&data[i..], b'>');
    if c == 0 {
        return None;
    }
    i += c;

    scan_blank_line(&data[i..]).map(|_| i)
}

pub fn scan_attribute(data: &[u8]) -> Option<usize> {
    let mut i = scan_whitespace_no_nl(data);
    i += scan_attribute_name(&data[i..])?;
    scan_attribute_value_spec(&data[i..]).map(|attr_valspec_bytes| attr_valspec_bytes + i)
}

pub fn scan_attribute_value_spec(data: &[u8]) -> Option<usize> {
    let mut i = scan_whitespace_no_nl(data);
    let eq = scan_ch(&data[i..], b'=');
    if eq == 0 { return None; }
    i += eq;
    i += scan_whitespace_no_nl(&data[i..]);
    i += scan_attribute_value(&data[i..])?;
    Some(i)
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn overflow_list() {
        assert_eq!((0, 0, 0, 0), scan_listitem(b"4444444444444444444444444444444444444444444444444444444444!"));
    }
}
