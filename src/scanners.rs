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

use crate::entities;
use crate::utils;
use std::borrow::Cow;
use std::borrow::Cow::{Borrowed, Owned};
use std::char;
use crate::parse::Alignment;

pub use crate::puncttable::{is_ascii_punctuation, is_punctuation};

// sorted for binary_search
const HTML_TAGS: [&'static str; 72] = ["address", "article", "aside", "base", 
    "basefont", "blockquote", "body", "button", "canvas", "caption", "center",
    "col", "colgroup", "dd", "details", "dialog", "dir", "div", "dl", "dt", 
    "embed", "fieldset", "figcaption", "figure", "footer", "form", "frame", 
    "frameset", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header", "hgroup", 
    "hr", "html", "iframe", "legend", "li", "link", "main", "menu", "menuitem", 
    "meta", "map", "nav", "noframes", "object", "ol", "optgroup", "option", 
    "output", "p", "param", "progress", "section", "source", "summary", 
    "table", "tbody", "td", "textarea", "tfoot", "th", "thead", "title", "tr", 
    "ul", "video"];

const URI_SCHEMES: [&'static str; 164] = ["aaa", "aaas", "about", "acap",
    "adiumxtra", "afp", "afs", "aim", "apt", "attachment", "aw", "beshare",
    "bitcoin", "bolo", "callto", "cap", "chrome", "chrome-extension", "cid",
    "coap", "com-eventbrite-attendee", "content", "crid", "cvs", "data", "dav",
    "dict", "dlna-playcontainer", "dlna-playsingle", "dns", "doi", "dtn",
    "dvb", "ed2k", "facetime", "feed", "file", "finger", "fish", "ftp", "geo",
    "gg", "git", "gizmoproject", "go", "gopher", "gtalk", "h323", "hcp",
    "http", "https", "iax", "icap", "icon", "im", "imap", "info", "ipn", "ipp",
    "irc", "irc6", "ircs", "iris", "iris.beep", "iris.lwz", "iris.xpc",
    "iris.xpcs", "itms", "jar", "javascript", "jms", "keyparc", "lastfm",
    "ldap", "ldaps", "magnet", "mailto", "maps", "market", "message", "mid",
    "mms", "ms-help", "msnim", "msrp", "msrps", "mtqp", "mumble", "mupdate",
    "mvn", "news", "nfs", "ni", "nih", "nntp", "notes", "oid",
    "opaquelocktoken", "palm", "paparazzi", "platform", "pop", "pres", "proxy",
    "psyc", "query", "res", "resource", "rmi", "rsync", "rtmp", "rtsp",
    "secondlife", "service", "session", "sftp", "sgn", "shttp", "sieve", "sip",
    "sips", "skype", "smb", "sms", "snmp", "soap.beep", "soap.beeps", "soldat",
    "spotify", "ssh", "steam", "svn", "tag", "teamspeak", "tel", "telnet",
    "tftp", "things", "thismessage", "tip", "tn3270", "tv", "udp", "unreal",
    "urn", "ut2004", "vemmi", "ventrilo", "view-source", "webcal", "ws", "wss",
    "wtai", "wyciwyg", "xcon", "xcon-userid", "xfire", "xmlrpc.beep",
    "xmlrpc.beeps", "xmpp", "xri", "ymsgr", "z39.50r", "z39.50s"];

/// Analysis of the beginning of a line, including indentation and container
/// markers.
#[derive(Clone)]
pub struct LineStart<'a> {
    text: &'a str,
    tab_start: usize,
    ix: usize,
    spaces_remaining: usize,
}

impl<'a> LineStart<'a> {
    pub fn new(text: &str) -> LineStart {
        LineStart { text, tab_start: 0, ix: 0, spaces_remaining: 0 }
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
        let bytes = self.text.as_bytes();
        while n_space > 0 && self.ix < bytes.len() {
            match bytes[self.ix] {
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
        let bytes = self.text.as_bytes();
        while self.ix < bytes.len() {
            match bytes[self.ix] {
                b' ' | b'\t' => self.ix += 1,
                _ => break,
            }
        }
    }

    /// Determine whether we're at end of line (includes end of file).
    pub fn is_at_eol(&mut self) -> bool {
        if self.ix == self.text.len() {
            return true;
        }
        let c = self.text.as_bytes()[self.ix];
        c == b'\r' || c == b'\n'
    }

    fn scan_ch(&mut self, c: u8) -> bool {
        if self.ix < self.text.len() && self.text.as_bytes()[self.ix] == c {
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
        if self.ix < self.text.len() {
            let c = self.text.as_bytes()[self.ix];
            if c == b'-' || c == b'+' || c == b'*' {
                if scan_hrule(&self.text[self.ix..]) > 0 {
                    *self = save;
                    return None;
                }
                self.ix += 1;
                if self.scan_space(1) || self.is_at_eol() {
                    return self.finish_list_marker(c, 0, indent + 2);
                }
            } else if c >= b'0' && c <= b'9' {
                let start_ix = self.ix;
                let mut ix = self.ix + 1;
                let mut val = (c - b'0') as u64;
                while ix < self.text.len() && ix - start_ix < 10 {
                    let c = self.text.as_bytes()[ix];
                    ix += 1;
                    if c >= b'0' && c <= b'9' {
                        val = val * 10 + (c - b'0') as u64;
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
        if scan_blank_line(&self.text[self.ix..]).is_some() {
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

pub fn is_ascii_upper(c: u8) -> bool {
    match c {
        b'A' ... b'Z' => true,
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

fn is_hexdigit(c: u8) -> bool {
    match c {
        b'0' ... b'9' | b'a' ... b'f' | b'A' ... b'F' => true,
        _ => false
    }
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
pub fn scan_ch(data: &str, c: u8) -> usize {
    if !data.is_empty() && data.as_bytes()[0] == c { 1 } else { 0 }
}

pub fn scan_while<F>(data: &str, mut f: F) -> usize
        where F: FnMut(u8) -> bool {
    match data.as_bytes().iter().position(|&c| !f(c)) {
        Some(i) => i,
        None => data.len()
    }
}

pub fn scan_ch_repeat(data: &str, c: u8) -> usize {
    scan_while(data, |x| x == c)
}

// TODO: maybe should scan unicode whitespace too
pub fn scan_whitespace_no_nl(data: &str) -> usize {
    scan_while(data, is_ascii_whitespace_no_nl)
}

pub fn scan_attr_value_chars(data: &str) -> usize {
    scan_while(data, is_valid_unquoted_attr_value_char)
}

// Maybe returning Option<usize> would be more Rustic?
pub fn scan_eol(s: &str) -> (usize, bool) {
    if s.is_empty() { return (0, true); }
    let bytes = s.as_bytes();
    match bytes[0] {
        b'\n' => (1, true),
        b'\r' => (if s[1..].starts_with('\n') { 2 } else { 1 }, true),
        _ => (0, false)
    }
}

pub fn scan_line_ending(s: &str) -> usize {
    match s.as_bytes().iter().position(|&c| c == b'\r' || c == b'\n') {
        Some(i) => i,
        None => s.len()
    }
}

// unusual among "scan" functions in that it scans from the _back_ of the string
// TODO: should also scan unicode whitespace?
pub fn scan_trailing_whitespace(data: &str) -> usize {
    match data.as_bytes().iter().rev().position(|&c| !is_ascii_whitespace_no_nl(c)) {
        Some(i) => i,
        None => data.len()
    }
}

fn scan_codepoint(data: &str) -> Option<char> {
    data.chars().next()
}

// get last codepoint in string
fn scan_trailing_codepoint(data: &str) -> Option<char> {
    // would just data.chars.next_back() be better?
    let size = data.len();
    if size == 0 {
        None
    } else {
        let c = data.as_bytes()[size - 1];
        if c < 0x80 {
            Some(c as char)
        } else {
            let mut i = size - 2;
            while (data.as_bytes()[i] & 0xc0) == 0x80 {
                i -= 1;
            }
            data[i..].chars().next()
        }
    }
}

pub fn scan_blank_line(text: &str) -> Option<usize> {
    let i = scan_whitespace_no_nl(text);
    if let (n, true) = scan_eol(&text[i..]) {
        Some(i + n)
    } else {
        None
    }
}

pub fn scan_nextline(s: &str) -> usize {
    match s.as_bytes().iter().position(|&c| c == b'\n') {
        Some(x) => x + 1,
        None => s.len()
    }
}

pub fn count_tab(bytes: &[u8]) -> usize {
    let mut count = 0;
    for &c in bytes.iter().rev() {
        match c {
            b'\t' | b'\n' => break,
            x if (x & 0xc0) != 0x80 => count += 1,
            _ => ()
        }
    }
    4 - count % 4
}

// takes a loc because tabs might require left context
// return: number of bytes, number of spaces
pub fn scan_leading_space(text: &str, loc: usize) -> (usize, usize) {
    let bytes = text.as_bytes();
    let mut i = 0;
    let mut spaces = 0;
    for &c in &bytes[loc..] {
        match c {
            b' ' => spaces += 1,
            b'\t' => spaces += count_tab(&bytes[.. loc + i]),
            _ => break
        }
        i += 1
    }
    (i, spaces)
}

// return: start byte for code text in indented code line, or None
// for a non-code line
pub fn scan_code_line(text: &str) -> Option<usize> {
    let bytes = text.as_bytes();
    let mut num_spaces = 0;
    let mut i = 0;
    for &c in bytes {
        if num_spaces == 4 { return Some(4); }
        match c {
            b' ' => num_spaces += 1,
            b'\t' => { return Some(i+1); },
            b'\n' | b'\r' => { return Some(i); }, 
            _ => { return None; },
        }
        i += 1;
    }
    return None;
}

// return: end byte for closing code fence, or None
// if the line is not a closing code fence
pub fn scan_closing_code_fence(text: &str, fence_char: u8, n_fence_char: usize) -> Option<usize> {
    if text.is_empty() { return Some(0); }
    let mut i = 0;
    let num_fence_chars_found = scan_ch_repeat(&text[i..], fence_char);
    if num_fence_chars_found < n_fence_char { return None; }
    i += num_fence_chars_found;
    let num_trailing_spaces = scan_ch_repeat(&text[i..], b' ');
    i += num_trailing_spaces;
    if scan_eol(&text[i..]).1 { return Some(i); }
    return None;
}

// returned pair is (number of bytes, number of spaces)
pub fn calc_indent(text: &str, max: usize) -> (usize, usize) {
    let bytes = text.as_bytes();
    let mut i = 0;
    let mut spaces = 0;
    while i < text.len() && spaces < max {
        match bytes[i] {
            b' ' => spaces += 1,
            b'\t' => {
                let new_spaces = spaces + 4 - (spaces & 3);
                if new_spaces > max {
                    break;
                }
                spaces = new_spaces;
            },
            _ => break
        }
        i += 1;
    }
    (i, spaces)
}

// return size of line containing hrule, including trailing newline, or 0
// TODO: switch to Option return type
pub fn scan_hrule(data: &str) -> usize {
    let bytes = data.as_bytes();
    let size = data.len();
    let mut i = 0;
    if i + 2 >= size { return 0; }
    let c = bytes[i];
    if !(c == b'*' || c == b'-' || c == b'_') { return 0; }
    let mut n = 0;
    while i < size {
        match bytes[i] {
            b'\n' | b'\r' => {
                i += scan_eol(&data[i..]).0;
                break;
            }
            c2 if c2 == c => n += 1,
            b' ' | b'\t' => (),
            _ => return 0
        }
        i += 1;
    }
    if n >= 3 { i } else { 0 }
}

// TODO: obsolete, remove in code cleanup
pub fn scan_atx_header(_data: &str) -> (usize, i32) {
    unimplemented!();
}

/// Scan an ATX heading opening sequence.
///
/// Returns number of bytes in prefix and level.
pub fn scan_atx_heading(data: &str) -> Option<(usize, i32)> {
    let size = data.len();
    let level = scan_ch_repeat(data, b'#');
    let i = level;
    if level >= 1 && level <= 6 {
        if i < size {
            match data.as_bytes()[i] {
                b' ' | b'\t' ... b'\r' => (),
                _ => return None
            }
        }
        Some((i, level as i32))
    } else {
        None
    }
}

pub fn scan_setext_header(_data: &str) -> (usize, i32) {
    unimplemented!();
}

/// Scan a setext heading underline.
///
/// Returns number of bytes in line (including trailing newline) and level.
pub fn scan_setext_heading(data: &str) -> Option<(usize, i32)> {
    let size = data.len();
    let mut i = 0;
    if i == size { return None; }
    let c = data.as_bytes()[i];
    if !(c == b'-' || c == b'=') { return None; }
    i += 1 + scan_ch_repeat(&data[i + 1 ..], c);
    if let Some(n) = scan_blank_line(&data[i..]) {
        i += n;
    } else {
        return None;
    }
    let level = if c == b'=' { 1 } else { 2 };
    Some((i, level))
}

// returns number of bytes in line (including trailing
// newline) and column alignments
pub fn scan_table_head(data: &str) -> (usize, Vec<Alignment>) {
    let (mut i, spaces) = calc_indent(data, 4);
    if spaces > 3 || i == data.len() {
        return (0, vec![]);
    }
    let mut cols = vec![];
    let mut active_col = Alignment::None;
    let mut start_col = true;
    if data.as_bytes()[i] == b'|' {
        i += 1;
    }
    for c in data.as_bytes()[i..].iter() {
        let eol = scan_eol(&data[i..]);
        if eol.1 {
            i += eol.0;
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

    if cols.len() < 2 {
        (0, vec![])
    } else {
        (i, cols)
    }
}

// TODO: change return type to Option.
// returns: number of bytes scanned, char
pub fn scan_code_fence(data: &str) -> (usize, u8) {
    if data.is_empty() {
        return (0, 0);
    }
    let c = data.as_bytes()[0];
    if !(c == b'`' || c == b'~') { return (0, 0); }
    let i = 1 + scan_ch_repeat(&data[1 ..], c);
    if i >= 3 {
        if c == b'`' {
            let next_line = i + scan_nextline(&data[i..]);
            if data[i..next_line].find('`').is_some() {
                return (0, 0);
            }
        }
        return (i, c);
    }
    (0, 0)
}

pub fn scan_backticks(data: &str) -> usize {
    scan_ch_repeat(data, b'`')
}

pub fn scan_blockquote_start(data: &str) -> usize {
    if data.starts_with('>') {
        let n = 1;
        n + scan_ch(&data[n..], b' ')
    } else {
        0
    }
}

/// This already assumes the list item has been scanned.
pub fn scan_empty_list(data: &str) -> bool {
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
pub fn scan_listitem(data: &str) -> (usize, u8, usize, usize) {
    if data.is_empty() { return (0, 0, 0, 0); }
    let mut c = data.as_bytes()[0];
    let mut start = 0;
    let w = match c {
        b'-' | b'+' | b'*' => 1,
        b'0' ... b'9' => {
            let mut i = 1;
            i += scan_while(&data[i..], is_digit);
            if i >= data.len() { return (0, 0, 0, 0); }
            start = match data[..i].parse() {
                Ok(start) => start,
                Err(_) => return (0, 0, 0, 0),
            };
            c = data.as_bytes()[i];
            if !(c == b'.' || c == b')') { return (0, 0, 0, 0); }
            i + 1
        }
        _ => { return (0, 0, 0, 0); }
    };
    // TODO: replace calc_indent with scan_leading_whitespace, for tab correctness
    let (mut postn, mut postindent) = calc_indent(&data[w.. ], 5);
    if postindent == 0 {
        if !scan_eol(&data[w..]).1 { return (0, 0, 0, 0); }
        postindent += 1;
    } else if postindent > 4 {
        postn = 1;
        postindent = 1;
    }
    if let Some(_) = scan_blank_line(&data[w..]) {
        postn = 0;
        postindent = 1;
    }
    (w + postn, c, start, w + postindent)
}

// return whether delimeter run can open or close
pub fn compute_open_close(data: &str, loc: usize, c: u8) -> (usize, bool, bool) {
    // TODO: handle Unicode, not just ASCII
    let size = data.len();
    let mut end = loc + 1;
    while end < size && data.as_bytes()[end] == c {
        end += 1;
    }
    let mut beg = loc;
    while beg > 0 && data.as_bytes()[beg - 1] == c {
        beg -= 1;
    }
    let (white_before, punc_before) = match scan_trailing_codepoint(&data[..beg]) {
        None => (true, false),
        Some(c) => (c.is_whitespace(), is_punctuation(c))
    };
    let (white_after, punc_after) = match scan_codepoint(&data[end..]) {
        None => (true, false),
        Some(c) => (c.is_whitespace(), is_punctuation(c))
    };
    let left_flanking = !white_after && (!punc_after || white_before || punc_before);
    let right_flanking = !white_before && (!punc_before || white_after || punc_after);
    let (can_open, can_close) = match c {
        b'*' => (left_flanking, right_flanking),
        b'_' => (left_flanking && (!right_flanking || punc_before),
                right_flanking && (!left_flanking || punc_after)),
        _ => (false, false)
    };
    (end - loc, can_open, can_close)
}

fn cow_from_codepoint_str(s: &str, radix: u32) -> Cow<'static, str> {
    let mut codepoint = u32::from_str_radix(s, radix).unwrap();
    if codepoint == 0 {
        codepoint = 0xFFFD;
    }
    Owned(char::from_u32(codepoint).unwrap_or('\u{FFFD}').to_string())
}

// doesn't bother to check data[0] == '&'
pub fn scan_entity(data: &str) -> (usize, Option<Cow<'static, str>>) {
    let size = data.len();
    let mut end = 1;
    if scan_ch(&data[end..], b'#') == 1 {
        end += 1;
        if end < size && (data.as_bytes()[end] == b'x' || data.as_bytes()[end] == b'X') {
            end += 1;
            end += scan_while(&data[end..], is_hexdigit);
            if end > 3 && end < 12 && scan_ch(&data[end..], b';') == 1 {
                return (end + 1, Some(cow_from_codepoint_str(&data[3..end], 16)));
            }
        } else {
            end += scan_while(&data[end..], is_digit);
            if end > 2 && end < 11 && scan_ch(&data[end..], b';') == 1 {
                return (end + 1, Some(cow_from_codepoint_str(&data[2..end], 10)));
            }
        }
        return (0, None);
    }
    end += scan_while(&data[end..], is_ascii_alphanumeric);
    if scan_ch(&data[end..], b';') == 1 {
        if let Some(value) = entities::get_entity(&data[1..end]) {
            return (end + 1, Some(Borrowed(value)));
        }
    }
    (0, None)
}

// note: dest returned is raw, still needs to be unescaped
pub fn scan_link_dest(data: &str) -> Option<(usize, &str)> {
    let size = data.len();
    let mut i = 0;
    let pointy_n = scan_ch(data, b'<');
    let pointy = pointy_n != 0;
    i += pointy_n;
    let dest_beg = i;
    let mut in_parens = false;
    while i < size {
        match data.as_bytes()[i] {
            b'\n' | b'\r' => break,
            b' ' => {
                if !pointy && !in_parens { break; }
            }
            b'(' => {
                if !pointy {
                    if in_parens { return None; }
                    in_parens = true;
                }
            }
            b')' => {
                if !pointy {
                    if !in_parens { break; }
                    in_parens = false;
                }
            }
            b'>' => {
                if pointy { break; }
            }
            b'\\' => i += 1,
            _ => ()
        }
        i += 1;
    }
    let dest_end = i;
    if dest_end > data.len() {
        return None;
    }
    if pointy {
        let n = scan_ch(&data[i..], b'>');
        if n == 0 { return None; }
        i += n;
    }

    Some((i, &data[dest_beg..dest_end]))
}

// return value is: total bytes, link uri
pub fn scan_autolink(data: &str) -> Option<(usize, Cow<str>)> {
    let mut i = 0;
    let n = scan_ch(data, b'<');
    if n == 0 { return None; }
    i += n;
    let link_beg = i;
    let n_uri = scan_uri(&data[i..]);
    let (n_link, link) = if n_uri != 0 {
        (n_uri, Borrowed(&data[link_beg .. i + n_uri]))
    } else {
        let n_email = scan_email(&data[i..]);
        if n_email == 0 { return None; }
        (n_email, Owned("mailto:".to_owned() + &data[link_beg.. i + n_email]))
    };
    i += n_link;
    if scan_ch(&data[i..], b'>') == 0 { return None; }
    Some((i + 1, link))
}

fn scan_uri(data: &str) -> usize {
    let mut i = 0;
    while i < data.len() {
        match data.as_bytes()[i] {
            c if is_ascii_alphanumeric(c)  => i += 1,
            b'.' | b'-' => i += 1,
            b':' => break,
            _ => return 0
        }
    }
    if i == data.len() { return 0; }
    let scheme = &data[..i];
    if !URI_SCHEMES.binary_search_by(|probe| utils::strcasecmp(probe, scheme)).is_ok() {
        return 0;
    }
    i += 1;  // scan the :
    while i < data.len() {
        match data.as_bytes()[i] {
            b'\0' ... b' ' | b'<' | b'>' => break,
            _ => i += 1
        }
    }
    if i == data.len() { return 0; }
    i
}

fn scan_email(data: &str) -> usize {
    // using a regex library would be convenient, but doing it by hand is not too bad
    let size = data.len();
    let mut i = 0;
    while i < size {
        match data.as_bytes()[i] {
            c if is_ascii_alphanumeric(c) => i += 1,
            b'.' | b'!' | b'#' | b'$' | b'%' | b'&' | b'\'' | b'*' | b'+' | b'/' |
            b'=' | b'?' | b'^' | b'_' | b'`' | b'{' | b'|' | b'}' | b'~' | b'-' => i += 1,
            _ => break
        }
    }
    if scan_ch(&data[i..], b'@') == 0 { return 0; }
    i += 1;
    loop {
        let label_beg = i;
        while i < size {
            match data.as_bytes()[i] {
                c if is_ascii_alphanumeric(c) => i += 1,
                b'-' => i += 1,
                _ => break,
            }
        }
        if i == label_beg || i - label_beg > 63 ||
                data.as_bytes()[label_beg] == b'-' || data.as_bytes()[i - 1] == b'-' { return 0; }
        if scan_ch(&data[i..], b'.') == 0 { break; }
        i += 1
    }
    i
}

pub fn scan_attribute_name(data: &str) -> Option<usize> {
    let size = data.len();
    if size == 0 { 
        return None; }
    match data.as_bytes()[0] {
        c if is_ascii_alpha(c) => (),
        b'_' | b':' => (),
        _ => {
   
            return None;
        }
    }
    let mut i = 1;
    while i < size {
        match data.as_bytes()[i] {
            c if is_ascii_alphanumeric(c) => i += 1,
            b'_' | b'.' | b':' | b'-' => i += 1,
            _ => break
        }
    }
    Some(i)
}

pub fn scan_attribute_value(data: &str) -> Option<usize> {
    let size = data.len();
    if size == 0 { return None; }
    let mut i = 0;
    match data.as_bytes()[0] {
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
            // return Some(i);
        }
    }
    if i >= data.len() { return None; }
    return Some(i);

}

pub fn is_escaped(data: &str, loc: usize) -> bool {
    let mut i = loc;
    while i >= 1 && data.as_bytes()[i - 1] == b'\\' {
        i -= 1;
    }
    ((loc - i) & 1) != 0
}

// Remove backslash escapes and resolve entities
pub fn unescape(input: &str) -> Cow<str> {
    let mut result = String::new();
    let mut mark = 0;
    let mut i = 0;
    let bytes = input.as_bytes();
    while i < input.len() {
        match bytes[i] {
            b'\\' if i + 1 < input.len() && (bytes[i + 1] as char).is_ascii_punctuation() => {
                result.push_str(&input[mark..i]);
                i += 1;
                mark = i;
            }
            b'&' => {
                match scan_entity(&input[i..]) {
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
        Borrowed(input)
    } else {
        result.push_str(&input[mark..]);
        Owned(result)
    }
}

pub fn scan_html_block_tag(data: &str) -> (usize, &str) {
    let mut i = scan_ch(data, b'<');
    if i == 0 { return (0, "") }
    i += scan_ch(&data[i..], b'/');
    let n = scan_while(&data[i..], is_ascii_alphanumeric);
    // TODO: scan attributes and >
    (i + n, &data[i .. i + n])
}

pub fn is_html_tag(tag: &str) -> bool {
    HTML_TAGS.binary_search_by(|probe| utils::strcasecmp(probe, tag)).is_ok()
}

pub fn scan_html_type_7(data: &str) -> Option<usize> {
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
            let c = data.as_bytes()[i];
            match c {
                b'/' | b'>' => { 
                    break; },
                _ => {},
            }
            if whitespace == 0 { return None; }
            if let Some(a) = scan_attribute(&data[i..]) {
                if a == 0 { break; }
                i += a;
            } else {
                return None;
            }
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

    if let Some(_) = scan_blank_line(&data[i..]) {
        return Some(i);
    } else {
        return None;
    }
}

pub fn scan_attribute(data: &str) -> Option<usize> {
    let mut i = scan_whitespace_no_nl(data);
    if let Some(attr_name_bytes) = scan_attribute_name(&data[i..]) {
        i += attr_name_bytes;
    } else {
        return None;
    }
    if let Some(attr_valspec_bytes) = scan_attribute_value_spec(&data[i..]) {
        i += attr_valspec_bytes;
    } else {
        return None;
    }
    return Some(i);
}

pub fn scan_attribute_value_spec(data: &str) -> Option<usize> {
    let mut i = scan_whitespace_no_nl(data);
    let eq = scan_ch(&data[i..], b'=');
    if eq == 0 { return None; }
    i += eq;
    i += scan_whitespace_no_nl(&data[i..]);
    if let Some(attr_val_bytes) = scan_attribute_value(&data[i..]) {
        i += attr_val_bytes;
        return Some(i);
    } else {
        return None;
    }
}

pub fn spaces(n: usize) -> Cow<'static, str> {
    let a_bunch_of_spaces = "                                ";
    if n <= a_bunch_of_spaces.len() {
        Borrowed(&a_bunch_of_spaces[..n])
    } else {
        let mut result = String::new();
        for _ in 0..n {
            result.push(' ');
        }
        Owned(result)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn overflow_list() {
        assert_eq!((0, 0, 0, 0), scan_listitem("4444444444444444444444444444444444444444444444444444444444!"));
    }
}
