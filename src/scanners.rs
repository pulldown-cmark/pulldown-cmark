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

use entities;
use utils;
use std::borrow::Cow;
use std::borrow::Cow::{Borrowed, Owned};
use std::char;
use parse::Alignment;

pub use puncttable::{is_ascii_punctuation, is_punctuation};

// sorted for binary_search
const HTML_TAGS: [&'static str; 50] = ["article", "aside", "blockquote",
    "body", "button", "canvas", "caption", "col", "colgroup", "dd", "div",
    "dl", "dt", "embed", "fieldset", "figcaption", "figure", "footer", "form",
    "h1", "h2", "h3", "h4", "h5", "h6", "header", "hgroup", "hr", "iframe",
    "li", "map", "object", "ol", "output", "p", "pre", "progress", "script",
    "section", "style", "table", "tbody", "td", "textarea", "tfoot", "th",
    "thead", "tr", "ul", "video"];

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

fn is_hexdigit(c: u8) -> bool {
    match c {
        b'0' ... b'9' | b'a' ... b'f' | b'A' ... b'F' => true,
        _ => false
    }
}

fn is_digit(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

// scan a single character
pub fn scan_ch(data: &str, c: u8) -> usize {
    if !data.is_empty() && data.as_bytes()[0] == c { 1 } else { 0 }
}

pub fn scan_while<F>(data: &str, f: F) -> usize
        where F: Fn(u8) -> bool {
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

// Maybe returning Option<usize> would be more Rustic?
pub fn scan_eol(s: &str) -> (usize, bool) {
    if s.is_empty() { return (0, true); }
    match s.as_bytes()[0] {
        b'\n' => (1, true),
        b'\r' => (if s[1..].starts_with('\n') { 2 } else { 1 }, true),
        _ => (0, false)
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

// Maybe Option, size of 0 makes sense at EOF
pub fn scan_blank_line(text: &str) -> usize {
    let i = scan_whitespace_no_nl(text);
    if let (n, true) = scan_eol(&text[i..]) {
        i + n
    } else {
        0
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
            b' '|b'\t' => (),
            _ => return 0
        }
        i += 1;
    }
    if n >= 3 { i } else { 0 }
}

// returns number of bytes in prefix and level
pub fn scan_atx_header(data: &str) -> (usize, i32) {
    let size = data.len();
    let level = scan_ch_repeat(data, b'#');
    let i = level;
    if level >= 1 && level <= 6 {
        if i < size {
            match data.as_bytes()[i] {
                b' ' | b'\t' ... b'\r' => (),
                _ => return (0, 0)
            }
        }
        (i, level as i32)
    } else {
        (0, 0)
    }
}

// returns number of bytes in line (including trailing newline) and level
pub fn scan_setext_header(data: &str) -> (usize, i32) {
    let size = data.len();
    let mut i = 0;
    if i == size { return (0, 0); }
    let c = data.as_bytes()[i];
    if !(c == b'-' || c == b'=') { return (0, 0); }
    i += 1 + scan_ch_repeat(&data[i + 1 ..], c);
    let n = scan_blank_line(&data[i..]);
    if n == 0 { return (0, 0); }
    i += n;
    let level = if c == b'=' { 1 } else { 2 };
    (i, level)
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

// return number of bytes scanned, delimeter, start index, and indent
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

pub fn scan_attribute_name(data: &str) -> usize {
    let size = data.len();
    if size == 0 { return 0; }
    match data.as_bytes()[0] {
        c if is_ascii_alpha(c) => (),
        b'_' | b':' => (),
        _ => return 0
    }
    let mut i = 1;
    while i < size {
        match data.as_bytes()[i] {
            c if is_ascii_alphanumeric(c) => i += 1,
            b'_' | b'.' | b':' | b'-' => i += 1,
            _ => break
        }
    }
    i
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
    if input.find(|c| c == '\\' || c == '&' || c == '\r').is_none() {
        Borrowed(input)
    } else {
        let mut result = String::new();
        let mut mark = 0;
        let mut i = 0;
        while i < input.len() {
            match input.as_bytes()[i] {
                b'\\' => {
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
        result.push_str(&input[mark..]);
        Owned(result)
    }
}

pub fn is_html_tag(tag: &str) -> bool {
    HTML_TAGS.binary_search_by(|probe| utils::strcasecmp(probe, tag)).is_ok()
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
