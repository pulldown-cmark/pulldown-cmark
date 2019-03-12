// Copyright 2018 Google LLC
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

//! Link label parsing and matching.

use unicase::UniCase;

use crate::strings::CowStr;

pub enum ReferenceLabel<'a> {
    Link(CowStr<'a>),
    Footnote(CowStr<'a>),
}

pub type LinkLabel<'a> = UniCase<CowStr<'a>>;

/// Returns number of bytes (including brackets) and label on success.
pub(crate) fn scan_link_label<'a>(text: &'a str) -> Option<(usize, ReferenceLabel<'a>)> {
    if text.len() < 2 || text.as_bytes()[0] != b'[' { return None; }
    let pair = if b'^' == text.as_bytes()[1] {
        let (byte_index, cow) = scan_link_label_rest(&text[2..])?;
        (byte_index + 2, ReferenceLabel::Footnote(cow))
    } else {
        let (byte_index, cow) = scan_link_label_rest(&text[1..])?;
        (byte_index + 1, ReferenceLabel::Link(cow))
    };
    Some(pair)
}

/// Assumes the opening bracket has already been scanned.
/// Returns the number of bytes read (including closing bracket) and label on success.
pub(crate) fn scan_link_label_rest<'a>(text: &'a str) -> Option<(usize, CowStr<'a>)> {
    let mut char_iter = text.chars().peekable();
    let mut byte_index = 0;
    let mut only_white_space = true;
    let mut still_borrowed = true;
    let mut codepoints = 0;
    // no worries, doesnt allocate until we push things onto it
    let mut label = String::new();
    let start_byte = byte_index;

    loop {
        if codepoints >= 1000 { return None; }
        let mut c = char_iter.next()?;
        byte_index += c.len_utf8();

        match c {
            '[' => return None,
            ']' => break,
            '\\' => {
                let next = char_iter.next()?;
                byte_index += next.len_utf8();
                codepoints += 2;
            }
            _ if c.is_whitespace() => {
                // normalize labels by collapsing whitespaces, including linebreaks
                let mut whitespaces = 1;
                let mut byte_addition = 0;
                loop {
                    match char_iter.peek() {
                        Some(w) if w.is_whitespace() => {
                            whitespaces += 1;
                            byte_addition += w.len_utf8();
                            let _ = char_iter.next();
                        }
                        _ => break,
                    }
                }
                if whitespaces > 1 || c != ' ' {
                    byte_index -= c.len_utf8();
                    if still_borrowed {
                        label.push_str(&text[start_byte..byte_index]);
                        still_borrowed = false;
                    }
                    c = ' ';
                    byte_index += c.len_utf8();
                }
                byte_index += byte_addition;
                codepoints += whitespaces;
            }
            _ => {
                only_white_space = false;
                codepoints += 1;
            }
        }

        if !still_borrowed {
            label.push(c);
        }
    }

    if only_white_space {
        return None;
    }
    let cow = if still_borrowed {
        text[start_byte..(byte_index - 1)].into()
    } else {
        label.into()
    };
    Some((byte_index, cow))
} 
