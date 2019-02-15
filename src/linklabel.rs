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

use std::borrow::Cow;

use unicase::UniCase;

pub enum ReferenceLabel<'a> {
    Link(Cow<'a, str>),
    Footnote(Cow<'a, str>),
}

pub type LinkLabel<'a> = UniCase<Cow<'a, str>>;

pub(crate) fn scan_link_label<'a>(text: &'a str) -> Option<(usize, ReferenceLabel<'a>)> {
    let mut char_iter = text.chars().peekable();
    if let Some('[') = char_iter.next() {} else { return None; }
    let mut only_white_space = true;
    let mut still_borrowed = true;
    let mut codepoints = 0;
    let mut byte_index = 1;
    // no worries, doesnt allocate until we push things onto it
    let mut label = String::new();
    let is_footnote = if let Some(&'^') = char_iter.peek() {
        // consume ^, but don't make it part of the label
        let _ = char_iter.next();
        byte_index += 1;
        true
    } else {
        false
    };
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
    let reference = if is_footnote {
        ReferenceLabel::Footnote(cow)
    } else {
        ReferenceLabel::Link(cow)
    };
    Some((byte_index, reference))
}
