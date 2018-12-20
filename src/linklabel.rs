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

use crate::scanners::is_ascii_whitespace;

pub struct LinkLabelBuilder<'a> {
    text: Cow<'a, str>,
    nest: usize,
    cp: usize,
    backslash: bool,
    ws: bool,
}

pub type LinkLabel<'a> = UniCase<Cow<'a, str>>;

impl<'a> LinkLabelBuilder<'a> {
    pub fn new() -> LinkLabelBuilder<'a> {
        LinkLabelBuilder { text: "".into(), nest: 0, cp: 0, backslash: false, ws: true }
    }

    /// Add text to the link.
    ///
    /// If the text contains the closing bracket, return the offset after that.
    pub fn add_text(&mut self, text: &'a str) -> Option<usize> {
        let mut start = 0;
        let mut i = start;
        while i < text.len() {
            let b = text.as_bytes()[i];
            if (b as i8) >= -0x40 {
                self.cp += 1;
                // TODO: should we bail on overflow here?
            }
            i += 1;
            if self.backslash {
                self.backslash = false;
                // TODO(spec clarification): is this guard necessary? It's here to match dingus
                if !is_ascii_whitespace(b) {
                    continue;
                }
            }
            match b {
                b' ' => {
                    if self.ws {
                        self.append(&text[start..i - 1]);
                        start = i;
                    }
                    self.ws = true;
                }
                0x09..=0x0d => {
                    self.append(&text[start..i - 1]);
                    start = i;
                    if !self.ws {
                        self.append(" ");
                    }
                    self.ws = true;
                }
                b'\\' => {
                    self.backslash = true;
                    self.ws = false;
                }
                b'[' => {
                    self.ws = false;
                    self.nest += 1;
                }
                b']' => {
                    if self.nest == 0 {
                        self.append(&text[start..i - 1]);
                        return Some(i);
                    }
                    self.ws = false;
                    self.nest -= 1;
                }
                _ => self.ws = false,
            }
        }
        self.append(&text[start..i]);
        None
    }

    /// Build a link label, representing the normalized text.
    pub fn build(mut self) -> LinkLabel<'a> {
        if self.ws {
            let len = self.text.len() - 1;
            match self.text {
                Cow::Borrowed(s) => self.text = s[..len].into(),
                Cow::Owned(ref mut s) => s.truncate(len),
            }
        }
        UniCase::new(self.text)
    }

    /// Get the number of codepoints (including close bracket).
    pub fn codepoint_count(&self) -> usize {
        self.cp
    }

    /// Append text, trying to preserve borrow-ness.
    ///
    /// Note: if the borrowed strings are actually contiguous, we could be more
    /// aggressive about the result also being borrowed.
    fn append(&mut self, text: &'a str) {
        if text.is_empty() {
            return;
        }
        if self.text.is_empty() {
            self.text = text.into();
        } else if let Cow::Owned(ref mut s) = self.text {
            s.push_str(text);
        } else {
            self.text = [&self.text, text].concat().into();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    fn ll(text: &str) -> LinkLabel {
        let mut builder = LinkLabelBuilder::new();
        builder.add_text(text);
        builder.build()
    }

    fn lcp(text: &str) -> usize {
        let mut builder = LinkLabelBuilder::new();
        builder.add_text(text);
        builder.codepoint_count()
    }

    fn lix(text: &str) -> Option<usize> {
        let mut builder = LinkLabelBuilder::new();
        builder.add_text(text)
    }

    fn u<'a>(text: &str) -> LinkLabel {
        LinkLabel::new(text.into())
    }

    #[test]
    fn link_label_normalize() {
        assert_eq!(ll("abc]"), u("abc"));
        assert_eq!(ll(" abc ]"), u("abc"));
        assert_eq!(ll("abc def]"), u("abc def"));
        assert_eq!(ll("abc\tdef]"), u("abc def"));
        assert_eq!(ll("abc  def]"), u("abc def"));
        assert_eq!(ll("\tabc\tdef\t]"), u("abc def"));
        assert_eq!(ll("ABC]"), u("abc"));
        assert_eq!(ll("Толпой]"), u("ТОЛПОЙ"));
    }

    #[test]
    fn link_label_count() {
        assert_eq!(lcp("abc]"), 4);
        assert_eq!(lcp(" abc ]"), 6);
        assert_eq!(lcp("Толпой]"), 7);
    }

    #[test]
    fn link_label_backslash() {
        assert_eq!(ll("abc\\]]"), u("abc\\]"));
        // Note: this one is tricky, we strip trailing whitespace even if escaped.
        assert_eq!(ll("abc\\ ]"), u("abc\\"));
        assert_eq!(ll("abc\\\\]"), u("abc\\\\"));
    }

    #[test]
    fn link_label_backslash_ws() {
        assert_eq!(ll("abc\\  def]"), u("abc\\ def"));
        assert_eq!(ll("abc\\\tdef]"), u("abc\\ def"));
    }

    #[test]
    fn link_label_nest() {
        assert_eq!(ll("[abc]]"), u("[abc]"));
        assert_eq!(ll("link [foo [bar]]]"), u("link [foo [bar]]"));
    }

    #[test]
    fn link_label_close() {
        assert_eq!(lix("abc"), None);
        assert_eq!(lix("abc]"), Some(4));
        assert_eq!(lix("abc][def]"), Some(4));
    }

    #[test]
    fn link_label_multi_add() {
        let mut builder = LinkLabelBuilder::new();
        assert_eq!(builder.add_text("abc"), None);
        assert_eq!(builder.add_text("]"), Some(1));
        assert_eq!(builder.build(), u("abc"));
    }
}
