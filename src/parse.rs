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

//! Raw parser, for doing a single pass over input.

use scanners::*;
use utils;
use std::borrow::Cow;
use std::borrow::Cow::{Borrowed};
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use std::cmp;

#[derive(PartialEq, Debug)]
enum State {
    StartBlock,
    InContainers,
    Inline,
    TableHead(usize, usize), // limit, next
    TableBody,
    TableRow,
    CodeLineStart,
    Code,
    InlineCode,
    Literal,
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Container {
    BlockQuote,
    List(usize, u8),
    ListItem(usize),
    FootnoteDefinition,
}

pub struct RawParser<'a> {
    text: &'a str,
    off: usize,

    opts: Options,
    active_tab: [u8; 256],

    state: State,
    stack: Vec<(Tag<'a>, usize, usize)>,
    leading_space: usize,

    containers: Vec<Container>,
    last_line_was_empty: bool,

    /// In case we have a broken link/image reference, we can call this callback
    /// with the reference name (both normalized and not normalized) and use
    /// the link/title pair returned instead
    broken_link_callback: Option<&'a Fn(&str, &str) -> Option<(String, String)>>,

    // state for code fences
    fence_char: u8,
    fence_count: usize,
    fence_indent: usize,

    // info, used in second pass
    loose_lists: HashSet<usize>,  // offset is at list marker
    links: HashMap<String, (Cow<'a, str>, Cow<'a, str>)>,
}

pub struct ParseInfo<'a> {
    pub loose_lists: HashSet<usize>,
    pub links: HashMap<String, (Cow<'a, str>, Cow<'a, str>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tag<'a> {
    // block-level tags
    Paragraph,
    Rule,

    /// A heading. The field indicates the level of the heading.
    Header(i32),

    BlockQuote,
    CodeBlock(Cow<'a, str>),

    /// A list. If the list is ordered the field indicates the number of the first item.
    List(Option<usize>),  // TODO: add delim and tight for ast (not needed for html)
    Item,
    FootnoteDefinition(Cow<'a, str>),

    // tables
    Table(Vec<Alignment>),
    TableHead,
    TableRow,
    TableCell,

    // span-level tags
    Emphasis,
    Strong,
    Code,

    /// A link. The first field is the destination URL, the second is a title
    Link(Cow<'a, str>, Cow<'a, str>),

    /// An image. The first field is the destination URL, the second is a title
    Image(Cow<'a, str>, Cow<'a, str>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Event<'a> {
    Start(Tag<'a>),
    End(Tag<'a>),
    Text(Cow<'a, str>),
    Html(Cow<'a, str>),
    InlineHtml(Cow<'a, str>),
    FootnoteReference(Cow<'a, str>),
    SoftBreak,
    HardBreak,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Alignment {
    None,
    Left,
    Center,
    Right,
}

bitflags! {
    pub struct Options: u32 {
        const FIRST_PASS = 1 << 0;
        const ENABLE_TABLES = 1 << 1;
        const ENABLE_FOOTNOTES = 1 << 2;
    }
}

const MAX_LINK_NEST: usize = 10;

impl<'a> RawParser<'a> {
    pub fn new_with_links_and_callback(text: &'a str, opts: Options,
            links: HashMap<String, (Cow<'a, str>, Cow<'a, str>)>,
            callback: Option<&'a Fn(&str, &str) -> Option<(String, String)>>)
    -> RawParser<'a> {
        let mut ret = RawParser {
            text: text,
            off: if text.starts_with("\u{FEFF}") { 3 } else { 0 },
            opts: opts,
            active_tab: [0; 256],
            state: State::StartBlock,
            leading_space: 0,
            stack: Vec::new(),
            containers: Vec::new(),
            last_line_was_empty: false,

            fence_char: 0,
            fence_count: 0,
            fence_indent: 0,

            broken_link_callback: callback,

            // info, used in second pass
            loose_lists: HashSet::new(),
            links: links,
        };
        ret.init_active();
        ret.skip_blank_lines();
        ret
    }

    pub fn new_with_links(text: &'a str, opts: Options,
            links: HashMap<String, (Cow<'a, str>, Cow<'a, str>)>) -> RawParser<'a> {
        Self::new_with_links_and_callback(text, opts, links, None)
    }

    pub fn new(text: &'a str, opts: Options) -> RawParser<'a> {
        Self::new_with_links(text, opts, HashMap::new())
    }

    // offset into text representing current parse position, hopefully
    // useful for building source maps
    pub fn get_offset(&self) -> usize {
        self.off
    }

    // extract info from parser on finish
    pub fn get_info(self) -> ParseInfo<'a> {
        ParseInfo {
            loose_lists: self.loose_lists,
            links: self.links,
        }
    }

    fn init_active(&mut self) {
        if self.opts.contains(Options::FIRST_PASS) {
            self.active_tab[b'\n' as usize] = 1
        } else {
            for &c in b"\x00\t\n\r_\\&*[!`<" {
                self.active_tab[c as usize] = 1;
            }
        }
    }

    fn limit(&self) -> usize {
        match self.stack.last() {
            Some(&(_, limit, _)) => limit,
            None => self.text.len()
        }
    }

    // if end is not known, limit should be text.len(), next should be 0
    fn start(&mut self, tag: Tag<'a>, limit: usize, next: usize) -> Event<'a> {
        self.stack.push((tag.clone(), limit, next));
        Event::Start(tag)
    }

    fn end(&mut self) -> Event<'a> {
        let (tag, _, next) = self.stack.pop().unwrap();
        match tag {
            // containers
            Tag::BlockQuote | Tag::List(_) | Tag::Item | Tag::FootnoteDefinition(_) => {
                let _ = self.containers.pop();
            }

            // block level tags
            Tag::Paragraph | Tag::Header(_) | Tag::Rule | Tag::CodeBlock(_) | Tag::Table(_) => {
                self.state = State::StartBlock;
                // TODO: skip blank lines (for cleaner source maps)
            }

            // tables
            Tag::TableCell => self.state = State::TableRow,
            Tag::TableRow | Tag::TableHead => self.state = State::TableBody,

            // inline
            Tag::Code => self.state = State::Inline,
            _ => (),
        }
        if next != 0 { self.off = next; }

        /*
        if self.stack.is_empty() {
            // TODO maybe: make block ends do this
            self.state = State::StartBlock;
            self.skip_blank_lines();
        }
        */
        Event::End(tag)
    }

    fn skip_leading_whitespace(&mut self) {
        self.off += scan_whitespace_no_nl(&self.text[self.off .. self.limit()]);
    }

    // TODO: this function doesn't respect containers
    fn skip_blank_lines(&mut self) {
        loop {
            let ret = scan_blank_line(&self.text[self.off..]);
            if ret == 0 {
                break;
            }
            self.off += ret;
        }
    }

    // Scan markers and indentation for current container stack
    // Return: bytes scanned, whether containers are complete, and remaining space
    fn scan_containers(&self, text: &str) -> (usize, bool, usize) {
        let (mut i, mut space) = scan_leading_space(text, 0);
        for container in &self.containers {
            match *container {
                Container::BlockQuote => {
                    if space <= 3 {
                        let n = scan_blockquote_start(&text[i..]);
                        if n > 0 {
                            let (n_sp, next_space) = scan_leading_space(text, i + n);
                            i += n + n_sp;
                            space = next_space;
                        } else {
                            return (i, false, space);
                        }
                    } else {
                        return (i, false, space);
                    }
                }
                Container::FootnoteDefinition |
                Container::List(_, _) => (),
                Container::ListItem(indent) => {
                    if space >= indent {
                        space -= indent;
                    } else if scan_eol(&text[i..]).1 {
                        space = 0;
                    } else {
                        return (i, false, 0);
                    }
                }
            }
        }
        (i, true, space)
    }

    // scans empty lines with current container stack
    // returns number of bytes scanned, number of empty lines
    // note: EOF counts as a line ending for counting lines
    fn scan_empty_lines(&self, text: &str) -> (usize, usize) {
        let mut i = 0;
        let mut lines = 0;
        loop {
            let (n, scanned, _) = self.scan_containers(&text[i..]);
            if !scanned {
                return (i, lines);
            }
            if i == text.len() {
                return (i, lines + 1);
            }
            let n_blank = scan_eol(&text[i + n ..]).0;
            if n_blank == 0 {
                return (i, lines);
            }
            i += n + n_blank;
            lines += 1;
        }
    }

    // scans whitespace, skipping past containers on newline
    fn scan_whitespace_inline(&self, text: &str) -> usize {
        let i = scan_whitespace_no_nl(text);
        if let (n, true) = scan_eol(&text[i..]) {
            let (n_containers, _, space) = self.scan_containers(&text[i + n ..]);
            let j = i + n + n_containers;
            if !self.is_inline_block_end(&text[j..], space) {
                return j;
            }
        }
        i
    }

    fn at_list(&self, level: usize) -> Option<usize> {
        let len = self.containers.len();
        if len >= level {
            if let Container::List(offset, _) = self.containers[len - level] {
                return Some(offset);
            }
        }
        None
    }

    fn start_block(&mut self) -> Option<Event<'a>> {
        let size = self.text.len();
        //println!("start_block {}", self.off);
        while self.off < size {
            //println!("start_block loop {} {}", self.off, self.last_line_was_empty);
            if self.off >= self.limit() {
                return Some(self.end());
            }
            if self.state != State::InContainers {
                let (n, scanned, space) = self.scan_containers(&self.text[self.off ..]);
                if !scanned {
                    return Some(self.end());
                }
                self.leading_space = space;
                self.off += n;
                self.state = State::InContainers;
            }

            let (n, at_eol) = scan_eol(&self.text[self.off ..]);
            if at_eol {
                self.off += n;
                self.state = State::StartBlock;
                // two empty lines closes lists, one empty line closes a footnote
                let (n, empty_lines) = self.scan_empty_lines(&self.text[self.off ..]);
                for i in (0..self.stack.len()).rev() {
                    let is_break = match self.stack[i].0 {
                        Tag::List(_) => empty_lines >= 1,
                        Tag::Item => false,
                        Tag::FootnoteDefinition(_) => true,
                        _ => break,
                    };
                    if is_break {
                        for tag in &mut self.stack[i..] {
                            tag.1 = self.off; // limit
                            tag.2 = self.off; // next
                        }
                        self.stack[i].2 = self.off + n; // next
                        return Some(self.end());
                    }
                }
                self.off += n;
                if let Some(_) = self.at_list(2) {
                    self.last_line_was_empty = true;
                }
                continue;
            }

            //println!("checking loose {} {:?}", self.last_line_was_empty, self.at_list(2));
            if self.last_line_was_empty {
                if let Some(offset) = self.at_list(2) {
                    // list item contains two blocks separated by empty line
                    self.loose_lists.insert(offset);
                }
            }

            if self.leading_space >= 4 && !self.at_list(1).is_some() {
                // see below
                if let Some(&Container::List(_, _)) = self.containers.last() {
                    return Some(self.end());
                }
                return Some(self.start_indented_code());
            }

            let tail = &self.text[self.off ..];

            // must be before list item because ambiguous
            let n = scan_hrule(tail);
            if n != 0 {
                self.last_line_was_empty = false;
                // see below
                if let Some(&Container::List(_, _)) = self.containers.last() {
                    return Some(self.end());
                }
                self.off += n;
                return Some(self.start_hrule());
            }

            let (n, c, start, indent) = scan_listitem(tail);
            if n != 0 {
                if self.last_line_was_empty {
                    if let Some(offset) = self.at_list(1) {
                        // two list items separated by empty line
                        self.loose_lists.insert(offset);
                    }
                }
                self.last_line_was_empty = false;
                return Some(self.start_listitem(n, c, start, indent));
            }

            // not a list item, so if we're in a list, close it
            if let Some(&Container::List(_, _)) = self.containers.last() {
                return Some(self.end());
            }
            self.last_line_was_empty = false;

            let c = tail.as_bytes()[0];
            match c {
                b'#' => {
                    let (n, level) = scan_atx_header(tail);
                    if n != 0 {
                        self.off += n;
                        return Some(self.start_atx_header(level));
                    }
                }
                b'`' | b'~' => {
                    let (n, ch) = scan_code_fence(tail);
                    if n != 0 {
                        return Some(self.start_code_fence(n, ch, n));
                    }
                }
                b'>' => {
                    let n = scan_blockquote_start(tail);
                    if n != 0 {
                        self.off += n;
                        let (n, space) = scan_leading_space(self.text, self.off);
                        self.off += n;
                        self.leading_space = space;
                        self.containers.push(Container::BlockQuote);
                        return Some(self.start(Tag::BlockQuote, self.text.len(), 0));
                    }
                }
                b'<' => {
                    if self.is_html_block(tail) {
                        return Some(self.do_html_block());
                    }
                }
                b'[' => {
                    if self.opts.contains(Options::ENABLE_FOOTNOTES) {
                        if let Some((name, n)) = self.parse_footnote_definition(tail) {
                            if self.containers.last() == Some(&Container::FootnoteDefinition) {
                                return Some(self.end());
                            }
                            self.off += n;
                            self.containers.push(Container::FootnoteDefinition);
                            return Some(self.start(Tag::FootnoteDefinition(Cow::Borrowed(name)), self.text.len(), 0));
                        }
                    }
                    if self.try_link_reference_definition(tail) {
                        continue;
                    }
                }
                _ => ()
            }
            return Some(self.start_paragraph());
        }
        None
    }

    // can start a paragraph, a setext header, or a table, as they start similarly
    fn start_paragraph(&mut self) -> Event<'a> {
        let mut i = self.off + scan_nextline(&self.text[self.off..]);

        if let (n, true, space) = self.scan_containers(&self.text[i..]) {
            i += n;
            if space <= 3 {
                let (n, level) = scan_setext_header(&self.text[i..]);
                if n != 0 {
                    let next = i + n;
                    while i > self.off && is_ascii_whitespace(self.text.as_bytes()[i - 1]) {
                        i -= 1;
                    }
                    self.state = State::Inline;
                    return self.start(Tag::Header(level), i, next);
                }
                if self.opts.contains(Options::ENABLE_TABLES) {
                    let (n, cols) = scan_table_head(&self.text[i..]);
                    if n != 0 {
                        let next = i + n;
                        while i > self.off && is_ascii_whitespace(self.text.as_bytes()[i - 1]) {
                            i -= 1;
                        }
                        self.state = State::TableHead(i, next);
                        return self.start(Tag::Table(cols), self.text.len(), 0);
                    }
                }
            }
        }

        let size = self.text.len();
        self.state = State::Inline;
        self.start(Tag::Paragraph, size, 0)
    }

    fn start_table_head(&mut self) -> Event<'a> {
        assert!(self.opts.contains(Options::ENABLE_TABLES));
        if let State::TableHead(limit, next) = self.state {
            self.state = State::TableRow;
            return self.start(Tag::TableHead, limit, next);
        } else {
            panic!();
        }
    }

    fn start_table_body(&mut self) -> Event<'a> {
        assert!(self.opts.contains(Options::ENABLE_TABLES));
        let (off, _) = match self.scan_containers(&self.text[self.off ..]) {
            (n, true, space) => (self.off + n, space),
            _ => {
                return self.end();
            }
        };
        let n = scan_blank_line(&self.text[off..]);
        if n != 0 {
            self.off = off + n;
            return self.end();
        }
        self.state = State::TableRow;
        self.off = off;
        self.start(Tag::TableRow, self.text.len(), 0)
    }

    fn start_hrule(&mut self) -> Event<'a> {
        let limit = self.off;  // body of hrule is empty
        self.state = State::Inline;  // handy state for producing correct end tag
        self.start(Tag::Rule, limit, limit)
    }

    fn start_atx_header(&mut self, level: i32) -> Event<'a> {
        self.skip_leading_whitespace();
        let tail = &self.text[self.off..];
        let next = scan_nextline(tail);
        let mut limit = next;
        while limit > 0 && is_ascii_whitespace(tail.as_bytes()[limit - 1]) {
            limit -= 1;
        }
        let mut end = limit;
        while end > 0 && tail.as_bytes()[end - 1] == b'#' {
            end -= 1;
        }
        if end == 0 {
            limit = end;
        } else if is_ascii_whitespace(tail.as_bytes()[end - 1]) {
            limit = end - 1;
        }
        while limit > 0 && is_ascii_whitespace(tail.as_bytes()[limit - 1]) {
            limit -= 1;
        }
        let limit = limit + self.off;
        let next = next + self.off;
        self.state = State::Inline;
        self.start(Tag::Header(level), limit, next)
    }

    fn start_indented_code(&mut self) -> Event<'a> {
        self.fence_char = b'\0';
        self.fence_indent = 4;
        let size = self.text.len();
        self.state = State::Code;
        self.start(Tag::CodeBlock(Borrowed("")), size, 0)
    }

    fn start_listitem(&mut self, n: usize, c: u8, start: usize, indent: usize) -> Event<'a> {
        let indent = self.leading_space + indent;
        match self.containers.last() {
            Some(&Container::List(_, c2)) => {
                if c != c2 {
                    // mismatched list type or delimeter
                    return self.end();
                }
                self.off += n;
                let n_blank = scan_blank_line(&self.text[self.off ..]);
                if n_blank != 0 {
                    self.off += n_blank;
                    self.state = State::StartBlock;
                } else {
                    // TODO: deal with tab
                    let (n, space) = scan_leading_space(self.text, self.off);
                    self.off += n;
                    self.leading_space = space;
                }
                self.containers.push(Container::ListItem(indent));
                self.start(Tag::Item, self.text.len(), 0)
            }
            _ => {
                self.containers.push(Container::List(self.off, c));
                // arguably this should be done in the scanner, it should return option
                let startopt = if c == b'.' || c == b')' { Some(start) } else { None };
                self.start(Tag::List(startopt), self.text.len(), 0)
            }
        }
    }

    fn start_code_fence(&mut self, n: usize, ch: u8, count: usize) -> Event<'a> {
        self.fence_char = ch;
        self.fence_count = count;
        self.fence_indent = self.leading_space;
        let beg_info = self.off + n;
        let next_line = beg_info + scan_nextline(&self.text[beg_info..]);
        self.off = next_line;
        let info = self.text[beg_info..next_line].trim();
        let size = self.text.len();
        self.state = State::CodeLineStart;
        self.start(Tag::CodeBlock(Cow::Borrowed(info)), size, 0)
    }

    fn next_code_line_start(&mut self) -> Event<'a> {
        let (off, space) = match self.scan_containers(&self.text[self.off ..]) {
            (n, true, space) => (self.off + n, space),
            _ => {
                return self.end();
            }
        };

        if self.fence_char == b'\0' {
            let n = scan_blank_line(&self.text[off..]);
            if n != 0 {
                // TODO performance: this scanning is O(n^2) in the number of empty lines
                let (n_empty, _lines) = self.scan_empty_lines(&self.text[off + n ..]);
                let next = off + n + n_empty;
                let (n_containers, scanned, nspace) = self.scan_containers(&self.text[next..]);
                // TODO; handle space
                if !scanned || self.is_code_block_end(next + n_containers, nspace) {
                    //println!("was end: {}", next + n_containers);
                    return self.end();
                } else {
                    self.off = off;
                    //println!("code line start space={}, off={}", space, off);
                    self.leading_space = space;
                    return self.next_code();
                }
            }
        }

        if self.is_code_block_end(off, space) {
            let ret = self.end();
            if self.fence_char != b'\0' {
                self.off = off + scan_nextline(&self.text[off..]);
            }
            ret
        } else {
            self.off = off;
            self.state = State::Code;
            self.leading_space = space;
            self.next_code()
        }
    }

    fn next_code(&mut self) -> Event<'a> {
        if self.leading_space > self.fence_indent {
            // TODO: might try to combine spaces in text, for fewer events
            let space = self.leading_space;
            self.leading_space = 0;
            return Event::Text(spaces(space - self.fence_indent));
        }
        let bytes = self.text.as_bytes();
        let mut beg = self.off;
        let mut i = beg;
        loop {
            match bytes[i..].iter().position(|&c| c < b' ') {
                Some(j) => i += j,
                None => {
                    i += bytes[i..].len();
                    break;
                }
            }
            match bytes[i] {
                b'\n' => {
                    i += 1;
                    self.state = State::CodeLineStart;
                    break;
                }
                b'\r' => {
                    // just skip it (does not support '\r' only line break)
                    if i > beg { break; }
                    beg += 1;
                }
                _ => ()
            }
            i += 1;
        }
        self.off = i;
        Event::Text(Borrowed(&self.text[beg..i]))
    }

    fn is_code_block_end(&self, loc: usize, space: usize) -> bool {
        let tail = &self.text[loc..];
        if self.fence_char == b'\0' {
            // indented code block
            space < 4
        } else if space <= 3 {
            let (n, c) = scan_code_fence(tail);
            c == self.fence_char && n >= self.fence_count &&
                (n >= tail.len() || scan_blank_line(&tail[n..]) != 0)
        } else {
            false
        }
    }

    // # HTML blocks

    fn scan_html_block_tag(&self, data: &'a str) -> (usize, &'a str) {
        let mut i = scan_ch(data, b'<');
        if i == 0 { return (0, "") }
        i += scan_ch(&data[i..], b'/');
        let n = scan_while(&data[i..], is_ascii_alphanumeric);
        // TODO: scan attributes and >
        (i + n, &data[i .. i + n])
    }

    fn is_html_block(&self, data: &str) -> bool {
        let (n_tag, tag) = self.scan_html_block_tag(data);
        (n_tag > 0 && is_html_tag(tag)) ||
                data.starts_with("<?") ||
                data.starts_with("<!")
    }

    // http://spec.commonmark.org/0.26/#html-blocks
    fn get_html_tag(&self) -> Option<&'static str> {
        static BEGIN_TAGS: &'static [&'static str; 3] = &["script", "pre", "style"];
        static END_TAGS: &'static [&'static str; 3] = &["</script>", "</pre>", "</style>"];

        for (beg_tag, end_tag) in BEGIN_TAGS.iter().zip(END_TAGS.iter()) {
            if self.off + 1 + beg_tag.len() < self.text.len() &&
               self.text[self.off + 1..].starts_with(&beg_tag[..]) {
                let pos = self.off + beg_tag.len() + 1;
                let s = self.text.as_bytes()[pos];
                if s == b' ' || s == b'\n' || s == b'>' {
                    return Some(end_tag);
                }
            }
        }
        static ST_BEGIN_TAGS: &'static [&'static str; 3] = &["<!--", "<?", "<![CDATA["];
        static ST_END_TAGS: &'static [&'static str; 3] = &["-->", "?>", "]]>"];
        for (beg_tag, end_tag) in ST_BEGIN_TAGS.iter().zip(ST_END_TAGS.iter()) {
            if self.off + 1 + beg_tag.len() < self.text.len() &&
               self.text[self.off + 1..].starts_with(&beg_tag[..]) {
                return Some(end_tag);
            }
        }
        if self.off + 4 < self.text.len() &&
           self.text[self.off + 1..].starts_with("<!") {
            let c = self.text[self.off + 4..self.off + 5].chars().next().unwrap();
            if c >= 'A' && c <= 'Z' {
                return Some(">");
            }
        }
        None
    }

    fn do_html_block(&mut self) -> Event<'a> {
        let mut i = self.off;
        if let Some(tag) = self.get_html_tag() {
            let text = self.text[i..].split(tag).take(1).next().unwrap_or("");
            self.off = i + text.len();
            self.state = State::StartBlock;
            return Event::Html(utils::cow_append(Borrowed(""), Borrowed(&self.text[i..self.off])));
        }
        let size = self.text.len();
        let mut out = Borrowed("");
        let mut mark = i;
        loop {
            let n = scan_nextline(&self.text[i..]);
            i += n;
            if n >= 2 && self.text.as_bytes()[i - 2] == b'\r' {
                if self.leading_space > 0 {
                    out = utils::cow_append(out, spaces(self.leading_space));
                    self.leading_space = 0;
                }
                out = utils::cow_append(out, Borrowed(&self.text[mark .. i - 2]));
                mark = i - 1;
            }
            let (n, scanned, space) = self.scan_containers(&self.text[i..]);
            let n_blank = scan_blank_line(&self.text[i + n ..]);
            if n != 0 || !scanned || i + n == size || n_blank != 0 {
                if self.leading_space > 0 {
                    out = utils::cow_append(out, spaces(self.leading_space));
                }
                self.leading_space = space;
                out = utils::cow_append(out, Borrowed(&self.text[mark..i]));
                mark = i + n;
            }
            if !scanned || i + n == size || n_blank != 0 {
                self.off = i;  // TODO: skip blank lines (cleaner source maps)
                self.state = State::StartBlock;
                return Event::Html(out)
            }
        }
    }

    // # Link reference definitions

    fn try_link_reference_definition(&mut self, data: &'a str) -> bool {
        let (n_link, text_beg, text_end, max_nest) = self.scan_link_label(data);
        if n_link == 0 || max_nest > 1 { return false; }
        let n_colon = scan_ch(&data[n_link ..], b':');
        if n_colon == 0 { return false; }
        let mut i = n_link + n_colon;
        i += self.scan_whitespace_inline(&data[i..]);
        let linkdest = scan_link_dest(&data[i..]);
        if linkdest.is_none() { return false; }
        let (n_dest, raw_dest) = linkdest.unwrap();
        if n_dest == 0 { return false; }
        i += n_dest;
        i += scan_whitespace_no_nl(&data[i..]);
        let n_nl = self.scan_whitespace_inline(&data[i..]);
        let (n_title, title_beg, title_end) = self.scan_link_title(&data[i + n_nl ..]);
        let title = if n_title == 0 {
            Borrowed("")
        } else {
            let (title_beg, title_end) = (i + n_nl + title_beg, i + n_nl + title_end);
            i += n_nl + n_title;
            unescape(&data[title_beg..title_end])
        };
        i += scan_whitespace_no_nl(&data[i..]);
        if let (n_eol, true) = scan_eol(&data[i..]) {
            i += n_eol;
        } else {
            return false;
        }

        let linktext = self.normalize_link_ref(&data[text_beg..text_end]);
        if linktext.is_empty() {
            return false;
        }
        if let Entry::Vacant(entry) = self.links.entry(linktext) {
            let dest = unescape(raw_dest);
            entry.insert((dest, title));
        }
        self.state = State::StartBlock;
        self.off += i;
        true
    }

    // normalize whitespace and case-fold
    fn normalize_link_ref(&self, raw: &str) -> String {
        let mut need_space = false;
        let mut result = String::new();
        let mut i = 0;
        while i < raw.len() {
            let n = scan_nextline(&raw[i..]);
            for c in raw[i.. i + n].chars() {
                if c.is_whitespace() {
                    need_space = true;
                } else {
                    if need_space && !result.is_empty() {
                        result.push(' ');
                    }
                    // TODO: Unicode case folding can differ from lowercase (ß)
                    result.extend(c.to_lowercase());
                    need_space = false;
                }
            }
            i += n;
            if i == raw.len() { break; }
            i += self.scan_containers(&raw[i..]).0;
            need_space = true;
        }
        result
    }

    // determine whether the line starting at loc ends the block
    fn is_inline_block_end(&self, data: &str, space: usize) -> bool {
        data.is_empty() ||
                scan_blank_line(data) != 0 ||
                space <= 3 && (scan_hrule(data) != 0 ||
                    scan_atx_header(data).0 != 0 ||
                    scan_code_fence(data).0 != 0 ||
                    scan_blockquote_start(data) != 0 ||
                    scan_listitem(data).0 != 0 ||
                    self.is_html_block(data))
    }

    fn next_table_cell(&mut self) -> Event<'a> {
        assert!(self.opts.contains(Options::ENABLE_TABLES));
        let bytes   = self.text.as_bytes();
        let mut beg = self.off + scan_whitespace_no_nl(&self.text[self.off ..]);
        let mut i   = beg;
        let limit   = self.limit();
        if i < limit && bytes[i] == b'|' {
            i   += 1;
            beg += 1;
            self.off += 1;
        }
        if i >= limit {
            self.off = limit;
            return self.end();
        }
        let mut n = 0;
        while i < limit {
            let c = bytes[i];
            if c == b'\\' && i + 1 < limit && bytes[i + 1] == b'|' {
                i += 2;
                continue;
            } else if c == b'|' {
                n = 0;
                break;
            }
            n = if is_ascii_whitespace(bytes[i]) { scan_blank_line(&self.text[i..]) } else { 0 };
            if n != 0 {
                if i > beg {
                    n = 0;
                }
                break;
            }
            i += 1;
        }
        if i > beg {
            self.state = State::Inline;
            self.start(Tag::TableCell, i, i + n)
        } else {
            self.off = i + n;
            self.end()
        }
    }

    fn next_inline(&mut self) -> Event<'a> {
        let bytes = self.text.as_bytes();
        let beg = self.off;
        let mut i = beg;
        let limit = self.limit();
        while i < limit {
            match bytes[i..limit].iter().position(|&c| self.active_tab[c as usize] != 0) {
                Some(pos) => i += pos,
                None => { i = limit; break; }
            }
            let c = bytes[i];
            if c == b'\n' || c == b'\r' {
                let n = scan_trailing_whitespace(&self.text[beg..i]);
                let end = i - n;
                if end > beg {
                    self.off = end;
                    return Event::Text(Borrowed(&self.text[beg..end]));
                }
                if c == b'\r' && i + 1 < limit && self.text.as_bytes()[i + 1] == b'\n' {
                    i += 1;
                }
                i += 1;
                let next = i;
                let (n_containers, _, space) = self.scan_containers(&self.text[i..limit]);
                i += n_containers;
                if self.is_inline_block_end(&self.text[i..limit], space) {
                    self.off = next;
                    return self.end();
                }
                i += scan_whitespace_no_nl(&self.text[i..limit]);
                self.off = i;
                return if n >= 2 { Event::HardBreak } else { Event::SoftBreak };
            }
            self.off = i;
            if i > beg {
                return Event::Text(Borrowed(&self.text[beg..i]));
            }
            if let Some(event) = self.active_char(c) {
                return event;
            }
            i = self.off;  // let handler advance offset even on None
            i += 1;
        }
        if i > beg {
            self.off = i;
            Event::Text(Borrowed(&self.text[beg..i]))
        } else {
            self.end()
        }
    }

    fn active_char(&mut self, c: u8) -> Option<Event<'a>> {
        match c {
            b'\x00' => Some(self.char_null()),
            b'\t' => Some(self.char_tab()),
            b'\\' => self.char_backslash(),
            b'&' => self.char_entity(),
            b'_' |
            b'*' => self.char_emphasis(),
            b'[' if self.opts.contains(Options::ENABLE_FOOTNOTES) => self.char_link_footnote(),
            b'[' | b'!' => self.char_link(),
            b'`' => self.char_backtick(),
            b'<' => self.char_lt(),
            _ => None
        }
    }

    fn char_null(&mut self) -> Event<'a> {
        self.off += 1;
        Event::Text(Borrowed("\u{fffd}"))
    }

    // expand tab in content (used for code and inline)
    // scan backward to find offset, counting unicode code points
    fn char_tab(&mut self) -> Event<'a> {
        let count = count_tab(&self.text.as_bytes()[.. self.off]);
        self.off += 1;
        Event::Text(Borrowed(&"    "[..count]))
    }

    fn char_backslash(&mut self) -> Option<Event<'a>> {
        let limit = self.limit();
        if self.off + 1 < limit {
            if let (_, true) = scan_eol(&self.text[self.off + 1 .. limit]) {
                let n_white = self.scan_whitespace_inline(&self.text[self.off + 1 .. limit]);
                let space = 0;  // TODO: figure this out
                if !self.is_inline_block_end(&self.text[self.off + 1 + n_white .. limit], space) {
                    self.off += 1 + n_white;
                    return Some(Event::HardBreak);
                }
            }
            let c = self.text.as_bytes()[self.off + 1];
            if is_ascii_punctuation(c) {
                self.off += 2;
                return Some(Event::Text(Borrowed(&self.text[self.off - 1 .. self.off])));
            }
        }
        None
    }

    fn char_entity(&mut self) -> Option<Event<'a>> {
        match scan_entity(&self.text[self.off ..]) {
            (n, Some(value)) => {
                self.off += n;
                Some(Event::Text(value))
            }
            _ => None
        }
    }

    fn char_emphasis(&mut self) -> Option<Event<'a>> {
        // can see to left for flanking info, but not past limit
        let limit = self.limit();
        let data = &self.text[..limit];

        let c = data.as_bytes()[self.off];
        let (n, can_open, _can_close) = compute_open_close(data, self.off, c);
        if !can_open {
            return None;
        }
        let mut stack = vec![n];  // TODO performance: don't allocate
        let mut i = self.off + n;
        while i < limit {
            let c2 = data.as_bytes()[i];
            if c2 == b'\n' && !is_escaped(data, i) {
                let (_, complete, space) = self.scan_containers(&self.text[i..]);
                if !complete {
                    i += 1;
                    continue;
                }
                if self.is_inline_block_end(&self.text[i + 1 .. limit], space) {
                    return None
                } else {
                    i += 1;
                }
            } else if c2 == c && !is_escaped(data, i) {
                let (mut n2, can_open, can_close) = compute_open_close(data, i, c);
                if can_close {
                    loop {
                        let ntos = stack.pop().unwrap();
                        if ntos > n2 {
                            stack.push(ntos - n2);
                            break;
                        }
                        if stack.is_empty() {
                            let npop = if ntos < n2 { ntos } else { n2 };
                            if npop == 1 {
                                self.off += 1;
                                return Some(self.start(Tag::Emphasis, i, i + 1));
                            } else {
                                self.off += 2;
                                let next = i + npop;
                                return Some(self.start(Tag::Strong, next - 2, next));
                            }
                        } else {
                            i += ntos;
                            n2 -= ntos;
                        }
                    }
                } else if can_open {
                    stack.push(n2);
                }
                i += n2;
            } else if c2 == b'`' {
                let (n, beg, _) = self.scan_inline_code(&self.text[i..limit]);
                if n != 0 {
                    i += n;
                } else {
                    i += beg;
                }
            } else if c2 == b'<' {
                let n = self.scan_autolink_or_html(&self.text[i..limit]);
                if n != 0 {
                    i += n;
                } else {
                    i += 1;
                }
            } else if c2 == b'[' {
                if self.opts.contains(Options::ENABLE_FOOTNOTES) {
                    if let Some((_, n)) = self.parse_footnote(&self.text[i..limit]) {
                        i += n;
                        continue;
                    }
                }
                if let Some((_, _, _, n)) = self.parse_link(&self.text[i..limit], false) {
                    i += n;
                } else {
                    i += 1;
                }
            } else {
                i += 1;
            }
        }
        None
    }

    // # Links

    // scans a link label, example [link]
    // return value is: total bytes, start of text, end of text, max nesting
    fn scan_link_label(&self, data: &str) -> (usize, usize, usize, usize) {
        let mut i = scan_ch(data, b'[');
        if i == 0 { return (0, 0, 0, 0); }
        let text_beg = i;
        let mut max_nest = 1;
        let mut nest = 1;
        loop {
            if i >= data.len() { return (0, 0, 0, 0); }
            match data.as_bytes()[i] {
                b'\n' => {
                    let n = self.scan_whitespace_inline(&data[i..]);
                    if n == 0 { return (0, 0, 0, 0); }
                    i += n;
                }
                b'[' => {
                    nest += 1;
                    if nest == MAX_LINK_NEST { return (0, 0, 0, 0); }
                    max_nest = cmp::max(max_nest, nest);
                    i += 1;
                }
                b']' => {
                    nest -= 1;
                    if nest == 0 {
                        break;
                    }
                    i += 1;
                }
                b'\\' => i += 1,
                b'<' => {
                    let n = self.scan_autolink_or_html(&data[i..]);
                    if n != 0 {
                        i += n;
                    } else {
                        i += 1;
                    }
                }
                b'`' => {
                    let (n, beg, _) = self.scan_inline_code(&data[i..]);
                    if n != 0 {
                        i += n;
                    } else {
                        i += beg;
                    }
                }
                _ => i += 1
            }
        }
        let text_end = i;
        i += 1;  // skip closing ]
        (i, text_beg, text_end, max_nest)
    }

    fn scan_link_title(&self, data: &str) -> (usize, usize, usize) {
        let size = data.len();
        if size == 0 { return (0, 0, 0); }
        let mut i = 0;
        let titleclose = match data.as_bytes()[i] {
            b'(' => b')',
            b'\'' => b'\'',
            b'\"' => b'\"',
            _ => return (0, 0, 0)
        };
        i += 1;
        let title_beg = i;
        while i < size {
            match data.as_bytes()[i] {
                x if x == titleclose => break,
                b'\\' => i += 2,  // may be > size
                b'\n' => {
                    let n = self.scan_whitespace_inline(&data[i..]);
                    if n == 0 { return (0, 0, 0); }
                    i += n;
                }
                _ => i += 1
            }
        }
        if i >= size { return (0, 0, 0); }
        let title_end = i;
        i += 1;
        (i, title_beg, title_end)
    }

    fn char_link(&mut self) -> Option<Event<'a>> {
        self.parse_link(&self.text[self.off .. self.limit()], false).map(|(tag, beg, end, n)| {
            let off = self.off;
            self.off += beg;
            self.start(tag, off + end, off + n)
        })
    }

    // return: tag, begin, end, total size
    fn parse_link(&self, data: &'a str, recur: bool) -> Option<(Tag<'a>, usize, usize, usize)> {
        let size = data.len();

        // scan link text
        let i = scan_ch(data, b'!');
        let is_image = i == 1;
        let (n, text_beg, text_end, max_nest) = self.scan_link_label(&data[i..]);
        if n == 0 { return None; }
        let (text_beg, text_end) = (text_beg + i, text_end + i);
        if !is_image && !recur && max_nest > 1 && self.contains_link(&data[text_beg..text_end]) {
            // disallow nested links in links (but ok in images)
            return None;
        }
        let mut i = i + n;

        // scan dest
        let (dest, title, beg, end, next) = if data[i..].starts_with('(') {
            i += 1;
            i += self.scan_whitespace_inline(&data[i..]);
            if i >= size { return None; }

            let linkdest = scan_link_dest(&data[i..]);
            if linkdest.is_none() { return None; }
            let (n, raw_dest) = linkdest.unwrap();
            let dest = unescape(raw_dest);
            i += n;

            i += self.scan_whitespace_inline(&data[i..]);
            if i == size { return None; }

            // scan title
            let (n_title, title_beg, title_end) = self.scan_link_title(&data[i..]);
            let title = if n_title == 0 {
                Borrowed("")
            } else {
                let (title_beg, title_end) = (i + title_beg, i + title_end);
                i += n_title;
                // TODO: not just unescape, remove containers from newlines
                unescape(&data[title_beg..title_end])
            };
            i += self.scan_whitespace_inline(&data[i..]);
            if i == size || data.as_bytes()[i] != b')' { return None; }
            i += 1;
            (dest, title, text_beg, text_end, i)
        } else {
            // try link reference
            let j = i + self.scan_whitespace_inline(&data[i..]);
            let (n_ref, ref_beg, ref_end, _) = self.scan_link_label(&data[j..]);
            let (ref_beg, ref_end) = if n_ref == 0 || ref_beg == ref_end {
                (text_beg, text_end)
            } else {
                (j + ref_beg, j + ref_end)
            };
            if n_ref != 0 {
                i = j + n_ref;
            }
            let reference = self.normalize_link_ref(&data[ref_beg..ref_end]);
            let (dest, title) = match self.links.get(&reference) {
                Some(&(ref dest, ref title)) => (dest.clone(), title.clone()),
                None => {
                    if let Some(ref callback) = self.broken_link_callback {
                        if let Some(val) = callback(&reference, &data[ref_beg..ref_end]) {
                            (val.0.into(), val.1.into())
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
            };
            (dest, title, text_beg, text_end, i)
        };
        if is_image {
            Some((Tag::Image(dest, title), beg, end, next))
        } else {
            Some((Tag::Link(dest, title), beg, end, next))
        }
    }

    // determine whether there's a link anywhere in the text
    // TODO: code duplication with scan_link_label is unpleasant
    fn contains_link(&self, data: &str) -> bool {
        let mut i = 0;
        while i < data.len() {
            match data.as_bytes()[i] {
                b'\n' => {
                    let n = self.scan_whitespace_inline(&data[i..]);
                    if n == 0 { return false; }
                    i += n;
                    continue;
                }
                b'!' => {
                    if scan_ch(&data[i + 1 ..], b'[') != 0 {
                        // ok to contain image, skip over opening bracket
                        i += 1;
                    }
                }
                b'[' => {
                    if self.opts.contains(Options::ENABLE_FOOTNOTES) && self.parse_footnote(&data[i..]).is_some() {
                        return false;
                    }
                    if self.parse_link(&data[i..], true).is_some() { return true; }
                }
                b'\\' => i += 1,
                b'<' => {
                    let n = self.scan_autolink_or_html(&data[i..]);
                    if n != 0 {
                        i += n;
                    } else {
                        i += 1;
                    }
                }
                b'`' => {
                    let (n, beg, _) = self.scan_inline_code(&data[i..]);
                    if n != 0 {
                        i += n;
                    } else {
                        i += beg;
                    }
                }
                _ => ()
            }
            i += 1;
        }
        false
    }

    // # Footnotes

    fn parse_footnote_definition<'b>(&self, data: &'b str) -> Option<(&'b str, usize)> {
        assert!(self.opts.contains(Options::ENABLE_FOOTNOTES));
        self.parse_footnote(data).and_then(|(name, len)| {
            let n_colon = scan_ch(&data[len ..], b':');
            if n_colon == 0 {
                None
            } else {
                let space = scan_whitespace_no_nl(&data[len + n_colon..]);
                // skip newline if definition is on a line by itself, as likely that
                // means the footnote definition is a complex block.
                let mut i = len + n_colon + space;
                if let (n, true) = scan_eol(&data[i..]) {
                    let (n_containers, _, _) = self.scan_containers(&data[i + n ..]);
                    i += n + n_containers;
                }
                Some((name, i))
            }
        })
    }

    fn char_link_footnote(&mut self) -> Option<Event<'a>> {
        assert!(self.opts.contains(Options::ENABLE_FOOTNOTES));
        if let Some((name, end)) = self.parse_footnote(&self.text[self.off .. self.limit()]) {
            self.off += end;
            Some(Event::FootnoteReference(Cow::Borrowed(name)))
        } else {
            self.char_link()
        }
    }

    fn parse_footnote<'b>(&self, data: &'b str) -> Option<(&'b str, usize)> {
        assert!(self.opts.contains(Options::ENABLE_FOOTNOTES));
        let (n_footnote, text_beg, text_end) = self.scan_footnote_label(data);
        if n_footnote == 0 { return None; }
        Some((&data[text_beg..text_end], n_footnote))
    }

    fn scan_footnote_label(&self, data: &str) -> (usize, usize, usize) {
        assert!(self.opts.contains(Options::ENABLE_FOOTNOTES));
        let mut i = scan_ch(data, b'[');
        if i == 0 { return (0, 0, 0); }
        if i >= data.len() || data.as_bytes()[i] != b'^' { return (0, 0, 0); }
        i += 1;
        let text_beg = i;
        loop {
            if i >= data.len() { return (0, 0, 0); }
            match data.as_bytes()[i] {
                b'\n' => {
                    let n = self.scan_whitespace_inline(&data[i..]);
                    if n == 0 { return (0, 0, 0); }
                    i += n;
                    continue;
                }
                b']' => break,
                b'\\' => i += 1,
                _ => ()
            }
            i += 1;
        }
        let text_end = i;
        i += 1;  // skip closing ]
        (i, text_beg, text_end)
    }

    // # Autolinks and inline HTML

    fn char_lt(&mut self) -> Option<Event<'a>> {
        let tail = &self.text[self.off .. self.limit()];
        if let Some((n, link)) = scan_autolink(tail) {
            let next = self.off + n;
            self.off += 1;
            self.state = State::Literal;
            return Some(self.start(Tag::Link(link, Borrowed("")), next - 1, next))
        }
        let n = self.scan_inline_html(tail);
        if n != 0 {
            return Some(self.inline_html_event(n))
        }
        None
    }

    fn scan_autolink_or_html(&self, data: &str) -> usize {
        if let Some((n, _)) = scan_autolink(data) {
            n
        } else {
            self.scan_inline_html(data)
        }
    }

    fn scan_inline_html(&self, data: &str) -> usize {
        let n = self.scan_html_tag(data);
        if n != 0 { return n; }
        let n = self.scan_html_comment(data);
        if n != 0 { return n; }
        let n = self.scan_processing_instruction(data);
        if n != 0 { return n; }
        let n = self.scan_declaration(data);
        if n != 0 { return n; }
        let n = self.scan_cdata(data);
        if n != 0 { return n; }
        0
    }

    fn scan_html_tag(&self, data: &str) -> usize {
        let size = data.len();
        let mut i = 0;
        if scan_ch(data, b'<') == 0 { return 0; }
        i += 1;
        let n_slash = scan_ch(&data[i..], b'/');
        i += n_slash;
        if i == size || !is_ascii_alpha(data.as_bytes()[i]) { return 0; }
        i += 1;
        i += scan_while(&data[i..], is_ascii_alphanumeric);
        if n_slash == 0 {
            loop {
                let n = self.scan_whitespace_inline(&data[i..]);
                if n == 0 { break; }
                i += n;
                let n = scan_attribute_name(&data[i..]);
                if n == 0 { break; }
                i += n;
                let n = self.scan_whitespace_inline(&data[i..]);
                if scan_ch(&data[i + n ..], b'=') != 0 {
                    i += n + 1;
                    i += self.scan_whitespace_inline(&data[i..]);
                    let n_attr = self.scan_attribute_value(&data[i..]);
                    if n_attr == 0 { return 0; }
                    i += n_attr;
                }
            }
            i += self.scan_whitespace_inline(&data[i..]);
            i += scan_ch(&data[i..], b'/');
        } else {
            i += self.scan_whitespace_inline(&data[i..]);
        }
        if scan_ch(&data[i..], b'>') == 0 { return 0; }
        i += 1;
        i
    }

    fn scan_attribute_value(&self, data: &str) -> usize {
        let size = data.len();
        if size == 0 { return 0; }
        let open = data.as_bytes()[0];
        let quoted = open == b'\'' || open == b'"';
        let mut i = if quoted { 1 } else { 0 };
        while i < size {
            let c = data.as_bytes()[i];
            match c {
                b'\n' => {
                    if !quoted { break; }
                    let n = self.scan_whitespace_inline(&data[i..]);
                    if n == 0 { return 0; }
                    i += n;
                }
                b'\'' | b'"' | b'=' | b'<' | b'>' | b'`' | b'\t' ... b' ' => {
                    if !quoted || c == open { break; }
                    i += 1;
                }
                _ => i += 1
            }
        }
        if quoted {
            if i == size || data.as_bytes()[i] != open { return 0; }
            i += 1;
        }
        i
    }

    fn scan_html_comment(&self, data: &str) -> usize {
        if !data.starts_with("<!--") { return 0; }
        if let Some(n) = data[4..].find("--") {
            let text = &data[4..4 + n];
            if !text.starts_with('>') && !text.starts_with("->") &&
                    data[n + 6 ..].starts_with('>') {
                return n + 7;
            }
        }
        0
    }

    fn scan_processing_instruction(&self, data: &str) -> usize {
        if !data.starts_with("<?") { return 0; }
        if let Some(n) = data[2..].find("?>") {
            return n + 4;
        }
        0
    }

    fn scan_declaration(&self, data: &str) -> usize {
        if !data.starts_with("<!") { return 0; }
        let n = scan_while(&data[2..], is_ascii_upper);
        if n == 0 { return 0; }
        let i = n + 2;
        let n = self.scan_whitespace_inline(&data[i..]);
        if n == 0 { return 0; }
        let mut i = i + n;
        while i < data.len() {
            match data.as_bytes()[i] {
                b'>' => return i + 1,
                b'\n' => {
                    let n = self.scan_whitespace_inline(&data[i..]);
                    if n == 0 { break; }
                    i += n;
                }
                _ => i += 1
            }
        }
        0
    }

    fn scan_cdata(&self, data: &str) -> usize {
        if !data.starts_with("<![CDATA[") { return 0; }
        if let Some(n) = data[9..].find("]]>") {
            return n + 12;
        }
        0
    }

    fn inline_html_event(&mut self, n: usize) -> Event<'a> {
        let data = &self.text[self.off .. self.off + n];
        let size = data.len();
        let mut out = Borrowed("");
        let mut i = 0;
        let mut mark = 0;
        while i < size {
            let n = scan_nextline(&data[i..]);
            i += n;
            if n >= 2 && data.as_bytes()[i - 2] == b'\r' {
                out = utils::cow_append(out, Borrowed(&data[mark .. i - 2]));
                mark = i - 1;
            }
            if i < size {
                let (n, _, _) = self.scan_containers(&data[i..]);
                if n != 0 {
                    out = utils::cow_append(out, Borrowed(&data[mark..i]));
                    mark = i + n;
                }
            }
        }
        out = utils::cow_append(out, Borrowed(&data[mark..n]));
        self.off += n;
        Event::InlineHtml(out)
    }

    // link text is literal, with no processing of markup
    fn next_literal(&mut self) -> Event<'a> {
        self.state = State::Inline;
        let beg = self.off;
        let end = self.limit();
        self.off = end;
        Event::Text(Borrowed(&self.text[beg..end]))
    }

    // second return value is number of backticks even if not closed
    fn scan_inline_code(&self, data: &str) -> (usize, usize, usize) {
        let size = data.len();
        let backtick_len = scan_backticks(data);
        let mut i = backtick_len;
        while i < size {
            match data.as_bytes()[i] {
                b'`' => {
                    let close_len = scan_backticks(&data[i..]);
                    if close_len == backtick_len {
                        return (i + backtick_len, backtick_len, i);
                    } else {
                        i += close_len;
                    }
                }
                b'\n' => {
                    i += 1;
                    let (n, _, space) = self.scan_containers(&data[i..]);
                    i += n;
                    if self.is_inline_block_end(&data[i..], space) {
                        return (0, backtick_len, 0);
                    }
                }
                // TODO: '<'
                _ => i += 1
            }
        }
        (0, backtick_len, 0)
    }

    fn char_backtick(&mut self) -> Option<Event<'a>> {
        let beg = self.off;
        let limit = self.limit();
        let mut i = beg;
        let (n, code_beg, code_end) = self.scan_inline_code(&self.text[i..limit]);
        if n == 0 {
            self.off += code_beg - 1;
            return None;
        }
        i += code_beg;
        let end = beg + code_end;
        let next = beg + n;
        i += self.scan_whitespace_inline(&self.text[i..limit]);
        self.off = i;
        self.state = State::InlineCode;
        Some(self.start(Tag::Code, end, next))
    }

    fn next_inline_code(&mut self) -> Event<'a> {
        let beg = self.off;
        let mut i = beg;
        let limit = self.limit();
        while i < limit {
            let c = self.text.as_bytes()[i];
            if is_ascii_whitespace(c) {
                let n = self.scan_whitespace_inline(&self.text[i..limit]);
                if i + n == limit || n == 0 {
                    if i > beg {
                        break;
                    } else {
                        return self.end();
                    }
                }
                if c == b' ' && n == 1 {
                    // optimization to reduce number of text blocks produced
                    i += 1;
                } else {
                    if i > beg {
                        break;
                    }
                    i += n;
                    self.off = i;
                    return Event::Text(Borrowed(" "));
                }
            } else {
                i += 1;
            }
        }
        if i > beg {
            self.off = i;
            Event::Text(Borrowed(&self.text[beg..i]))
        } else {
            self.end()
        }
    }
}

impl<'a> Iterator for RawParser<'a> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Event<'a>> {
        //println!("off {} {:?}, stack {:?} containers {:?}",
        //        self.off, self.state, self.stack, self.containers);
        if self.off < self.text.len() {
            match self.state {
                State::StartBlock | State::InContainers => {
                    let ret = self.start_block();
                    if ret.is_some() {
                        return ret;
                    }
                }
                State::Inline => return Some(self.next_inline()),
                State::TableHead(_, _) => return Some(self.start_table_head()),
                State::TableBody => return Some(self.start_table_body()),
                State::TableRow => return Some(self.next_table_cell()),
                State::CodeLineStart => return Some(self.next_code_line_start()),
                State::Code => return Some(self.next_code()),
                State::InlineCode => return Some(self.next_inline_code()),
                State::Literal => return Some(self.next_literal()),
            }
        }
        match self.stack.pop() {
            Some((tag, _, _)) => Some(Event::End(tag)),
            None => None
        }
    }
}
