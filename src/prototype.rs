// Copyright 2017 Google Inc. All rights reserved.
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

//! Prototype of tree-based two pass parser.

use scanners::*;
use parse::{Event, Tag, Options};
use std::borrow::Cow;

const NIL: usize = !0;

#[derive(Debug)]
struct Node<T> {
    child: usize,
    next: usize,
    item: T,
}

/// A tree abstraction, intended for fast building as a preorder traversal.
struct Tree<T> {
    nodes: Vec<Node<T>>,
    spine: Vec<usize>, // indices of nodes on path to current node
    cur: usize,
}

impl<T> Tree<T> {
    fn new() -> Tree<T> {
        Tree {
            nodes: Vec::new(),
            spine: Vec::new(),
            cur: NIL,
        }
    }

    /// Append one item to the current position in the tree.
    fn append(&mut self, item: T) {
        let this = self.nodes.len();
        self.nodes.push(Node {
            child: NIL,
            next: NIL,
            item: item,
        });
        if self.cur != NIL {
            self.nodes[self.cur].next = this;
        } else if let Some(&parent) = self.spine.last() {
            self.nodes[parent].child = this;
        }
        self.cur = this;
    }

    /// Push down one level, so that new items become children of the current node.
    fn push(&mut self) {
        self.spine.push(self.cur);
        self.cur = NIL;
    }

    /// Pop back up a level.
    fn pop(&mut self) {
        self.cur = self.spine.pop().unwrap();
    }
}

#[derive(Debug)]
struct Item {
    start: usize,
    end: usize,
    body: ItemBody,
}

#[derive(Debug)]
enum ItemBody {
    Paragraph,
    Text,
    SoftBreak,
    Inline(usize, bool, bool),
    Emphasis,
    Strong,
    Rule,
    Header(i32), // header level
    CodeBlock,
    SynthesizeNewLine,
    Html,
    BlockQuote,
    List(usize, u8), // indent level, list character
    ListItem(usize), // indent level
}

impl Tree<Item> {
    fn append_text(&mut self, start: usize, end: usize) {
        if end > start {
            self.append(Item {
                start: start,
                end: end,
                body: ItemBody::Text,
            });
        }
    }

    fn append_html_line(&mut self, start: usize, end: usize) {
        if end >= start {
            self.append(Item {
                start: start,
                end: end,
                body: ItemBody::Html,
            });
            self.append(Item {
                start: end,
                end: end,
                body: ItemBody::SynthesizeNewLine,
            });
        }
    }
}

#[allow(dead_code)]
fn dump_tree(nodes: &Vec<Node<Item>>, mut ix: usize, level: usize) {
    while ix != NIL {
        let node = &nodes[ix];
        for _ in 0..level {
            print!("  ");
        }
        println!("{}: {:?} {} {}", ix, node.item.body, node.item.start, node.item.end);
        dump_tree(nodes, node.child, level + 1);
        ix = node.next;
    }
}



// Return: number of bytes parsed
fn parse_line(tree: &mut Tree<Item>, s: &str, mut ix: usize) -> usize {
    let start = ix;
    let mut begin_text = start;
    while ix < s.len() {
        match s.as_bytes()[ix] {
            b'\n' | b'\r' => {
                tree.append_text(begin_text, ix);
                return ix - start;
            }
            c @ b'*' | c @b'_' => {
                tree.append_text(begin_text, ix);
                let mut count = 1;
                while ix + count < s.len() && s.as_bytes()[ix + count] == c {
                    count += 1;
                }
                let can_open = ix + count < s.len() && !is_ascii_whitespace(s.as_bytes()[ix + count]);
                let can_close = ix > start && !is_ascii_whitespace(s.as_bytes()[ix - 1]);
                // TODO: can skip if neither can_open nor can_close
                for i in 0..count {
                    tree.append(Item {
                        start: ix + i,
                        end: ix + i + 1,
                        body: ItemBody::Inline(count - i, can_open, can_close),
                    });
                }
                ix += count;
                begin_text = ix;
            }
            _ => ix += 1,
        }
    }
    // need to close text at eof
    tree.append_text(begin_text, ix);
    ix - start
}

fn parse_indented_code_block(tree: &mut Tree<Item>, s: &str, mut ix: usize) -> usize {
    let codeblock_parent = tree.cur;
    tree.append(Item {
            start: ix,
            end: 0, // set later
            body: ItemBody::CodeBlock
        });
    let codeblock_node = tree.cur;
    tree.push();
    let mut last_chunk = NIL;

    // skip leading blanklines
    while ix < s.len() {
        if let Some(bl_size) = scan_blank_line(&s[ix..]) {
            ix += bl_size;
        } else { break; }
    }

    while ix < s.len() {
        if let Some(codeline_start_offset) = scan_code_line(&s[ix..]) {

            let codeline_end_offset = scan_line_ending(&s[ix..]);
            tree.append_text(codeline_start_offset + ix, codeline_end_offset + ix);

            tree.append(Item {
                    start: codeline_end_offset,
                    end: codeline_end_offset,
                    body: ItemBody::SynthesizeNewLine,
            });

            if scan_blank_line(&s[ix..]).is_none() {
                last_chunk = tree.cur;
            }
            ix += codeline_end_offset;
            ix += scan_eol(&s[ix..]).0;
        } else { // we've hit a non-indented line
            break;
        }
    }
    if last_chunk != NIL {
        // everything after last_chunk is blank,
        // and should be detached
        tree.nodes[last_chunk].next = NIL;
    } else {
        // if we never saw a nonblank chunk then
        // this isn't a valid codeblock, so detach it
        if codeblock_parent != NIL { tree.nodes[codeblock_parent].child = NIL; }
        // pop to the codeblock, next pop will rise
        // to the parent, leaving detached codeblock behind
        tree.pop();
    }
    tree.pop();
    tree.nodes[codeblock_node].item.end = ix;
    ix
}

fn parse_atx_header(mut tree: &mut Tree<Item>, s: &str, mut ix: usize,
    atx_level: i32, atx_size: usize) -> usize {
    
    tree.append(Item {
        start: ix,
        end: 0, // set later
        body: ItemBody::Header(atx_level),
    });
    ix += atx_size;
    // next char is space or scan_eol
    // (guaranteed by scan_atx_header)
    let b = s.as_bytes()[ix];
    if b == b'\n' || b == b'\r' {
        ix += scan_eol(&s[ix..]).0;
        return ix;
    }
    // skip leading spaces
    let skip_spaces = scan_ch_repeat(&s[ix..], b' ');
    ix += skip_spaces;

    // now handle the header text
    let header_start = ix;
    let header_node_idx = tree.cur; // so that we can set the endpoint later
    tree.push();
    let header_text_size = parse_line(&mut tree, s, ix);
    ix += header_text_size;
    tree.nodes[header_node_idx].item.end = ix;


    // remove trailing matter from header text
    let header_text = &s[header_start..];
    let mut limit = ix - header_start;
    while limit > 0 && header_text.as_bytes()[limit-1] == b' ' {
        limit -= 1;
    }
    let mut closer = limit;
    while closer > 0 && header_text.as_bytes()[closer-1] == b'#' {
        closer -= 1;
    }
    if closer > 0 && header_text.as_bytes()[closer-1] == b' ' {
        limit = closer;
        while limit > 0 && header_text.as_bytes()[limit-1] == b' ' {
            limit -= 1;
        }
    } else if closer == 0 { limit = closer; }
    if tree.cur != NIL {
        tree.nodes[tree.cur].item.end = limit + header_start;
    }

    tree.pop();
    ix += scan_eol(&s[ix..]).0;
    ix
}

fn parse_hrule(tree: &mut Tree<Item>, hrule_size: usize, mut ix: usize) -> usize {
    tree.append(Item {
        start: ix,
        end: ix + hrule_size,
        body: ItemBody::Rule,
    });
    ix += hrule_size;
    ix
}

fn parse_code_fence_block(tree: &mut Tree<Item>, s: &str, mut ix: usize, indentation: usize) -> usize {
    tree.append(Item {
        start: ix,
        end: 0, // set later
        body: ItemBody::CodeBlock,
    });
    
    let (num_code_fence_chars, code_fence_char) = scan_code_fence(&s[ix..]);
    // TODO: parse code fence info
    ix += scan_nextline(&s[ix..]);

    let codeblock_node = tree.cur;
    tree.push();

    while ix < s.len() {
        if let Some(code_fence_end) = scan_closing_code_fence(&s[ix..], code_fence_char, num_code_fence_chars) {
            ix += code_fence_end;
            ix += scan_eol(&s[ix..]).0;
            break;
        }
        let fenced_line_start_offset = scan_fenced_code_line(&s[ix..], indentation);
        let fenced_line_end_offset = scan_line_ending(&s[ix..]);
        tree.append_text(fenced_line_start_offset + ix, fenced_line_end_offset + ix);
        tree.append(Item {
                start: fenced_line_end_offset,
                end: fenced_line_end_offset,
                body: ItemBody::SynthesizeNewLine,
        });
        ix += fenced_line_end_offset;
        ix += scan_eol(&s[ix..]).0;
    }
    tree.nodes[codeblock_node].item.end = ix;
    tree.pop();
    ix
}


fn parse_html_block_type_1_to_5(tree : &mut Tree<Item>, s : &str, mut ix : usize, html_end_tag : &'static str) -> usize {
    while ix < s.len() {
        let nextline_offset = scan_nextline(&s[ix..]);
        let htmlline_end_offset = scan_line_ending(&s[ix..]);
        tree.append_html_line(ix, ix+htmlline_end_offset);
        if (&s[ix..ix+htmlline_end_offset]).contains(html_end_tag) {
            return ix + nextline_offset;
        }
        ix += nextline_offset;
    }
    s.len()
}

fn parse_html_block_type_6(tree : &mut Tree<Item>, s : &str, mut ix : usize) -> usize {
    while ix < s.len() {
        let nextline_offset = scan_nextline(&s[ix..]);
        let htmlline_end_offset = scan_line_ending(&s[ix..]);
        tree.append_html_line(ix, ix+htmlline_end_offset);
        if let Some(_) = scan_blank_line(&s[ix+nextline_offset..]) {
            return ix + nextline_offset;
        }
        ix += nextline_offset;
    }
    s.len()
}

fn scan_paragraph_interrupt(s: &str, leading_spaces: usize) -> bool {
    leading_spaces < 4 &&
    (s.is_empty() ||
        s.as_bytes()[0] <= b' ' ||
        scan_hrule(s) > 0 ||
        scan_atx_header(s).0 > 0 ||
        scan_code_fence(s).0 > 0 ||
        get_html_end_tag(s).is_some() ||
        scan_blockquote_start(s) > 0 ||
        scan_listitem(s).0 > 0 ||
        is_html_tag(scan_html_block_tag(s).1))
}

fn parse_paragraph(mut tree : &mut Tree<Item>, s : &str, mut ix : usize) -> usize {
    tree.append(Item {
        start: ix,
        end: 0,  // will get set later
        body: ItemBody::Paragraph,
    });
    let cur = tree.cur;
    tree.push();
    let mut last_soft_break = None;
    while ix < s.len() {
        let (leading_bytes, leading_spaces) = scan_leading_space(&s[ix..], 0);
        ix += leading_bytes;

        let container_scan = scan_containers(&tree, &s[ix..]);
        if let Some(container_bytes) = container_scan {
            ix += container_bytes;
        }

       
        let (setext_bytes, setext_level) = scan_setext_header(&s[ix..]);
        // setext headers can't be lazy paragraph continuations
        if let None = container_scan {
            if setext_bytes > 0 && leading_spaces < 4 {
                break; 
            }
        }
        // setext headers can interrupt paragraphs
        // but can't be preceded by an empty line. 
        if setext_bytes > 0 && leading_spaces < 4 && tree.cur != NIL {
            ix += setext_bytes;
            tree.nodes[cur].item.body = ItemBody::Header(setext_level);
            break;
        }

        if scan_paragraph_interrupt(&s[ix..], leading_spaces) { break; }

        if let Some(pos) = last_soft_break {
            tree.append(Item {
                start: pos,
                end: pos + 1,  // TODO: handle \r\n
                body: ItemBody::SoftBreak,
            });
        }
        let n = parse_line(&mut tree, s, ix);
        ix += n;
        if let (n, true) = scan_eol(&s[ix..]) {
            last_soft_break = Some(ix);
            ix += n;  // skip newline
        }
    }
    tree.pop();
    tree.nodes[cur].item.end = ix;
    ix
}

// Scan markers and indentation for current container stack
// Scans to the first character after the container marks
// Return: bytes scanned, or None if containers are not complete
fn scan_containers(tree: &Tree<Item>, text: &str) -> Option<usize> {
    let leading_bytes = scan_leading_space(text, 0).0;
    let mut i = 0;
    for &vertebra in &(tree.spine) {
        let (space_bytes, num_spaces) = scan_leading_space(&text[i..],0);
        i += space_bytes;
        match tree.nodes[vertebra].item.body {
            ItemBody::BlockQuote => {
                if num_spaces >= 4 { return None; }
                let n = scan_blockquote_start(&text[i..]);
                if n > 0 {
                    i += n
                } else {
                    return None;
                }
            },
            ItemBody::ListItem(indent) => {
                if !(num_spaces >= indent || scan_eol(&text[i..]).1) {
                    return None;
                }
                i += indent;
            },
            _ => (),
        }
    }
    // Only move forward if any container marks were found
    if i > leading_bytes { return Some(i); }
    else {return Some(0); }
}

// Used on a new line, after scan_containers
// scans to first character after new container markers
fn parse_new_containers(mut tree: &mut Tree<Item>, s: &str, mut ix: usize) -> usize {
    let begin = ix;
    let leading_bytes = scan_leading_space(s, ix).0;
    loop {
        let (leading_bytes, leading_spaces) = scan_leading_space(s, ix);
        if leading_spaces >= 4 { break; }
        ix += leading_bytes;
        
        let blockquote_bytes = scan_blockquote_start(&s[ix..]);
        if blockquote_bytes > 0 {
            tree.append(Item {
                start: ix,
                end: ix, // TODO: set this correctly
                body: ItemBody::BlockQuote,
            });
            tree.push();
            ix += blockquote_bytes;
            continue;
        }

        let (listitem_bytes, listitem_delimiter, listitem_start, listitem_indent) = scan_listitem(&s[ix..]);
        if listitem_bytes > 0 {
            // thematic breaks take precedence over listitems
            if scan_hrule(&s[ix..]) > 0 { break; }

            tree.append(Item {
                start: ix,
                end: ix, // TODO: set this correctly
                body: ItemBody::List(listitem_indent, listitem_delimiter),
            });
            tree.push();
            tree.append(Item {
                start: ix,
                end: ix, // TODO: set this correctly
                body: ItemBody::ListItem(listitem_indent),
            });
            tree.push();
            ix += listitem_bytes;
            continue;
        }
        break;
    }
    if ix > leading_bytes + begin {
        return ix;
    } else {
        return begin;
    }
}

// Used on a new line, after scan_containers and scan_new_containers
fn parse_blocks(mut tree: &mut Tree<Item>, s: &str, mut ix: usize) -> usize {
    if ix >= s.len() { return ix; }
    let b = s.as_bytes()[ix];
    if b == b'\n' || b == b'\r' {
        // blank line
        ix += scan_eol(&s[ix..]).0;
        return ix
    } else {
        let (leading_bytes, leading_spaces) = scan_leading_space(&s[ix..], 0);
        
        if leading_spaces >= 4 {
            return parse_indented_code_block(&mut tree, s, ix);
        }

        // leading spaces are preserved in html blocks
        if let Some(html_end_tag) = get_html_end_tag(&s[ix+leading_bytes..]) {
            return parse_html_block_type_1_to_5(&mut tree, s, ix, html_end_tag);
        }

        let possible_tag = scan_html_block_tag(&s[ix+leading_bytes..]).1;
        if is_html_tag(possible_tag) {
            return parse_html_block_type_6(&mut tree, s, ix);
        }

        ix += leading_bytes;

        let (atx_size, atx_level) = scan_atx_header(&s[ix..]);
        if atx_level > 0 {
            return parse_atx_header(&mut tree, s, ix, atx_level, atx_size);
        }

        let hrule_size = scan_hrule(&s[ix..]);
        if hrule_size > 0 {
            return parse_hrule(&mut tree, hrule_size, ix);
        }

        let code_fence_size = scan_code_fence(&s[ix..]).0;
        if code_fence_size > 0 {
            ix = parse_code_fence_block(&mut tree, s, ix, leading_spaces);
            continue;
        }

        if let Some(html_end_tag) = get_html_end_tag(&s[ix..]) {
            return parse_html_block_type_1_to_5(&mut tree, s, ix, html_end_tag);
        }

        return parse_paragraph(&mut tree, s, ix);
    }
}

// Root is node 0
fn first_pass(s: &str) -> Tree<Item> {
    let mut tree = Tree::new();
    let mut ix = 0;
    while ix < s.len() {
        while let None = scan_containers(&tree, &s[ix..]) { tree.pop(); }
        ix += scan_containers(&tree, &s[ix..]).unwrap();
        ix = parse_new_containers(&mut tree, s, ix);
        ix = parse_blocks(&mut tree, s, ix);
    }
    tree
}

fn get_html_end_tag(text : &str) -> Option<&'static str> {
    static BEGIN_TAGS: &'static [&'static str; 3] = &["<script", "<pre", "<style"];
    static END_TAGS: &'static [&'static str; 3] = &["</script>", "</pre>", "</style>"];

    for (beg_tag, end_tag) in BEGIN_TAGS.iter().zip(END_TAGS.iter()) {
        if 1 + beg_tag.len() < text.len() &&
            text.starts_with(&beg_tag[..]) {
            let pos = beg_tag.len();
            let s = text.as_bytes()[pos];
            if s == b' ' || s == b'\r' || s == b'\n' || s == b'>' {
                return Some(end_tag);
            }
        }
    }
    static ST_BEGIN_TAGS: &'static [&'static str; 3] = &["<!--", "<?", "<![CDATA["];
    static ST_END_TAGS: &'static [&'static str; 3] = &["-->", "?>", "]]>"];
    for (beg_tag, end_tag) in ST_BEGIN_TAGS.iter().zip(ST_END_TAGS.iter()) {
        if 1 + beg_tag.len() < text.len() &&
           text.starts_with(&beg_tag[..]) {
            return Some(end_tag);
        }
    }
    if text.len() > 2 &&
        text.starts_with("<!") {
        let c = text[2..].chars().next().unwrap();
        if c >= 'A' && c <= 'Z' {
            return Some(">");
        }
    }
    None
}

#[derive(Copy, Clone, Debug)]
struct InlineEl {
    start: usize,  // offset of tree node
    count: usize,
    c: u8,  // b'*' or b'_'
    both: bool,  // can both open and close
}

#[derive(Debug)]
struct InlineStack {
    stack: Vec<InlineEl>,
}

impl InlineStack {
    fn new() -> InlineStack {
        InlineStack {
            stack: Vec::new(),
        }
    }

    fn pop_to(&mut self, tree: &mut Tree<Item>, new_len: usize) {
        while self.stack.len() > new_len {
            let el = self.stack.pop().unwrap();
            for i in 0..el.count {
                tree.nodes[el.start + i].item.body = ItemBody::Text;
            }
        }
    }

    fn find_match(&self, c: u8, count: usize, both: bool) -> Option<(usize, InlineEl)> {
        for (j, el) in self.stack.iter().enumerate().rev() {
            if el.c == c && !((both || el.both) && (count + el.count) % 3 == 0) {
                return Some((j, *el));
            }
        }
        None
    }

    fn push(&mut self, el: InlineEl) {
        self.stack.push(el)
    }

    fn pop(&mut self) -> Option<InlineEl> {
        self.stack.pop()
    }
}

fn handle_inline(tree: &mut Tree<Item>, s: &str) {
    let mut stack = InlineStack::new();
    let mut prev = NIL;
    let mut cur = tree.cur;
    while cur != NIL {
        if let ItemBody::Inline(mut count, can_open, can_close) = tree.nodes[cur].item.body {
            //println!("considering {}: {:?}, {:?}", cur, tree.nodes[cur].item, stack);
            let c = s.as_bytes()[tree.nodes[cur].item.start];
            let both = can_open && can_close;
            if can_close {
                while let Some((j, el)) = stack.find_match(c, count, both) {
                    // have a match!
                    tree.nodes[prev].next = NIL;
                    let match_count = ::std::cmp::min(count, el.count);
                    let mut end = cur + match_count;
                    cur = tree.nodes[end - 1].next;
                    let mut next = cur;
                    let mut start = el.start + el.count - match_count;
                    prev = start;
                    while start < el.start + el.count {
                        let (inc, ty) = if el.start + el.count - start > 1 {
                            (2, ItemBody::Strong)
                        } else {
                            (1, ItemBody::Emphasis)
                        };
                        let root = start + inc;
                        end -= inc;
                        tree.nodes[start].item.body = ty;
                        tree.nodes[start].item.end = tree.nodes[end].item.end;
                        tree.nodes[start].child = root;
                        tree.nodes[start].next = next;
                        start = root;
                        next = NIL;
                    }
                    stack.pop_to(tree, j + 1);
                    let _ = stack.pop();
                    if el.count > match_count {
                        stack.push(InlineEl {
                            start: el.start,
                            count: el.count - match_count,
                            c: el.c,
                            both: both,
                        })
                    }
                    count -= match_count;
                    if count == 0 {
                        break;
                    }
                }
            }
            if count > 0 {
                if can_open {
                    stack.push(InlineEl {
                        start: cur,
                        count: count,
                        c: c,
                        both: both,
                    });
                } else {
                    for i in 0..count {
                        tree.nodes[cur + i].item.body = ItemBody::Text;
                    }
                }
                prev = cur + count - 1;
                cur = tree.nodes[prev].next;
            }
            //println!("after inline, cur = {}, prev = {}, {:?}", cur, prev, stack);
        } else {
            prev = cur;
            cur = tree.nodes[cur].next;
        }
    }
    stack.pop_to(tree, 0);
}

pub struct Parser<'a> {
    text: &'a str,
    tree: Tree<Item>,
}

#[allow(unused_variables)]
impl<'a> Parser<'a> {
    pub fn new(text: &'a str) -> Parser<'a> {
        Parser::new_ext(text, Options::empty())
    }
    pub fn new_ext(text: &'a str, opts: Options) -> Parser<'a> {
        let mut tree = first_pass(text);
        tree.cur = if tree.nodes.is_empty() { NIL } else { 0 };
        tree.spine = vec![];
        Parser {
            text: text,
            tree: tree,
        }
    }

    pub fn get_offset(&self) -> usize {
        0  // TODO
    }
}

fn item_to_tag(item: &Item) -> Option<Tag<'static>> {
    match item.body {
        ItemBody::Paragraph => Some(Tag::Paragraph),
        ItemBody::Emphasis => Some(Tag::Emphasis),
        ItemBody::Strong => Some(Tag::Strong),
        ItemBody::Rule => Some(Tag::Rule),
        ItemBody::Header(level) => Some(Tag::Header(level)),
        ItemBody::CodeBlock => Some(Tag::CodeBlock(Cow::from(""))),
        ItemBody::BlockQuote => Some(Tag::BlockQuote),
        ItemBody::List(_, _) => Some(Tag::List(None)),
        ItemBody::ListItem(_) => Some(Tag::Item),
        _ => None,
    }
}

// leaf items only
fn item_to_event<'a>(item: &Item, text: &'a str) -> Event<'a> {
    match item.body {
        ItemBody::Text => {
            Event::Text(Cow::from(&text[item.start..item.end]))
        },
        ItemBody::SynthesizeNewLine => {
            Event::Text(Cow::from("\n"))
        },
        ItemBody::Html => {
            Event::Html(Cow::from(&text[item.start..item.end]))
        },
        ItemBody::SoftBreak => Event::SoftBreak,
        _ => panic!("unexpected item body {:?}", item.body)
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Event<'a>> {
        if self.tree.cur == NIL {
            if let Some(cur) = self.tree.spine.pop() {
                let tag = item_to_tag(&self.tree.nodes[cur].item).unwrap();
                self.tree.cur = self.tree.nodes[cur].next;
                return Some(Event::End(tag));
            } else {
                return None;
            }
        }
        if let ItemBody::Inline(..) = self.tree.nodes[self.tree.cur].item.body {
            handle_inline(&mut self.tree, self.text);
        }
        let item = &self.tree.nodes[self.tree.cur].item;
        if let Some(tag) = item_to_tag(item) {
            let child = self.tree.nodes[self.tree.cur].child;
            self.tree.spine.push(self.tree.cur);
            self.tree.cur = child;
            return Some(Event::Start(tag))
        } else {
            self.tree.cur = self.tree.nodes[self.tree.cur].next;
            return Some(item_to_event(item, self.text))
        }
    }
}
