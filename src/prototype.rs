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
    Header(i32),
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
}

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

// Root is node 0
fn first_pass(s: &str) -> Tree<Item> {
    let mut tree = Tree::new();
    let mut ix = 0;
    while ix < s.len() {
        let b = s.as_bytes()[ix];
        if b == b'\n' || b == b'\r' {
            // blank line
            ix += scan_eol(&s[ix..]).0;
        } else {
            let (mut leading_bytes, mut leading_space) = scan_leading_space(&s[ix..], 0);
            ix += leading_bytes;

            let hrule_size = scan_hrule(&s[ix..]);
            if hrule_size > 0 {
                tree.append(Item {
                    start: ix,
                    end: ix + hrule_size,
                    body: ItemBody::Rule,
                });
                ix += hrule_size;
                continue;
            }

            let (atx_size, atx_level) = scan_atx_header(&s[ix..]);
            if atx_level > 0 {
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
                    continue;
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
                while limit > 0 && header_text.as_bytes()[limit-1] == b'#' {
                    limit -= 1;
                }
                while limit > 0 && header_text.as_bytes()[limit-1] == b' ' {
                    limit -= 1;
                }
                if tree.cur != NIL {
                    tree.nodes[tree.cur].item.end = limit + header_start;
                }

                tree.pop();
                continue;
            }

            // start of paragraph
            tree.append(Item {
                start: ix,
                end: 0,  // will get set later
                body: ItemBody::Paragraph,
            });
            let cur = tree.cur;
            tree.push();
            let mut last_soft_break = None;
            while ix < s.len() {
                ix += scan_whitespace_no_nl(&s[ix..]);
                if ix == s.len() || s.as_bytes()[ix] <= b' ' {
                    // EOF or empty line
                    break;
                }
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
        }
    }
    tree
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

impl<'a> Parser<'a> {
    pub fn new(text: &'a str) -> Parser<'a> {
        Parser::new_ext(text, Options::empty())
    }
    pub fn new_ext(text: &'a str, opts: Options) -> Parser<'a> {
        let mut tree = first_pass(text);
        tree.cur = if tree.nodes.is_empty() { NIL } else { 0 };
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
        _ => None,
    }
}

// leaf items only
fn item_to_event<'a>(item: &Item, text: &'a str) -> Event<'a> {
    match item.body {
        ItemBody::Text => {
            Event::Text(Cow::from(&text[item.start..item.end]))
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
