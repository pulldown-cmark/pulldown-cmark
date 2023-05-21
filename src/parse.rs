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

//! Tree-based two pass parser.

use std::cmp::{max, min};
use std::collections::{HashMap, VecDeque};
use std::iter::FusedIterator;
use std::num::NonZeroUsize;
use std::ops::{Index, Range};

use unicase::UniCase;

use crate::firstpass::run_first_pass;
use crate::linklabel::{scan_link_label_rest, LinkLabel, ReferenceLabel};
use crate::scanners::*;
use crate::strings::CowStr;
use crate::tree::{Tree, TreeIndex};
use crate::{Alignment, CodeBlockKind, Event, HeadingLevel, LinkType, Options, Tag};

// Allowing arbitrary depth nested parentheses inside link destinations
// can create denial of service vulnerabilities if we're not careful.
// The simplest countermeasure is to limit their depth, which is
// explicitly allowed by the spec as long as the limit is at least 3:
// https://spec.commonmark.org/0.29/#link-destination
const LINK_MAX_NESTED_PARENS: usize = 5;

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct Item {
    pub start: usize,
    pub end: usize,
    pub body: ItemBody,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum ItemBody {
    Paragraph,
    Text,
    SoftBreak,
    HardBreak,

    // These are possible inline items, need to be resolved in second pass.

    // repeats, can_open, can_close
    MaybeEmphasis(usize, bool, bool),
    // quote byte, can_open, can_close
    MaybeSmartQuote(u8, bool, bool),
    MaybeCode(usize, bool), // number of backticks, preceded by backslash
    MaybeHtml,
    MaybeLinkOpen,
    // bool indicates whether or not the preceding section could be a reference
    MaybeLinkClose(bool),
    MaybeImage,

    // These are inline items after resolution.
    Emphasis,
    Strong,
    Strikethrough,
    Code(CowIndex),
    Link(LinkIndex),
    Image(LinkIndex),
    FootnoteReference(CowIndex),
    TaskListMarker(bool), // true for checked

    Rule,
    Heading(HeadingLevel, Option<HeadingIndex>), // heading level
    FencedCodeBlock(CowIndex),
    IndentCodeBlock,
    Html,
    OwnedHtml(CowIndex),
    BlockQuote,
    List(bool, u8, u64), // is_tight, list character, list start index
    ListItem(usize),     // indent level
    SynthesizeText(CowIndex),
    SynthesizeChar(char),
    FootnoteDefinition(CowIndex),

    // Tables
    Table(AlignmentIndex),
    TableHead,
    TableRow,
    TableCell,

    // Dummy node at the top of the tree - should not be used otherwise!
    Root,
}

impl<'a> ItemBody {
    fn is_inline(&self) -> bool {
        matches!(
            *self,
            ItemBody::MaybeEmphasis(..)
                | ItemBody::MaybeSmartQuote(..)
                | ItemBody::MaybeHtml
                | ItemBody::MaybeCode(..)
                | ItemBody::MaybeLinkOpen
                | ItemBody::MaybeLinkClose(..)
                | ItemBody::MaybeImage
        )
    }
}

impl<'a> Default for ItemBody {
    fn default() -> Self {
        ItemBody::Root
    }
}

#[derive(Debug)]
pub struct BrokenLink<'a> {
    pub span: std::ops::Range<usize>,
    pub link_type: LinkType,
    pub reference: CowStr<'a>,
}

/// Markdown event iterator.
pub struct Parser<'input, 'callback> {
    text: &'input str,
    options: Options,
    tree: Tree<Item>,
    allocs: Allocations<'input>,
    broken_link_callback: BrokenLinkCallback<'input, 'callback>,
    html_scan_guard: HtmlScanGuard,

    // used by inline passes. store them here for reuse
    inline_stack: InlineStack,
    link_stack: LinkStack,
}

impl<'input, 'callback> std::fmt::Debug for Parser<'input, 'callback> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Only print the fileds that have public types.
        f.debug_struct("Parser")
            .field("text", &self.text)
            .field("options", &self.options)
            .field(
                "broken_link_callback",
                &self.broken_link_callback.as_ref().map(|_| ..),
            )
            .finish()
    }
}

impl<'input, 'callback> Parser<'input, 'callback> {
    /// Creates a new event iterator for a markdown string without any options enabled.
    pub fn new(text: &'input str) -> Self {
        Parser::new_ext(text, Options::empty())
    }

    /// Creates a new event iterator for a markdown string with given options.
    pub fn new_ext(text: &'input str, options: Options) -> Self {
        Parser::new_with_broken_link_callback(text, options, None)
    }

    /// In case the parser encounters any potential links that have a broken
    /// reference (e.g `[foo]` when there is no `[foo]: ` entry at the bottom)
    /// the provided callback will be called with the reference name,
    /// and the returned pair will be used as the link name and title if it is not
    /// `None`.
    pub fn new_with_broken_link_callback(
        text: &'input str,
        options: Options,
        broken_link_callback: BrokenLinkCallback<'input, 'callback>,
    ) -> Self {
        let (mut tree, allocs) = run_first_pass(text, options);
        tree.reset();
        let inline_stack = Default::default();
        let link_stack = Default::default();
        let html_scan_guard = Default::default();
        Parser {
            text,
            options,
            tree,
            allocs,
            broken_link_callback,
            inline_stack,
            link_stack,
            html_scan_guard,
        }
    }

    /// Returns a reference to the internal `RefDefs` object, which provides access
    /// to the internal map of reference definitions.
    pub fn reference_definitions(&self) -> &RefDefs {
        &self.allocs.refdefs
    }

    /// Handle inline markup.
    ///
    /// When the parser encounters any item indicating potential inline markup, all
    /// inline markup passes are run on the remainder of the chain.
    ///
    /// Note: there's some potential for optimization here, but that's future work.
    fn handle_inline(&mut self) {
        self.handle_inline_pass1();
        self.handle_emphasis();
    }

    /// Handle inline HTML, code spans, and links.
    ///
    /// This function handles both inline HTML and code spans, because they have
    /// the same precedence. It also handles links, even though they have lower
    /// precedence, because the URL of links must not be processed.
    fn handle_inline_pass1(&mut self) {
        let mut code_delims = CodeDelims::new();
        let mut cur = self.tree.cur();
        let mut prev = None;

        let block_end = self.tree[self.tree.peek_up().unwrap()].item.end;
        let block_text = &self.text[..block_end];

        while let Some(mut cur_ix) = cur {
            match self.tree[cur_ix].item.body {
                ItemBody::MaybeHtml => {
                    let next = self.tree[cur_ix].next;
                    let autolink = if let Some(next_ix) = next {
                        scan_autolink(block_text, self.tree[next_ix].item.start)
                    } else {
                        None
                    };

                    if let Some((ix, uri, link_type)) = autolink {
                        let node = scan_nodes_to_ix(&self.tree, next, ix);
                        let text_node = self.tree.create_node(Item {
                            start: self.tree[cur_ix].item.start + 1,
                            end: ix - 1,
                            body: ItemBody::Text,
                        });
                        let link_ix = self.allocs.allocate_link(link_type, uri, "".into());
                        self.tree[cur_ix].item.body = ItemBody::Link(link_ix);
                        self.tree[cur_ix].item.end = ix;
                        self.tree[cur_ix].next = node;
                        self.tree[cur_ix].child = Some(text_node);
                        prev = cur;
                        cur = node;
                        if let Some(node_ix) = cur {
                            self.tree[node_ix].item.start = max(self.tree[node_ix].item.start, ix);
                        }
                        continue;
                    } else {
                        let inline_html = next.and_then(|next_ix| {
                            self.scan_inline_html(
                                block_text.as_bytes(),
                                self.tree[next_ix].item.start,
                            )
                        });
                        if let Some((span, ix)) = inline_html {
                            let node = scan_nodes_to_ix(&self.tree, next, ix);
                            self.tree[cur_ix].item.body = if !span.is_empty() {
                                let converted_string =
                                    String::from_utf8(span).expect("invalid utf8");
                                ItemBody::OwnedHtml(
                                    self.allocs.allocate_cow(converted_string.into()),
                                )
                            } else {
                                ItemBody::Html
                            };
                            self.tree[cur_ix].item.end = ix;
                            self.tree[cur_ix].next = node;
                            prev = cur;
                            cur = node;
                            if let Some(node_ix) = cur {
                                self.tree[node_ix].item.start =
                                    max(self.tree[node_ix].item.start, ix);
                            }
                            continue;
                        }
                    }
                    self.tree[cur_ix].item.body = ItemBody::Text;
                }
                ItemBody::MaybeCode(mut search_count, preceded_by_backslash) => {
                    if preceded_by_backslash {
                        search_count -= 1;
                        if search_count == 0 {
                            self.tree[cur_ix].item.body = ItemBody::Text;
                            prev = cur;
                            cur = self.tree[cur_ix].next;
                            continue;
                        }
                    }

                    if code_delims.is_populated() {
                        // we have previously scanned all codeblock delimiters,
                        // so we can reuse that work
                        if let Some(scan_ix) = code_delims.find(cur_ix, search_count) {
                            self.make_code_span(cur_ix, scan_ix, preceded_by_backslash);
                        } else {
                            self.tree[cur_ix].item.body = ItemBody::Text;
                        }
                    } else {
                        // we haven't previously scanned all codeblock delimiters,
                        // so walk the AST
                        let mut scan = if search_count > 0 {
                            self.tree[cur_ix].next
                        } else {
                            None
                        };
                        while let Some(scan_ix) = scan {
                            if let ItemBody::MaybeCode(delim_count, _) =
                                self.tree[scan_ix].item.body
                            {
                                if search_count == delim_count {
                                    self.make_code_span(cur_ix, scan_ix, preceded_by_backslash);
                                    code_delims.clear();
                                    break;
                                } else {
                                    code_delims.insert(delim_count, scan_ix);
                                }
                            }
                            scan = self.tree[scan_ix].next;
                        }
                        if scan == None {
                            self.tree[cur_ix].item.body = ItemBody::Text;
                        }
                    }
                }
                ItemBody::MaybeLinkOpen => {
                    self.tree[cur_ix].item.body = ItemBody::Text;
                    self.link_stack.push(LinkStackEl {
                        node: cur_ix,
                        ty: LinkStackTy::Link,
                    });
                }
                ItemBody::MaybeImage => {
                    self.tree[cur_ix].item.body = ItemBody::Text;
                    self.link_stack.push(LinkStackEl {
                        node: cur_ix,
                        ty: LinkStackTy::Image,
                    });
                }
                ItemBody::MaybeLinkClose(could_be_ref) => {
                    self.tree[cur_ix].item.body = ItemBody::Text;
                    if let Some(tos) = self.link_stack.pop() {
                        if tos.ty == LinkStackTy::Disabled {
                            continue;
                        }
                        let next = self.tree[cur_ix].next;
                        if let Some((next_ix, url, title)) =
                            self.scan_inline_link(block_text, self.tree[cur_ix].item.end, next)
                        {
                            let next_node = scan_nodes_to_ix(&self.tree, next, next_ix);
                            if let Some(prev_ix) = prev {
                                self.tree[prev_ix].next = None;
                            }
                            cur = Some(tos.node);
                            cur_ix = tos.node;
                            let link_ix = self.allocs.allocate_link(LinkType::Inline, url, title);
                            self.tree[cur_ix].item.body = if tos.ty == LinkStackTy::Image {
                                ItemBody::Image(link_ix)
                            } else {
                                ItemBody::Link(link_ix)
                            };
                            self.tree[cur_ix].child = self.tree[cur_ix].next;
                            self.tree[cur_ix].next = next_node;
                            self.tree[cur_ix].item.end = next_ix;
                            if let Some(next_node_ix) = next_node {
                                self.tree[next_node_ix].item.start =
                                    max(self.tree[next_node_ix].item.start, next_ix);
                            }

                            if tos.ty == LinkStackTy::Link {
                                self.link_stack.disable_all_links();
                            }
                        } else {
                            // ok, so its not an inline link. maybe it is a reference
                            // to a defined link?
                            let scan_result = scan_reference(
                                &self.tree,
                                block_text,
                                next,
                                self.options.contains(Options::ENABLE_FOOTNOTES),
                            );
                            let (node_after_link, link_type) = match scan_result {
                                // [label][reference]
                                RefScan::LinkLabel(_, end_ix) => {
                                    // Toggle reference viability of the last closing bracket,
                                    // so that we can skip it on future iterations in case
                                    // it fails in this one. In particular, we won't call
                                    // the broken link callback twice on one reference.
                                    let reference_close_node = if let Some(node) =
                                        scan_nodes_to_ix(&self.tree, next, end_ix - 1)
                                    {
                                        node
                                    } else {
                                        continue;
                                    };
                                    self.tree[reference_close_node].item.body =
                                        ItemBody::MaybeLinkClose(false);
                                    let next_node = self.tree[reference_close_node].next;

                                    (next_node, LinkType::Reference)
                                }
                                // [reference][]
                                RefScan::Collapsed(next_node) => {
                                    // This reference has already been tried, and it's not
                                    // valid. Skip it.
                                    if !could_be_ref {
                                        continue;
                                    }
                                    (next_node, LinkType::Collapsed)
                                }
                                // [shortcut]
                                //
                                // [shortcut]: /blah
                                RefScan::Failed => {
                                    if !could_be_ref {
                                        continue;
                                    }
                                    (next, LinkType::Shortcut)
                                }
                            };

                            // FIXME: references and labels are mixed in the naming of variables
                            // below. Disambiguate!

                            // (label, source_ix end)
                            let label: Option<(ReferenceLabel<'input>, usize)> = match scan_result {
                                RefScan::LinkLabel(l, end_ix) => {
                                    Some((ReferenceLabel::Link(l), end_ix))
                                }
                                RefScan::Collapsed(..) | RefScan::Failed => {
                                    // No label? maybe it is a shortcut reference
                                    let label_start = self.tree[tos.node].item.end - 1;
                                    let label_end = self.tree[cur_ix].item.end;
                                    scan_link_label(
                                        &self.tree,
                                        &self.text[label_start..label_end],
                                        self.options.contains(Options::ENABLE_FOOTNOTES),
                                    )
                                    .map(|(ix, label)| (label, label_start + ix))
                                    .filter(|(_, end)| *end == label_end)
                                }
                            };

                            // see if it's a footnote reference
                            if let Some((ReferenceLabel::Footnote(l), end)) = label {
                                self.tree[tos.node].next = node_after_link;
                                self.tree[tos.node].child = None;
                                self.tree[tos.node].item.body =
                                    ItemBody::FootnoteReference(self.allocs.allocate_cow(l));
                                self.tree[tos.node].item.end = end;
                                prev = Some(tos.node);
                                cur = node_after_link;
                                self.link_stack.clear();
                                continue;
                            } else if let Some((ReferenceLabel::Link(link_label), end)) = label {
                                let type_url_title = self
                                    .allocs
                                    .refdefs
                                    .get(link_label.as_ref())
                                    .map(|matching_def| {
                                        // found a matching definition!
                                        let title = matching_def
                                            .title
                                            .as_ref()
                                            .cloned()
                                            .unwrap_or_else(|| "".into());
                                        let url = matching_def.dest.clone();
                                        (link_type, url, title)
                                    })
                                    .or_else(|| {
                                        match self.broken_link_callback.as_mut() {
                                            Some(callback) => {
                                                // Construct a BrokenLink struct, which will be passed to the callback
                                                let broken_link = BrokenLink {
                                                    span: (self.tree[tos.node].item.start)..end,
                                                    link_type,
                                                    reference: link_label,
                                                };

                                                callback(broken_link).map(|(url, title)| {
                                                    (link_type.to_unknown(), url, title)
                                                })
                                            }
                                            None => None,
                                        }
                                    });

                                if let Some((def_link_type, url, title)) = type_url_title {
                                    let link_ix =
                                        self.allocs.allocate_link(def_link_type, url, title);
                                    self.tree[tos.node].item.body = if tos.ty == LinkStackTy::Image
                                    {
                                        ItemBody::Image(link_ix)
                                    } else {
                                        ItemBody::Link(link_ix)
                                    };
                                    let label_node = self.tree[tos.node].next;

                                    // lets do some tree surgery to add the link to the tree
                                    // 1st: skip the label node and close node
                                    self.tree[tos.node].next = node_after_link;

                                    // then, if it exists, add the label node as a child to the link node
                                    if label_node != cur {
                                        self.tree[tos.node].child = label_node;

                                        // finally: disconnect list of children
                                        if let Some(prev_ix) = prev {
                                            self.tree[prev_ix].next = None;
                                        }
                                    }

                                    self.tree[tos.node].item.end = end;

                                    // set up cur so next node will be node_after_link
                                    cur = Some(tos.node);
                                    cur_ix = tos.node;

                                    if tos.ty == LinkStackTy::Link {
                                        self.link_stack.disable_all_links();
                                    }
                                }
                            }
                        }
                    }
                }
                _ => (),
            }
            prev = cur;
            cur = self.tree[cur_ix].next;
        }
        self.link_stack.clear();
    }

    fn handle_emphasis(&mut self) {
        let mut prev = None;
        let mut prev_ix: TreeIndex;
        let mut cur = self.tree.cur();

        let mut single_quote_open: Option<TreeIndex> = None;
        let mut double_quote_open: bool = false;

        while let Some(mut cur_ix) = cur {
            match self.tree[cur_ix].item.body {
                ItemBody::MaybeEmphasis(mut count, can_open, can_close) => {
                    let c = self.text.as_bytes()[self.tree[cur_ix].item.start];
                    let both = can_open && can_close;
                    if can_close {
                        while let Some(el) =
                            self.inline_stack.find_match(&mut self.tree, c, count, both)
                        {
                            // have a match!
                            if let Some(prev_ix) = prev {
                                self.tree[prev_ix].next = None;
                            }
                            let match_count = min(count, el.count);
                            // start, end are tree node indices
                            let mut end = cur_ix - 1;
                            let mut start = el.start + el.count;

                            // work from the inside out
                            while start > el.start + el.count - match_count {
                                let inc = if start > el.start + el.count - match_count + 1 {
                                    2
                                } else {
                                    1
                                };
                                let ty = if c == b'~' {
                                    ItemBody::Strikethrough
                                } else if inc == 2 {
                                    ItemBody::Strong
                                } else {
                                    ItemBody::Emphasis
                                };

                                let root = start - inc;
                                end = end + inc;
                                self.tree[root].item.body = ty;
                                self.tree[root].item.end = self.tree[end].item.end;
                                self.tree[root].child = Some(start);
                                self.tree[root].next = None;
                                start = root;
                            }

                            // set next for top most emph level
                            prev_ix = el.start + el.count - match_count;
                            prev = Some(prev_ix);
                            cur = self.tree[cur_ix + match_count - 1].next;
                            self.tree[prev_ix].next = cur;

                            if el.count > match_count {
                                self.inline_stack.push(InlineEl {
                                    start: el.start,
                                    count: el.count - match_count,
                                    c: el.c,
                                    both,
                                })
                            }
                            count -= match_count;
                            if count > 0 {
                                cur_ix = cur.unwrap();
                            } else {
                                break;
                            }
                        }
                    }
                    if count > 0 {
                        if can_open {
                            self.inline_stack.push(InlineEl {
                                start: cur_ix,
                                count,
                                c,
                                both,
                            });
                        } else {
                            for i in 0..count {
                                self.tree[cur_ix + i].item.body = ItemBody::Text;
                            }
                        }
                        prev_ix = cur_ix + count - 1;
                        prev = Some(prev_ix);
                        cur = self.tree[prev_ix].next;
                    }
                }
                ItemBody::MaybeSmartQuote(c, can_open, can_close) => {
                    self.tree[cur_ix].item.body = match c {
                        b'\'' => {
                            if let (Some(open_ix), true) = (single_quote_open, can_close) {
                                self.tree[open_ix].item.body = ItemBody::SynthesizeChar('‘');
                                single_quote_open = None;
                            } else if can_open {
                                single_quote_open = Some(cur_ix);
                            }
                            ItemBody::SynthesizeChar('’')
                        }
                        _ /* double quote */ => {
                            if can_close && double_quote_open {
                                double_quote_open = false;
                                ItemBody::SynthesizeChar('”')
                            } else {
                                if can_open && !double_quote_open {
                                    double_quote_open = true;
                                }
                                ItemBody::SynthesizeChar('“')
                            }
                        }
                    };
                    prev = cur;
                    cur = self.tree[cur_ix].next;
                }
                _ => {
                    prev = cur;
                    cur = self.tree[cur_ix].next;
                }
            }
        }
        self.inline_stack.pop_all(&mut self.tree);
    }

    /// Returns next byte index, url and title.
    fn scan_inline_link(
        &self,
        underlying: &'input str,
        mut ix: usize,
        node: Option<TreeIndex>,
    ) -> Option<(usize, CowStr<'input>, CowStr<'input>)> {
        if scan_ch(&underlying.as_bytes()[ix..], b'(') == 0 {
            return None;
        }
        ix += 1;
        ix += scan_while(&underlying.as_bytes()[ix..], is_ascii_whitespace);

        let (dest_length, dest) = scan_link_dest(underlying, ix, LINK_MAX_NESTED_PARENS)?;
        let dest = unescape(dest);
        ix += dest_length;

        ix += scan_while(&underlying.as_bytes()[ix..], is_ascii_whitespace);

        let title = if let Some((bytes_scanned, t)) = self.scan_link_title(underlying, ix, node) {
            ix += bytes_scanned;
            ix += scan_while(&underlying.as_bytes()[ix..], is_ascii_whitespace);
            t
        } else {
            "".into()
        };
        if scan_ch(&underlying.as_bytes()[ix..], b')') == 0 {
            return None;
        }
        ix += 1;

        Some((ix, dest, title))
    }

    // returns (bytes scanned, title cow)
    fn scan_link_title(
        &self,
        text: &'input str,
        start_ix: usize,
        node: Option<TreeIndex>,
    ) -> Option<(usize, CowStr<'input>)> {
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

            if c == b'\n' || c == b'\r' {
                if let Some(node_ix) = scan_nodes_to_ix(&self.tree, node, i + 1) {
                    if self.tree[node_ix].item.start > i {
                        title.push_str(&text[mark..i]);
                        title.push('\n');
                        i = self.tree[node_ix].item.start;
                        mark = i;
                        continue;
                    }
                }
            }
            if c == b'&' {
                if let (n, Some(value)) = scan_entity(&bytes[i..]) {
                    title.push_str(&text[mark..i]);
                    title.push_str(&value);
                    i += n;
                    mark = i;
                    continue;
                }
            }
            if c == b'\\' && i + 1 < bytes.len() && is_ascii_punctuation(bytes[i + 1]) {
                title.push_str(&text[mark..i]);
                i += 1;
                mark = i;
            }

            i += 1;
        }

        None
    }

    /// Make a code span.
    ///
    /// Both `open` and `close` are matching MaybeCode items.
    fn make_code_span(&mut self, open: TreeIndex, close: TreeIndex, preceding_backslash: bool) {
        let first_ix = self.tree[open].next.unwrap();
        let bytes = self.text.as_bytes();
        let mut span_start = self.tree[open].item.end;
        let mut span_end = self.tree[close].item.start;
        let mut buf: Option<String> = None;

        // detect all-space sequences, since they are kept as-is as of commonmark 0.29
        if !bytes[span_start..span_end].iter().all(|&b| b == b' ') {
            let opening = matches!(bytes[span_start], b' ' | b'\r' | b'\n');
            let closing = matches!(bytes[span_end - 1], b' ' | b'\r' | b'\n');
            let drop_enclosing_whitespace = opening && closing;

            if drop_enclosing_whitespace {
                span_start += 1;
                if span_start < span_end {
                    span_end -= 1;
                }
            }

            let mut ix = first_ix;

            while ix != close {
                let next_ix = self.tree[ix].next.unwrap();
                if let ItemBody::HardBreak | ItemBody::SoftBreak = self.tree[ix].item.body {
                    if drop_enclosing_whitespace {
                        // check whether break should be ignored
                        if ix == first_ix {
                            ix = next_ix;
                            span_start = min(span_end, self.tree[ix].item.start);
                            continue;
                        } else if next_ix == close && ix > first_ix {
                            break;
                        }
                    }

                    let end = bytes[self.tree[ix].item.start..]
                        .iter()
                        .position(|&b| b == b'\r' || b == b'\n')
                        .unwrap()
                        + self.tree[ix].item.start;
                    if let Some(ref mut buf) = buf {
                        buf.push_str(&self.text[self.tree[ix].item.start..end]);
                        buf.push(' ');
                    } else {
                        let mut new_buf = String::with_capacity(span_end - span_start);
                        new_buf.push_str(&self.text[span_start..end]);
                        new_buf.push(' ');
                        buf = Some(new_buf);
                    }
                } else if let Some(ref mut buf) = buf {
                    let end = if next_ix == close {
                        span_end
                    } else {
                        self.tree[ix].item.end
                    };
                    buf.push_str(&self.text[self.tree[ix].item.start..end]);
                }
                ix = next_ix;
            }
        }

        let cow = if let Some(buf) = buf {
            buf.into()
        } else {
            self.text[span_start..span_end].into()
        };
        if preceding_backslash {
            self.tree[open].item.body = ItemBody::Text;
            self.tree[open].item.end = self.tree[open].item.start + 1;
            self.tree[open].next = Some(close);
            self.tree[close].item.body = ItemBody::Code(self.allocs.allocate_cow(cow));
            self.tree[close].item.start = self.tree[open].item.start + 1;
        } else {
            self.tree[open].item.body = ItemBody::Code(self.allocs.allocate_cow(cow));
            self.tree[open].item.end = self.tree[close].item.end;
            self.tree[open].next = self.tree[close].next;
        }
    }

    /// On success, returns a buffer containing the inline html and byte offset.
    /// When no bytes were skipped, the buffer will be empty and the html can be
    /// represented as a subslice of the input string.
    fn scan_inline_html(&mut self, bytes: &[u8], ix: usize) -> Option<(Vec<u8>, usize)> {
        let c = *bytes.get(ix)?;
        if c == b'!' {
            Some((
                vec![],
                scan_inline_html_comment(bytes, ix + 1, &mut self.html_scan_guard)?,
            ))
        } else if c == b'?' {
            Some((
                vec![],
                scan_inline_html_processing(bytes, ix + 1, &mut self.html_scan_guard)?,
            ))
        } else {
            let (span, i) = scan_html_block_inner(
                // Subtract 1 to include the < character
                &bytes[(ix - 1)..],
                Some(&|bytes| {
                    let mut line_start = LineStart::new(bytes);
                    let _ = scan_containers(&self.tree, &mut line_start);
                    line_start.bytes_scanned()
                }),
            )?;
            Some((span, i + ix - 1))
        }
    }

    /// Consumes the event iterator and produces an iterator that produces
    /// `(Event, Range)` pairs, where the `Range` value maps to the corresponding
    /// range in the markdown source.
    pub fn into_offset_iter(self) -> OffsetIter<'input, 'callback> {
        OffsetIter { inner: self }
    }
}

/// Returns number of containers scanned.
pub(crate) fn scan_containers(tree: &Tree<Item>, line_start: &mut LineStart) -> usize {
    let mut i = 0;
    for &node_ix in tree.walk_spine() {
        match tree[node_ix].item.body {
            ItemBody::BlockQuote => {
                // `scan_blockquote_marker` saves & restores internally
                if !line_start.scan_blockquote_marker() {
                    break;
                }
            }
            ItemBody::ListItem(indent) => {
                let save = line_start.clone();
                if !line_start.scan_space(indent) && !line_start.is_at_eol() {
                    *line_start = save;
                    break;
                }
            }
            _ => (),
        }
        i += 1;
    }
    i
}

impl<'a> Tree<Item> {
    pub(crate) fn append_text(&mut self, start: usize, end: usize) {
        if end > start {
            if let Some(ix) = self.cur() {
                if ItemBody::Text == self[ix].item.body && self[ix].item.end == start {
                    self[ix].item.end = end;
                    return;
                }
            }
            self.append(Item {
                start,
                end,
                body: ItemBody::Text,
            });
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct InlineEl {
    start: TreeIndex, // offset of tree node
    count: usize,
    c: u8,      // b'*' or b'_'
    both: bool, // can both open and close
}

#[derive(Debug, Clone, Default)]
struct InlineStack {
    stack: Vec<InlineEl>,
    // Lower bounds for matching indices in the stack. For example
    // a strikethrough delimiter will never match with any element
    // in the stack with index smaller than
    // `lower_bounds[InlineStack::TILDES]`.
    lower_bounds: [usize; 7],
}

impl InlineStack {
    /// These are indices into the lower bounds array.
    /// Not both refers to the property that the delimiter can not both
    /// be opener as a closer.
    const UNDERSCORE_NOT_BOTH: usize = 0;
    const ASTERISK_NOT_BOTH: usize = 1;
    const ASTERISK_BASE: usize = 2;
    const TILDES: usize = 5;
    const UNDERSCORE_BOTH: usize = 6;

    fn pop_all(&mut self, tree: &mut Tree<Item>) {
        for el in self.stack.drain(..) {
            for i in 0..el.count {
                tree[el.start + i].item.body = ItemBody::Text;
            }
        }
        self.lower_bounds = [0; 7];
    }

    fn get_lowerbound(&self, c: u8, count: usize, both: bool) -> usize {
        if c == b'_' {
            if both {
                self.lower_bounds[InlineStack::UNDERSCORE_BOTH]
            } else {
                self.lower_bounds[InlineStack::UNDERSCORE_NOT_BOTH]
            }
        } else if c == b'*' {
            let mod3_lower = self.lower_bounds[InlineStack::ASTERISK_BASE + count % 3];
            if both {
                mod3_lower
            } else {
                min(
                    mod3_lower,
                    self.lower_bounds[InlineStack::ASTERISK_NOT_BOTH],
                )
            }
        } else {
            self.lower_bounds[InlineStack::TILDES]
        }
    }

    fn set_lowerbound(&mut self, c: u8, count: usize, both: bool, new_bound: usize) {
        if c == b'_' {
            if both {
                self.lower_bounds[InlineStack::UNDERSCORE_BOTH] = new_bound;
            } else {
                self.lower_bounds[InlineStack::UNDERSCORE_NOT_BOTH] = new_bound;
            }
        } else if c == b'*' {
            self.lower_bounds[InlineStack::ASTERISK_BASE + count % 3] = new_bound;
            if !both {
                self.lower_bounds[InlineStack::ASTERISK_NOT_BOTH] = new_bound;
            }
        } else {
            self.lower_bounds[InlineStack::TILDES] = new_bound;
        }
    }

    fn find_match(
        &mut self,
        tree: &mut Tree<Item>,
        c: u8,
        count: usize,
        both: bool,
    ) -> Option<InlineEl> {
        let lowerbound = min(self.stack.len(), self.get_lowerbound(c, count, both));
        let res = self.stack[lowerbound..]
            .iter()
            .cloned()
            .enumerate()
            .rfind(|(_, el)| {
                el.c == c && (!both && !el.both || (count + el.count) % 3 != 0 || count % 3 == 0)
            });

        if let Some((matching_ix, matching_el)) = res {
            let matching_ix = matching_ix + lowerbound;
            for el in &self.stack[(matching_ix + 1)..] {
                for i in 0..el.count {
                    tree[el.start + i].item.body = ItemBody::Text;
                }
            }
            self.stack.truncate(matching_ix);
            Some(matching_el)
        } else {
            self.set_lowerbound(c, count, both, self.stack.len());
            None
        }
    }

    fn push(&mut self, el: InlineEl) {
        self.stack.push(el)
    }
}

#[derive(Debug, Clone)]
enum RefScan<'a> {
    // label, source ix of label end
    LinkLabel(CowStr<'a>, usize),
    // contains next node index
    Collapsed(Option<TreeIndex>),
    Failed,
}

/// Skips forward within a block to a node which spans (ends inclusive) the given
/// index into the source.
fn scan_nodes_to_ix(
    tree: &Tree<Item>,
    mut node: Option<TreeIndex>,
    ix: usize,
) -> Option<TreeIndex> {
    while let Some(node_ix) = node {
        if tree[node_ix].item.end <= ix {
            node = tree[node_ix].next;
        } else {
            break;
        }
    }
    node
}

/// Scans an inline link label, which cannot be interrupted.
/// Returns number of bytes (including brackets) and label on success.
fn scan_link_label<'text, 'tree>(
    tree: &'tree Tree<Item>,
    text: &'text str,
    allow_footnote_refs: bool,
) -> Option<(usize, ReferenceLabel<'text>)> {
    let bytes = &text.as_bytes();
    if bytes.len() < 2 || bytes[0] != b'[' {
        return None;
    }
    let linebreak_handler = |bytes: &[u8]| {
        let mut line_start = LineStart::new(bytes);
        let _ = scan_containers(tree, &mut line_start);
        Some(line_start.bytes_scanned())
    };
    let pair = if allow_footnote_refs && b'^' == bytes[1] {
        let (byte_index, cow) = scan_link_label_rest(&text[2..], &linebreak_handler)?;
        (byte_index + 2, ReferenceLabel::Footnote(cow))
    } else {
        let (byte_index, cow) = scan_link_label_rest(&text[1..], &linebreak_handler)?;
        (byte_index + 1, ReferenceLabel::Link(cow))
    };
    Some(pair)
}

fn scan_reference<'a, 'b>(
    tree: &'a Tree<Item>,
    text: &'b str,
    cur: Option<TreeIndex>,
    allow_footnote_refs: bool,
) -> RefScan<'b> {
    let cur_ix = match cur {
        None => return RefScan::Failed,
        Some(cur_ix) => cur_ix,
    };
    let start = tree[cur_ix].item.start;
    let tail = &text.as_bytes()[start..];

    if tail.starts_with(b"[]") {
        // TODO: this unwrap is sus and should be looked at closer
        let closing_node = tree[cur_ix].next.unwrap();
        RefScan::Collapsed(tree[closing_node].next)
    } else if let Some((ix, ReferenceLabel::Link(label))) =
        scan_link_label(tree, &text[start..], allow_footnote_refs)
    {
        RefScan::LinkLabel(label, start + ix)
    } else {
        RefScan::Failed
    }
}

#[derive(Clone, Default)]
struct LinkStack {
    inner: Vec<LinkStackEl>,
    disabled_ix: usize,
}

impl LinkStack {
    fn push(&mut self, el: LinkStackEl) {
        self.inner.push(el);
    }

    fn pop(&mut self) -> Option<LinkStackEl> {
        let el = self.inner.pop();
        self.disabled_ix = std::cmp::min(self.disabled_ix, self.inner.len());
        el
    }

    fn clear(&mut self) {
        self.inner.clear();
        self.disabled_ix = 0;
    }

    fn disable_all_links(&mut self) {
        for el in &mut self.inner[self.disabled_ix..] {
            if el.ty == LinkStackTy::Link {
                el.ty = LinkStackTy::Disabled;
            }
        }
        self.disabled_ix = self.inner.len();
    }
}

#[derive(Clone, Debug)]
struct LinkStackEl {
    node: TreeIndex,
    ty: LinkStackTy,
}

#[derive(PartialEq, Clone, Debug)]
enum LinkStackTy {
    Link,
    Image,
    Disabled,
}

/// Contains the destination URL, title and source span of a reference definition.
#[derive(Clone, Debug)]
pub struct LinkDef<'a> {
    pub dest: CowStr<'a>,
    pub title: Option<CowStr<'a>>,
    pub span: Range<usize>,
}

/// Tracks tree indices of code span delimiters of each length. It should prevent
/// quadratic scanning behaviours by providing (amortized) constant time lookups.
struct CodeDelims {
    inner: HashMap<usize, VecDeque<TreeIndex>>,
    seen_first: bool,
}

impl CodeDelims {
    fn new() -> Self {
        Self {
            inner: Default::default(),
            seen_first: false,
        }
    }

    fn insert(&mut self, count: usize, ix: TreeIndex) {
        if self.seen_first {
            self.inner
                .entry(count)
                .or_insert_with(Default::default)
                .push_back(ix);
        } else {
            // Skip the first insert, since that delimiter will always
            // be an opener and not a closer.
            self.seen_first = true;
        }
    }

    fn is_populated(&self) -> bool {
        !self.inner.is_empty()
    }

    fn find(&mut self, open_ix: TreeIndex, count: usize) -> Option<TreeIndex> {
        while let Some(ix) = self.inner.get_mut(&count)?.pop_front() {
            if ix > open_ix {
                return Some(ix);
            }
        }
        None
    }

    fn clear(&mut self) {
        self.inner.clear();
        self.seen_first = false;
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) struct LinkIndex(usize);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) struct CowIndex(usize);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) struct AlignmentIndex(usize);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) struct HeadingIndex(NonZeroUsize);

#[derive(Clone)]
pub(crate) struct Allocations<'a> {
    pub refdefs: RefDefs<'a>,
    links: Vec<(LinkType, CowStr<'a>, CowStr<'a>)>,
    cows: Vec<CowStr<'a>>,
    alignments: Vec<Vec<Alignment>>,
    headings: Vec<HeadingAttributes<'a>>,
}

/// Used by the heading attributes extension.
#[derive(Clone)]
pub(crate) struct HeadingAttributes<'a> {
    pub id: Option<&'a str>,
    pub classes: Vec<&'a str>,
}

/// Keeps track of the reference definitions defined in the document.
#[derive(Clone, Default, Debug)]
pub struct RefDefs<'input>(pub(crate) HashMap<LinkLabel<'input>, LinkDef<'input>>);

impl<'input, 'b, 's> RefDefs<'input>
where
    's: 'b,
{
    /// Performs a lookup on reference label using unicode case folding.
    pub fn get(&'s self, key: &'b str) -> Option<&'b LinkDef<'input>> {
        self.0.get(&UniCase::new(key.into()))
    }

    /// Provides an iterator over all the document's reference definitions.
    pub fn iter(&'s self) -> impl Iterator<Item = (&'s str, &'s LinkDef<'input>)> {
        self.0.iter().map(|(k, v)| (k.as_ref(), v))
    }
}

impl<'a> Allocations<'a> {
    pub fn new() -> Self {
        Self {
            refdefs: RefDefs::default(),
            links: Vec::with_capacity(128),
            cows: Vec::new(),
            alignments: Vec::new(),
            headings: Vec::new(),
        }
    }

    pub fn allocate_cow(&mut self, cow: CowStr<'a>) -> CowIndex {
        let ix = self.cows.len();
        self.cows.push(cow);
        CowIndex(ix)
    }

    pub fn allocate_link(&mut self, ty: LinkType, url: CowStr<'a>, title: CowStr<'a>) -> LinkIndex {
        let ix = self.links.len();
        self.links.push((ty, url, title));
        LinkIndex(ix)
    }

    pub fn allocate_alignment(&mut self, alignment: Vec<Alignment>) -> AlignmentIndex {
        let ix = self.alignments.len();
        self.alignments.push(alignment);
        AlignmentIndex(ix)
    }

    pub fn allocate_heading(&mut self, attrs: HeadingAttributes<'a>) -> HeadingIndex {
        let ix = self.headings.len();
        self.headings.push(attrs);
        // This won't panic. `self.headings.len()` can't be `usize::MAX` since
        // such a long Vec cannot fit in memory.
        let ix_nonzero = NonZeroUsize::new(ix.wrapping_add(1)).expect("too many headings");
        HeadingIndex(ix_nonzero)
    }
}

impl<'a> Index<CowIndex> for Allocations<'a> {
    type Output = CowStr<'a>;

    fn index(&self, ix: CowIndex) -> &Self::Output {
        self.cows.index(ix.0)
    }
}

impl<'a> Index<LinkIndex> for Allocations<'a> {
    type Output = (LinkType, CowStr<'a>, CowStr<'a>);

    fn index(&self, ix: LinkIndex) -> &Self::Output {
        self.links.index(ix.0)
    }
}

impl<'a> Index<AlignmentIndex> for Allocations<'a> {
    type Output = Vec<Alignment>;

    fn index(&self, ix: AlignmentIndex) -> &Self::Output {
        self.alignments.index(ix.0)
    }
}

impl<'a> Index<HeadingIndex> for Allocations<'a> {
    type Output = HeadingAttributes<'a>;

    fn index(&self, ix: HeadingIndex) -> &Self::Output {
        self.headings.index(ix.0.get() - 1)
    }
}

/// A struct containing information on the reachability of certain inline HTML
/// elements. In particular, for cdata elements (`<![CDATA[`), processing
/// elements (`<?`) and declarations (`<!DECLARATION`). The respectives usizes
/// represent the indices before which a scan will always fail and can hence
/// be skipped.
#[derive(Clone, Default)]
pub(crate) struct HtmlScanGuard {
    pub cdata: usize,
    pub processing: usize,
    pub declaration: usize,
}

pub type BrokenLinkCallback<'input, 'borrow> =
    Option<&'borrow mut dyn FnMut(BrokenLink<'input>) -> Option<(CowStr<'input>, CowStr<'input>)>>;

/// Markdown event and source range iterator.
///
/// Generates tuples where the first element is the markdown event and the second
/// is a the corresponding range in the source string.
///
/// Constructed from a `Parser` using its
/// [`into_offset_iter`](struct.Parser.html#method.into_offset_iter) method.
#[derive(Debug)]
pub struct OffsetIter<'a, 'b> {
    inner: Parser<'a, 'b>,
}

impl<'a, 'b> OffsetIter<'a, 'b> {
    /// Returns a reference to the internal reference definition tracker.
    pub fn reference_definitions(&self) -> &RefDefs {
        self.inner.reference_definitions()
    }
}

impl<'a, 'b> Iterator for OffsetIter<'a, 'b> {
    type Item = (Event<'a>, Range<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.tree.cur() {
            None => {
                let ix = self.inner.tree.pop()?;
                let tag = item_to_tag(&self.inner.tree[ix].item, &self.inner.allocs);
                self.inner.tree.next_sibling(ix);
                let span = self.inner.tree[ix].item.start..self.inner.tree[ix].item.end;
                debug_assert!(span.start <= span.end);
                Some((Event::End(tag), span))
            }
            Some(cur_ix) => {
                if self.inner.tree[cur_ix].item.body.is_inline() {
                    self.inner.handle_inline();
                }

                let node = self.inner.tree[cur_ix];
                let item = node.item;
                let event = item_to_event(item, self.inner.text, &self.inner.allocs);
                if let Event::Start(..) = event {
                    self.inner.tree.push();
                } else {
                    self.inner.tree.next_sibling(cur_ix);
                }
                debug_assert!(item.start <= item.end);
                Some((event, item.start..item.end))
            }
        }
    }
}

fn item_to_tag<'a>(item: &Item, allocs: &Allocations<'a>) -> Tag<'a> {
    match item.body {
        ItemBody::Paragraph => Tag::Paragraph,
        ItemBody::Emphasis => Tag::Emphasis,
        ItemBody::Strong => Tag::Strong,
        ItemBody::Strikethrough => Tag::Strikethrough,
        ItemBody::Link(link_ix) => {
            let &(ref link_type, ref url, ref title) = allocs.index(link_ix);
            Tag::Link(*link_type, url.clone(), title.clone())
        }
        ItemBody::Image(link_ix) => {
            let &(ref link_type, ref url, ref title) = allocs.index(link_ix);
            Tag::Image(*link_type, url.clone(), title.clone())
        }
        ItemBody::Heading(level, Some(heading_ix)) => {
            let HeadingAttributes { id, classes } = allocs.index(heading_ix);
            Tag::Heading(level, *id, classes.clone())
        }
        ItemBody::Heading(level, None) => Tag::Heading(level, None, Vec::new()),
        ItemBody::FencedCodeBlock(cow_ix) => {
            Tag::CodeBlock(CodeBlockKind::Fenced(allocs[cow_ix].clone()))
        }
        ItemBody::IndentCodeBlock => Tag::CodeBlock(CodeBlockKind::Indented),
        ItemBody::BlockQuote => Tag::BlockQuote,
        ItemBody::List(_, c, listitem_start) => {
            if c == b'.' || c == b')' {
                Tag::List(Some(listitem_start))
            } else {
                Tag::List(None)
            }
        }
        ItemBody::ListItem(_) => Tag::Item,
        ItemBody::TableHead => Tag::TableHead,
        ItemBody::TableCell => Tag::TableCell,
        ItemBody::TableRow => Tag::TableRow,
        ItemBody::Table(alignment_ix) => Tag::Table(allocs[alignment_ix].clone()),
        ItemBody::FootnoteDefinition(cow_ix) => Tag::FootnoteDefinition(allocs[cow_ix].clone()),
        _ => panic!("unexpected item body {:?}", item.body),
    }
}

fn item_to_event<'a>(item: Item, text: &'a str, allocs: &Allocations<'a>) -> Event<'a> {
    let tag = match item.body {
        ItemBody::Text => return Event::Text(text[item.start..item.end].into()),
        ItemBody::Code(cow_ix) => return Event::Code(allocs[cow_ix].clone()),
        ItemBody::SynthesizeText(cow_ix) => return Event::Text(allocs[cow_ix].clone()),
        ItemBody::SynthesizeChar(c) => return Event::Text(c.into()),
        ItemBody::Html => return Event::Html(text[item.start..item.end].into()),
        ItemBody::OwnedHtml(cow_ix) => return Event::Html(allocs[cow_ix].clone()),
        ItemBody::SoftBreak => return Event::SoftBreak,
        ItemBody::HardBreak => return Event::HardBreak,
        ItemBody::FootnoteReference(cow_ix) => {
            return Event::FootnoteReference(allocs[cow_ix].clone())
        }
        ItemBody::TaskListMarker(checked) => return Event::TaskListMarker(checked),
        ItemBody::Rule => return Event::Rule,

        ItemBody::Paragraph => Tag::Paragraph,
        ItemBody::Emphasis => Tag::Emphasis,
        ItemBody::Strong => Tag::Strong,
        ItemBody::Strikethrough => Tag::Strikethrough,
        ItemBody::Link(link_ix) => {
            let &(ref link_type, ref url, ref title) = allocs.index(link_ix);
            Tag::Link(*link_type, url.clone(), title.clone())
        }
        ItemBody::Image(link_ix) => {
            let &(ref link_type, ref url, ref title) = allocs.index(link_ix);
            Tag::Image(*link_type, url.clone(), title.clone())
        }
        ItemBody::Heading(level, Some(heading_ix)) => {
            let HeadingAttributes { id, classes } = allocs.index(heading_ix);
            Tag::Heading(level, *id, classes.clone())
        }
        ItemBody::Heading(level, None) => Tag::Heading(level, None, Vec::new()),
        ItemBody::FencedCodeBlock(cow_ix) => {
            Tag::CodeBlock(CodeBlockKind::Fenced(allocs[cow_ix].clone()))
        }
        ItemBody::IndentCodeBlock => Tag::CodeBlock(CodeBlockKind::Indented),
        ItemBody::BlockQuote => Tag::BlockQuote,
        ItemBody::List(_, c, listitem_start) => {
            if c == b'.' || c == b')' {
                Tag::List(Some(listitem_start))
            } else {
                Tag::List(None)
            }
        }
        ItemBody::ListItem(_) => Tag::Item,
        ItemBody::TableHead => Tag::TableHead,
        ItemBody::TableCell => Tag::TableCell,
        ItemBody::TableRow => Tag::TableRow,
        ItemBody::Table(alignment_ix) => Tag::Table(allocs[alignment_ix].clone()),
        ItemBody::FootnoteDefinition(cow_ix) => Tag::FootnoteDefinition(allocs[cow_ix].clone()),
        _ => panic!("unexpected item body {:?}", item.body),
    };

    Event::Start(tag)
}

impl<'a, 'b> Iterator for Parser<'a, 'b> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Event<'a>> {
        match self.tree.cur() {
            None => {
                let ix = self.tree.pop()?;
                let tag = item_to_tag(&self.tree[ix].item, &self.allocs);
                self.tree.next_sibling(ix);
                Some(Event::End(tag))
            }
            Some(cur_ix) => {
                if self.tree[cur_ix].item.body.is_inline() {
                    self.handle_inline();
                }

                let node = self.tree[cur_ix];
                let item = node.item;
                let event = item_to_event(item, self.text, &self.allocs);
                if let Event::Start(..) = event {
                    self.tree.push();
                } else {
                    self.tree.next_sibling(cur_ix);
                }
                Some(event)
            }
        }
    }
}

impl FusedIterator for Parser<'_, '_> {}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tree::Node;

    // TODO: move these tests to tests/html.rs?

    fn parser_with_extensions(text: &str) -> Parser<'_, 'static> {
        let mut opts = Options::empty();
        opts.insert(Options::ENABLE_TABLES);
        opts.insert(Options::ENABLE_FOOTNOTES);
        opts.insert(Options::ENABLE_STRIKETHROUGH);
        opts.insert(Options::ENABLE_TASKLISTS);

        Parser::new_ext(text, opts)
    }

    #[test]
    #[cfg(target_pointer_width = "64")]
    fn node_size() {
        let node_size = std::mem::size_of::<Node<Item>>();
        assert_eq!(48, node_size);
    }

    #[test]
    #[cfg(target_pointer_width = "64")]
    fn body_size() {
        let body_size = std::mem::size_of::<ItemBody>();
        assert_eq!(16, body_size);
    }

    #[test]
    fn single_open_fish_bracket() {
        // dont crash
        assert_eq!(3, Parser::new("<").count());
    }

    #[test]
    fn lone_hashtag() {
        // dont crash
        assert_eq!(2, Parser::new("#").count());
    }

    #[test]
    fn lots_of_backslashes() {
        // dont crash
        Parser::new("\\\\\r\r").count();
        Parser::new("\\\r\r\\.\\\\\r\r\\.\\").count();
    }

    #[test]
    fn issue_320() {
        // dont crash
        parser_with_extensions(":\r\t> |\r:\r\t> |\r").count();
    }

    #[test]
    fn issue_319() {
        // dont crash
        parser_with_extensions("|\r-]([^|\r-]([^").count();
        parser_with_extensions("|\r\r=][^|\r\r=][^car").count();
    }

    #[test]
    fn issue_303() {
        // dont crash
        parser_with_extensions("[^\r\ra]").count();
        parser_with_extensions("\r\r]Z[^\x00\r\r]Z[^\x00").count();
    }

    #[test]
    fn issue_313() {
        // dont crash
        parser_with_extensions("*]0[^\r\r*]0[^").count();
        parser_with_extensions("[^\r> `][^\r> `][^\r> `][").count();
    }

    #[test]
    fn issue_311() {
        // dont crash
        parser_with_extensions("\\\u{0d}-\u{09}\\\u{0d}-\u{09}").count();
    }

    #[test]
    fn issue_283() {
        let input = std::str::from_utf8(b"\xf0\x9b\xb2\x9f<td:^\xf0\x9b\xb2\x9f").unwrap();
        // dont crash
        parser_with_extensions(input).count();
    }

    #[test]
    fn issue_289() {
        // dont crash
        parser_with_extensions("> - \\\n> - ").count();
        parser_with_extensions("- \n\n").count();
    }

    #[test]
    fn issue_306() {
        // dont crash
        parser_with_extensions("*\r_<__*\r_<__*\r_<__*\r_<__").count();
    }

    #[test]
    fn issue_305() {
        // dont crash
        parser_with_extensions("_6**6*_*").count();
    }

    #[test]
    fn another_emphasis_panic() {
        parser_with_extensions("*__#_#__*").count();
    }

    #[test]
    fn offset_iter() {
        let event_offsets: Vec<_> = Parser::new("*hello* world")
            .into_offset_iter()
            .map(|(_ev, range)| range)
            .collect();
        let expected_offsets = vec![(0..13), (0..7), (1..6), (0..7), (7..13), (0..13)];
        assert_eq!(expected_offsets, event_offsets);
    }

    #[test]
    fn reference_link_offsets() {
        let range =
            Parser::new("# H1\n[testing][Some reference]\n\n[Some reference]: https://github.com")
                .into_offset_iter()
                .filter_map(|(ev, range)| match ev {
                    Event::Start(Tag::Link(LinkType::Reference, ..), ..) => Some(range),
                    _ => None,
                })
                .next()
                .unwrap();
        assert_eq!(5..30, range);
    }

    #[test]
    fn footnote_offsets() {
        let range = parser_with_extensions("Testing this[^1] out.\n\n[^1]: Footnote.")
            .into_offset_iter()
            .filter_map(|(ev, range)| match ev {
                Event::FootnoteReference(..) => Some(range),
                _ => None,
            })
            .next()
            .unwrap();
        assert_eq!(12..16, range);
    }

    #[test]
    fn table_offset() {
        let markdown = "a\n\nTesting|This|Outtt\n--|:--:|--:\nSome Data|Other data|asdf";
        let event_offset = parser_with_extensions(markdown)
            .into_offset_iter()
            .map(|(_ev, range)| range)
            .nth(3)
            .unwrap();
        let expected_offset = 3..59;
        assert_eq!(expected_offset, event_offset);
    }

    #[test]
    fn table_cell_span() {
        let markdown = "a|b|c\n--|--|--\na|  |c";
        let event_offset = parser_with_extensions(markdown)
            .into_offset_iter()
            .filter_map(|(ev, span)| match ev {
                Event::Start(Tag::TableCell) => Some(span),
                _ => None,
            })
            .nth(4)
            .unwrap();
        let expected_offset_start = "a|b|c\n--|--|--\na|".len();
        assert_eq!(
            expected_offset_start..(expected_offset_start + 2),
            event_offset
        );
    }

    #[test]
    fn offset_iter_issue_378() {
        let event_offsets: Vec<_> = Parser::new("a [b](c) d")
            .into_offset_iter()
            .map(|(_ev, range)| range)
            .collect();
        let expected_offsets = vec![(0..10), (0..2), (2..8), (3..4), (2..8), (8..10), (0..10)];
        assert_eq!(expected_offsets, event_offsets);
    }

    #[test]
    fn offset_iter_issue_404() {
        let event_offsets: Vec<_> = Parser::new("###\n")
            .into_offset_iter()
            .map(|(_ev, range)| range)
            .collect();
        let expected_offsets = vec![(0..4), (0..4)];
        assert_eq!(expected_offsets, event_offsets);
    }

    // FIXME: add this one regression suite
    #[test]
    fn link_def_at_eof() {
        let test_str = "[My site][world]\n\n[world]: https://vincentprouillet.com";
        let expected = "<p><a href=\"https://vincentprouillet.com\">My site</a></p>\n";

        let mut buf = String::new();
        crate::html::push_html(&mut buf, Parser::new(test_str));
        assert_eq!(expected, buf);
    }

    #[test]
    fn no_footnote_refs_without_option() {
        let test_str = "a [^a]\n\n[^a]: yolo";
        let expected = "<p>a <a href=\"yolo\">^a</a></p>\n";

        let mut buf = String::new();
        crate::html::push_html(&mut buf, Parser::new(test_str));
        assert_eq!(expected, buf);
    }

    #[test]
    fn ref_def_at_eof() {
        let test_str = "[test]:\\";
        let expected = "";

        let mut buf = String::new();
        crate::html::push_html(&mut buf, Parser::new(test_str));
        assert_eq!(expected, buf);
    }

    #[test]
    fn ref_def_cr_lf() {
        let test_str = "[a]: /u\r\n\n[a]";
        let expected = "<p><a href=\"/u\">a</a></p>\n";

        let mut buf = String::new();
        crate::html::push_html(&mut buf, Parser::new(test_str));
        assert_eq!(expected, buf);
    }

    #[test]
    fn no_dest_refdef() {
        let test_str = "[a]:";
        let expected = "<p>[a]:</p>\n";

        let mut buf = String::new();
        crate::html::push_html(&mut buf, Parser::new(test_str));
        assert_eq!(expected, buf);
    }

    #[test]
    fn broken_links_called_only_once() {
        for &(markdown, expected) in &[
            ("See also [`g()`][crate::g].", 1),
            ("See also [`g()`][crate::g][].", 1),
            ("[brokenlink1] some other node [brokenlink2]", 2),
        ] {
            let mut times_called = 0;
            let callback = &mut |_broken_link: BrokenLink| {
                times_called += 1;
                None
            };
            let parser =
                Parser::new_with_broken_link_callback(markdown, Options::empty(), Some(callback));
            for _ in parser {}
            assert_eq!(times_called, expected);
        }
    }

    #[test]
    fn simple_broken_link_callback() {
        let test_str = "This is a link w/o def: [hello][world]";
        let mut callback = |broken_link: BrokenLink| {
            assert_eq!("world", broken_link.reference.as_ref());
            assert_eq!(&test_str[broken_link.span], "[hello][world]");
            let url = "YOLO".into();
            let title = "SWAG".to_owned().into();
            Some((url, title))
        };
        let parser =
            Parser::new_with_broken_link_callback(test_str, Options::empty(), Some(&mut callback));
        let mut link_tag_count = 0;
        for (typ, url, title) in parser.filter_map(|event| match event {
            Event::Start(tag) | Event::End(tag) => match tag {
                Tag::Link(typ, url, title) => Some((typ, url, title)),
                _ => None,
            },
            _ => None,
        }) {
            link_tag_count += 1;
            assert_eq!(typ, LinkType::ReferenceUnknown);
            assert_eq!(url.as_ref(), "YOLO");
            assert_eq!(title.as_ref(), "SWAG");
        }
        assert!(link_tag_count > 0);
    }

    #[test]
    fn code_block_kind_check_fenced() {
        let parser = Parser::new("hello\n```test\ntadam\n```");
        let mut found = 0;
        for (ev, _range) in parser.into_offset_iter() {
            match ev {
                Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(syntax))) => {
                    assert_eq!(syntax.as_ref(), "test");
                    found += 1;
                }
                _ => {}
            }
        }
        assert_eq!(found, 1);
    }

    #[test]
    fn code_block_kind_check_indented() {
        let parser = Parser::new("hello\n\n    ```test\n    tadam\nhello");
        let mut found = 0;
        for (ev, _range) in parser.into_offset_iter() {
            match ev {
                Event::Start(Tag::CodeBlock(CodeBlockKind::Indented)) => {
                    found += 1;
                }
                _ => {}
            }
        }
        assert_eq!(found, 1);
    }

    #[test]
    fn ref_defs() {
        let input = r###"[a B c]: http://example.com
[another]: https://google.com

text

[final ONE]: http://wikipedia.org
"###;
        let mut parser = Parser::new(input);

        assert!(parser.reference_definitions().get("a b c").is_some());
        assert!(parser.reference_definitions().get("nope").is_none());

        if let Some(_event) = parser.next() {
            // testing keys with shorter lifetimes than parser and its input
            let s = "final one".to_owned();
            let link_def = parser.reference_definitions().get(&s).unwrap();
            let span = &input[link_def.span.clone()];
            assert_eq!(span, "[final ONE]: http://wikipedia.org");
        }
    }

    #[test]
    fn common_lifetime_patterns_allowed<'b>() {
        let temporary_str = String::from("xyz");

        // NOTE: this is a limitation of Rust, it doesn't allow putting lifetime parameters on the closure itself.
        // Hack it by attaching the lifetime to the test function instead.
        // TODO: why is the `'b` lifetime required at all? Changing it to `'_` breaks things :(
        let mut closure = |link: BrokenLink<'b>| Some(("#".into(), link.reference.into()));

        fn function<'a>(link: BrokenLink<'a>) -> Option<(CowStr<'a>, CowStr<'a>)> {
            Some(("#".into(), link.reference))
        }

        for _ in Parser::new_with_broken_link_callback(
            "static lifetime",
            Options::empty(),
            Some(&mut closure),
        ) {}
        /* This fails to compile. Because the closure can't say `for <'a> fn(BrokenLink<'a>) ->
         * CowStr<'a>` and has to use the enclosing `'b` lifetime parameter, `temporary_str` lives
         * shorter than `'b`. I think this is unlikely to occur in real life, and if it does, the
         * fix is simple: move it out to a function that allows annotating the lifetimes.
         */
        //for _ in Parser::new_with_broken_link_callback(&temporary_str, Options::empty(), Some(&mut callback)) {
        //}

        for _ in Parser::new_with_broken_link_callback(
            "static lifetime",
            Options::empty(),
            Some(&mut function),
        ) {}
        for _ in Parser::new_with_broken_link_callback(
            &temporary_str,
            Options::empty(),
            Some(&mut function),
        ) {}
    }
}
