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
use std::ops::{Index, Range};

use unicase::UniCase;

use crate::linklabel::{scan_link_label_rest, LinkLabel, ReferenceLabel};
use crate::scanners::*;
use crate::strings::CowStr;
use crate::tree::{Tree, TreeIndex};

// Allowing arbitrary depth nested parentheses inside link destinations
// can create denial of service vulnerabilities if we're not careful.
// The simplest countermeasure is to limit their depth, which is
// explicitly allowed by the spec as long as the limit is at least 3:
// https://spec.commonmark.org/0.29/#link-destination
const LINK_MAX_NESTED_PARENS: usize = 5;

/// Codeblock kind.
#[derive(Clone, Debug, PartialEq)]
pub enum CodeBlockKind<'a> {
    Indented,
    /// The value contained in the tag describes the language of the code, which may be empty.
    Fenced(CowStr<'a>),
}

impl<'a> CodeBlockKind<'a> {
    pub fn is_indented(&self) -> bool {
        match *self {
            CodeBlockKind::Indented => true,
            _ => false,
        }
    }

    pub fn is_fenced(&self) -> bool {
        match *self {
            CodeBlockKind::Fenced(_) => true,
            _ => false,
        }
    }
}

/// Tags for elements that can contain other elements.
#[derive(Clone, Debug, PartialEq)]
pub enum Tag<'a> {
    /// A paragraph of text and other inline elements.
    Paragraph,

    /// A heading. The field indicates the level of the heading.
    Heading(u32),

    BlockQuote,
    /// A code block.
    CodeBlock(CodeBlockKind<'a>),

    /// A list. If the list is ordered the field indicates the number of the first item.
    /// Contains only list items.
    List(Option<u64>), // TODO: add delim and tight for ast (not needed for html)
    /// A list item.
    Item,
    /// A footnote definition. The value contained is the footnote's label by which it can
    /// be referred to.
    FootnoteDefinition(CowStr<'a>),

    /// A table. Contains a vector describing the text-alignment for each of its columns.
    Table(Vec<Alignment>),
    /// A table header. Contains only `TableRow`s. Note that the table body starts immediately
    /// after the closure of the `TableHead` tag. There is no `TableBody` tag.
    TableHead,
    /// A table row. Is used both for header rows as body rows. Contains only `TableCell`s.
    TableRow,
    TableCell,

    // span-level tags
    Emphasis,
    Strong,
    Strikethrough,

    /// A link. The first field is the link type, the second the destination URL and the third is a title.
    Link(LinkType, CowStr<'a>, CowStr<'a>),

    /// An image. The first field is the link type, the second the destination URL and the third is a title.
    Image(LinkType, CowStr<'a>, CowStr<'a>),
}

/// Type specifier for inline links. See [the Tag::Link](enum.Tag.html#variant.Link) for more information.
#[derive(Clone, Debug, PartialEq, Copy)]
pub enum LinkType {
    /// Inline link like `[foo](bar)`
    Inline,
    /// Reference link like `[foo][bar]`
    Reference,
    /// Reference without destination in the document, but resolved by the broken_link_callback
    ReferenceUnknown,
    /// Collapsed link like `[foo][]`
    Collapsed,
    /// Collapsed link without destination in the document, but resolved by the broken_link_callback
    CollapsedUnknown,
    /// Shortcut link like `[foo]`
    Shortcut,
    /// Shortcut without destination in the document, but resolved by the broken_link_callback
    ShortcutUnknown,
    /// Autolink like `<http://foo.bar/baz>`
    Autolink,
    /// Email address in autolink like `<john@example.org>`
    Email,
}

impl LinkType {
    fn to_unknown(self) -> Self {
        match self {
            LinkType::Reference => LinkType::ReferenceUnknown,
            LinkType::Collapsed => LinkType::CollapsedUnknown,
            LinkType::Shortcut => LinkType::ShortcutUnknown,
            _ => unreachable!(),
        }
    }
}

/// Markdown events that are generated in a preorder traversal of the document
/// tree, with additional `End` events whenever all of an inner node's children
/// have been visited.
#[derive(Clone, Debug, PartialEq)]
pub enum Event<'a> {
    /// Start of a tagged element. Events that are yielded after this event
    /// and before its corresponding `End` event are inside this element.
    /// Start and end events are guaranteed to be balanced.
    Start(Tag<'a>),
    /// End of a tagged element.
    End(Tag<'a>),
    /// A text node.
    Text(CowStr<'a>),
    /// An inline code node.
    Code(CowStr<'a>),
    /// An HTML node.
    Html(CowStr<'a>),
    /// A reference to a footnote with given label, which may or may not be defined
    /// by an event with a `Tag::FootnoteDefinition` tag. Definitions and references to them may
    /// occur in any order.
    FootnoteReference(CowStr<'a>),
    /// A soft line break.
    SoftBreak,
    /// A hard line break.
    HardBreak,
    /// A horizontal ruler.
    Rule,
    /// A task list marker, rendered as a checkbox in HTML. Contains a true when it is checked.
    TaskListMarker(bool),
}

/// Table column text alignment.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Alignment {
    /// Default text alignment.
    None,
    Left,
    Center,
    Right,
}

bitflags! {
    /// Option struct containing flags for enabling extra features
    /// that are not part of the CommonMark spec.
    pub struct Options: u32 {
        const ENABLE_TABLES = 1 << 1;
        const ENABLE_FOOTNOTES = 1 << 2;
        const ENABLE_STRIKETHROUGH = 1 << 3;
        const ENABLE_TASKLISTS = 1 << 4;
        const ENABLE_SMART_PUNCTUATION = 1 << 5;
    }
}

#[derive(Debug, Default, Clone, Copy)]
struct Item {
    start: usize,
    end: usize,
    body: ItemBody,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum ItemBody {
    Paragraph,
    Text,
    SoftBreak,
    HardBreak,

    // These are possible inline items, need to be resolved in second pass.

    // repeats, can_open, can_close
    MaybeEmphasis(usize, bool, bool),
    // quote byte, can_open, can_close
    MaybeSmartQuote(u8, bool, bool),
    MaybeCode(usize, bool), // number of backticks, preceeded by backslash
    MaybeHtml,
    MaybeLinkOpen,
    // bool indicates whether or not the preceeding section could be a reference
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
    Heading(u32), // heading level
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
        match *self {
            ItemBody::MaybeEmphasis(..)
            | ItemBody::MaybeSmartQuote(..)
            | ItemBody::MaybeHtml
            | ItemBody::MaybeCode(..)
            | ItemBody::MaybeLinkOpen
            | ItemBody::MaybeLinkClose(..)
            | ItemBody::MaybeImage => true,
            _ => false,
        }
    }
}

impl<'a> Default for ItemBody {
    fn default() -> Self {
        ItemBody::Root
    }
}

/// Scanning modes for `Parser`'s `parse_line` method.
#[derive(PartialEq, Eq, Copy, Clone)]
enum TableParseMode {
    /// Inside a paragraph, scanning for table headers.
    Scan,
    /// Inside a table.
    Active,
    /// Inside a paragraph, not scanning for table headers.
    Disabled,
}

pub struct BrokenLink<'a> {
    pub span: std::ops::Range<usize>,
    pub link_type: LinkType,
    pub reference: &'a str,
}

/// State for the first parsing pass.
///
/// The first pass resolves all block structure, generating an AST. Within a block, items
/// are in a linear chain with potential inline markup identified.
struct FirstPass<'a, 'b> {
    text: &'a str,
    tree: Tree<Item>,
    begin_list_item: bool,
    last_line_blank: bool,
    allocs: Allocations<'a>,
    options: Options,
    list_nesting: usize,
    lookup_table: &'b LookupTable,
}

impl<'a, 'b> FirstPass<'a, 'b> {
    fn new(text: &'a str, options: Options, lookup_table: &'b LookupTable) -> FirstPass<'a, 'b> {
        // This is a very naive heuristic for the number of nodes
        // we'll need.
        let start_capacity = max(128, text.len() / 32);
        let tree = Tree::with_capacity(start_capacity);
        FirstPass {
            text,
            tree,
            begin_list_item: false,
            last_line_blank: false,
            allocs: Allocations::new(),
            options,
            list_nesting: 0,
            lookup_table,
        }
    }

    fn run(mut self) -> (Tree<Item>, Allocations<'a>) {
        let mut ix = 0;
        while ix < self.text.len() {
            ix = self.parse_block(ix);
        }
        for _ in 0..self.tree.spine_len() {
            self.pop(ix);
        }
        (self.tree, self.allocs)
    }

    /// Returns offset after block.
    fn parse_block(&mut self, mut start_ix: usize) -> usize {
        let bytes = self.text.as_bytes();
        let mut line_start = LineStart::new(&bytes[start_ix..]);

        let i = scan_containers(&self.tree, &mut line_start);
        for _ in i..self.tree.spine_len() {
            self.pop(start_ix);
        }

        if self.options.contains(Options::ENABLE_FOOTNOTES) {
            // finish footnote if it's still open and was preceeded by blank line
            if let Some(node_ix) = self.tree.peek_up() {
                if let ItemBody::FootnoteDefinition(..) = self.tree[node_ix].item.body {
                    if self.last_line_blank {
                        self.pop(start_ix);
                    }
                }
            }

            // Footnote definitions of the form
            // [^bar]:
            // * anything really
            let container_start = start_ix + line_start.bytes_scanned();
            if let Some(bytecount) = self.parse_footnote(container_start) {
                start_ix = container_start + bytecount;
                start_ix += scan_blank_line(&bytes[start_ix..]).unwrap_or(0);
                line_start = LineStart::new(&bytes[start_ix..]);
            }
        }

        // Process new containers
        loop {
            let container_start = start_ix + line_start.bytes_scanned();
            if let Some((ch, index, indent)) = line_start.scan_list_marker() {
                let after_marker_index = start_ix + line_start.bytes_scanned();
                self.continue_list(container_start, ch, index);
                self.tree.append(Item {
                    start: container_start,
                    end: after_marker_index, // will get updated later if item not empty
                    body: ItemBody::ListItem(indent),
                });
                self.tree.push();
                if let Some(n) = scan_blank_line(&bytes[after_marker_index..]) {
                    self.begin_list_item = true;
                    return after_marker_index + n;
                }
                if self.options.contains(Options::ENABLE_TASKLISTS) {
                    if let Some(is_checked) = line_start.scan_task_list_marker() {
                        self.tree.append(Item {
                            start: after_marker_index,
                            end: start_ix + line_start.bytes_scanned(),
                            body: ItemBody::TaskListMarker(is_checked),
                        });
                    }
                }
            } else if line_start.scan_blockquote_marker() {
                self.finish_list(start_ix);
                self.tree.append(Item {
                    start: container_start,
                    end: 0, // will get set later
                    body: ItemBody::BlockQuote,
                });
                self.tree.push();
            } else {
                break;
            }
        }

        let ix = start_ix + line_start.bytes_scanned();

        if let Some(n) = scan_blank_line(&bytes[ix..]) {
            if let Some(node_ix) = self.tree.peek_up() {
                match self.tree[node_ix].item.body {
                    ItemBody::BlockQuote => (),
                    _ => {
                        if self.begin_list_item {
                            // A list item can begin with at most one blank line.
                            self.pop(start_ix);
                        }
                        self.last_line_blank = true;
                    }
                }
            }
            return ix + n;
        }

        self.begin_list_item = false;
        self.finish_list(start_ix);

        // Save `remaining_space` here to avoid needing to backtrack `line_start` for HTML blocks
        let remaining_space = line_start.remaining_space();

        let indent = line_start.scan_space_upto(4);
        if indent == 4 {
            let ix = start_ix + line_start.bytes_scanned();
            let remaining_space = line_start.remaining_space();
            return self.parse_indented_code_block(ix, remaining_space);
        }

        let ix = start_ix + line_start.bytes_scanned();

        // HTML Blocks
        if bytes[ix] == b'<' {
            // Types 1-5 are all detected by one function and all end with the same
            // pattern
            if let Some(html_end_tag) = get_html_end_tag(&bytes[(ix + 1)..]) {
                return self.parse_html_block_type_1_to_5(ix, html_end_tag, remaining_space);
            }

            // Detect type 6
            let possible_tag = scan_html_block_tag(&bytes[(ix + 1)..]).1;
            if is_html_tag(possible_tag) {
                return self.parse_html_block_type_6_or_7(ix, remaining_space);
            }

            // Detect type 7
            if let Some(_html_bytes) = scan_html_type_7(&bytes[ix..]) {
                return self.parse_html_block_type_6_or_7(ix, remaining_space);
            }
        }

        if let Ok(n) = scan_hrule(&bytes[ix..]) {
            return self.parse_hrule(n, ix);
        }

        if let Some(atx_size) = scan_atx_heading(&bytes[ix..]) {
            return self.parse_atx_heading(ix, atx_size);
        }

        // parse refdef
        if let Some((bytecount, label, link_def)) = self.parse_refdef_total(ix) {
            self.allocs.refdefs.entry(label).or_insert(link_def);
            let ix = ix + bytecount;
            // try to read trailing whitespace or it will register as a completely blank line
            // TODO: shouldn't we do this for all block level items?
            return ix + scan_blank_line(&bytes[ix..]).unwrap_or(0);
        }

        if let Some((n, fence_ch)) = scan_code_fence(&bytes[ix..]) {
            return self.parse_fenced_code_block(ix, indent, fence_ch, n);
        }
        self.parse_paragraph(ix)
    }

    /// Returns the offset of the first line after the table.
    /// Assumptions: current focus is a table element and the table header
    /// matches the separator line (same number of columns).
    fn parse_table(&mut self, table_cols: usize, head_start: usize, body_start: usize) -> usize {
        // parse header. this shouldn't fail because we made sure the table header is ok
        let (_sep_start, thead_ix) = self.parse_table_row_inner(head_start, table_cols);
        self.tree[thead_ix].item.body = ItemBody::TableHead;

        // parse body
        let mut ix = body_start;
        while let Some((next_ix, _row_ix)) = self.parse_table_row(ix, table_cols) {
            ix = next_ix;
        }

        self.pop(ix);
        ix
    }

    /// Call this when containers are taken care of.
    /// Returns bytes scanned, row_ix
    fn parse_table_row_inner(&mut self, mut ix: usize, row_cells: usize) -> (usize, TreeIndex) {
        let bytes = self.text.as_bytes();
        let mut cells = 0;
        let mut final_cell_ix = None;

        let row_ix = self.tree.append(Item {
            start: ix,
            end: 0, // set at end of this function
            body: ItemBody::TableRow,
        });
        self.tree.push();

        loop {
            ix += scan_ch(&bytes[ix..], b'|');
            ix += scan_whitespace_no_nl(&bytes[ix..]);

            if let Some(eol_bytes) = scan_eol(&bytes[ix..]) {
                ix += eol_bytes;
                break;
            }

            let cell_ix = self.tree.append(Item {
                start: ix,
                end: ix,
                body: ItemBody::TableCell,
            });
            self.tree.push();
            let (next_ix, _brk) = self.parse_line(ix, TableParseMode::Active);
            let trailing_whitespace = scan_rev_while(&bytes[..next_ix], is_ascii_whitespace);

            if let Some(cur_ix) = self.tree.cur() {
                self.tree[cur_ix].item.end -= trailing_whitespace;
            }

            self.tree[cell_ix].item.end = next_ix - trailing_whitespace;
            self.tree.pop();

            ix = next_ix;
            cells += 1;

            if cells == row_cells {
                final_cell_ix = Some(cell_ix);
            }
        }

        // fill empty cells if needed
        // note: this is where GFM and commonmark-extra diverge. we follow
        // GFM here
        for _ in cells..row_cells {
            self.tree.append(Item {
                start: ix,
                end: ix,
                body: ItemBody::TableCell,
            });
        }

        // drop excess cells
        if let Some(cell_ix) = final_cell_ix {
            self.tree[cell_ix].next = None;
        }

        self.pop(ix);

        (ix, row_ix)
    }

    /// Returns first offset after the row and the tree index of the row.
    fn parse_table_row(&mut self, mut ix: usize, row_cells: usize) -> Option<(usize, TreeIndex)> {
        let bytes = self.text.as_bytes();
        let mut line_start = LineStart::new(&bytes[ix..]);
        let containers = scan_containers(&self.tree, &mut line_start);
        if containers != self.tree.spine_len() {
            return None;
        }
        line_start.scan_all_space();
        ix += line_start.bytes_scanned();
        if scan_paragraph_interrupt(&bytes[ix..]) {
            return None;
        }

        let (ix, row_ix) = self.parse_table_row_inner(ix, row_cells);
        Some((ix, row_ix))
    }

    /// Returns offset of line start after paragraph.
    fn parse_paragraph(&mut self, start_ix: usize) -> usize {
        let node_ix = self.tree.append(Item {
            start: start_ix,
            end: 0, // will get set later
            body: ItemBody::Paragraph,
        });
        self.tree.push();
        let bytes = self.text.as_bytes();

        let mut ix = start_ix;
        loop {
            let scan_mode = if self.options.contains(Options::ENABLE_TABLES) && ix == start_ix {
                TableParseMode::Scan
            } else {
                TableParseMode::Disabled
            };
            let (next_ix, brk) = self.parse_line(ix, scan_mode);

            // break out when we find a table
            if let Some(Item {
                body: ItemBody::Table(alignment_ix),
                ..
            }) = brk
            {
                let table_cols = self.allocs[alignment_ix].len();
                self.tree[node_ix].item.body = ItemBody::Table(alignment_ix);
                // this clears out any stuff we may have appended - but there may
                // be a cleaner way
                self.tree[node_ix].child = None;
                self.tree.pop();
                self.tree.push();
                return self.parse_table(table_cols, ix, next_ix);
            }

            ix = next_ix;
            let mut line_start = LineStart::new(&bytes[ix..]);
            let n_containers = scan_containers(&self.tree, &mut line_start);
            if !line_start.scan_space(4) {
                let ix_new = ix + line_start.bytes_scanned();
                if n_containers == self.tree.spine_len() {
                    if let Some(ix_setext) = self.parse_setext_heading(ix_new, node_ix) {
                        if let Some(Item {
                            start,
                            body: ItemBody::HardBreak,
                            ..
                        }) = brk
                        {
                            if bytes[start] == b'\\' {
                                self.tree.append_text(start, start + 1);
                            }
                        }
                        ix = ix_setext;
                        break;
                    }
                }
                // first check for non-empty lists, then for other interrupts
                let suffix = &bytes[ix_new..];
                if self.interrupt_paragraph_by_list(suffix) || scan_paragraph_interrupt(suffix) {
                    break;
                }
            }
            line_start.scan_all_space();
            if line_start.is_at_eol() {
                break;
            }
            ix = next_ix + line_start.bytes_scanned();
            if let Some(item) = brk {
                self.tree.append(item);
            }
        }

        self.pop(ix);
        ix
    }

    /// Returns end ix of setext_heading on success.
    fn parse_setext_heading(&mut self, ix: usize, node_ix: TreeIndex) -> Option<usize> {
        let bytes = self.text.as_bytes();
        let (n, level) = scan_setext_heading(&bytes[ix..])?;
        self.tree[node_ix].item.body = ItemBody::Heading(level);

        // strip trailing whitespace
        if let Some(cur_ix) = self.tree.cur() {
            self.tree[cur_ix].item.end -= scan_rev_while(
                &bytes[..self.tree[cur_ix].item.end],
                is_ascii_whitespace_no_nl,
            );
        }

        Some(ix + n)
    }

    /// Parse a line of input, appending text and items to tree.
    ///
    /// Returns: index after line and an item representing the break.
    fn parse_line(&mut self, start: usize, mode: TableParseMode) -> (usize, Option<Item>) {
        let bytes = &self.text.as_bytes();
        let mut pipes = 0;
        let mut last_pipe_ix = start;
        let mut begin_text = start;

        let (final_ix, brk) =
            iterate_special_bytes(&self.lookup_table, bytes, start, |ix, byte| {
                match byte {
                    b'\n' | b'\r' => {
                        if let TableParseMode::Active = mode {
                            return LoopInstruction::BreakAtWith(ix, None);
                        }

                        let mut i = ix;
                        let eol_bytes = scan_eol(&bytes[ix..]).unwrap();
                        if mode == TableParseMode::Scan && pipes > 0 {
                            // check if we may be parsing a table
                            let next_line_ix = ix + eol_bytes;
                            let mut line_start = LineStart::new(&bytes[next_line_ix..]);
                            if scan_containers(&self.tree, &mut line_start) == self.tree.spine_len()
                            {
                                let table_head_ix = next_line_ix + line_start.bytes_scanned();
                                let (table_head_bytes, alignment) =
                                    scan_table_head(&bytes[table_head_ix..]);

                                if table_head_bytes > 0 {
                                    // computing header count from number of pipes
                                    let header_count =
                                        count_header_cols(bytes, pipes, start, last_pipe_ix);

                                    // make sure they match the number of columns we find in separator line
                                    if alignment.len() == header_count {
                                        let alignment_ix =
                                            self.allocs.allocate_alignment(alignment);
                                        let end_ix = table_head_ix + table_head_bytes;
                                        return LoopInstruction::BreakAtWith(
                                            end_ix,
                                            Some(Item {
                                                start: i,
                                                end: end_ix, // must update later
                                                body: ItemBody::Table(alignment_ix),
                                            }),
                                        );
                                    }
                                }
                            }
                        }

                        let end_ix = ix + eol_bytes;
                        let trailing_backslashes = scan_rev_while(&bytes[..ix], |b| b == b'\\');
                        if trailing_backslashes % 2 == 1 && end_ix < self.text.len() {
                            i -= 1;
                            self.tree.append_text(begin_text, i);
                            return LoopInstruction::BreakAtWith(
                                end_ix,
                                Some(Item {
                                    start: i,
                                    end: end_ix,
                                    body: ItemBody::HardBreak,
                                }),
                            );
                        }
                        let trailing_whitespace =
                            scan_rev_while(&bytes[..ix], is_ascii_whitespace_no_nl);
                        if trailing_whitespace >= 2 {
                            i -= trailing_whitespace;
                            self.tree.append_text(begin_text, i);
                            return LoopInstruction::BreakAtWith(
                                end_ix,
                                Some(Item {
                                    start: i,
                                    end: end_ix,
                                    body: ItemBody::HardBreak,
                                }),
                            );
                        }

                        self.tree.append_text(begin_text, ix);
                        LoopInstruction::BreakAtWith(
                            end_ix,
                            Some(Item {
                                start: i,
                                end: end_ix,
                                body: ItemBody::SoftBreak,
                            }),
                        )
                    }
                    b'\\' => {
                        if ix + 1 < self.text.len() && is_ascii_punctuation(bytes[ix + 1]) {
                            self.tree.append_text(begin_text, ix);
                            if bytes[ix + 1] == b'`' {
                                let count = 1 + scan_ch_repeat(&bytes[(ix + 2)..], b'`');
                                self.tree.append(Item {
                                    start: ix + 1,
                                    end: ix + count + 1,
                                    body: ItemBody::MaybeCode(count, true),
                                });
                                begin_text = ix + 1 + count;
                                LoopInstruction::ContinueAndSkip(count)
                            } else {
                                begin_text = ix + 1;
                                LoopInstruction::ContinueAndSkip(1)
                            }
                        } else {
                            LoopInstruction::ContinueAndSkip(0)
                        }
                    }
                    c @ b'*' | c @ b'_' | c @ b'~' => {
                        let string_suffix = &self.text[ix..];
                        let count = 1 + scan_ch_repeat(&string_suffix.as_bytes()[1..], c);
                        let can_open = delim_run_can_open(self.text, string_suffix, count, ix);
                        let can_close = delim_run_can_close(self.text, string_suffix, count, ix);
                        let is_valid_seq = c != b'~' || count == 2;

                        if (can_open || can_close) && is_valid_seq {
                            self.tree.append_text(begin_text, ix);
                            for i in 0..count {
                                self.tree.append(Item {
                                    start: ix + i,
                                    end: ix + i + 1,
                                    body: ItemBody::MaybeEmphasis(count - i, can_open, can_close),
                                });
                            }
                            begin_text = ix + count;
                        }
                        LoopInstruction::ContinueAndSkip(count - 1)
                    }
                    b'`' => {
                        self.tree.append_text(begin_text, ix);
                        let count = 1 + scan_ch_repeat(&bytes[(ix + 1)..], b'`');
                        self.tree.append(Item {
                            start: ix,
                            end: ix + count,
                            body: ItemBody::MaybeCode(count, false),
                        });
                        begin_text = ix + count;
                        LoopInstruction::ContinueAndSkip(count - 1)
                    }
                    b'<' => {
                        // Note: could detect some non-HTML cases and early escape here, but not
                        // clear that's a win.
                        self.tree.append_text(begin_text, ix);
                        self.tree.append(Item {
                            start: ix,
                            end: ix + 1,
                            body: ItemBody::MaybeHtml,
                        });
                        begin_text = ix + 1;
                        LoopInstruction::ContinueAndSkip(0)
                    }
                    b'!' => {
                        if ix + 1 < self.text.len() && bytes[ix + 1] == b'[' {
                            self.tree.append_text(begin_text, ix);
                            self.tree.append(Item {
                                start: ix,
                                end: ix + 2,
                                body: ItemBody::MaybeImage,
                            });
                            begin_text = ix + 2;
                            LoopInstruction::ContinueAndSkip(1)
                        } else {
                            LoopInstruction::ContinueAndSkip(0)
                        }
                    }
                    b'[' => {
                        self.tree.append_text(begin_text, ix);
                        self.tree.append(Item {
                            start: ix,
                            end: ix + 1,
                            body: ItemBody::MaybeLinkOpen,
                        });
                        begin_text = ix + 1;
                        LoopInstruction::ContinueAndSkip(0)
                    }
                    b']' => {
                        self.tree.append_text(begin_text, ix);
                        self.tree.append(Item {
                            start: ix,
                            end: ix + 1,
                            body: ItemBody::MaybeLinkClose(true),
                        });
                        begin_text = ix + 1;
                        LoopInstruction::ContinueAndSkip(0)
                    }
                    b'&' => match scan_entity(&bytes[ix..]) {
                        (n, Some(value)) => {
                            self.tree.append_text(begin_text, ix);
                            self.tree.append(Item {
                                start: ix,
                                end: ix + n,
                                body: ItemBody::SynthesizeText(self.allocs.allocate_cow(value)),
                            });
                            begin_text = ix + n;
                            LoopInstruction::ContinueAndSkip(n - 1)
                        }
                        _ => LoopInstruction::ContinueAndSkip(0),
                    },
                    b'|' => {
                        if let TableParseMode::Active = mode {
                            LoopInstruction::BreakAtWith(ix, None)
                        } else {
                            last_pipe_ix = ix;
                            pipes += 1;
                            LoopInstruction::ContinueAndSkip(0)
                        }
                    }
                    b'.' => {
                        if ix + 2 < bytes.len() && bytes[ix + 1] == b'.' && bytes[ix + 2] == b'.' {
                            self.tree.append_text(begin_text, ix);
                            self.tree.append(Item {
                                start: ix,
                                end: ix + 3,
                                body: ItemBody::SynthesizeChar('…'),
                            });
                            begin_text = ix + 3;
                            LoopInstruction::ContinueAndSkip(2)
                        } else {
                            LoopInstruction::ContinueAndSkip(0)
                        }
                    }
                    b'-' => {
                        let count = 1 + scan_ch_repeat(&bytes[(ix + 1)..], b'-');
                        if count == 1 {
                            LoopInstruction::ContinueAndSkip(0)
                        } else {
                            let itembody = if count == 2 {
                                ItemBody::SynthesizeChar('–')
                            } else if count == 3 {
                                ItemBody::SynthesizeChar('—')
                            } else {
                                let (ems, ens) = match count % 6 {
                                    0 | 3 => (count / 3, 0),
                                    2 | 4 => (0, count / 2),
                                    1 => (count / 3 - 1, 2),
                                    _ => (count / 3, 1),
                                };
                                // – and — are 3 bytes each in utf8
                                let mut buf = String::with_capacity(3 * (ems + ens));
                                for _ in 0..ems {
                                    buf.push('—');
                                }
                                for _ in 0..ens {
                                    buf.push('–');
                                }
                                ItemBody::SynthesizeText(self.allocs.allocate_cow(buf.into()))
                            };

                            self.tree.append_text(begin_text, ix);
                            self.tree.append(Item {
                                start: ix,
                                end: ix + count,
                                body: itembody,
                            });
                            begin_text = ix + count;
                            LoopInstruction::ContinueAndSkip(count - 1)
                        }
                    }
                    c @ b'\'' | c @ b'"' => {
                        let string_suffix = &self.text[ix..];
                        let can_open = delim_run_can_open(self.text, string_suffix, 1, ix);
                        let can_close = delim_run_can_close(self.text, string_suffix, 1, ix);

                        self.tree.append_text(begin_text, ix);
                        self.tree.append(Item {
                            start: ix,
                            end: ix + 1,
                            body: ItemBody::MaybeSmartQuote(c, can_open, can_close),
                        });
                        begin_text = ix + 1;

                        LoopInstruction::ContinueAndSkip(0)
                    }
                    _ => LoopInstruction::ContinueAndSkip(0),
                }
            });

        if brk.is_none() {
            // need to close text at eof
            self.tree.append_text(begin_text, final_ix);
        }
        (final_ix, brk)
    }

    /// Check whether we should allow a paragraph interrupt by lists. Only non-empty
    /// lists are allowed.
    fn interrupt_paragraph_by_list(&self, suffix: &[u8]) -> bool {
        scan_listitem(suffix).map_or(false, |(ix, delim, index, _)| {
            self.list_nesting > 0 ||
            // we don't allow interruption by either empty lists or
            // numbered lists starting at an index other than 1
            !scan_empty_list(&suffix[ix..]) && (delim == b'*' || delim == b'-' || index == 1)
        })
    }

    /// When start_ix is at the beginning of an HTML block of type 1 to 5,
    /// this will find the end of the block, adding the block itself to the
    /// tree and also keeping track of the lines of HTML within the block.
    ///
    /// The html_end_tag is the tag that must be found on a line to end the block.
    fn parse_html_block_type_1_to_5(
        &mut self,
        start_ix: usize,
        html_end_tag: &str,
        mut remaining_space: usize,
    ) -> usize {
        let bytes = self.text.as_bytes();
        let mut ix = start_ix;
        loop {
            let line_start_ix = ix;
            ix += scan_nextline(&bytes[ix..]);
            self.append_html_line(remaining_space, line_start_ix, ix);

            let mut line_start = LineStart::new(&bytes[ix..]);
            let n_containers = scan_containers(&self.tree, &mut line_start);
            if n_containers < self.tree.spine_len() {
                break;
            }

            if (&self.text[line_start_ix..ix]).contains(html_end_tag) {
                break;
            }

            let next_line_ix = ix + line_start.bytes_scanned();
            if next_line_ix == self.text.len() {
                break;
            }
            ix = next_line_ix;
            remaining_space = line_start.remaining_space();
        }
        ix
    }

    /// When start_ix is at the beginning of an HTML block of type 6 or 7,
    /// this will consume lines until there is a blank line and keep track of
    /// the HTML within the block.
    fn parse_html_block_type_6_or_7(
        &mut self,
        start_ix: usize,
        mut remaining_space: usize,
    ) -> usize {
        let bytes = self.text.as_bytes();
        let mut ix = start_ix;
        loop {
            let line_start_ix = ix;
            ix += scan_nextline(&bytes[ix..]);
            self.append_html_line(remaining_space, line_start_ix, ix);

            let mut line_start = LineStart::new(&bytes[ix..]);
            let n_containers = scan_containers(&self.tree, &mut line_start);
            if n_containers < self.tree.spine_len() || line_start.is_at_eol() {
                break;
            }

            let next_line_ix = ix + line_start.bytes_scanned();
            if next_line_ix == self.text.len() || scan_blank_line(&bytes[next_line_ix..]).is_some()
            {
                break;
            }
            ix = next_line_ix;
            remaining_space = line_start.remaining_space();
        }
        ix
    }

    fn parse_indented_code_block(&mut self, start_ix: usize, mut remaining_space: usize) -> usize {
        self.tree.append(Item {
            start: start_ix,
            end: 0, // will get set later
            body: ItemBody::IndentCodeBlock,
        });
        self.tree.push();
        let bytes = self.text.as_bytes();
        let mut last_nonblank_child = None;
        let mut last_nonblank_ix = 0;
        let mut end_ix = 0;
        let mut last_line_blank = false;

        let mut ix = start_ix;
        loop {
            let line_start_ix = ix;
            ix += scan_nextline(&bytes[ix..]);
            self.append_code_text(remaining_space, line_start_ix, ix);
            // TODO(spec clarification): should we synthesize newline at EOF?

            if !last_line_blank {
                last_nonblank_child = self.tree.cur();
                last_nonblank_ix = ix;
                end_ix = ix;
            }

            let mut line_start = LineStart::new(&bytes[ix..]);
            let n_containers = scan_containers(&self.tree, &mut line_start);
            if n_containers < self.tree.spine_len()
                || !(line_start.scan_space(4) || line_start.is_at_eol())
            {
                break;
            }
            let next_line_ix = ix + line_start.bytes_scanned();
            if next_line_ix == self.text.len() {
                break;
            }
            ix = next_line_ix;
            remaining_space = line_start.remaining_space();
            last_line_blank = scan_blank_line(&bytes[ix..]).is_some();
        }

        // Trim trailing blank lines.
        if let Some(child) = last_nonblank_child {
            self.tree[child].next = None;
            self.tree[child].item.end = last_nonblank_ix;
        }
        self.pop(end_ix);
        ix
    }

    fn parse_fenced_code_block(
        &mut self,
        start_ix: usize,
        indent: usize,
        fence_ch: u8,
        n_fence_char: usize,
    ) -> usize {
        let bytes = self.text.as_bytes();
        let mut info_start = start_ix + n_fence_char;
        info_start += scan_whitespace_no_nl(&bytes[info_start..]);
        // TODO: info strings are typically very short. wouldnt it be faster
        // to just do a forward scan here?
        let mut ix = info_start + scan_nextline(&bytes[info_start..]);
        let info_end = ix - scan_rev_while(&bytes[info_start..ix], is_ascii_whitespace);
        let info_string = unescape(&self.text[info_start..info_end]);
        self.tree.append(Item {
            start: start_ix,
            end: 0, // will get set later
            body: ItemBody::FencedCodeBlock(self.allocs.allocate_cow(info_string)),
        });
        self.tree.push();
        loop {
            let mut line_start = LineStart::new(&bytes[ix..]);
            let n_containers = scan_containers(&self.tree, &mut line_start);
            if n_containers < self.tree.spine_len() {
                break;
            }
            line_start.scan_space(indent);
            let mut close_line_start = line_start.clone();
            if !close_line_start.scan_space(4) {
                let close_ix = ix + close_line_start.bytes_scanned();
                if let Some(n) = scan_closing_code_fence(&bytes[close_ix..], fence_ch, n_fence_char)
                {
                    ix = close_ix + n;
                    break;
                }
            }
            let remaining_space = line_start.remaining_space();
            ix += line_start.bytes_scanned();
            let next_ix = ix + scan_nextline(&bytes[ix..]);
            self.append_code_text(remaining_space, ix, next_ix);
            ix = next_ix;
        }

        self.pop(ix);

        // try to read trailing whitespace or it will register as a completely blank line
        ix + scan_blank_line(&bytes[ix..]).unwrap_or(0)
    }

    fn append_code_text(&mut self, remaining_space: usize, start: usize, end: usize) {
        if remaining_space > 0 {
            let cow_ix = self.allocs.allocate_cow("   "[..remaining_space].into());
            self.tree.append(Item {
                start,
                end: start,
                body: ItemBody::SynthesizeText(cow_ix),
            });
        }
        if self.text.as_bytes()[end - 2] == b'\r' {
            // Normalize CRLF to LF
            self.tree.append_text(start, end - 2);
            self.tree.append_text(end - 1, end);
        } else {
            self.tree.append_text(start, end);
        }
    }

    /// Appends a line of HTML to the tree.
    fn append_html_line(&mut self, remaining_space: usize, start: usize, end: usize) {
        if remaining_space > 0 {
            let cow_ix = self.allocs.allocate_cow("   "[..remaining_space].into());
            self.tree.append(Item {
                start,
                end: start,
                // TODO: maybe this should synthesize to html rather than text?
                body: ItemBody::SynthesizeText(cow_ix),
            });
        }
        if self.text.as_bytes()[end - 2] == b'\r' {
            // Normalize CRLF to LF
            self.tree.append(Item {
                start,
                end: end - 2,
                body: ItemBody::Html,
            });
            self.tree.append(Item {
                start: end - 1,
                end,
                body: ItemBody::Html,
            });
        } else {
            self.tree.append(Item {
                start,
                end,
                body: ItemBody::Html,
            });
        }
    }

    /// Pop a container, setting its end.
    fn pop(&mut self, ix: usize) {
        let cur_ix = self.tree.pop().unwrap();
        self.tree[cur_ix].item.end = ix;
        if let ItemBody::List(true, _, _) = self.tree[cur_ix].item.body {
            surgerize_tight_list(&mut self.tree, cur_ix);
        }
    }

    /// Close a list if it's open. Also set loose if last line was blank
    fn finish_list(&mut self, ix: usize) {
        if let Some(node_ix) = self.tree.peek_up() {
            if let ItemBody::List(_, _, _) = self.tree[node_ix].item.body {
                self.pop(ix);
                self.list_nesting -= 1;
            }
        }
        if self.last_line_blank {
            if let Some(node_ix) = self.tree.peek_grandparent() {
                if let ItemBody::List(ref mut is_tight, _, _) = self.tree[node_ix].item.body {
                    *is_tight = false;
                }
            }
            self.last_line_blank = false;
        }
    }

    /// Continue an existing list or start a new one if there's not an open
    /// list that matches.
    fn continue_list(&mut self, start: usize, ch: u8, index: u64) {
        if let Some(node_ix) = self.tree.peek_up() {
            if let ItemBody::List(ref mut is_tight, existing_ch, _) = self.tree[node_ix].item.body {
                if existing_ch == ch {
                    if self.last_line_blank {
                        *is_tight = false;
                        self.last_line_blank = false;
                    }
                    return;
                }
            }
            // TODO: this is not the best choice for end; maybe get end from last list item.
            self.finish_list(start);
        }
        self.tree.append(Item {
            start,
            end: 0, // will get set later
            body: ItemBody::List(true, ch, index),
        });
        self.list_nesting += 1;
        self.tree.push();
        self.last_line_blank = false;
    }

    /// Parse a thematic break.
    ///
    /// Returns index of start of next line.
    fn parse_hrule(&mut self, hrule_size: usize, ix: usize) -> usize {
        self.tree.append(Item {
            start: ix,
            end: ix + hrule_size,
            body: ItemBody::Rule,
        });
        ix + hrule_size
    }

    /// Parse an ATX heading.
    ///
    /// Returns index of start of next line.
    fn parse_atx_heading(&mut self, mut ix: usize, atx_size: usize) -> usize {
        let heading_ix = self.tree.append(Item {
            start: ix,
            end: 0, // set later
            body: ItemBody::Heading(atx_size as u32),
        });
        ix += atx_size;
        // next char is space or eol (guaranteed by scan_atx_heading)
        let bytes = self.text.as_bytes();
        if let Some(eol_bytes) = scan_eol(&bytes[ix..]) {
            self.tree[heading_ix].item.end = ix + eol_bytes;
            return ix + eol_bytes;
        }
        // skip leading spaces
        let skip_spaces = scan_whitespace_no_nl(&bytes[ix..]);
        ix += skip_spaces;

        // now handle the header text
        let header_start = ix;
        let header_node_idx = self.tree.push(); // so that we can set the endpoint later
        ix = self.parse_line(ix, TableParseMode::Disabled).0;
        self.tree[header_node_idx].item.end = ix;

        // remove trailing matter from header text
        if let Some(cur_ix) = self.tree.cur() {
            let header_text = &bytes[header_start..ix];
            let mut limit = header_text
                .iter()
                .rposition(|&b| !(b == b'\n' || b == b'\r' || b == b' '))
                .map_or(0, |i| i + 1);
            let closer = header_text[..limit]
                .iter()
                .rposition(|&b| b != b'#')
                .map_or(0, |i| i + 1);
            if closer == 0 {
                limit = closer;
            } else {
                let spaces = scan_rev_while(&header_text[..closer], |b| b == b' ');
                if spaces > 0 {
                    limit = closer - spaces;
                }
            }
            self.tree[cur_ix].item.end = limit + header_start;
        }

        self.tree.pop();
        ix
    }

    /// Returns the number of bytes scanned on success.
    fn parse_footnote(&mut self, start: usize) -> Option<usize> {
        let bytes = &self.text.as_bytes()[start..];
        if !bytes.starts_with(b"[^") {
            return None;
        }
        let (mut i, label) = self.parse_refdef_label(start + 2)?;
        i += 2;
        if scan_ch(&bytes[i..], b':') == 0 {
            return None;
        }
        i += 1;
        self.finish_list(start);
        self.tree.append(Item {
            start,
            end: 0, // will get set later
            // TODO: check whether the label here is strictly necessary
            body: ItemBody::FootnoteDefinition(self.allocs.allocate_cow(label)),
        });
        self.tree.push();
        Some(i)
    }

    /// Tries to parse a reference label, which can be interrupted by new blocks.
    /// On success, returns the number of bytes of the label and the label itself.
    fn parse_refdef_label(&self, start: usize) -> Option<(usize, CowStr<'a>)> {
        scan_link_label_rest(&self.text[start..], &|bytes| {
            let mut line_start = LineStart::new(bytes);
            let _ = scan_containers(&self.tree, &mut line_start);
            let bytes_scanned = line_start.bytes_scanned();

            let suffix = &bytes[bytes_scanned..];
            if self.interrupt_paragraph_by_list(suffix) || scan_paragraph_interrupt(suffix) {
                None
            } else {
                Some(bytes_scanned)
            }
        })
    }

    /// Returns number of bytes scanned, label and definition on success.
    fn parse_refdef_total(&mut self, start: usize) -> Option<(usize, LinkLabel<'a>, LinkDef<'a>)> {
        let bytes = &self.text.as_bytes()[start..];
        if scan_ch(bytes, b'[') == 0 {
            return None;
        }
        let (mut i, label) = self.parse_refdef_label(start + 1)?;
        i += 1;
        if scan_ch(&bytes[i..], b':') == 0 {
            return None;
        }
        i += 1;
        let (bytecount, link_def) = self.scan_refdef(start + i)?;
        Some((bytecount + i, UniCase::new(label), link_def))
    }

    /// Returns number of bytes and number of newlines
    fn scan_refdef_space(&self, bytes: &[u8], mut i: usize) -> Option<(usize, usize)> {
        let mut newlines = 0;
        loop {
            let whitespaces = scan_whitespace_no_nl(&bytes[i..]);
            i += whitespaces;
            if let Some(eol_bytes) = scan_eol(&bytes[i..]) {
                i += eol_bytes;
                newlines += 1;
                if newlines > 1 {
                    return None;
                }
            } else {
                break;
            }
            let mut line_start = LineStart::new(&bytes[i..]);
            if self.tree.spine_len() != scan_containers(&self.tree, &mut line_start) {
                return None;
            }
            i += line_start.bytes_scanned();
        }
        Some((i, newlines))
    }

    /// Returns # of bytes and definition.
    /// Assumes the label of the reference including colon has already been scanned.
    fn scan_refdef(&self, start: usize) -> Option<(usize, LinkDef<'a>)> {
        let bytes = self.text.as_bytes();

        // whitespace between label and url (including up to one newline)
        let (mut i, _newlines) = self.scan_refdef_space(bytes, start)?;

        // scan link dest
        let (dest_length, dest) = scan_link_dest(self.text, i, 1)?;
        if dest_length == 0 {
            return None;
        }
        let dest = unescape(dest);
        i += dest_length;

        // no title
        let mut backup = (i - start, LinkDef { dest, title: None });

        // scan whitespace between dest and label
        let (mut i, newlines) =
            if let Some((new_i, mut newlines)) = self.scan_refdef_space(bytes, i) {
                if i == self.text.len() {
                    newlines += 1;
                }
                if new_i == i && newlines == 0 {
                    return None;
                }
                if newlines > 1 {
                    return Some(backup);
                };
                (new_i, newlines)
            } else {
                return Some(backup);
            };

        // scan title
        // if this fails but newline == 1, return also a refdef without title
        if let Some((title_length, title)) = scan_refdef_title(&self.text[i..]) {
            i += title_length;
            backup.1.title = Some(unescape(title));
        } else if newlines > 0 {
            return Some(backup);
        } else {
            return None;
        };

        // scan EOL
        if let Some(bytes) = scan_blank_line(&bytes[i..]) {
            backup.0 = i + bytes - start;
            Some(backup)
        } else if newlines > 0 {
            Some(backup)
        } else {
            None
        }
    }
}

/// Returns number of containers scanned.
fn scan_containers(tree: &Tree<Item>, line_start: &mut LineStart) -> usize {
    let mut i = 0;
    for &node_ix in tree.walk_spine() {
        match tree[node_ix].item.body {
            ItemBody::BlockQuote => {
                let save = line_start.clone();
                if !line_start.scan_blockquote_marker() {
                    *line_start = save;
                    break;
                }
            }
            ItemBody::ListItem(indent) => {
                let save = line_start.clone();
                if !line_start.scan_space(indent) {
                    if !line_start.is_at_eol() {
                        *line_start = save;
                        break;
                    }
                }
            }
            _ => (),
        }
        i += 1;
    }
    i
}

/// Computes the number of header columns in a table line by computing the number of dividing pipes
/// that aren't followed or preceeded by whitespace.
fn count_header_cols(
    bytes: &[u8],
    mut pipes: usize,
    mut start: usize,
    last_pipe_ix: usize,
) -> usize {
    // was first pipe preceeded by whitespace? if so, subtract one
    start += scan_whitespace_no_nl(&bytes[start..]);
    if bytes[start] == b'|' {
        pipes -= 1;
    }

    // was last pipe followed by whitespace? if so, sub one
    if scan_blank_line(&bytes[(last_pipe_ix + 1)..]).is_some() {
        pipes
    } else {
        pipes + 1
    }
}

impl<'a> Tree<Item> {
    fn append_text(&mut self, start: usize, end: usize) {
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

/// Determines whether the delimiter run starting at given index is
/// left-flanking, as defined by the commonmark spec (and isn't intraword
/// for _ delims).
/// suffix is &s[ix..], which is passed in as an optimization, since taking
/// a string subslice is O(n).
fn delim_run_can_open(s: &str, suffix: &str, run_len: usize, ix: usize) -> bool {
    let next_char = if let Some(c) = suffix.chars().nth(run_len) {
        c
    } else {
        return false;
    };
    if next_char.is_whitespace() {
        return false;
    }
    if ix == 0 {
        return true;
    }
    let delim = suffix.chars().next().unwrap();
    if delim == '*' && !is_punctuation(next_char) {
        return true;
    }

    let prev_char = s[..ix].chars().last().unwrap();

    prev_char.is_whitespace()
        || is_punctuation(prev_char) && (delim != '\'' || ![']', ')'].contains(&prev_char))
}

/// Determines whether the delimiter run starting at given index is
/// left-flanking, as defined by the commonmark spec (and isn't intraword
/// for _ delims)
fn delim_run_can_close(s: &str, suffix: &str, run_len: usize, ix: usize) -> bool {
    if ix == 0 {
        return false;
    }
    let prev_char = s[..ix].chars().last().unwrap();
    if prev_char.is_whitespace() {
        return false;
    }
    let next_char = if let Some(c) = suffix.chars().nth(run_len) {
        c
    } else {
        return true;
    };
    let delim = suffix.chars().next().unwrap();
    if delim == '*' && !is_punctuation(prev_char) {
        return true;
    }

    next_char.is_whitespace() || is_punctuation(next_char)
}

/// Checks whether we should break a paragraph on the given input.
/// Note: lists are dealt with in `interrupt_paragraph_by_list`, because determing
/// whether to break on a list requires additional context.
fn scan_paragraph_interrupt(bytes: &[u8]) -> bool {
    if scan_eol(bytes).is_some()
        || scan_hrule(bytes).is_ok()
        || scan_atx_heading(bytes).is_some()
        || scan_code_fence(bytes).is_some()
        || scan_blockquote_start(bytes).is_some()
    {
        return true;
    }
    bytes.starts_with(b"<")
        && (get_html_end_tag(&bytes[1..]).is_some()
            || is_html_tag(scan_html_block_tag(&bytes[1..]).1))
}

/// Assumes `text_bytes` is preceded by `<`.
fn get_html_end_tag(text_bytes: &[u8]) -> Option<&'static str> {
    static BEGIN_TAGS: &[&[u8]; 3] = &[b"pre", b"style", b"script"];
    static ST_BEGIN_TAGS: &[&[u8]; 3] = &[b"!--", b"?", b"![CDATA["];

    for (beg_tag, end_tag) in BEGIN_TAGS
        .iter()
        .zip(["</pre>", "</style>", "</script>"].iter())
    {
        let tag_len = beg_tag.len();

        if text_bytes.len() < tag_len {
            // begin tags are increasing in size
            break;
        }

        if !text_bytes[..tag_len].eq_ignore_ascii_case(beg_tag) {
            continue;
        }

        // Must either be the end of the line...
        if text_bytes.len() == tag_len {
            return Some(end_tag);
        }

        // ...or be followed by whitespace, newline, or '>'.
        let s = text_bytes[tag_len];
        if is_ascii_whitespace(s) || s == b'>' {
            return Some(end_tag);
        }
    }

    for (beg_tag, end_tag) in ST_BEGIN_TAGS.iter().zip(["-->", "?>", "]]>"].iter()) {
        if text_bytes.starts_with(beg_tag) {
            return Some(end_tag);
        }
    }

    if text_bytes.len() > 1
        && text_bytes[0] == b'!'
        && text_bytes[1] >= b'A'
        && text_bytes[1] <= b'Z'
    {
        Some(">")
    } else {
        None
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

#[derive(Clone)]
struct LinkDef<'a> {
    dest: CowStr<'a>,
    title: Option<CowStr<'a>>,
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
struct LinkIndex(usize);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct CowIndex(usize);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct AlignmentIndex(usize);

#[derive(Clone)]
struct Allocations<'a> {
    refdefs: HashMap<LinkLabel<'a>, LinkDef<'a>>,
    links: Vec<(LinkType, CowStr<'a>, CowStr<'a>)>,
    cows: Vec<CowStr<'a>>,
    alignments: Vec<Vec<Alignment>>,
}

impl<'a> Allocations<'a> {
    fn new() -> Self {
        Self {
            refdefs: HashMap::new(),
            links: Vec::with_capacity(128),
            cows: Vec::new(),
            alignments: Vec::new(),
        }
    }

    fn allocate_cow(&mut self, cow: CowStr<'a>) -> CowIndex {
        let ix = self.cows.len();
        self.cows.push(cow);
        CowIndex(ix)
    }

    fn allocate_link(&mut self, ty: LinkType, url: CowStr<'a>, title: CowStr<'a>) -> LinkIndex {
        let ix = self.links.len();
        self.links.push((ty, url, title));
        LinkIndex(ix)
    }

    fn allocate_alignment(&mut self, alignment: Vec<Alignment>) -> AlignmentIndex {
        let ix = self.alignments.len();
        self.alignments.push(alignment);
        AlignmentIndex(ix)
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

fn special_bytes(options: &Options) -> [bool; 256] {
    let mut bytes = [false; 256];
    let standard_bytes = [
        b'\n', b'\r', b'*', b'_', b'&', b'\\', b'[', b']', b'<', b'!', b'`',
    ];

    for &byte in &standard_bytes {
        bytes[byte as usize] = true;
    }
    if options.contains(Options::ENABLE_TABLES) {
        bytes[b'|' as usize] = true;
    }
    if options.contains(Options::ENABLE_STRIKETHROUGH) {
        bytes[b'~' as usize] = true;
    }
    if options.contains(Options::ENABLE_SMART_PUNCTUATION) {
        for &byte in &[b'.', b'-', b'"', b'\''] {
            bytes[byte as usize] = true;
        }
    }

    bytes
}

pub(crate) fn create_lut(options: &Options) -> LookupTable {
    #[cfg(all(target_arch = "x86_64", feature = "simd"))]
    {
        LookupTable {
            simd: crate::simd::compute_lookup(options),
            scalar: special_bytes(options),
        }
    }
    #[cfg(not(all(target_arch = "x86_64", feature = "simd")))]
    {
        special_bytes(options)
    }
}

pub type BrokenLinkCallback<'a> =
    Option<&'a mut dyn FnMut(BrokenLink) -> Option<(CowStr<'a>, CowStr<'a>)>>;

/// Markdown event iterator.
pub struct Parser<'a> {
    text: &'a str,
    options: Options,
    tree: Tree<Item>,
    allocs: Allocations<'a>,
    broken_link_callback: BrokenLinkCallback<'a>,
    html_scan_guard: HtmlScanGuard,

    // used by inline passes. store them here for reuse
    inline_stack: InlineStack,
    link_stack: LinkStack,
}

impl<'a> Parser<'a> {
    /// Creates a new event iterator for a markdown string without any options enabled.
    pub fn new(text: &'a str) -> Parser<'a> {
        Parser::new_ext(text, Options::empty())
    }

    /// Creates a new event iterator for a markdown string with given options.
    pub fn new_ext(text: &'a str, options: Options) -> Parser<'a> {
        Parser::new_with_broken_link_callback(text, options, None)
    }

    /// In case the parser encounters any potential links that have a broken
    /// reference (e.g `[foo]` when there is no `[foo]: ` entry at the bottom)
    /// the provided callback will be called with the reference name,
    /// and the returned pair will be used as the link name and title if it is not
    /// `None`.
    pub fn new_with_broken_link_callback(
        text: &'a str,
        options: Options,
        broken_link_callback: BrokenLinkCallback<'a>,
    ) -> Parser<'a> {
        let lut = create_lut(&options);
        let first_pass = FirstPass::new(text, options, &lut);
        let (mut tree, allocs) = first_pass.run();
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
                                    let reference_close_node =
                                        scan_nodes_to_ix(&self.tree, next, end_ix - 1).unwrap();
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
                            let label: Option<(ReferenceLabel<'a>, usize)> = match scan_result {
                                RefScan::LinkLabel(l, end_ix) => {
                                    Some((ReferenceLabel::Link(l), end_ix))
                                }
                                RefScan::Collapsed(..) | RefScan::Failed => {
                                    // No label? maybe it is a shortcut reference
                                    let label_start = self.tree[tos.node].item.end - 1;
                                    scan_link_label(
                                        &self.tree,
                                        &self.text[label_start..self.tree[cur_ix].item.end],
                                        self.options.contains(Options::ENABLE_FOOTNOTES),
                                    )
                                    .map(|(ix, label)| (label, label_start + ix))
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
                                    .get(&UniCase::new(link_label.as_ref().into()))
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
                                                    link_type: link_type,
                                                    reference: link_label.as_ref(),
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
                                let (inc, ty) = if c == b'~' {
                                    (2, ItemBody::Strikethrough)
                                } else if start > el.start + el.count - match_count + 1 {
                                    (2, ItemBody::Strong)
                                } else {
                                    (1, ItemBody::Emphasis)
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
        underlying: &'a str,
        mut ix: usize,
        node: Option<TreeIndex>,
    ) -> Option<(usize, CowStr<'a>, CowStr<'a>)> {
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
        text: &'a str,
        start_ix: usize,
        node: Option<TreeIndex>,
    ) -> Option<(usize, CowStr<'a>)> {
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
        let first_ix = open + 1;
        let last_ix = close - 1;
        let bytes = self.text.as_bytes();
        let mut span_start = self.tree[open].item.end;
        let mut span_end = self.tree[close].item.start;
        let mut buf: Option<String> = None;

        // detect all-space sequences, since they are kept as-is as of commonmark 0.29
        if !bytes[span_start..span_end].iter().all(|&b| b == b' ') {
            let opening = match bytes[span_start] {
                b' ' | b'\r' | b'\n' => true,
                _ => false,
            };
            let closing = match bytes[span_end - 1] {
                b' ' | b'\r' | b'\n' => true,
                _ => false,
            };
            let drop_enclosing_whitespace = opening && closing;

            if drop_enclosing_whitespace {
                span_start += 1;
                if span_start < span_end {
                    span_end -= 1;
                }
            }

            let mut ix = first_ix;

            while ix < close {
                if let ItemBody::HardBreak | ItemBody::SoftBreak = self.tree[ix].item.body {
                    if drop_enclosing_whitespace {
                        // check whether break should be ignored
                        if ix == first_ix {
                            ix = ix + 1;
                            span_start = min(span_end, self.tree[ix].item.start);
                            continue;
                        } else if ix == last_ix && last_ix > first_ix {
                            ix = ix + 1;
                            continue;
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
                    let end = if ix == last_ix {
                        span_end
                    } else {
                        self.tree[ix].item.end
                    };
                    buf.push_str(&self.text[self.tree[ix].item.start..end]);
                }
                ix = ix + 1;
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
                Some(&|_bytes| {
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
    pub fn into_offset_iter(self) -> OffsetIter<'a> {
        OffsetIter { inner: self }
    }
}

pub(crate) enum LoopInstruction<T> {
    /// Continue looking for more special bytes, but skip next few bytes.
    ContinueAndSkip(usize),
    /// Break looping immediately, returning with the given index and value.
    BreakAtWith(usize, T),
}

#[cfg(all(target_arch = "x86_64", feature = "simd"))]
pub(crate) struct LookupTable {
    pub simd: [u8; 16],
    pub scalar: [bool; 256],
}

#[cfg(not(all(target_arch = "x86_64", feature = "simd")))]
type LookupTable = [bool; 256];

/// This function walks the byte slices from the given index and
/// calls the callback function on all bytes (and their indices) that are in the following set:
/// `` ` ``, `\`, `&`, `*`, `_`, `~`, `!`, `<`, `[`, `]`, `|`, `\r`, `\n`
/// It is guaranteed not call the callback on other bytes.
/// Whenever `callback(ix, byte)` returns a `ContinueAndSkip(n)` value, the callback
/// will not be called with an index that is less than `ix + n + 1`.
/// When the callback returns a `BreakAtWith(end_ix, opt+val)`, no more callbacks will be
/// called and the function returns immediately with the return value `(end_ix, opt_val)`.
/// If `BreakAtWith(..)` is never returned, this function will return the first
/// index that is outside the byteslice bound and a `None` value.
fn iterate_special_bytes<F, T>(
    lut: &LookupTable,
    bytes: &[u8],
    ix: usize,
    callback: F,
) -> (usize, Option<T>)
where
    F: FnMut(usize, u8) -> LoopInstruction<Option<T>>,
{
    #[cfg(all(target_arch = "x86_64", feature = "simd"))]
    {
        crate::simd::iterate_special_bytes(lut, bytes, ix, callback)
    }
    #[cfg(not(all(target_arch = "x86_64", feature = "simd")))]
    {
        scalar_iterate_special_bytes(lut, bytes, ix, callback)
    }
}

pub(crate) fn scalar_iterate_special_bytes<F, T>(
    lut: &[bool; 256],
    bytes: &[u8],
    mut ix: usize,
    mut callback: F,
) -> (usize, Option<T>)
where
    F: FnMut(usize, u8) -> LoopInstruction<Option<T>>,
{
    while ix < bytes.len() {
        let b = bytes[ix];
        if lut[b as usize] {
            match callback(ix, b) {
                LoopInstruction::ContinueAndSkip(skip) => {
                    ix += skip;
                }
                LoopInstruction::BreakAtWith(ix, val) => {
                    return (ix, val);
                }
            }
        }
        ix += 1;
    }

    (ix, None)
}

/// Markdown event and source range iterator.
///
/// Generates tuples where the first element is the markdown event and the second
/// is a the corresponding range in the source string.
///
/// Constructed from a `Parser` using its
/// [`into_offset_iter`](struct.Parser.html#method.into_offset_iter) method.
pub struct OffsetIter<'a> {
    inner: Parser<'a>,
}

impl<'a> Iterator for OffsetIter<'a> {
    type Item = (Event<'a>, Range<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.tree.cur() {
            None => {
                let ix = self.inner.tree.pop()?;
                let tag = item_to_tag(&self.inner.tree[ix].item, &self.inner.allocs);
                self.inner.tree.next_sibling(ix);
                Some((
                    Event::End(tag),
                    self.inner.tree[ix].item.start..self.inner.tree[ix].item.end,
                ))
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
        ItemBody::Heading(level) => Tag::Heading(level),
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
        ItemBody::Heading(level) => Tag::Heading(level),
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

// https://english.stackexchange.com/a/285573
fn surgerize_tight_list(tree: &mut Tree<Item>, list_ix: TreeIndex) {
    let mut list_item = tree[list_ix].child;
    while let Some(listitem_ix) = list_item {
        // first child is special, controls how we repoint list_item.child
        let list_item_firstborn = tree[listitem_ix].child;

        // Check that list item has children - this is not necessarily the case!
        if let Some(firstborn_ix) = list_item_firstborn {
            if let ItemBody::Paragraph = tree[firstborn_ix].item.body {
                tree[listitem_ix].child = tree[firstborn_ix].child;
            }

            let mut list_item_child = Some(firstborn_ix);
            let mut node_to_repoint = None;
            while let Some(child_ix) = list_item_child {
                // surgerize paragraphs
                let repoint_ix = if let ItemBody::Paragraph = tree[child_ix].item.body {
                    if let Some(child_firstborn) = tree[child_ix].child {
                        if let Some(repoint_ix) = node_to_repoint {
                            tree[repoint_ix].next = Some(child_firstborn);
                        }
                        let mut child_lastborn = child_firstborn;
                        while let Some(lastborn_next_ix) = tree[child_lastborn].next {
                            child_lastborn = lastborn_next_ix;
                        }
                        child_lastborn
                    } else {
                        child_ix
                    }
                } else {
                    child_ix
                };

                node_to_repoint = Some(repoint_ix);
                tree[repoint_ix].next = tree[child_ix].next;
                list_item_child = tree[child_ix].next;
            }
        }

        list_item = tree[listitem_ix].next;
    }
}

impl<'a> Iterator for Parser<'a> {
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::tree::Node;

    // TODO: move these tests to tests/html.rs?

    fn parser_with_extensions(text: &str) -> Parser<'_> {
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
            assert_eq!("world", broken_link.reference);
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
}
