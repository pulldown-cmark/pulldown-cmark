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

use std::collections::HashMap;

use unicase::UniCase;

use crate::strings::{CowStr, InlineStr};
use crate::scanners::*;
use crate::tree::{TreePointer, TreeIndex, Tree};
use crate::linklabel::{scan_link_label, scan_link_label_rest, LinkLabel, ReferenceLabel};

#[derive(Clone, Debug, PartialEq)]
pub enum Tag<'a> {
    // block-level tags
    Paragraph,
    Rule,

    /// A heading. The field indicates the level of the heading.
    Header(i32),

    BlockQuote,
    CodeBlock(CowStr<'a>),

    /// A list. If the list is ordered the field indicates the number of the first item.
    List(Option<usize>),  // TODO: add delim and tight for ast (not needed for html)
    Item,
    FootnoteDefinition(CowStr<'a>),
    HtmlBlock,

    // tables
    Table(Vec<Alignment>),
    TableHead,
    TableRow,
    TableCell,

    // span-level tags
    Emphasis,
    Strong,
    Strikethrough,
    Code,

    /// A link. The first field is the link type, the second the destination URL and the third is a title
    Link(LinkType, CowStr<'a>, CowStr<'a>),

    /// An image. The first field is the link type, the second the destination URL and the third is a title
    Image(LinkType, CowStr<'a>, CowStr<'a>),
}

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

#[derive(Clone, Debug, PartialEq)]
pub enum Event<'a> {
    Start(Tag<'a>),
    End(Tag<'a>),
    Text(CowStr<'a>),
    Html(CowStr<'a>),
    InlineHtml(CowStr<'a>),
    FootnoteReference(CowStr<'a>),
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
        const ENABLE_STRIKETHROUGH = 1 << 3;
    }
}

#[derive(Debug, Default)]
struct Item<'a> {
    start: usize,
    end: usize,
    body: ItemBody<'a>,
}

#[derive(Debug, PartialEq)]
enum ItemBody<'a> {
    Paragraph,
    Text,
    SoftBreak,
    HardBreak,

    // These are possible inline items, need to be resolved in second pass.

    // repeats, can_open, can_close
    MaybeEmphasis(usize, bool, bool),
    MaybeCode(usize),
    MaybeHtml,
    MaybeLinkOpen,
    MaybeLinkClose,
    MaybeImage,
    Backslash,

    // These are inline items after resolution.
    Emphasis,
    Strong,
    Strikethrough,
    Code,
    InlineHtml,
    // Link params: destination, title.
    Link(LinkType, CowStr<'a>, CowStr<'a>),
    Image(LinkType, CowStr<'a>, CowStr<'a>),
    FootnoteReference(CowStr<'a>), // label

    Rule,
    Header(i32), // header level
    FencedCodeBlock(CowStr<'a>), // info string
    IndentCodeBlock,
    HtmlBlock(Option<&'static str>), // end tag, or none for type 6
    Html,
    BlockQuote,
    List(bool, u8, Option<usize>), // is_tight, list character, list start index
    ListItem(usize), // indent level
    SynthesizeText(CowStr<'static>),
    FootnoteDefinition(CowStr<'a>), // label

    // Tables
    Table(Vec<Alignment>),
    TableHead,
    TableRow,
    TableCell,

    // Dummy node at the top of the tree - should not be used otherwise!
    Root,
}

impl<'a> ItemBody<'a> {
    fn is_inline(&self) -> bool {
        match *self {
            ItemBody::MaybeEmphasis(..) | ItemBody::MaybeHtml | ItemBody::MaybeCode(_)
            | ItemBody::MaybeLinkOpen | ItemBody::MaybeLinkClose | ItemBody::MaybeImage => true,
            _ => false,
        }
    }
}

impl<'a> Default for ItemBody<'a> {
    fn default() -> Self {
        ItemBody::Root
    }
}

/// State for the first parsing pass.
///
/// The first pass resolves all block structure, generating an AST. Within a block, items
/// are in a linear chain with potential inline markup identified.
struct FirstPass<'a> {
    text: &'a str,
    tree: Tree<Item<'a>>,
    begin_list_item: bool,
    last_line_blank: bool,
    references: HashMap<LinkLabel<'a>, LinkDef<'a>>,
    options: Options,
}

impl<'a> FirstPass<'a> {
    fn new(text: &'a str, options: Options) -> FirstPass {
        let tree = Tree::new();
        let begin_list_item = false;
        let last_line_blank = false;
        let references = HashMap::new();
        FirstPass { text, tree, begin_list_item, last_line_blank, references, options }
    }

    fn run(mut self) -> (Tree<Item<'a>>, HashMap<LinkLabel<'a>, LinkDef<'a>>) {
        let mut ix = 0;
        while ix < self.text.len() {
            ix = self.parse_block(ix);
        }
        for _ in 0..self.tree.spine_len() {
            self.pop(ix);
        }
        (self.tree, self.references)
    }

    /// Returns offset after block.
    fn parse_block(&mut self, mut start_ix: usize) -> usize {
        let mut line_start = LineStart::new(&self.text[start_ix..]);

        let i = self.scan_containers(&mut line_start);
        for _ in i..self.tree.spine_len() {
            self.pop(start_ix);
        }

        // finish footnote if it's still open and was preceeded by blank line
        if let Some(node_ix) = self.tree.peek_up() {
            if let ItemBody::FootnoteDefinition(..) = self.tree[node_ix].item.body {
                if self.last_line_blank {
                    self.pop(start_ix);
                }
            }
        }
        
        if self.options.contains(Options::ENABLE_FOOTNOTES) {
            // Footnote definitions of the form
            // [^bar]:
            // * anything really
            let container_start = start_ix + line_start.bytes_scanned();
            if let Some(bytecount) = self.parse_footnote(container_start) {
                start_ix = container_start + bytecount;
                start_ix += scan_blank_line(&self.text[start_ix..]).unwrap_or(0);
                line_start = LineStart::new(&self.text[start_ix..]);      
            }
        }

        // Process new containers
        loop {
            let container_start = start_ix + line_start.bytes_scanned();
            if line_start.scan_blockquote_marker() {
                self.finish_list(start_ix);
                self.tree.append(Item {
                    start: container_start,
                    end: 0, // will get set later
                    body: ItemBody::BlockQuote,
                });
                self.tree.push();
            } else if let Some((ch, index, indent)) = line_start.scan_list_marker() {
                let opt_index = if ch == b'.' || ch == b')' { Some(index) } else { None };
                let after_marker_index = start_ix + line_start.bytes_scanned();
                self.continue_list(container_start, ch, opt_index);
                self.tree.append(Item {
                    start: container_start,
                    end: after_marker_index, // will get updated later if item not empty
                    body: ItemBody::ListItem(indent),
                });
                self.tree.push();
                if let Some(n) = scan_blank_line(&self.text[after_marker_index..]) {
                    self.begin_list_item = true;
                    return after_marker_index + n;
                }
            }
            else {
                break;
            }
        }

        let ix = start_ix + line_start.bytes_scanned();

        if let Some(n) = scan_blank_line(&self.text[ix..]) {
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


        // HTML Blocks

        // Start scanning at the first nonspace character, but don't advance `ix` yet because any
        // spaces present before the HTML block begins should be preserved.
        let nonspace_ix = start_ix + line_start.bytes_scanned();

        // Types 1-5 are all detected by one function and all end with the same
        // pattern
        if let Some(html_end_tag) = get_html_end_tag(&self.text[nonspace_ix..]) {
            return self.parse_html_block_type_1_to_5(ix, html_end_tag, remaining_space);
        }

        // Detect type 6
        let possible_tag = scan_html_block_tag(&self.text[nonspace_ix..]).1;
        if is_html_tag(possible_tag) {
            return self.parse_html_block_type_6_or_7(ix, remaining_space);
        }

        // Detect type 7
        if let Some(_html_bytes) = scan_html_type_7(&self.text[nonspace_ix..]) {
            return self.parse_html_block_type_6_or_7(ix, remaining_space);
        }

        // Advance `ix` after HTML blocks have been scanned
        let ix = start_ix + line_start.bytes_scanned();

        let n = scan_hrule(&self.text[ix..]);
        if n > 0 {
            return self.parse_hrule(n, ix);
        }

        if let Some((atx_size, atx_level)) = scan_atx_heading(&self.text[ix..]) {
            return self.parse_atx_heading(ix, atx_level, atx_size);
        }

        // parse refdef
        if let Some((bytecount, label, link_def)) = self.parse_refdef_total(ix) {
            self.references.entry(label).or_insert(link_def);
            return ix + bytecount;
        }

        let (n, fence_ch) = scan_code_fence(&self.text[ix..]);
        if n > 0 {
            return self.parse_fenced_code_block(ix, indent, fence_ch, n);
        }
        
        self.parse_paragraph(ix)
    }

    /// Returns the offset of the first line after the table.
    /// Assumptions: current focus is a table element and the table header
    /// matches the separator line (same number of columns).
    fn parse_table(&mut self, start_ix: usize, body_start: usize) -> usize {
        // parse header. this shouldn't fail because we (should have) made sure the table
        // header is ok
        let row_cells = {
            let table_ix = self.tree.peek_up().unwrap();
            if let ItemBody::Table(ref alignment) = self.tree[table_ix].item.body {
                alignment.len()
            } else {
                unreachable!()
            }
        };
        self.parse_table_row(start_ix, row_cells).unwrap();
        let thead_ix = self.tree.cur().unwrap();
        self.tree[thead_ix].item.body = ItemBody::TableHead;

        // parse body
        let mut ix = body_start;
        while let Some(next_ix) = self.parse_table_row(ix, row_cells) {
            ix = next_ix;
        }

        let table_ix = self.tree.pop().unwrap();
        self.tree[table_ix].item.end = ix;
        ix
    }

    /// Returns first offset after the row.
    fn parse_table_row(&mut self, mut ix: usize, row_cells: usize) -> Option<usize> {
        let mut line_start = LineStart::new(&self.text[ix..]);
        let _n_containers = self.scan_containers(&mut line_start);
        let mut cells = 0;
        let mut final_cell_ix = None;
        ix += line_start.bytes_scanned();

        if scan_paragraph_interrupt(&self.text[ix..]) {
            return None;
        }

        let row_ix = self.tree.append(Item {
            start: ix,
            end: 0, // set at end of this function
            body: ItemBody::TableRow,
        });
        self.tree.push();

        loop {
            ix += scan_ch(&self.text[ix..], b'|');
            ix += scan_whitespace_no_nl(&self.text[ix..]);
            let (eol_bytes, is_eol) = scan_eol(&self.text[ix..]);
            
            if is_eol {
                ix += eol_bytes;
                break;
            }

            let cell_ix = self.tree.append(Item {
                start: ix,
                end: ix,
                body: ItemBody::TableCell,
            });
            self.tree.push();
            let (next_ix, _brk) = self.parse_line(ix, true);
            self.tree[cell_ix].item.end = next_ix;
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
            self.tree[cell_ix].next = TreePointer::Nil;
        }

        self.tree.pop();
        self.tree[row_ix].item.end = ix;
        Some(ix)
    }

    /// Returns offset of line start after paragraph.
    fn parse_paragraph(&mut self, start_ix: usize) -> usize {
        let node_ix = self.tree.append(Item {
            start: start_ix,
            end: 0,  // will get set later
            body: ItemBody::Paragraph,
        });
        self.tree.push();

        let mut ix = start_ix;
        loop {
            let (next_ix, brk) = self.parse_line(ix, false);

            // break out when we find a table
            // rust's pattern matching really shines here!
            if let Some(table @ Item { body: ItemBody::Table(..), .. }) = brk {
                self.tree[node_ix].item = table;
                // this clears out any stuff we may have appended - but there may
                // be a cleaner way
                self.tree[node_ix].child = TreePointer::Nil;
                self.tree.pop();
                self.tree.push();
                return self.parse_table(ix, next_ix);
            }

            ix = next_ix;
            let mut line_start = LineStart::new(&self.text[ix..]);
            let n_containers = self.scan_containers(&mut line_start);
            if !line_start.scan_space(4) {
                let ix_new = ix + line_start.bytes_scanned();
                if n_containers == self.tree.spine_len() {
                    if let Some((n, level)) = scan_setext_heading(&self.text[ix_new..]) {
                        self.tree[node_ix].item.body = ItemBody::Header(level);
                        if let Some(Item { start, end: _, body: ItemBody::HardBreak }) = brk {
                            if self.text.as_bytes()[start] == b'\\' {
                                self.tree.append_text(start, start + 1);
                            }
                        }
                        ix = ix_new + n;
                        break;
                    }
                }
                // first check for non-empty lists, then for other interrupts    
                let suffix = &self.text[ix_new..];
                if self.interrupt_paragraph_by_list(suffix) || scan_paragraph_interrupt(suffix) {
                    break;
                }
            }
            line_start.scan_all_space();
            ix = next_ix + line_start.bytes_scanned();
            if let Some(item) = brk {
                self.tree.append(item);
            }
        }

        self.tree.pop();
        self.tree[node_ix].item.end = ix;
        ix
    }

    /// Parse a line of input, appending text and items to tree.
    ///
    /// Returns: index after line and an item representing the break.
    fn parse_line(&mut self, mut ix: usize, inside_table: bool) -> (usize, Option<Item<'a>>) {
        let bytes = &self.text.as_bytes();
        let start = ix;
        let mut pipes = 0;
        let mut last_pipe_ix = start;
        let mut begin_text = start;
        while ix < self.text.len() {
            match bytes[ix] {
                b'\r' | b'\n' | b'|' if inside_table => {
                    break;
                }
                b'\n' | b'\r' => {
                    let mut i = ix;
                    let eol_bytes = scan_eol(&self.text[ix..]).0;
                    if ix >= begin_text + 1 && bytes[ix - 1] == b'\\' && ix + eol_bytes < self.text.len() {
                        i -= 1;
                        self.tree.append_text(begin_text, i);
                        ix += eol_bytes;
                        return (ix, Some(Item {
                            start: i,
                            end: ix,
                            body: ItemBody::HardBreak,
                        }));
                    } else if ix >= begin_text + 2
                        && is_ascii_whitespace_no_nl(bytes[ix - 1])
                        && is_ascii_whitespace_no_nl(bytes[ix - 2]) {
                        i -= 2;
                        while i > 0 && is_ascii_whitespace_no_nl(bytes[i - 1]) {
                            i -= 1;
                        }
                        ix += eol_bytes;
                        self.tree.append_text(begin_text, i);

                        return (ix, Some(Item {
                            start: i,
                            end: ix,
                            body: ItemBody::HardBreak,
                        }));
                    } else if self.options.contains(Options::ENABLE_TABLES) && !inside_table && pipes > 0 {
                        // check if we may be parsing a table
                        let next_line_ix = ix + eol_bytes;
                        let mut line_start = LineStart::new(&self.text[next_line_ix..]);
                        let _n_containers = self.scan_containers(&mut line_start);
                        let table_head_ix = next_line_ix + line_start.bytes_scanned();
                        let (table_head_bytes, alignment) = scan_table_head(&self.text[table_head_ix..]);

                        if table_head_bytes > 0 {
                            // computing header count from number of pipes
                            let header_count = count_header_cols(&self.text, pipes, start, last_pipe_ix);

                            // make sure they match the number of columns we find in separator line
                            if alignment.len() == header_count {
                                ix = table_head_ix + table_head_bytes;
                                return (ix, Some(Item {
                                    start: i,
                                    end: ix, // must update later
                                    body: ItemBody::Table(alignment),
                                }));
                            }
                        }
                    }

                    self.tree.append_text(begin_text, ix);
                    ix += eol_bytes;
                    return (ix, Some(Item {
                        start: i,
                        end: ix,
                        body: ItemBody::SoftBreak,
                    }));
                }
                b'\\' if ix + 1 < self.text.len() && is_ascii_punctuation(bytes[ix + 1]) => {
                    self.tree.append_text(begin_text, ix);
                    if !inside_table || bytes[ix + 1] != b'|' {
                        self.tree.append(Item {
                            start: ix,
                            end: ix + 1,
                            body: ItemBody::Backslash,
                        });
                    }
                    begin_text = ix + 1;
                    if bytes[ix + 1] == b'`' {
                        ix += 1;
                    } else {
                        ix += 2;
                    }
                }
                c @ b'*' | c @ b'_' | c @ b'~' => {
                    let string_suffix = &self.text[ix..];
                    let count = 1 + scan_ch_repeat(&string_suffix[1..], c);
                    let can_open = delim_run_can_open(&self.text, string_suffix, count, ix);
                    let can_close = delim_run_can_close(&self.text, string_suffix, count, ix);
                    let is_valid_seq = c != b'~' || count == 2 && self.options.contains(Options::ENABLE_STRIKETHROUGH);

                    if (can_open || can_close) && is_valid_seq {
                        self.tree.append_text(begin_text, ix);
                        for i in 0..count {
                            self.tree.append(Item {
                                start: ix + i,
                                end: ix + i + 1,
                                body: ItemBody::MaybeEmphasis(count - i, can_open, can_close),
                            });
                        }
                        ix += count;
                        begin_text = ix;
                    } else {
                        ix += count;
                    }
                }
                b'`' => {
                    self.tree.append_text(begin_text, ix);
                    let count = 1 + scan_ch_repeat(&self.text[ix+1..], b'`');
                    self.tree.append(Item {
                        start: ix,
                        end: ix + count,
                        body: ItemBody::MaybeCode(count),
                    });
                    ix += count;
                    begin_text = ix;
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
                    ix += 1;
                    begin_text = ix;
                }
                b'!' if ix + 1 < self.text.len() && bytes[ix + 1] == b'[' => {
                    self.tree.append_text(begin_text, ix);
                    self.tree.append(Item {
                        start: ix,
                        end: ix + 2,
                        body: ItemBody::MaybeImage,
                    });
                    ix += 2;
                    begin_text = ix;
                }
                b'[' => {
                    self.tree.append_text(begin_text, ix);
                    self.tree.append(Item {
                        start: ix,
                        end: ix + 1,
                        body: ItemBody::MaybeLinkOpen,
                    });
                    ix += 1;
                    begin_text = ix;
                }
                b']' => {
                    self.tree.append_text(begin_text, ix);
                    self.tree.append(Item {
                        start: ix,
                        end: ix + 1,
                        body: ItemBody::MaybeLinkClose,
                    });
                    ix += 1;
                    begin_text = ix;
                }
                b'&' => {
                    match scan_entity(&self.text[ix..]) {
                        (n, Some(value)) => {
                            self.tree.append_text(begin_text, ix);
                            self.tree.append(Item {
                                start: ix,
                                end: ix + n,
                                body: ItemBody::SynthesizeText(value),
                            });
                            ix += n;
                            begin_text = ix;
                        }
                        _ => ix += 1
                    }
                }
                b'|' if !inside_table => {
                    last_pipe_ix = ix;
                    pipes += 1;
                    ix += 1;
                }
                _ => ix += 1,
            }
        }
        // need to close text at eof
        self.tree.append_text(begin_text, ix);
        (ix, None)
    }

    /// Check whether we should allow a paragraph interrupt by lists. Only non-empty
    /// lists are allowed.
    fn interrupt_paragraph_by_list(&self, suffix: &str) -> bool {
        let (ix, delim, index, _) = scan_listitem(suffix);

        if ix == 0 {
            return false;
        }

        // we don't allow interruption by either empty lists or
        // numbered lists starting at an index other than 1
        if !scan_empty_list(&suffix[ix..]) && (delim == b'*' || delim == b'-' || index == 1) {
            return true;
        }

        // check if we are currently in a list
        self.tree.peek_grandparent().map_or(false, |gp_ix| {
            match self.tree[gp_ix].item.body {
                ItemBody::ListItem(..) => true,
                _ => false,
            }
        })
    }

    /// When start_ix is at the beginning of an HTML block of type 1 to 5,
    /// this will find the end of the block, adding the block itself to the
    /// tree and also keeping track of the lines of HTML within the block.
    ///
    /// The html_end_tag is the tag that must be found on a line to end the block.
    fn parse_html_block_type_1_to_5(&mut self, start_ix: usize, html_end_tag: &'static str,
            mut remaining_space: usize) -> usize
    {
        self.tree.append(Item {
            start: start_ix,
            end: 0, // set later
            body: ItemBody::HtmlBlock(Some(html_end_tag)),
        });
        self.tree.push();

        let mut ix = start_ix;
        let end_ix;
        loop {
            let line_start_ix = ix;
            ix += scan_nextline(&self.text[ix..]);
            self.append_html_line(remaining_space, line_start_ix, ix);

            let mut line_start = LineStart::new(&self.text[ix..]);
            let n_containers = self.scan_containers(&mut line_start);
            if n_containers < self.tree.spine_len() {
                end_ix = ix;
                break;
            }

            if (&self.text[line_start_ix..ix]).contains(html_end_tag) {
                end_ix = ix;
                break;
            }

            let next_line_ix = ix + line_start.bytes_scanned();
            if next_line_ix == self.text.len() {
                end_ix = next_line_ix;
                break;
            }
            ix = next_line_ix;
            remaining_space = line_start.remaining_space();
        }
        &self.pop(end_ix);
        ix
    }

    /// When start_ix is at the beginning of an HTML block of type 6 or 7,
    /// this will consume lines until there is a blank line and keep track of
    /// the HTML within the block.
    fn parse_html_block_type_6_or_7(&mut self, start_ix: usize, mut remaining_space: usize)
        -> usize
    {
        self.tree.append(Item {
            start: start_ix,
            end: 0, // set later
            body: ItemBody::HtmlBlock(None)
        });
        self.tree.push();

        let mut ix = start_ix;
        let end_ix;
        loop {
            let line_start_ix = ix;
            ix += scan_nextline(&self.text[ix..]);
            self.append_html_line(remaining_space, line_start_ix, ix);

            let mut line_start = LineStart::new(&self.text[ix..]);
            let n_containers = self.scan_containers(&mut line_start);
            if n_containers < self.tree.spine_len() || line_start.is_at_eol()
            {
                end_ix = ix;
                break;
            }

            let next_line_ix = ix + line_start.bytes_scanned();
            if next_line_ix == self.text.len()
                || scan_blank_line(&self.text[next_line_ix..]).is_some()
            {
                end_ix = next_line_ix;
                break;
            }
            ix = next_line_ix;
            remaining_space = line_start.remaining_space();
        }
        self.pop(end_ix);
        ix
    }

    fn parse_indented_code_block(&mut self, start_ix: usize, mut remaining_space: usize)
        -> usize
    {
        self.tree.append(Item {
            start: start_ix,
            end: 0,  // will get set later
            body: ItemBody::IndentCodeBlock,
        });
        self.tree.push();
        let mut last_nonblank_child = TreePointer::Nil;
        let mut last_nonblank_ix = 0;
        let mut end_ix = 0;
        let mut last_line_blank = false;

        let mut ix = start_ix;
        loop {
            let line_start_ix = ix;
            ix += scan_nextline(&self.text[ix..]);
            self.append_code_text(remaining_space, line_start_ix, ix);
            // TODO(spec clarification): should we synthesize newline at EOF?

            if !last_line_blank {
                last_nonblank_child = self.tree.cur();
                last_nonblank_ix = ix;
                end_ix = ix;
            }

            let mut line_start = LineStart::new(&self.text[ix..]);
            let n_containers = self.scan_containers(&mut line_start);
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
            last_line_blank = scan_blank_line(&self.text[ix..]).is_some();
        }

        // Trim trailing blank lines.
        if let TreePointer::Valid(child) = last_nonblank_child {
            self.tree[child].next = TreePointer::Nil;
            self.tree[child].item.end = last_nonblank_ix;
        }
        self.pop(end_ix);
        ix
    }

    fn parse_fenced_code_block(&mut self, start_ix: usize, indent: usize,
        fence_ch: u8, n_fence_char: usize) -> usize
    {
        let mut info_start = start_ix + n_fence_char;
        info_start += scan_whitespace_no_nl(&self.text[info_start..]);
        let mut info_end = info_start + scan_nextline(&self.text[info_start..]);
        while info_end > info_start && is_ascii_whitespace(self.text.as_bytes()[info_end - 1]) {
            info_end -= 1;
        }
        let info_string = unescape(&self.text[info_start..info_end]);
        self.tree.append(Item {
            start: start_ix,
            end: 0,  // will get set later
            body: ItemBody::FencedCodeBlock(info_string),
        });
        self.tree.push();
        let mut ix = start_ix + scan_nextline(&self.text[start_ix..]);
        loop {
            let mut line_start = LineStart::new(&self.text[ix..]);
            let n_containers = self.scan_containers(&mut line_start);
            if n_containers < self.tree.spine_len() {
                break;
            }
            line_start.scan_space(indent);
            let mut close_line_start = line_start.clone();
            if !close_line_start.scan_space(4) {
                let close_ix = ix + close_line_start.bytes_scanned();
                if let Some(n) =
                    scan_closing_code_fence(&self.text[close_ix..], fence_ch, n_fence_char)
                {
                    ix = close_ix + n;
                    break;
                }
            }
            let remaining_space = line_start.remaining_space();
            ix += line_start.bytes_scanned();
            let next_ix = ix + scan_nextline(&self.text[ix..]);
            self.append_code_text(remaining_space, ix, next_ix);
            ix = next_ix;
        }

        self.pop(ix);

        // try to read trailing whitespace or it will register as a completely blank line
        ix + scan_blank_line(&self.text[ix..]).unwrap_or(0)
    }

    fn append_code_text(&mut self, remaining_space: usize, start: usize, end: usize) {
        if remaining_space > 0 {
            self.tree.append(Item {
                start: start,
                end: start,
                body: ItemBody::SynthesizeText("   "[..remaining_space].into()),
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
            self.tree.append(Item {
                start: start,
                end: start,
                // TODO: maybe this should synthesize to html rather than text?
                body: ItemBody::SynthesizeText("   "[..remaining_space].into()),
            });
        }
        if self.text.as_bytes()[end - 2] == b'\r' {
            // Normalize CRLF to LF
            self.tree.append(Item {
                start: start,
                end: end - 2,
                body: ItemBody::Html,
            });
            self.tree.append(Item {
                start: end - 1,
                end: end,
                body: ItemBody::Html,
            });
        } else {
            self.tree.append(Item {
                start: start,
                end: end,
                body: ItemBody::Html,
            });
        }
    }

    /// Returns number of containers scanned.
    fn scan_containers(&self, line_start: &mut LineStart) -> usize {
        let mut i = 0;
        for &node_ix in self.tree.walk_spine() {
            match self.tree[node_ix].item.body {
                ItemBody::BlockQuote => {
                    let save = line_start.clone();
                    if !line_start.scan_blockquote_marker() {
                        *line_start = save;
                        break;
                    }
                }
                ItemBody::ListItem(indent) => {
                    let save = line_start.clone();
                    if !(line_start.scan_space(indent) || line_start.is_at_eol()) {
                        *line_start = save;
                        break;
                    }
                }
                ItemBody::Table(..) | ItemBody::TableHead | ItemBody::TableRow |
                ItemBody::TableCell |
                ItemBody::FootnoteDefinition(..) | ItemBody::List(..) |
                ItemBody::Paragraph | ItemBody::IndentCodeBlock |
                ItemBody::FencedCodeBlock(_) | ItemBody::HtmlBlock(_) => (),
                ref node => panic!("unexpected node in tree: {:?}", node),
            }
            i += 1;
        }
        i
    }

    /// Pop a container, setting its end.
    fn pop(&mut self, ix: usize) {
        let cur_ix = self.tree.pop().unwrap();
        self.tree[cur_ix].item.end = ix;
        if let ItemBody::List(true, _, _) = self.tree[cur_ix].item.body {
            surgerize_tight_list(&mut self.tree);
        }
    }

    /// Close a list if it's open. Also set loose if last line was blank
    fn finish_list(&mut self, ix: usize) {
        if let Some(node_ix) = self.tree.peek_up() {
            if let ItemBody::List(_, _, _) = self.tree[node_ix].item.body {
                self.pop(ix);
            }
        }
        if self.last_line_blank {
            if let Some(node_ix) = self.tree.peek_grandparent() {
                if let ItemBody::List(ref mut is_tight, _, _) =
                    self.tree[node_ix].item.body
                {
                    *is_tight = false;
                }
            }
            self.last_line_blank = false;
        }
    }

    /// Continue an existing list or start a new one if there's not an open
    /// list that matches.
    fn continue_list(&mut self, start: usize, ch: u8, index: Option<usize>) {
        if let Some(node_ix) = self.tree.peek_up() {
            if let ItemBody::List(ref mut is_tight, existing_ch, _) =
                self.tree[node_ix].item.body
            {
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
            start: start,
            end: 0,  // will get set later
            body: ItemBody::List(true, ch, index),
        });
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
    fn parse_atx_heading(&mut self, mut ix: usize, atx_level: i32, atx_size: usize) -> usize {
        self.tree.append(Item {
            start: ix,
            end: 0, // set later
            body: ItemBody::Header(atx_level),
        });
        ix += atx_size;
        // next char is space or scan_eol
        // (guaranteed by scan_atx_heading)
        let b = self.text.as_bytes()[ix];
        if b == b'\n' || b == b'\r' {
            ix += scan_eol(&self.text[ix..]).0;
            return ix;
        }
        // skip leading spaces
        let skip_spaces = scan_whitespace_no_nl(&self.text[ix..]);
        ix += skip_spaces;

        // now handle the header text
        let header_start = ix;
        let header_node_idx = self.tree.cur().unwrap(); // so that we can set the endpoint later
        self.tree.push();
        ix = self.parse_line(ix, false).0;
        self.tree[header_node_idx].item.end = ix;

        // remove trailing matter from header text
        // TODO: probably better to find limit before parsing; this makes assumptions
        // about the way the line is parsed.
        let header_text = &self.text[header_start..];
        let mut limit = ix - header_start;
        if limit > 0 && header_text.as_bytes()[limit-1] == b'\n' {
            limit -= 1;
        }
        if limit > 0 && header_text.as_bytes()[limit-1] == b'\r' {
            limit -= 1;
        }
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
        if let TreePointer::Valid(cur_ix) = self.tree.cur() {
            self.tree[cur_ix].item.end = limit + header_start;
        }

        self.tree.pop();
        ix
    }

    /// Returns the number of bytes scanned on success.
    fn parse_footnote(&mut self, start: usize) -> Option<usize> {
        let tail = &self.text[start..];
        if !tail.starts_with("[^") { return None; }
        let (mut i, label) = scan_link_label_rest(&tail[2..])?;
        i += 2;
        if scan_ch(&tail[i..], b':') == 0 {
            return None;
        }
        i += 1;
        self.tree.append(Item {
            start: start,
            end: 0,  // will get set later
            body: ItemBody::FootnoteDefinition(label.clone()), // TODO: check whether the label here is strictly necessary
        });
        self.tree.push();
        Some(i)
    }

    /// Returns number of bytes scanned, label and definition on success.
    fn parse_refdef_total(&mut self, start: usize) -> Option<(usize, LinkLabel<'a>, LinkDef<'a>)> {
        let tail = &self.text[start..];
        if !tail.starts_with('[') { return None; }
        let (mut i, label) = scan_link_label_rest(&tail[1..])?;
        i += 1;
        if scan_ch(&tail[i..], b':') == 0 {
            return None;
        }
        i += 1;
        let (bytes, link_def) = self.scan_refdef(start + i)?;
        Some((bytes + i, UniCase::new(label), link_def))
    }

    /// Returns # of bytes and definition.
    /// Assumes the label of the reference including colon has already been scanned.
    fn scan_refdef(&self, start: usize) -> Option<(usize, LinkDef<'a>)> {
        let mut i = start;

        // whitespace between label and url (including up to one newline)
        let mut newlines = 0;
        for c in self.text[i..].bytes() {
            if c == b'\n' {
                i += 1;
                newlines += 1;
                if newlines > 1 {
                    return None;
                } else {
                    let mut line_start = LineStart::new(&self.text[i..]);
                    let _n_containers = self.scan_containers(&mut line_start);
                    // TODO: what to do with these containers?
                }
            } else if is_ascii_whitespace_no_nl(c) {
                i += 1;
            } else {
                break;
            }
        }

        // scan link dest
        let (dest_length, dest) = scan_link_dest(&self.text[i..])?;
        let dest = unescape(dest);
        i += dest_length;

        // scan whitespace between dest and label
        // FIXME: dedup with similar block above
        newlines = 0;
        let mut whitespace_bytes = 0;
        for c in self.text[i..].bytes() {
            if c == b'\n' {
                whitespace_bytes += 1;
                newlines += 1;
                let mut line_start = LineStart::new(&self.text[(i + whitespace_bytes)..]);
                let _n_containers = self.scan_containers(&mut line_start);
            } else if is_ascii_whitespace_no_nl(c) {
                whitespace_bytes += 1;
            } else {
                break;
            }
        }
        if i == self.text.len() {
            newlines += 1;
        }
        if whitespace_bytes == 0 && newlines == 0 {
            return None;
        }

        // no title
        let mut backup = 
            (i - start,
            LinkDef {
                dest,
                title: None,
            });

        if newlines > 1 {
            return Some(backup);
        } else {
            i += whitespace_bytes;
        }        

        // scan title
        // if this fails but newline == 1, return also a refdef without title
        if let Some((title_length, title)) = self.scan_refdef_title(i) {
            i += title_length;
            backup.1.title = Some(unescape_cow(title));
        } else if newlines > 0 {
            return Some(backup);
        } else {
            return None;
        };

        // scan EOL
        if let Some(bytes) = scan_blank_line(&self.text[i..]) {
            backup.0 = i + bytes - start;
            Some(backup)
        } else if newlines > 0 {
            Some(backup)
        } else {
            None
        }
    }

    // FIXME: use scan_link_title ? but we need an inline scanner
    // and that fn seems to allow blank lines.
    // FIXME: or, reuse scanner::scan_link_title ?
    // TODO: rename. this isnt just for refdef_titles, but all titles
    // returns (bytelength, title_str)
    fn scan_refdef_title(&self, start: usize) -> Option<(usize, CowStr<'a>)> {
        let mut title = String::new();
        let text = &self.text[start..];
        let mut chars = text.chars().peekable();
        let mut still_borrowed = true;
        let closing_delim = match chars.next()? {
            '\'' => '\'',
            '"' => '"',
            '(' => ')',
            _ => return None,
        };
        let mut bytecount = 1;

        while let Some(c) = chars.next() {
            match c {
                '\n' => {
                    if !still_borrowed {
                        title.push(c);
                    }                    
                    bytecount += 1;
                    let mut next = *chars.peek()?;
                    while is_ascii_whitespace_no_nl(next as u8) {
                        if !still_borrowed {
                            title.push(next);
                        }
                        bytecount += chars.next()?.len_utf8();
                        next = *chars.peek()?;
                    }
                    if *chars.peek()? == '\n' {
                        // blank line - not allowed
                        return None;
                    }
                }
                '\\' => {
                    let next_char = chars.next()?;
                    bytecount += 1;
                    if next_char != closing_delim {
                        if still_borrowed {
                            title.push_str(&text[1..bytecount]);
                        }
                        still_borrowed = false;
                        title.push('\\');
                    }
                    if !still_borrowed {
                        title.push(next_char);
                    }                    
                    bytecount += next_char.len_utf8();
                }
                c if c == closing_delim => {
                    let cow = if still_borrowed {
                        text[1..bytecount].into()
                    } else {
                        title.into()
                    };
                    return Some((bytecount + 1, cow));
                }
                c => {
                    if !still_borrowed {
                        title.push(c);
                    }
                    bytecount += c.len_utf8();
                }
            }
        }
        None
    }
}

/// Computes the number of header columns in a table line by computing the number of dividing pipes
/// that aren't followed or preceeded by whitespace.
fn count_header_cols(text: &str, mut pipes: usize, mut start: usize, last_pipe_ix: usize) -> usize {
    // was first pipe preceeded by whitespace? if so, subtract one
    start += scan_whitespace_no_nl(&text[start..]);
    if text.as_bytes()[start] == b'|' {
        pipes -= 1;
    }

    // was last pipe followed by whitespace? if so, sub one
    if scan_blank_line(&text[(last_pipe_ix + 1)..]).is_some() {
        pipes -= 1;
    }

    pipes + 1
}

fn unescape_cow<'a>(c: CowStr<'a>) -> CowStr<'a> {
    match c {
        CowStr::Inlined(s) => unescape_str_line(s),
        CowStr::Boxed(s) => unescape_str_line(s),
        CowStr::Borrowed(s) => unescape(&s),
    }
}

fn unescape_str_line<'a, S: AsRef<str>>(s: S) -> CowStr<'a> {
    match unescape(s.as_ref()) {
        CowStr::Borrowed(s) => {
            if let Ok(inline_str) = InlineStr::try_from_str(s) {
                CowStr::Inlined(inline_str)
            } else {
                s.to_owned().into()
            }
        }
        CowStr::Inlined(s) => CowStr::Inlined(s),
        CowStr::Boxed(s) => CowStr::Boxed(s),
    }
}

impl<'a> Tree<Item<'a>> {
    fn append_text(&mut self, start: usize, end: usize) {
        if end > start {
            if let TreePointer::Valid(ix) = self.cur() {
                if ItemBody::Text == self[ix].item.body && self[ix].item.end == start {
                    self[ix].item.end = end;
                    return;
                }
            }
            self.append(Item {
                start: start,
                end: end,
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

    let prev_char = s[..ix].chars().rev().next().unwrap();

    prev_char.is_whitespace() || is_punctuation(prev_char)
}

/// Determines whether the delimiter run starting at given index is
/// left-flanking, as defined by the commonmark spec (and isn't intraword
/// for _ delims)
fn delim_run_can_close(s: &str, suffix: &str, run_len: usize, ix: usize) -> bool {
    if ix == 0 {
        return false;
    }
    let prev_char = s[..ix].chars().rev().next().unwrap();
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
fn scan_paragraph_interrupt(s: &str) -> bool {
    scan_eol(s).1 ||
    scan_hrule(s) > 0 ||
    scan_atx_heading(s).is_some() ||
    scan_code_fence(s).0 > 0 ||
    get_html_end_tag(s).is_some() ||
    scan_blockquote_start(s) > 0 ||
    is_html_tag(scan_html_block_tag(s).1)
}

fn get_html_end_tag(text : &str) -> Option<&'static str> {
    static BEGIN_TAGS: &'static [&'static str; 3] = &["<script", "<pre", "<style"];
    static END_TAGS: &'static [&'static str; 3] = &["</script>", "</pre>", "</style>"];

    // TODO: Consider using `strcasecmp` here
    'type_1: for (beg_tag, end_tag) in BEGIN_TAGS.iter().zip(END_TAGS.iter()) {
        if text.len() >= beg_tag.len() && text.starts_with("<") {
            for (i, c) in beg_tag.as_bytes()[1..].iter().enumerate() {
                if ! (&text.as_bytes()[i+1] == c || &text.as_bytes()[i+1] == &(c - 32)) {
                    continue 'type_1;
                }
            }

            // Must either be the end of the line...
            if text.len() == beg_tag.len() {
                return Some(end_tag);
            }

            // ...or be followed by whitespace, newline, or '>'.
            let pos = beg_tag.len();
            let s = text.as_bytes()[pos] as char;
            // TODO: I think this should be ASCII whitespace only
            if s.is_whitespace() || s == '>' {
                return Some(end_tag);
            }
        }
    }
    static ST_BEGIN_TAGS: &'static [&'static str; 3] = &["<!--", "<?", "<![CDATA["];
    static ST_END_TAGS: &'static [&'static str; 3] = &["-->", "?>", "]]>"];
    for (beg_tag, end_tag) in ST_BEGIN_TAGS.iter().zip(ST_END_TAGS.iter()) {
        if text.starts_with(&beg_tag[..]) {
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
    start: TreeIndex,  // offset of tree node
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

    fn pop_to<'a>(&mut self, tree: &mut Tree<Item<'a>>, new_len: usize) {
        for el in self.stack.drain(new_len..) {
            for i in 0..el.count {
                tree[el.start + i].item.body = ItemBody::Text;
            }
        }
    }

    fn find_match(&self, c: u8, count: usize, both: bool) -> Option<(usize, InlineEl)> {
        self.stack
            .iter()
            .cloned()
            .enumerate()
            .rev()
            .find(|(_, el)| {
                el.c == c && (!both && !el.both || (count + el.count) % 3 != 0)
            })
    }

    fn push(&mut self, el: InlineEl) {
        self.stack.push(el)
    }

    fn pop(&mut self) -> Option<InlineEl> {
        self.stack.pop()
    }
}

/// An iterator for text in an inline chain.
#[derive(Clone)]
struct InlineScanner<'t, 'a> {
    tree: &'t Tree<Item<'a>>,
    text: &'a str,
    cur: TreePointer,
    ix: usize,
}

impl<'t, 'a> InlineScanner<'t, 'a> {
    fn new(tree: &'t Tree<Item<'a>>, text: &'a str, cur: TreePointer) -> InlineScanner<'t, 'a> {
        let ix = if let TreePointer::Valid(cur_ix) = cur {
            tree[cur_ix].item.start
        } else {
            !0
        };
        InlineScanner { tree, text, cur, ix }
    }

    fn unget(&mut self) {
        self.ix -= 1;
    }

    // Consumes byte if it was next and returns true. Does
    // nothing and returns false otherwise.
    fn scan_ch(&mut self, c: u8) -> bool {
        self.scan_if(|scanned| scanned == c)
    }

    // Note(optimization): could use memchr
    fn scan_upto(&mut self, c: u8) -> usize {
        self.scan_while(|scanned| scanned != c)
    }

    fn scan_if<F>(&mut self, mut f: F) -> bool
    where
        F: FnMut(u8) -> bool,
    {
        if let Some(c) = self.next() {
            if !f(c) {
                self.unget();
            } else {
                return true;
            }
        }
        false
    }

    fn scan_while<F>(&mut self, mut f: F) -> usize
    where
        F: FnMut(u8) -> bool,
    {
        let mut n = 0;
        while let Some(c) = self.next() {
            if !f(c) {
                self.unget();
                break;
            }
            n += 1;
        }
        n
    }

    // Note: will consume the prefix of the string.
    fn scan_str(&mut self, s: &str) -> bool {
        s.as_bytes().iter().all(|b| self.scan_ch(*b))
    }

    fn to_node_and_ix(&self) -> (TreePointer, usize) {
        let mut cur = self.cur;
        if let TreePointer::Valid(cur_ix) = cur {
            if self.tree[cur_ix].item.end == self.ix {
                cur = self.tree[cur_ix].next;
            }
        }
        (cur, self.ix)
    }

    fn next_char(&mut self) -> Option<char> {
        if let TreePointer::Valid(mut cur_ix) = self.cur {
            while self.ix == self.tree[cur_ix].item.end {
                self.cur = self.tree[cur_ix].next;
                if let TreePointer::Valid(new_cur_ix) = self.cur {
                    cur_ix = new_cur_ix;
                    self.ix = self.tree[cur_ix].item.start;
                } else {
                    return None;
                }
            }
            self.text[self.ix..].chars().next().map(|c| {
                self.ix += c.len_utf8();
                c
            })
        } else {
            None
        }
    }
}

impl<'t, 'a> Iterator for InlineScanner<'t, 'a> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        match self.cur {
            TreePointer::Nil => None,
            TreePointer::Valid(mut cur_ix) => {
                while self.ix == self.tree[cur_ix].item.end {
                    self.cur = self.tree[cur_ix].next;
                    match self.cur {
                        TreePointer::Nil => return None,
                        TreePointer::Valid(new_cur_ix) => {
                            cur_ix = new_cur_ix;
                            self.ix = self.tree[cur_ix].item.start;
                        }
                    }
                }
                let c = self.text.as_bytes()[self.ix];
                self.ix += 1;
                Some(c)
            }
        }
    }
}

fn scan_inline_attribute_name(scanner: &mut InlineScanner) -> bool {
    if !scanner.scan_if(|c| is_ascii_alpha(c) || c == b'_' || c == b':') {
        return false;
    }
    scanner.scan_while(|c| is_ascii_alphanumeric(c)
        || c == b'_' || c == b'.' || c == b':' || c == b'-');
    true
}

fn scan_inline_attribute_value(scanner: &mut InlineScanner) -> bool {
    if let Some(c) = scanner.next() {
        if is_ascii_whitespace(c) || c == b'=' || c == b'<' || c == b'>' || c == b'`' {
            scanner.unget();
        } else if c == b'\'' {
            scanner.scan_while(|c| c != b'\'');
            return scanner.scan_ch(b'\'')
        } else if c == b'"' {
            scanner.scan_while(|c| c != b'"');
            return scanner.scan_ch(b'"')
        } else {
            scanner.scan_while(|c| !(is_ascii_whitespace(c)
                || c == b'=' || c == b'<' || c == b'>' || c == b'`' || c == b'\'' || c == b'"'));
            return true;
        }
    }
    false
}

fn scan_inline_attribute(scanner: &mut InlineScanner) -> bool {
    if !scan_inline_attribute_name(scanner) { return false; }
    let n_whitespace = scanner.scan_while(is_ascii_whitespace);
    if scanner.scan_ch(b'=') {
        scanner.scan_while(is_ascii_whitespace);
        return scan_inline_attribute_value(scanner);
    } else if n_whitespace > 0 {
        // Leave whitespace for next attribute.
        scanner.unget();
    }
    true
}

/// Scan comment, declaration, or CDATA section, with initial "<!" already consumed.
fn scan_inline_html_comment(scanner: &mut InlineScanner) -> bool {
    if let Some(c) = scanner.next() {
        if c == b'-' {
            if !scanner.scan_ch(b'-') { return false; }
            // Saw "<!--", scan comment.
            if scanner.scan_ch(b'>') { return false; }
            if scanner.scan_ch(b'-') {
                if scanner.scan_ch(b'>') {
                    return false;
                } else {
                    scanner.unget();
                }
            }
            while scanner.scan_upto(b'-') > 0 {
                scanner.scan_ch(b'-');
                if scanner.scan_ch(b'-') { return scanner.scan_ch(b'>'); }
            }
        } else if c == b'[' {
            if !scanner.scan_str("CDATA[") { return false; }
            loop {
                scanner.scan_upto(b']');
                if !scanner.scan_ch(b']') { return false; }
                if scanner.scan_while(|c| c == b']') > 0 && scanner.scan_ch(b'>') {
                    return true;
                }
            }
        } else {
            // Scan declaration.
            if scanner.scan_while(|c| c >= b'A' && c <= b'Z') == 0 { return false; }
            if scanner.scan_while(is_ascii_whitespace) == 0 { return false; }
            scanner.scan_upto(b'>');
            return scanner.scan_ch(b'>');
        }
    }
    false
}

/// Scan processing directive, with initial "<?" already consumed.
fn scan_inline_html_processing(scanner: &mut InlineScanner) -> bool {
    while let Some(c) = scanner.next() {
        if c == b'?' && scanner.scan_ch(b'>') { return true; }
    }
    false
}

fn scan_inline_html(scanner: &mut InlineScanner) -> bool {
    if let Some(c) = scanner.next() {
        if c == b'!' {
            return scan_inline_html_comment(scanner);
        } else if c == b'?' {
            return scan_inline_html_processing(scanner);
        } else if c == b'/' {
            if !scanner.scan_if(is_ascii_alpha) {
                return false;
            }
            scanner.scan_while(is_ascii_letterdigitdash);
            scanner.scan_while(is_ascii_whitespace);
            return scanner.scan_ch(b'>');
        } else if is_ascii_alpha(c) {
            // open tag (first character of tag consumed)
            scanner.scan_while(is_ascii_letterdigitdash);
            loop {
                let n_whitespace = scanner.scan_while(is_ascii_whitespace);
                if let Some(c) = scanner.next() {
                    if c == b'/' {
                        return scanner.scan_ch(b'>');
                    } else if c == b'>' {
                        return true;
                    } else if n_whitespace == 0 {
                        return false;
                    } else {
                        scanner.unget();
                        if !scan_inline_attribute(scanner) {
                            return false;
                        }
                    }
                } else {
                    return false;
                }
            }
        }
    }
    false
}

/// Make a code span.
///
/// Both `open` and `close` are matching MaybeCode items.
fn make_code_span<'a>(tree: &mut Tree<Item<'a>>, s: &str, open: TreeIndex, close: TreeIndex) {
    tree[open].item.end = tree[close].item.end;
    tree[open].item.body = ItemBody::Code;
    let first = tree[open].next;
    let first_ix = first.unwrap();
    tree[open].next = tree[close].next;
    tree[open].child = first;
    let mut node = first_ix;
    let last;
    loop {
        let next = tree[node].next;
        match tree[node].item.body {
            ItemBody::SoftBreak => {
                // TODO: trailing space is stripped in parse_line, and we don't want it
                // stripped.
                tree[node].item.body = ItemBody::SynthesizeText(" ".into());
            }
            ItemBody::HardBreak => {
                let start = tree[node].item.start;
                if s.as_bytes()[start] == b'\\' {
                    tree[node].item.body = ItemBody::Text;
                    let end = tree[node].item.end;
                    let space = tree.create_node(Item {
                        start: start + 1,
                        end,
                        body: ItemBody::SynthesizeText(" ".into())
                    });
                    tree[space].next = next;
                    tree[node].next = TreePointer::Valid(space);
                    tree[node].item.end = start + 1;
                } else {
                    tree[node].item.body = ItemBody::SynthesizeText(" ".into());
                }
            }
            _ => tree[node].item.body = ItemBody::Text,
        }
        if next == TreePointer::Valid(close) {
            last = node;
            tree[node].next = TreePointer::Nil;
            break;
        }
        node = next.unwrap();
    }
    // Strip opening and closing space, if appropriate.
    let opening = match &tree[first_ix].item.body {
        ItemBody::Text => s.as_bytes()[tree[first_ix].item.start] == b' ',
        ItemBody::SynthesizeText(text) => text.starts_with(' '),
        _ => unreachable!("unexpected item"),
    };
    let closing = match &tree[last].item.body {
        ItemBody::Text => s.as_bytes()[tree[last].item.end - 1] == b' ',
        ItemBody::SynthesizeText(text) => text.ends_with(' '),
        _ => unreachable!("unexpected item"),
    };
    // TODO(spec clarification): This makes n-2 spaces for n spaces input. Correct?
    if opening && closing {
        if tree[first_ix].item.body == ItemBody::SynthesizeText(" ".into())
            || tree[first_ix].item.end - tree[first_ix].item.start == 1
        {
            tree[open].child = tree[first_ix].next;
        } else {
            tree[first_ix].item.start += 1;
        }
        if tree[last].item.body == ItemBody::SynthesizeText(" ".into()) {
            tree[last].item.body = ItemBody::SynthesizeText("".into());
        } else {
            tree[last].item.end -= 1;
        }
        // TODO: if last is now empty, remove it (we have size-0 items in the tree)
    }
}

fn scan_link_destination_plain<'t, 'a>(scanner: &mut InlineScanner<'t, 'a>) -> Option<CowStr<'a>> {
    let mut url = String::new();
    let mut nest = 0;
    let mut bytecount = 0;
    let mut still_borrowed = true;
    let underlying = &scanner.text[scanner.ix..];
    while let Some(mut c) = scanner.next_char() {
        match c {
            '(' => {
                nest += 1;
            }
            ')' => {
                if nest == 0 {
                    scanner.unget();
                    return Some(if still_borrowed { underlying[..bytecount].into() } else { url.into() });
                }
                nest -= 1;
            }
            '\x00'..=' ' => {
                scanner.unget();
                return Some(if still_borrowed { underlying[..bytecount].into() } else { url.into() });
            },
            '\\' => {
                if let Some(c_next) = scanner.next_char() {
                    if !(c_next <= '\x7f' && is_ascii_punctuation(c_next as u8)) {
                        if !still_borrowed {
                            url.push('\\');
                        } else {
                            bytecount += '\\'.len_utf8();
                        }
                    } else if still_borrowed {
                        url.push_str(&underlying[..bytecount]);
                        still_borrowed = false;
                    }
                    c = c_next;
                } else {
                    return None;
                }
            }
            _ => {}
        }
        if still_borrowed {
            bytecount += c.len_utf8();
        } else {
            url.push(c);
        }
    }
    None
}

fn scan_link_destination_pointy<'t, 'a>(scanner: &mut InlineScanner<'t, 'a>) -> Option<CowStr<'a>> {
    if !scanner.scan_ch(b'<') {
        return None;
    }
    let underlying = &scanner.text[scanner.ix..];
    let mut url = String::new();
    let mut still_borrowed = true;
    let mut bytecount = 0;
    while let Some(mut c) = scanner.next_char() {
        match c {
            '>' => {
                return Some(if still_borrowed { underlying[..bytecount].into() } else { url.into() })
            }
            '\x00'..='\x1f' | '<' => return None,
            '\\' => {
                let c_next = scanner.next_char()?;
                if !(c_next <= '\x7f' && is_ascii_punctuation(c_next as u8)) {
                    if !still_borrowed {
                        url.push('\\');
                    } else {
                        bytecount += '\\'.len_utf8()
                    }
                } else if still_borrowed {
                    url.push_str(&underlying[..bytecount]);
                    still_borrowed = false;
                }
                c = c_next;
            }
            _ => {}
        }
        if still_borrowed {
            bytecount += c.len_utf8();
        } else {
            url.push(c);
        }
    }
    None
}

fn scan_link_destination<'t, 'a>(scanner: &mut InlineScanner<'t, 'a>) -> Option<CowStr<'a>> {
    let save = scanner.clone();
    if let Some(url) = scan_link_destination_pointy(scanner) {
        return Some(url);
    }
    *scanner = save;
    scan_link_destination_plain(scanner)
}

fn scan_link_title<'t, 'a>(scanner: &mut InlineScanner<'t, 'a>) -> Option<CowStr<'a>> {
    let open = scanner.next_char()?;
    if !(open == '\'' || open == '\"' || open == '(') {
        return None;
    }
    let underlying = &scanner.text[scanner.ix..];
    let mut title = String::new();
    let mut still_borrowed = true;
    let mut bytecount = 0;
    let mut nest = 0;
    while let Some(mut c) = scanner.next_char() {
        if c == open {
            if open == '(' {
                nest += 1;
            } else {
                return Some(if still_borrowed { underlying[..bytecount].into() } else { title.into() });
            }
        }
        if open == '(' && c == ')' {
            if nest == 0 {
                return Some(if still_borrowed { underlying[..bytecount].into() } else { title.into() });
            } else {
                nest -= 1;
            }
        }
        if c == '\\' {
            let c_next = scanner.next_char()?;
            if !(c_next <= '\x7f' && is_ascii_punctuation(c_next as u8)) {
                if !still_borrowed {
                    title.push('\\');
                } else {
                    bytecount += '\\'.len_utf8()
                }
            } else if still_borrowed {
                title.push_str(&underlying[..bytecount]);
                still_borrowed = false;
            }
            c = c_next;
        }
        if still_borrowed {
            bytecount += c.len_utf8();
        } else {
            title.push(c);
        }
    }
    None
}

fn scan_autolink<'t, 'a>(scanner: &mut InlineScanner<'t, 'a>) -> Option<(CowStr<'a>, LinkType)> {
    let save = scanner.clone();
    let scans = scan_uri(scanner).map(|s| (s, LinkType::Autolink)).or_else(|| {
        *scanner = save.clone();
        scan_email(scanner).map(|s| (s, LinkType::Email))
    });
    if let Some(uri) = scans {
        if scanner.scan_ch(b'>') {
            return Some(uri);
        }
    }
    *scanner = save;
    None
}

// must return scanner to original state -- this doesnt seem true?
// TODO: such invariants should probably be captured by the type system
fn scan_uri<'t, 'a>(scanner: &mut InlineScanner<'t, 'a>) -> Option<CowStr<'a>> {
    let start_ix = scanner.ix;

    // scheme's first byte must be an ascii letter
    let first = scanner.next()?;
    if !is_ascii_alpha(first) {
        return None;
    }

    while let Some(c) = scanner.next() {
        match c {
            c if is_ascii_alphanumeric(c) => (),
            b'.' | b'-' | b'+' => (),
            b':' => break,
            _ => return None,
        }
    }

    // scheme length must be between 2 and 32 characters long. scheme
    // must be followed by colon
    let uri_len = scanner.ix - start_ix;
    if uri_len < 3 || uri_len > 33  {
        return None;
    }

    let mut ended = false;
    while let Some(c) = scanner.next() {
        match c {
            b'\0' ... b' ' => {
                ended = true;
            }
            b'>' | b'<' => break,
            _ if ended => return None,
            _ => (),
        }
    };
    scanner.unget();

    Some(scanner.text[start_ix..scanner.ix].into())
}

fn scan_email<'t, 'a>(scanner: &mut InlineScanner<'t, 'a>) -> Option<CowStr<'a>> {
    // using a regex library would be convenient, but doing it by hand is not too bad
    let start_ix = scanner.ix;

    while let Some(c) = scanner.next() {
        match c {
            c if is_ascii_alphanumeric(c) => (),
            b'.' | b'!' | b'#' | b'$' | b'%' | b'&' | b'\'' | b'*' | b'+' | b'/' |
            b'=' | b'?' | b'^' | b'_' | b'`' | b'{' | b'|' | b'}' | b'~' | b'-' => (),
            _ => break,
        }
    }

    if scanner.ix == start_ix || scanner.text.as_bytes()[scanner.ix - 1] != b'@' {
        return None;
    }

    loop {
        let label_start_ix = scanner.ix;
        let mut fresh_label = true;

        while let Some(c) = scanner.next() {
            match c {
                c if is_ascii_alphanumeric(c) => (),
                b'-' if fresh_label => {
                    return None;
                }
                b'-' => (),
                _ => {
                    scanner.unget();
                    break;
                }
            }
            fresh_label = false;
        }
    
        if scanner.ix == label_start_ix || scanner.ix - label_start_ix > 63
            || scanner.text.as_bytes()[scanner.ix - 1] == b'-' {
            return None;
        }

        if !scanner.scan_ch(b'.') {
            break;
        }
    }

    Some(scanner.text[start_ix..scanner.ix].into())
}

#[derive(Debug, Clone)]
enum RefScan<'a> {
    // label, next node index
    LinkLabel(CowStr<'a>, TreePointer),
    // contains next node index
    Collapsed(TreePointer),
    Failed,
}

fn scan_reference<'a, 'b>(tree: &'a Tree<Item<'a>>, text: &'b str, cur: TreePointer) -> RefScan<'b> {
    let cur_ix = match cur {
        TreePointer::Nil => return RefScan::Failed,
        TreePointer::Valid(cur_ix) => cur_ix,
    };
    let start = tree[cur_ix].item.start;
    
    if text[start..].starts_with("[]") {
        let closing_node = tree[cur_ix].next.unwrap();
        RefScan::Collapsed(tree[closing_node].next)
    } else if let Some((ix, ReferenceLabel::Link(label))) = scan_link_label(&text[start..]) {
        let mut scanner = InlineScanner::new(tree, text, cur);
        for _ in 0..ix { scanner.next(); } // move to right node in tree
        let next_node = tree[scanner.cur.unwrap()].next;
        RefScan::LinkLabel(label, next_node)
    } else {
        RefScan::Failed
    }
}

/// Returns url and title cows.
fn scan_inline_link<'t, 'a>(scanner: &mut InlineScanner<'t, 'a>) -> Option<(CowStr<'a>, CowStr<'a>)> {
    if !scanner.scan_ch(b'(') {
        return None;
    }
    scanner.scan_while(is_ascii_whitespace);
    let url = scan_link_destination(scanner)?;
    let mut title = "".into();
    let save = scanner.clone();
    if scanner.scan_while(is_ascii_whitespace) > 0 {
        if let Some(t) = scan_link_title(scanner) {
            title = t;
            scanner.scan_while(is_ascii_whitespace);
        } else {
            *scanner = save;
        }
    }
    if !scanner.scan_ch(b')') {
        return None;
    }
    // in the worst case, title/ url is already owned, and we allocated
    // *again* on unescaping. Can this be avoided?
    Some((unescape_cow(url), unescape_cow(title)))
}

struct LinkStackEl {
    node: TreeIndex,
    ty: LinkStackTy,
}

#[derive(PartialEq)]
enum LinkStackTy {
    Link,
    Image,
    Disabled,
}

struct LinkDef<'a> {
    dest: CowStr<'a>,
    title: Option<CowStr<'a>>,
}

pub struct Parser<'a> {
    text: &'a str,
    tree: Tree<Item<'a>>,
    refdefs: HashMap<LinkLabel<'a>, LinkDef<'a>>,
    broken_link_callback: Option<&'a Fn(&str, &str) -> Option<(String, String)>>,
    offset: usize,
}

impl<'a> Parser<'a> {
    pub fn new(text: &'a str) -> Parser<'a> {
        Parser::new_ext(text, Options::empty())
    }

    pub fn new_ext(text: &'a str, options: Options) -> Parser<'a> {
        Parser::new_with_broken_link_callback(text, options, None)
    }

    /// In case the parser encounters any potential links that have a broken
    /// reference (e.g `[foo]` when there is no `[foo]: ` entry at the bottom)
    /// the provided callback will be called with the reference name,
    /// and the returned pair will be used as the link name and title if not
    /// None.
    pub fn new_with_broken_link_callback(
        text: &'a str,
        options: Options,
        broken_link_callback: Option<&'a Fn(&str, &str) -> Option<(String, String)>>
    ) -> Parser<'a> {
        let first_pass = FirstPass::new(text, options);
        let (mut tree, refdefs) = first_pass.run();
        tree.reset();
        Parser { text, tree, refdefs, broken_link_callback, offset: 0 }
    }

    pub fn get_offset(&self) -> usize {
        self.offset
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
        let mut link_stack = Vec::new();
        let mut cur = self.tree.cur();
        let mut prev = TreePointer::Nil;

        while let TreePointer::Valid(mut cur_ix) = cur {
            match self.tree[cur_ix].item.body {
                ItemBody::MaybeHtml => {
                    let next = self.tree[cur_ix].next;
                    let scanner = &mut InlineScanner::new(&self.tree, self.text, next);

                    if let Some((uri, link_type)) = scan_autolink(scanner) {
                        let (node, ix) = scanner.to_node_and_ix();
                        let text_node = self.tree.create_node(Item {
                            start: self.tree[cur_ix].item.start + 1,
                            end: ix - 1,
                            body: ItemBody::Text,
                        });
                        self.tree[cur_ix].item.body = ItemBody::Link(link_type, uri, "".into());
                        self.tree[cur_ix].item.end = ix;
                        self.tree[cur_ix].next = node;
                        self.tree[cur_ix].child = TreePointer::Valid(text_node);
                        cur = node;
                        if let TreePointer::Valid(node_ix) = cur {
                            self.tree[node_ix].item.start = ix;
                        }
                        continue;
                    } else if scan_inline_html(scanner) {
                        let (node, ix) = scanner.to_node_and_ix();
                        // TODO: this logic isn't right if the replaced chain has
                        // tricky stuff (skipped containers, replaced nulls).
                        self.tree[cur_ix].item.body = ItemBody::InlineHtml;
                        self.tree[cur_ix].item.end = ix;
                        self.tree[cur_ix].next = node;
                        cur = node;
                        if let TreePointer::Valid(node_ix) = cur {
                            self.tree[node_ix].item.start = ix;
                        }
                        continue;
                    }
                    self.tree[cur_ix].item.body = ItemBody::Text;
                }
                ItemBody::MaybeCode(mut count) => {
                    if let TreePointer::Valid(prev_ix) = prev {
                        if self.tree[prev_ix].item.body == ItemBody::Backslash {
                            count -= 1;
                        }
                    }
                    let mut scan = if count > 0 { self.tree[cur_ix].next } else { TreePointer::Nil };
                    // TODO(performance): this has quadratic pathological behavior, I think
                    while let TreePointer::Valid(scan_ix) = scan {
                        if self.tree[scan_ix].item.body == ItemBody::MaybeCode(count) {
                            make_code_span(&mut self.tree, self.text, cur_ix, scan_ix);
                            break;
                        }
                        scan = self.tree[scan_ix].next;
                    }
                    if scan == TreePointer::Nil {
                        self.tree[cur_ix].item.body = ItemBody::Text;
                    }
                }
                ItemBody::MaybeLinkOpen => {
                    self.tree[cur_ix].item.body = ItemBody::Text;
                    link_stack.push( LinkStackEl { node: cur_ix, ty: LinkStackTy::Link });
                }
                ItemBody::MaybeImage => {
                    self.tree[cur_ix].item.body = ItemBody::Text;
                    link_stack.push( LinkStackEl { node: cur_ix, ty: LinkStackTy::Image });
                }
                ItemBody::MaybeLinkClose => {
                    if let Some(tos) = link_stack.last() {
                        if tos.ty == LinkStackTy::Disabled {
                            self.tree[cur_ix].item.body = ItemBody::Text;
                            link_stack.pop();
                            continue;
                        }
                        let next = self.tree[cur_ix].next;
                        let scanner = &mut InlineScanner::new(&self.tree, self.text, next);

                        if let Some((url, title)) = scan_inline_link(scanner) {
                            let (next_node, next_ix) = scanner.to_node_and_ix();
                            if let TreePointer::Valid(prev_ix) = prev {
                                self.tree[prev_ix].next = TreePointer::Nil;
                            }                            
                            cur = TreePointer::Valid(tos.node);
                            cur_ix = tos.node;
                            self.tree[cur_ix].item.body = if tos.ty == LinkStackTy::Image {
                                ItemBody::Image(LinkType::Inline, url.into(), title.into())
                            } else {
                                ItemBody::Link(LinkType::Inline, url.into(), title.into())
                            };
                            self.tree[cur_ix].child = self.tree[cur_ix].next;
                            self.tree[cur_ix].next = next_node;
                            if let TreePointer::Valid(next_node_ix) = next_node {
                                self.tree[next_node_ix].item.start = next_ix;
                            }

                            if tos.ty == LinkStackTy::Link {
                                for el in &mut link_stack {
                                    if el.ty == LinkStackTy::Link {
                                        el.ty = LinkStackTy::Disabled;
                                    }
                                }
                            }
                            link_stack.pop();

                        } else {
                            // ok, so its not an inline link. maybe it is a reference
                            // to a defined link?
                            let scan_result = scan_reference(&self.tree, &self.text, next);
                            let label_node = self.tree[tos.node].next;
                            let node_after_link = match scan_result {
                                RefScan::LinkLabel(_, next_node) => next_node,
                                RefScan::Collapsed(next_node) => next_node,
                                RefScan::Failed => next,
                            };
                            let link_type = match &scan_result {
                                RefScan::LinkLabel(..) => LinkType::Reference,
                                RefScan::Collapsed(..) => LinkType::Collapsed,
                                RefScan::Failed => LinkType::Shortcut,
                            };
                            let label: Option<ReferenceLabel<'a>> = match scan_result {
                                RefScan::LinkLabel(l, ..) => Some(ReferenceLabel::Link(l)),
                                RefScan::Collapsed(..) | RefScan::Failed => {
                                    // No label? maybe it is a shortcut reference
                                    let start = self.tree[tos.node].item.end - 1;
                                    let end = self.tree[cur_ix].item.end;
                                    let search_text = &self.text[start..end];

                                    scan_link_label(search_text).map(|(_ix, label)| label)
                                }
                            };

                            // see if it's a footnote reference
                            if let Some(ReferenceLabel::Footnote(l)) = label {
                                self.tree[tos.node].next = node_after_link;
                                self.tree[tos.node].child = TreePointer::Nil;
                                self.tree[tos.node].item.body = ItemBody::FootnoteReference(l);
                                prev = TreePointer::Valid(tos.node);
                                cur = node_after_link;
                                link_stack.clear();
                                continue;
                            }
                            else if let Some(ReferenceLabel::Link(link_label)) = label {
                                let type_url_title = if let Some(matching_def) = self.refdefs.get(&UniCase::new(link_label.as_ref().into())) {
                                    // found a matching definition!
                                    let title = matching_def.title.as_ref().cloned().unwrap_or("".into());
                                    let url = matching_def.dest.clone();
                                    Some((link_type, url, title))
                                } else if let Some(callback) = self.broken_link_callback {
                                    // looked for matching definition, but didn't find it. try to fix
                                    // link with callback, if it is defined
                                    if let Some((url, title)) = callback(link_label.as_ref(), link_label.as_ref()) {
                                        Some((link_type.to_unknown(), url.into(), title.into()))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                };

                                if let Some((def_link_type, url, title)) = type_url_title {
                                    self.tree[tos.node].item.body = if tos.ty == LinkStackTy::Image {
                                        ItemBody::Image(def_link_type, url, title)
                                    } else {
                                        ItemBody::Link(def_link_type, url, title)
                                    };

                                    // lets do some tree surgery to add the link to the tree
                                    // 1st: skip the label node and close node
                                    self.tree[tos.node].next = node_after_link;

                                    // then, add the label node as a child to the link node
                                    self.tree[tos.node].child = label_node;

                                    // finally: disconnect list of children
                                    if let TreePointer::Valid(prev_ix) = prev {
                                        self.tree[prev_ix].next = TreePointer::Nil;
                                    }                                

                                    // set up cur so next node will be node_after_link
                                    cur = TreePointer::Valid(tos.node);
                                    cur_ix = tos.node;

                                    if tos.ty == LinkStackTy::Link {
                                        for el in &mut link_stack {
                                            if el.ty == LinkStackTy::Link {
                                                el.ty = LinkStackTy::Disabled;
                                            }
                                        }
                                    }
                                } else {
                                    self.tree[cur_ix].item.body = ItemBody::Text;
                                }
                            } else {
                                self.tree[cur_ix].item.body = ItemBody::Text;
                            }
                            link_stack.pop();
                        }
                    } else {
                        self.tree[cur_ix].item.body = ItemBody::Text;
                    }
                }
                _ => (),
            }
            prev = cur;
            cur = self.tree[cur_ix].next;
        }
    }

    fn handle_emphasis(&mut self) {
        let mut stack = InlineStack::new();
        let mut prev = TreePointer::Nil;
        let mut prev_ix: TreeIndex;
        let mut cur = self.tree.cur();
        while let TreePointer::Valid(mut cur_ix) = cur {
            if let ItemBody::MaybeEmphasis(mut count, can_open, can_close) = self.tree[cur_ix].item.body {
                let c = self.text.as_bytes()[self.tree[cur_ix].item.start];
                let both = can_open && can_close;
                if can_close {
                    while let Some((j, el)) = stack.find_match(c, count, both) {
                        // have a match!
                        if let TreePointer::Valid(prev_ix) = prev {
                            self.tree[prev_ix].next = TreePointer::Nil;
                        }                        
                        let match_count = ::std::cmp::min(count, el.count);
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
                            self.tree[root].child = TreePointer::Valid(start);
                            self.tree[root].next = TreePointer::Nil;
                            start = root;
                        }

                        // set next for top most emph level
                        prev_ix = el.start + el.count - match_count;
                        prev = TreePointer::Valid(prev_ix);
                        cur = self.tree[cur_ix + match_count - 1].next;
                        self.tree[prev_ix].next = cur;

                        stack.pop_to(&mut self.tree, j + 1);
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
                        if count > 0 {
                            cur_ix = cur.unwrap();
                        } else {
                            break;
                        }
                    }
                }
                if count > 0 {
                    if can_open {
                        stack.push(InlineEl {
                            start: cur_ix,
                            count: count,
                            c: c,
                            both: both,
                        });
                    } else {
                        for i in 0..count {
                            self.tree[cur_ix + i].item.body = ItemBody::Text;
                        }
                    }
                    prev_ix = cur_ix + count - 1;
                    prev = TreePointer::Valid(prev_ix);
                    cur = self.tree[prev_ix].next;
                }
            } else {
                prev = cur;
                cur = self.tree[cur_ix].next;
            }
        }
        stack.pop_to(&mut self.tree, 0);
    }
}

fn item_to_tag<'a>(item: &Item<'a>) -> Option<Tag<'a>> {
    match item.body {
        ItemBody::Paragraph => Some(Tag::Paragraph),
        ItemBody::Code => Some(Tag::Code),
        ItemBody::Emphasis => Some(Tag::Emphasis),
        ItemBody::Strong => Some(Tag::Strong),
        ItemBody::Strikethrough => Some(Tag::Strikethrough),
        ItemBody::Link(ref link_type, ref url, ref title) =>
            Some(Tag::Link(*link_type, url.clone(), title.clone())),
        ItemBody::Image(ref link_type, ref url, ref title) =>
            Some(Tag::Image(*link_type, url.clone(), title.clone())),
        ItemBody::Rule => Some(Tag::Rule),
        ItemBody::Header(level) => Some(Tag::Header(level)),
        ItemBody::FencedCodeBlock(ref info_string) =>
            Some(Tag::CodeBlock(info_string.clone())),
        ItemBody::IndentCodeBlock => Some(Tag::CodeBlock("".into())),
        ItemBody::BlockQuote => Some(Tag::BlockQuote),
        ItemBody::List(_, _, listitem_start) => Some(Tag::List(listitem_start)),
        ItemBody::ListItem(_) => Some(Tag::Item),
        ItemBody::HtmlBlock(_) => Some(Tag::HtmlBlock),
        ItemBody::TableHead => Some(Tag::TableHead),
        ItemBody::TableCell => Some(Tag::TableCell),
        ItemBody::TableRow => Some(Tag::TableRow),
        ItemBody::Table(ref alignment) => Some(Tag::Table(alignment.clone())),
        ItemBody::FootnoteDefinition(ref label) =>
            Some(Tag::FootnoteDefinition(label.clone())),
        _ => None,
    }
}

// leaf items only
fn item_to_event<'a>(item: &Item<'a>, text: &'a str) -> Event<'a> {
    match item.body {
        ItemBody::Text => {
            Event::Text(text[item.start..item.end].into())
        }
        ItemBody::SynthesizeText(ref text) => {
            Event::Text(text.clone())
        }
        ItemBody::Html => {
            Event::Html(text[item.start..item.end].into())
        }
        ItemBody::InlineHtml => {
            Event::InlineHtml(text[item.start..item.end].into())
        }
        ItemBody::SoftBreak => Event::SoftBreak,
        ItemBody::HardBreak => Event::HardBreak,
        ItemBody::FootnoteReference(ref l) => Event::FootnoteReference(l.clone()),
        _ => panic!("unexpected item body {:?}", item.body)
    }
}

// https://english.stackexchange.com/a/285573
// tree.cur points to a List<_, _, _, false> Item Node
fn surgerize_tight_list<'a>(tree : &mut Tree<Item<'a>>) {
    let mut this_listitem = tree[tree.cur().unwrap()].child;
    while let TreePointer::Valid(listitem_ix) = this_listitem {
        if let ItemBody::ListItem(_) = tree[listitem_ix].item.body {
            // first child is special, controls how we repoint this_listitem.child
            let this_listitem_firstborn = tree[listitem_ix].child;
            if let TreePointer::Valid(firstborn_ix) = this_listitem_firstborn {
                if let ItemBody::Paragraph = tree[firstborn_ix].item.body {
                    // paragraphs should always have children
                    tree[listitem_ix].child = tree[firstborn_ix].child;
                }

                let mut this_listitem_child = TreePointer::Valid(firstborn_ix);
                let mut node_to_repoint = TreePointer::Nil;
                while let TreePointer::Valid(child_ix) = this_listitem_child {
                    // surgerize paragraphs
                    if let ItemBody::Paragraph = tree[child_ix].item.body {
                        let this_listitem_child_firstborn = tree[child_ix].child;
                        if let TreePointer::Valid(repoint_ix) = node_to_repoint {
                            tree[repoint_ix].next = this_listitem_child_firstborn;
                        }
                        let mut this_listitem_child_lastborn = this_listitem_child_firstborn;
                        while let TreePointer::Valid(lastborn_next_ix) = tree[this_listitem_child_lastborn.unwrap()].next {
                            this_listitem_child_lastborn = TreePointer::Valid(lastborn_next_ix);
                        }
                        node_to_repoint = this_listitem_child_lastborn;
                    } else {
                        node_to_repoint = this_listitem_child;
                    }

                    tree[node_to_repoint.unwrap()].next = tree[child_ix].next;
                    this_listitem_child = tree[child_ix].next;
                }
            } // listitems should always have children, let this pass during testing
        } // failure should be a panic, but I'll let it pass during testing

        this_listitem = tree[listitem_ix].next;
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Event<'a>;

    // TODO: this should probably be destructive. actually remove items from the tree
    // so we don't have to clone owned items (Strings)
    fn next(&mut self) -> Option<Event<'a>> {
        match self.tree.cur() {
            TreePointer::Nil => {
                let ix = self.tree.pop()?;
                let tag = item_to_tag(&self.tree[ix].item).unwrap();
                self.offset = self.tree[ix].item.end;
                self.tree.next_sibling();
                return Some(Event::End(tag));
            }
            TreePointer::Valid(mut cur_ix) => {
                if let ItemBody::Backslash = self.tree[cur_ix].item.body {
                    if let TreePointer::Valid(next) = self.tree.next_sibling() {
                        cur_ix = next;
                    }
                }
                if self.tree[cur_ix].item.body.is_inline() {
                    self.handle_inline();
                }
            }
        }

        if let TreePointer::Valid(cur_ix) = self.tree.cur() {
            if let Some(tag) = item_to_tag(&self.tree[cur_ix].item) {
                self.offset = if let TreePointer::Valid(child_ix) = self.tree[cur_ix].child {
                    self.tree[child_ix].item.start
                } else {
                    self.tree[cur_ix].item.end
                };
                self.tree.push();                
                Some(Event::Start(tag))
            } else {
                self.tree.next_sibling();
                self.offset = self.tree[cur_ix].item.end;
                Some(item_to_event(&self.tree[cur_ix].item, self.text))
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tree::Node;

    #[test]
    #[cfg(target_pointer_width = "64")]
    fn node_size() {
        let node_size = std::mem::size_of::<Node<Item>>();
        assert_eq!(88, node_size);
    }

    #[test]
    fn single_open_fish_bracket() {
        // dont crash
        assert_eq!(3, Parser::new("<").count());
    }

    #[test]
    fn link_def_at_eof() {
        let test_str = "[My site][world]\n\n[world]: https://vincentprouillet.com";
        let expected = "<p><a href=\"https://vincentprouillet.com\">My site</a></p>\n";

        let mut buf = String::new();
        crate::html::push_html(&mut buf, Parser::new(test_str));
        assert_eq!(expected, buf);
    }

    #[test]
    fn simple_broken_link_callback() {
        let test_str = "This is a link w/o def: [hello][world]";
        let parser = Parser::new_with_broken_link_callback(test_str, Options::empty(), Some(&|norm, raw| {
            assert_eq!("world", raw);
            assert_eq!("world", norm);
            Some(("YOLO".to_owned(), "SWAG".to_owned()))
        }));
        let mut link_tag_count = 0;
        for (typ, url, title) in parser.filter_map(|event| match event {
            Event::Start(tag) | Event::End(tag) => match tag {
                Tag::Link(typ, url, title) => Some((typ, url, title)),
                _ => None,
            }
            _ => None,
        }) {
            link_tag_count += 1;
            assert_eq!(typ, LinkType::ReferenceUnknown);
            assert_eq!(url.as_ref(), "YOLO");
            assert_eq!(title.as_ref(), "SWAG");
        }
        assert!(link_tag_count > 0);
    }
}
