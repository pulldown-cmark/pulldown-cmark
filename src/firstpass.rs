//! The first pass resolves all block structure, generating an AST. Within a block, items
//! are in a linear chain with potential inline markup identified.

use std::cmp::max;
use std::ops::Range;

use crate::parse::{scan_containers, Allocations, HeadingAttributes, Item, ItemBody, LinkDef};
use crate::scanners::*;
use crate::strings::CowStr;
use crate::tree::{Tree, TreeIndex};
use crate::Options;
use crate::{
    linklabel::{scan_link_label_rest, LinkLabel},
    HeadingLevel,
};

use unicase::UniCase;

/// Runs the first pass, which resolves the block structure of the document,
/// and returns the resulting tree.
pub(crate) fn run_first_pass(text: &str, options: Options) -> (Tree<Item>, Allocations) {
    // This is a very naive heuristic for the number of nodes
    // we'll need.
    let start_capacity = max(128, text.len() / 32);
    let lookup_table = &create_lut(&options);
    let first_pass = FirstPass {
        text,
        tree: Tree::with_capacity(start_capacity),
        begin_list_item: false,
        last_line_blank: false,
        allocs: Allocations::new(),
        options,
        lookup_table,
    };
    first_pass.run()
}

/// State for the first parsing pass.
struct FirstPass<'a, 'b> {
    text: &'a str,
    tree: Tree<Item>,
    begin_list_item: bool,
    last_line_blank: bool,
    allocs: Allocations<'a>,
    options: Options,
    lookup_table: &'b LookupTable,
}

impl<'a, 'b> FirstPass<'a, 'b> {
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
            // finish footnote if it's still open and was preceded by blank line
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
            if starts_html_block_type_6(&bytes[(ix + 1)..]) {
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
            self.allocs.refdefs.0.entry(label).or_insert(link_def);
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
            let start_ix = ix;
            ix += scan_whitespace_no_nl(&bytes[ix..]);

            if let Some(eol_bytes) = scan_eol(&bytes[ix..]) {
                ix += eol_bytes;
                break;
            }

            let cell_ix = self.tree.append(Item {
                start: start_ix,
                end: ix,
                body: ItemBody::TableCell,
            });
            self.tree.push();
            let (next_ix, _brk) = self.parse_line(ix, None, TableParseMode::Active);

            if let Some(cur_ix) = self.tree.cur() {
                let trailing_whitespace = scan_rev_while(&bytes[..next_ix], is_ascii_whitespace);
                self.tree[cur_ix].item.end -= trailing_whitespace;
            }

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
            self.tree[cell_ix].next = None;
        }

        self.pop(ix);

        (ix, row_ix)
    }

    /// Returns first offset after the row and the tree index of the row.
    fn parse_table_row(&mut self, mut ix: usize, row_cells: usize) -> Option<(usize, TreeIndex)> {
        let bytes = self.text.as_bytes();
        let mut line_start = LineStart::new(&bytes[ix..]);
        let current_container =
            scan_containers(&self.tree, &mut line_start) == self.tree.spine_len();
        if !current_container {
            return None;
        }
        line_start.scan_all_space();
        ix += line_start.bytes_scanned();
        if scan_paragraph_interrupt(&bytes[ix..], current_container) {
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
            let (next_ix, brk) = self.parse_line(ix, None, scan_mode);

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
            let current_container =
                scan_containers(&self.tree, &mut line_start) == self.tree.spine_len();
            if !line_start.scan_space(4) {
                let ix_new = ix + line_start.bytes_scanned();
                if current_container {
                    let trailing_backslash_pos = match brk {
                        Some(Item {
                            start,
                            body: ItemBody::HardBreak,
                            ..
                        }) if bytes[start] == b'\\' => Some(start),
                        _ => None,
                    };
                    if let Some(ix_setext) =
                        self.parse_setext_heading(ix_new, node_ix, trailing_backslash_pos.is_some())
                    {
                        if let Some(pos) = trailing_backslash_pos {
                            self.tree.append_text(pos, pos + 1);
                        }
                        ix = ix_setext;
                        break;
                    }
                }
                // first check for non-empty lists, then for other interrupts
                let suffix = &bytes[ix_new..];
                if scan_paragraph_interrupt(suffix, current_container) {
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
    fn parse_setext_heading(
        &mut self,
        ix: usize,
        node_ix: TreeIndex,
        has_trailing_content: bool,
    ) -> Option<usize> {
        let bytes = self.text.as_bytes();
        let (n, level) = scan_setext_heading(&bytes[ix..])?;
        let mut attrs = None;

        if let Some(cur_ix) = self.tree.cur() {
            let parent_ix = self.tree.peek_up().unwrap();
            let header_start = self.tree[parent_ix].item.start;
            // Note that `self.tree[parent_ix].item.end` might be zero at this point.
            // Use the end position of the current node (i.e. the last known child
            // of the parent) instead.
            let header_end = self.tree[cur_ix].item.end;

            // extract the trailing attribute block
            let (content_end, attrs_) =
                self.extract_and_parse_heading_attribute_block(header_start, header_end);
            attrs = attrs_;

            // strip trailing whitespace
            let new_end = if has_trailing_content {
                content_end
            } else {
                let trailing_ws =
                    scan_rev_while(&bytes[header_start..content_end], is_ascii_whitespace_no_nl);
                content_end - trailing_ws
            };

            if attrs.is_some() {
                // remove trailing block attributes
                self.tree.truncate_siblings(self.text.as_bytes(), new_end);
            }

            if let Some(cur_ix) = self.tree.cur() {
                self.tree[cur_ix].item.end = new_end;
            }
        }

        self.tree[node_ix].item.body = ItemBody::Heading(
            level,
            attrs.map(|attrs| self.allocs.allocate_heading(attrs)),
        );

        Some(ix + n)
    }

    /// Parse a line of input, appending text and items to tree.
    ///
    /// Returns: index after line and an item representing the break.
    fn parse_line(
        &mut self,
        start: usize,
        end: Option<usize>,
        mode: TableParseMode,
    ) -> (usize, Option<Item>) {
        let bytes = self.text.as_bytes();
        let bytes = match end {
            Some(end) => &bytes[..end],
            None => bytes,
        };
        let bytes_len = bytes.len();
        let mut pipes = 0;
        let mut last_pipe_ix = start;
        let mut begin_text = start;

        let (final_ix, brk) = iterate_special_bytes(self.lookup_table, bytes, start, |ix, byte| {
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
                        if scan_containers(&self.tree, &mut line_start) == self.tree.spine_len() {
                            let table_head_ix = next_line_ix + line_start.bytes_scanned();
                            let (table_head_bytes, alignment) =
                                scan_table_head(&bytes[table_head_ix..]);

                            if table_head_bytes > 0 {
                                // computing header count from number of pipes
                                let header_count =
                                    count_header_cols(bytes, pipes, start, last_pipe_ix);

                                // make sure they match the number of columns we find in separator line
                                if alignment.len() == header_count {
                                    let alignment_ix = self.allocs.allocate_alignment(alignment);
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
                    if trailing_backslashes % 2 == 1 && end_ix < bytes_len {
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
                    if ix + 1 < bytes_len && is_ascii_punctuation(bytes[ix + 1]) {
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
                    let is_valid_seq = c != b'~' || count <= 2;

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
                    if ix + 1 < bytes_len && bytes[ix + 1] == b'[' {
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
        // TODO: info strings are typically very short. wouldn't it be faster
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
    fn parse_atx_heading(&mut self, start: usize, atx_level: HeadingLevel) -> usize {
        let mut ix = start;
        let heading_ix = self.tree.append(Item {
            start,
            end: 0,                    // set later
            body: ItemBody::default(), // set later
        });
        ix += atx_level as usize;
        // next char is space or eol (guaranteed by scan_atx_heading)
        let bytes = self.text.as_bytes();
        if let Some(eol_bytes) = scan_eol(&bytes[ix..]) {
            self.tree[heading_ix].item.end = ix + eol_bytes;
            self.tree[heading_ix].item.body = ItemBody::Heading(atx_level, None);
            return ix + eol_bytes;
        }
        // skip leading spaces
        let skip_spaces = scan_whitespace_no_nl(&bytes[ix..]);
        ix += skip_spaces;

        // now handle the header text
        let header_start = ix;
        let header_node_idx = self.tree.push(); // so that we can set the endpoint later

        // trim the trailing attribute block before parsing the entire line, if necessary
        let (end, content_end, attrs) = if self.options.contains(Options::ENABLE_HEADING_ATTRIBUTES)
        {
            // the start of the next line is the end of the header since the
            // header cannot have line breaks
            let header_end = header_start + scan_nextline(&bytes[header_start..]);
            let (content_end, attrs) =
                self.extract_and_parse_heading_attribute_block(header_start, header_end);
            self.parse_line(ix, Some(content_end), TableParseMode::Disabled);
            (header_end, content_end, attrs)
        } else {
            ix = self.parse_line(ix, None, TableParseMode::Disabled).0;
            (ix, ix, None)
        };
        self.tree[header_node_idx].item.end = end;

        // remove trailing matter from header text
        if let Some(cur_ix) = self.tree.cur() {
            // remove closing of the ATX heading
            let header_text = &bytes[header_start..content_end];
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
        self.tree[heading_ix].item.body = ItemBody::Heading(
            atx_level,
            attrs.map(|attrs| self.allocs.allocate_heading(attrs)),
        );
        end
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
            let current_container =
                scan_containers(&self.tree, &mut line_start) == self.tree.spine_len();
            let bytes_scanned = line_start.bytes_scanned();
            let suffix = &bytes[bytes_scanned..];
            if scan_paragraph_interrupt(suffix, current_container) {
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
        let (bytecount, link_def) = self.scan_refdef(start, start + i)?;
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
    fn scan_refdef(&self, span_start: usize, start: usize) -> Option<(usize, LinkDef<'a>)> {
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
        let mut backup = (
            i - start,
            LinkDef {
                dest,
                title: None,
                span: span_start..i,
            },
        );

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
            backup.1.span = span_start..i;
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

    /// Extracts and parses a heading attribute block if exists.
    ///
    /// Returns `(end_offset_of_heading_content, (id, classes))`.
    ///
    /// If `header_end` is less than or equal to `header_start`, the given
    /// input is considered as empty.
    fn extract_and_parse_heading_attribute_block(
        &mut self,
        header_start: usize,
        header_end: usize,
    ) -> (usize, Option<HeadingAttributes<'a>>) {
        if !self.options.contains(Options::ENABLE_HEADING_ATTRIBUTES) {
            return (header_end, None);
        }

        // extract the trailing attribute block
        let header_bytes = &self.text.as_bytes()[header_start..header_end];
        let (content_len, attr_block_range_rel) =
            extract_attribute_block_content_from_header_text(header_bytes);
        let content_end = header_start + content_len;
        let attrs = attr_block_range_rel.and_then(|r| {
            parse_inside_attribute_block(
                &self.text[(header_start + r.start)..(header_start + r.end)],
            )
        });
        (content_end, attrs)
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

/// Computes the number of header columns in a table line by computing the number of dividing pipes
/// that aren't followed or preceded by whitespace.
fn count_header_cols(
    bytes: &[u8],
    mut pipes: usize,
    mut start: usize,
    last_pipe_ix: usize,
) -> usize {
    // was first pipe preceded by whitespace? if so, subtract one
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

/// Checks whether we should break a paragraph on the given input.
fn scan_paragraph_interrupt(bytes: &[u8], current_container: bool) -> bool {
    scan_eol(bytes).is_some()
        || scan_hrule(bytes).is_ok()
        || scan_atx_heading(bytes).is_some()
        || scan_code_fence(bytes).is_some()
        || scan_blockquote_start(bytes).is_some()
        || scan_listitem(bytes).map_or(false, |(ix, delim, index, _)| {
            ! current_container ||
            // we don't allow interruption by either empty lists or
            // numbered lists starting at an index other than 1
            (delim == b'*' || delim == b'-' || delim == b'+' || index == 1)
                && !scan_empty_list(&bytes[ix..])
        })
        || bytes.starts_with(b"<")
            && (get_html_end_tag(&bytes[1..]).is_some() || starts_html_block_type_6(&bytes[1..]))
}

/// Assumes `text_bytes` is preceded by `<`.
fn get_html_end_tag(text_bytes: &[u8]) -> Option<&'static str> {
    static BEGIN_TAGS: &[&[u8]; 4] = &[b"pre", b"style", b"script", b"textarea"];
    static ST_BEGIN_TAGS: &[&[u8]; 3] = &[b"!--", b"?", b"![CDATA["];

    for (beg_tag, end_tag) in BEGIN_TAGS
        .iter()
        .zip(["</pre>", "</style>", "</script>", "</textarea>"].iter())
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

fn create_lut(options: &Options) -> LookupTable {
    #[cfg(all(target_arch = "x86_64", feature = "simd"))]
    {
        LookupTable {
            simd: simd::compute_lookup(options),
            scalar: special_bytes(options),
        }
    }
    #[cfg(not(all(target_arch = "x86_64", feature = "simd")))]
    {
        special_bytes(options)
    }
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

enum LoopInstruction<T> {
    /// Continue looking for more special bytes, but skip next few bytes.
    ContinueAndSkip(usize),
    /// Break looping immediately, returning with the given index and value.
    BreakAtWith(usize, T),
}

#[cfg(all(target_arch = "x86_64", feature = "simd"))]
struct LookupTable {
    simd: [u8; 16],
    scalar: [bool; 256],
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
        simd::iterate_special_bytes(lut, bytes, ix, callback)
    }
    #[cfg(not(all(target_arch = "x86_64", feature = "simd")))]
    {
        scalar_iterate_special_bytes(lut, bytes, ix, callback)
    }
}

fn scalar_iterate_special_bytes<F, T>(
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

/// Split the usual heading content range and the content inside the trailing attribute block.
///
/// Returns `(leading_content_len, Option<trailing_attr_block_range>)`.
///
/// Note that `trailing_attr_block_range` will be empty range when the block
/// is `{}`, since the range is content inside the wrapping `{` and `}`.
///
/// The closing `}` of an attribute block can have trailing whitespaces.
/// They are automatically trimmed when the attribute block is being searched.
///
/// However, this method does not trim the trailing whitespaces of heading content.
/// It is callers' responsibility to trim them if necessary.
fn extract_attribute_block_content_from_header_text(
    heading: &[u8],
) -> (usize, Option<Range<usize>>) {
    let heading_len = heading.len();
    let mut ix = heading_len;
    ix -= scan_rev_while(heading, |b| {
        b == b'\n' || b == b'\r' || b == b' ' || b == b'\t'
    });
    if ix == 0 {
        return (heading_len, None);
    }

    let attr_block_close = ix - 1;
    if heading.get(attr_block_close) != Some(&b'}') {
        // The last character is not `}`. No attribute blocks found.
        return (heading_len, None);
    }
    // move cursor before the closing right brace (`}`)
    ix -= 1;

    ix -= scan_rev_while(&heading[..ix], |b| {
        // Characters to be excluded:
        //  * `{` and `}`: special characters to open and close an attribute block.
        //  * `\\`: a special character to escape many characters and disable some syntaxes.
        //      + Handling of this escape character differs among markdown processors.
        //      + Escaped characters will be separate text node from neighbors, so
        //        it is not easy to handle unescaped string and trim the trailing block.
        //  * `<` and `>`: special characters to start and end HTML tag.
        //      + No known processors converts `{#<i>foo</i>}` into
        //        `id="&lt;i&gt;foo&lt;/&gt;"` as of this writing, so hopefully
        //        this restriction won't cause compatibility issues.
        //  * `\n` and `\r`: a newline character.
        //      + Setext heading can have multiple lines. However it is hard to support
        //        attribute blocks that have newline inside, since the parsing proceeds line by
        //        line and lines will be separate nodes even they are logically a single text.
        !matches!(b, b'{' | b'}' | b'<' | b'>' | b'\\' | b'\n' | b'\r')
    });
    if ix == 0 {
        // `{` is not found. No attribute blocks available.
        return (heading_len, None);
    }
    let attr_block_open = ix - 1;
    if heading[attr_block_open] != b'{' {
        // `{` is not found. No attribute blocks available.
        return (heading_len, None);
    }

    (attr_block_open, Some(ix..attr_block_close))
}

/// Parses an attribute block content, such as `.class1 #id .class2`.
///
/// Returns `(id, classes)`.
///
/// It is callers' responsibility to find opening and closing characters of the attribute
/// block. Usually [`extract_attribute_block_content_from_header_text`] function does it for you.
///
/// Note that this parsing requires explicit whitespace separators between
/// attributes. This is intentional design with the reasons below:
///
/// * to keep conversion simple and easy to understand for any possible input,
/// * to avoid adding less obvious conversion rule that can reduce compatibility
///   with other implementations more, and
/// * to follow the major design of implementations with the support for the
///   attribute blocks extension (as of this writing).
///
/// See also: [`Options::ENABLE_HEADING_ATTRIBUTES`].
///
/// [`Options::ENABLE_HEADING_ATTRIBUTES`]: `crate::Options::ENABLE_HEADING_ATTRIBUTES`
fn parse_inside_attribute_block(inside_attr_block: &str) -> Option<HeadingAttributes> {
    let mut id = None;
    let mut classes = Vec::new();

    for attr in inside_attr_block.split_ascii_whitespace() {
        // iterator returned by `str::split_ascii_whitespace` never emits empty
        // strings, so taking first byte won't panic.
        if attr.len() > 1 {
            let first_byte = attr.as_bytes()[0];
            if first_byte == b'#' {
                id = Some(&attr[1..]);
            } else if first_byte == b'.' {
                classes.push(&attr[1..]);
            }
        }
    }

    Some(HeadingAttributes { id, classes })
}

#[cfg(all(target_arch = "x86_64", feature = "simd"))]
mod simd {
    //! SIMD byte scanning logic.
    //!
    //! This module provides functions that allow walking through byteslices, calling
    //! provided callback functions on special bytes and their indices using SIMD.
    //! The byteset is defined in `compute_lookup`.
    //!
    //! The idea is to load in a chunk of 16 bytes and perform a lookup into a set of
    //! bytes on all the bytes in this chunk simultaneously. We produce a 16 bit bitmask
    //! from this and call the callback on every index corresponding to a 1 in this mask
    //! before moving on to the next chunk. This allows us to move quickly when there
    //! are no or few matches.
    //!
    //! The table lookup is inspired by this [great overview]. However, since all of the
    //! bytes we're interested in are ASCII, we don't quite need the full generality of
    //! the universal algorithm and are hence able to skip a few instructions.
    //!
    //! [great overview]: http://0x80.pl/articles/simd-byte-lookup.html

    use super::{LookupTable, LoopInstruction};
    use crate::Options;
    use core::arch::x86_64::*;

    const VECTOR_SIZE: usize = std::mem::size_of::<__m128i>();

    /// Generates a lookup table containing the bitmaps for our
    /// special marker bytes. This is effectively a 128 element 2d bitvector,
    /// that can be indexed by a four bit row index (the lower nibble)
    /// and a three bit column index (upper nibble).
    pub(super) fn compute_lookup(options: &Options) -> [u8; 16] {
        let mut lookup = [0u8; 16];
        let standard_bytes = [
            b'\n', b'\r', b'*', b'_', b'&', b'\\', b'[', b']', b'<', b'!', b'`',
        ];

        for &byte in &standard_bytes {
            add_lookup_byte(&mut lookup, byte);
        }
        if options.contains(Options::ENABLE_TABLES) {
            add_lookup_byte(&mut lookup, b'|');
        }
        if options.contains(Options::ENABLE_STRIKETHROUGH) {
            add_lookup_byte(&mut lookup, b'~');
        }
        if options.contains(Options::ENABLE_SMART_PUNCTUATION) {
            for &byte in &[b'.', b'-', b'"', b'\''] {
                add_lookup_byte(&mut lookup, byte);
            }
        }

        lookup
    }

    fn add_lookup_byte(lookup: &mut [u8; 16], byte: u8) {
        lookup[(byte & 0x0f) as usize] |= 1 << (byte >> 4);
    }

    /// Computes a bit mask for the given byteslice starting from the given index,
    /// where the 16 least significant bits indicate (by value of 1) whether or not
    /// there is a special character at that byte position. The least significant bit
    /// corresponds to `bytes[ix]` and the most significant bit corresponds to
    /// `bytes[ix + 15]`.
    /// It is only safe to call this function when `bytes.len() >= ix + VECTOR_SIZE`.
    #[target_feature(enable = "ssse3")]
    #[inline]
    unsafe fn compute_mask(lut: &[u8; 16], bytes: &[u8], ix: usize) -> i32 {
        debug_assert!(bytes.len() >= ix + VECTOR_SIZE);

        let bitmap = _mm_loadu_si128(lut.as_ptr() as *const __m128i);
        // Small lookup table to compute single bit bitshifts
        // for 16 bytes at once.
        let bitmask_lookup =
            _mm_setr_epi8(1, 2, 4, 8, 16, 32, 64, -128, -1, -1, -1, -1, -1, -1, -1, -1);

        // Load input from memory.
        let raw_ptr = bytes.as_ptr().add(ix) as *const __m128i;
        let input = _mm_loadu_si128(raw_ptr);
        // Compute the bitmap using the bottom nibble as an index
        // into the lookup table. Note that non-ascii bytes will have
        // their most significant bit set and will map to lookup[0].
        let bitset = _mm_shuffle_epi8(bitmap, input);
        // Compute the high nibbles of the input using a 16-bit rightshift of four
        // and a mask to prevent most-significant bit issues.
        let higher_nibbles = _mm_and_si128(_mm_srli_epi16(input, 4), _mm_set1_epi8(0x0f));
        // Create a bitmask for the bitmap by perform a left shift of the value
        // of the higher nibble. Bytes with their most significant set are mapped
        // to -1 (all ones).
        let bitmask = _mm_shuffle_epi8(bitmask_lookup, higher_nibbles);
        // Test the bit of the bitmap by AND'ing the bitmap and the mask together.
        let tmp = _mm_and_si128(bitset, bitmask);
        // Check whether the result was not null. NEQ is not a SIMD intrinsic,
        // but comparing to the bitmask is logically equivalent. This also prevents us
        // from matching any non-ASCII bytes since none of the bitmaps were all ones
        // (-1).
        let result = _mm_cmpeq_epi8(tmp, bitmask);

        // Return the resulting bitmask.
        _mm_movemask_epi8(result)
    }

    /// Calls callback on byte indices and their value.
    /// Breaks when callback returns LoopInstruction::BreakAtWith(ix, val). And skips the
    /// number of bytes in callback return value otherwise.
    /// Returns the final index and a possible break value.
    pub(super) fn iterate_special_bytes<F, T>(
        lut: &LookupTable,
        bytes: &[u8],
        ix: usize,
        callback: F,
    ) -> (usize, Option<T>)
    where
        F: FnMut(usize, u8) -> LoopInstruction<Option<T>>,
    {
        if is_x86_feature_detected!("ssse3") && bytes.len() >= VECTOR_SIZE {
            unsafe { simd_iterate_special_bytes(&lut.simd, bytes, ix, callback) }
        } else {
            super::scalar_iterate_special_bytes(&lut.scalar, bytes, ix, callback)
        }
    }

    /// Calls the callback function for every 1 in the given bitmask with
    /// the index `offset + ix`, where `ix` is the position of the 1 in the mask.
    /// Returns `Ok(ix)` to continue from index `ix`, `Err((end_ix, opt_val)` to break with
    /// final index `end_ix` and optional value `opt_val`.
    unsafe fn process_mask<F, T>(
        mut mask: i32,
        bytes: &[u8],
        mut offset: usize,
        callback: &mut F,
    ) -> Result<usize, (usize, Option<T>)>
    where
        F: FnMut(usize, u8) -> LoopInstruction<Option<T>>,
    {
        while mask != 0 {
            let mask_ix = mask.trailing_zeros() as usize;
            offset += mask_ix;
            match callback(offset, *bytes.get_unchecked(offset)) {
                LoopInstruction::ContinueAndSkip(skip) => {
                    offset += skip + 1;
                    mask >>= skip + 1 + mask_ix;
                }
                LoopInstruction::BreakAtWith(ix, val) => return Err((ix, val)),
            }
        }
        Ok(offset)
    }

    #[target_feature(enable = "ssse3")]
    /// Important: only call this function when `bytes.len() >= 16`. Doing
    /// so otherwise may exhibit undefined behaviour.
    unsafe fn simd_iterate_special_bytes<F, T>(
        lut: &[u8; 16],
        bytes: &[u8],
        mut ix: usize,
        mut callback: F,
    ) -> (usize, Option<T>)
    where
        F: FnMut(usize, u8) -> LoopInstruction<Option<T>>,
    {
        debug_assert!(bytes.len() >= VECTOR_SIZE);
        let upperbound = bytes.len() - VECTOR_SIZE;

        while ix < upperbound {
            let mask = compute_mask(lut, bytes, ix);
            let block_start = ix;
            ix = match process_mask(mask, bytes, ix, &mut callback) {
                Ok(ix) => std::cmp::max(ix, VECTOR_SIZE + block_start),
                Err((end_ix, val)) => return (end_ix, val),
            };
        }

        if bytes.len() > ix {
            // shift off the bytes at start we have already scanned
            let mask = compute_mask(lut, bytes, upperbound) >> ix - upperbound;
            if let Err((end_ix, val)) = process_mask(mask, bytes, ix, &mut callback) {
                return (end_ix, val);
            }
        }

        (bytes.len(), None)
    }

    #[cfg(test)]
    mod simd_test {
        use super::super::create_lut;
        use super::{iterate_special_bytes, LoopInstruction};
        use crate::Options;

        fn check_expected_indices(bytes: &[u8], expected: &[usize], skip: usize) {
            let mut opts = Options::empty();
            opts.insert(Options::ENABLE_TABLES);
            opts.insert(Options::ENABLE_FOOTNOTES);
            opts.insert(Options::ENABLE_STRIKETHROUGH);
            opts.insert(Options::ENABLE_TASKLISTS);

            let lut = create_lut(&opts);
            let mut indices = vec![];

            iterate_special_bytes::<_, i32>(&lut, bytes, 0, |ix, _byte_ty| {
                indices.push(ix);
                LoopInstruction::ContinueAndSkip(skip)
            });

            assert_eq!(&indices[..], expected);
        }

        #[test]
        fn simple_no_match() {
            check_expected_indices("abcdef0123456789".as_bytes(), &[], 0);
        }

        #[test]
        fn simple_match() {
            check_expected_indices("*bcd&f0123456789".as_bytes(), &[0, 4], 0);
        }

        #[test]
        fn single_open_fish() {
            check_expected_indices("<".as_bytes(), &[0], 0);
        }

        #[test]
        fn long_match() {
            check_expected_indices("0123456789abcde~*bcd&f0".as_bytes(), &[15, 16, 20], 0);
        }

        #[test]
        fn border_skip() {
            check_expected_indices("0123456789abcde~~~~d&f0".as_bytes(), &[15, 20], 3);
        }

        #[test]
        fn exhaustive_search() {
            let chars = [
                b'\n', b'\r', b'*', b'_', b'~', b'|', b'&', b'\\', b'[', b']', b'<', b'!', b'`',
            ];

            for &c in &chars {
                for i in 0u8..=255 {
                    if !chars.contains(&i) {
                        // full match
                        let mut buf = [i; 18];
                        buf[3] = c;
                        buf[6] = c;

                        check_expected_indices(&buf[..], &[3, 6], 0);
                    }
                }
            }
        }
    }
}
