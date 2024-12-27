# Block Structure Parsing

The first pass of pulldown-cmark's parsing process handles block-level elements and constructs the basic document structure.

It roughly corresponds to Phase 1 of the CommonMark spec appendix ["A Parsing Strategy"](https://spec.commonmark.org/0.31.2/#appendix-a-parsing-strategy).
This chapter explains how the block parsing works in pulldown-cmark.

## Overview

Block parsing is implemented in `firstpass.rs` and has two main responsibilities:

1. Identifying block-level elements like paragraphs, lists, and code blocks
2. Building a tree structure representing the nesting of these blocks

The block parser operates line-by-line, maintaining a stack of currently open blocks (called the "spine") and handling both container blocks (like blockquotes) and leaf blocks (like paragraphs).

## Block Types

The main block types handled by the first pass are:

- Container blocks:
  - Block quotes
  - Lists (ordered and unordered)  
  - List items
  - Footnote definitions
  
- Leaf blocks:
  - Paragraphs
  - Headings (ATX and Setext style)
  - Code blocks (fenced and indented)
  - HTML blocks
  - Thematic breaks (horizontal rules)
  - Tables (with GFM extension)

## The Parsing Process

The block parsing process works like this:

1. Input text is processed line by line

2. For each line:
   - Check if it continues any blocks from the current spine
   - Scan for the start of any new blocks  
   - Handle transitions between blocks
   - Track indentation and container prefixes
   
3. Build tree nodes for each block encountered

4. Handle tight/loose list detection 

Here's a simplified example of how a nested list is parsed:

```markdown
- First item
  - Nested item
    with continuation
- Second item
```

The parser:
1. Recognizes the first `-` as starting a list and list item
2. Sees the next `-` as starting a nested list
3. Identifies the indented line as continuing the nested item
4. Recognizes the unindented `-` as closing the nested list

## Tree Construction

The block structure is stored in a `Tree<Item>` where each node contains:

```rust
struct Item {
    start: usize,      // Start byte offset
    end: usize,        // End byte offset  
    body: ItemBody,    // Type and attributes
}

struct Node<T> {
    child: Option<TreeIndex>,  // First child node
    next: Option<TreeIndex>,   // Next sibling
    item: T,                   // Node data (T = Item in our tree)
}
```

The tree is built incrementally as blocks are parsed. Key operations:

- `push()`: Move down into a new block's children
- `pop()`: Move back up to the parent block
- `append()`: Add a new sibling block
- `truncate_siblings()`: End open blocks at a certain point

## Container Block Handling

Container blocks like blockquotes and lists require special handling:

1. Track container prefixes (>, -, 1., etc)
2. Calculate correct indentation levels
3. Handle lazy continuation lines
4. Determine tight/loose status for lists

The `LineStart` struct helps manage this by:
- Tracking indentation and remaining space
- Scanning container markers
- Handling tab stops correctly

## Leaf Block Processing

Leaf blocks are handled by specific scanner functions that:

1. Identify the block type
2. Calculate its bounds
3. Handle internal structure like table columns
4. Manage transitions between blocks

For example, table parsing:
```rust
pub(crate) fn scan_table_head(data: &[u8]) -> (usize, Vec<Alignment>) {
    // Check initial conditions
    let (mut i, spaces) = calc_indent(data, 4);
    if spaces > 3 || i == data.len() {
        return (0, vec![]);
    }
    
    // Parse cells and alignments
    let mut cols = vec![];
    let mut active_col = Alignment::None;
    // ...

    // Return parsed structure
    (i, cols)
}
```

## Error Recovery

The parser is designed to be robust and recover from invalid syntax:

- Malformed containers fall back to paragraphs
- Invalid indentation is normalized
- Unclosed blocks are implicitly closed
- HTML parsing has fallback modes

This ensures it can handle real-world Markdown without failing.

## Interfacing with Inline Parsing

The block parser prepares for inline parsing by:

1. Identifying inline-containing blocks
2. Marking potential inline boundaries (e.g. `MaybeLinkOpen`)
3. Providing context (like table cells)
4. Tracking source positions

The tree structure is then used by the inline parser to process inline elements within the appropriate blocks.

## Implementation Notes

Some key implementation details:

- Line scanning is optimized using SIMD on x86_64
- The tree structure uses indexed nodes to avoid lifetimes
- Container context is maintained in a stack-like structure
- Source positions are tracked for use by the inline parser and [`OffsetIter`](https://docs.rs/pulldown-cmark/latest/pulldown_cmark/struct.OffsetIter.html)

The block parser aims to be:
- Fast for common cases
- Memory efficient
- Robust against bad input
- Compliant with CommonMark
