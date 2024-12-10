# HTML Generation

This chapter explains how pulldown-cmark generates HTML output from Markdown events.

## Overview

HTML generation is implemented in the `html` module and consists of two main components:

1. The `HtmlWriter` struct which manages state and writes HTML tags
2. Helper functions for converting events to HTML tags and handling special cases

The HTML generation process works by:
1. Taking an iterator of Markdown events 
2. Converting each event into corresponding HTML tags
3. Managing state for special cases like tables and tight lists
4. Writing the HTML tags to the provided output

## The HtmlWriter

The core type responsible for HTML generation is `HtmlWriter`:

```rust
struct HtmlWriter<'a, I, W> {
    iter: I,        // Iterator supplying events
    writer: W,      // Writer to write to
    end_newline: bool,  // Whether last write ended with newline
    in_non_writing_block: bool,  // In metadata block (no output)
    table_state: TableState,  // Current state for table processing
    table_alignments: Vec<Alignment>,  // Column alignments for current table
    table_cell_index: usize,  // Current cell index in table row
    numbers: HashMap<CowStr<'a>, usize>,  // For footnote numbering
}
```

The writer keeps track of:

- The current table state (head vs body)
- Table column alignments
- Current cell index
- Footnote numbering
- Whether we're in a non-writing block like metadata
- Whether the last write ended with a newline

## Event Processing

The main event processing loop lives in `HtmlWriter::run()`. For each event:

1. The event is matched and dispatched to the appropriate handler
2. HTML tags are written based on the event type
3. State is updated as needed

Key event handling patterns:

### Block Elements

Block elements like paragraphs, headings, lists etc. are wrapped in HTML tags:

```rust
match event {
    Start(Tag::Paragraph) => write("<p>"),
    End(Tag::Paragraph) => write("</p>\n"),
    // etc
}
```

### Inline Elements

Inline elements like emphasis and links are handled similarly but without newlines:

```rust 
match event {
    Start(Tag::Emphasis) => write("<em>"),
    End(Tag::Emphasis) => write("</em>"),
    // etc
}
```

### Text Content

Text content is HTML escaped and written directly:

```rust
match event {
    Text(text) => escape_html(&mut writer, &text),
    // etc
}
```

### Complex Elements

More complex elements like tables require managing state:

```rust
match event {
    Start(Tag::Table(alignments)) => {
        table_alignments = alignments;
        table_state = TableState::Head;
        write("<table>");
    }
    // etc
}
```

## HTML Safety

The functions `escape_html()` and ``escape_href()`` are used throughout the library for escaping special characters. The escaping functions live in the `pulldown-cmark-escape` crate.

## Tables

Table generation requires special handling to:

1. Track the current table section (head vs body)
2. Apply column alignments
3. Track cell indices
4. Insert proper nested tags (thead, tbody, tr, td/th)

Key table-related state:

```rust
enum TableState {
    Head,
    Body,  
}

struct HtmlWriter {
    table_state: TableState,
    table_alignments: Vec<Alignment>,
    table_cell_index: usize,
}
```

## Lists

List generation handles:

1. Ordered vs unordered lists
2. List item nesting 
3. Tight vs loose lists
4. Task list items with checkboxes
5. Start indices for ordered lists

For example, an ordered list with start index:

```html
<ol start="7">
  <li>Item one</li>
  <li>Item two</li>
</ol>
```

## Writer Interface

The HTML writer is generic over the writer type `W`, allowing output to:

- Strings via `fmt::Write`
- Files/IO via `io::Write` 
- Custom writer implementations

This generic design lets users choose the most efficient output method for their use case. For example:
- Using `String` is convenient for in-memory processing and testing
- Using `BufWriter<File>` is efficient for writing directly to disk
- Using a network socket allows streaming HTML over a connection
- Using a custom writer enables special handling like compression or logging

The `StrWrite` trait provides a common interface to abstract over these different writers:

```rust
pub trait StrWrite {
    type Error;
    fn write_str(&mut self, s: &str) -> Result<(), Self::Error>;  
}
```

This abstraction over the writer type means the HTML generation code can focus on correct tag generation and structure without worrying about the specific output destination. It also allows users to easily integrate pulldown-cmark's HTML output into their existing I/O pipelines.
```

## Public API

The main public API consists of:

```rust
// Write HTML to a String
pub fn push_html<'a, I>(s: &mut String, iter: I) 
where I: Iterator<Item = Event<'a>>

// Write HTML to an IO writer
pub fn write_html_io<'a, I, W>(writer: W, iter: I) -> io::Result<()> 
where I: Iterator<Item = Event<'a>>,
      W: io::Write

// Write HTML to a fmt writer
pub fn write_html_fmt<'a, I, W>(writer: W, iter: I) -> fmt::Result
where I: Iterator<Item = Event<'a>>,
      W: fmt::Write
```

## Performance Considerations

HTML generation aims to be efficient by:

1. Minimizing string allocations
2. Using buffered writers
3. Avoiding recursion in the core loop
4. Pre-allocating common strings
5. Reusing string buffers where possible

Key performance note:

```rust 
// Using unbuffered writers (like Files) will be slow
// Wrap them in BufWriter for better performance
let file = BufWriter::new(File::create("output.html")?);
write_html_io(file, parser);
```

This ensures good performance even with large documents.

## Correctness

The HTML generator ensures correctness through:

1. Proper tag nesting and matching
2. Handling of all CommonMark syntax elements
3. Following HTML5 standards
4. Proper escaping of HTML and URLs
5. Handling edge cases like empty lists

The test suite validates both basic functionality and edge cases.

## Customization

The HTML output can be customized by:

1. Using a custom writer implementation
2. Preprocessing the event stream 
3. Post-processing the HTML output
4. Using the parser options to enable/disable features

However, the core HTML structure follows CommonMark conventions.
