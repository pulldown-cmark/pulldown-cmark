# Inline Processing

The second pass of pulldown-cmark's parsing process handles inline elements like emphasis, links, and code spans. This chapter explains how inline processing works.

## Overview

Inline processing happens during event iteration rather than as a separate full-document pass. When the parser encounters a block that can contain inlines, it processes the inline elements on demand.

The main inline elements handled are:

- Emphasis and strong emphasis (* and _)
- Code spans (`)
- Links and images
- HTML tags and entities
- Autolinks
- Hard/soft line breaks
- Extension elements like strikethrough and math

## Processing Model

The inline processor:

1. Scans text for special characters
2. Identifies potential inline markers
3. Resolves matched pairs (like * for emphasis)
4. Handles nested elements
5. Processes escapes and entities

## Special Character Scanning

The first step is identifying special characters that might start inline elements:

```rust
const SPECIAL_CHARS: &[u8] = b"\\*_`&<[!'\"";

fn scan_special_chars(text: &[u8]) -> usize {
    // Optimized scanning for special characters
    // Uses SIMD on x86_64
}
```

Each special character triggers specific handling:

- `\`: Escape sequence
- `*_~`: Delimiter run for emphasis-like elements
- `` ` ``: Code span delimiter
- `&`: HTML entity
- `<`: HTML tag or autolink
- `[`: Link/image start
- `!`: Image start
- `'\"`: Smart quotes (with extension)

## Delimiter Handling

Emphasis-type elements use a sophisticated delimiter handling system:

1. Identify delimiter runs (consecutive *, _, etc)
2. Determine if they can open and/or close
3. Match pairs according to CommonMark rules
4. Handle nested cases correctly


The `InlineStack` struct manages this:

```rust
struct InlineStack {
    stack: Vec<InlineEl>,
    lower_bounds: [usize; 9],
}

struct InlineEl {
    start: TreeIndex,
    count: usize,      // Number of delimiters
    run_length: usize, // Full run length
    c: u8,            // Delimiter character
    both: bool,       // Can both open and close
}
```

## Link Processing

Link processing involves:

1. Finding link text in brackets
2. Handling different link types:
   - Inline `[text](url)`  
   - Reference `[text][ref]`
   - Collapsed `[ref][]`
   - Shortcut `[ref]`
   
3. Resolving references in link definitions
4. Processing link destinations and titles

The link processor maintains a stack to handle nested links and images:

```rust
struct LinkStackEl {
    node: TreeIndex,
    ty: LinkStackTy,
}

enum LinkStackTy {
    Link,
    Image,
    Disabled, // For nested links
}
```

## Code Spans

Code span processing has special rules:

1. Match backtick sequences of equal length
2. Handle backslash escapes
3. Strip leading/trailing spaces according to spec
4. Prevent misinterpreting internal backticks

Example:
```rust
fn make_code_span(&mut self, open: TreeIndex, close: TreeIndex) {
    let span_start = self.tree[open].item.end;
    let span_end = self.tree[close].item.start;
    
    // Process content
    let content = &self.text[span_start..span_end];
    let content = normalize_code_span(content);
    
    // Create code span node
    self.tree[open].item.body = ItemBody::Code(
        self.allocs.allocate_cow(content)
    );
}
```

## HTML Processing

HTML handling involves:

1. Identifying HTML constructs:
   - Tags
   - Comments  
   - CDATA sections
   - Processing instructions
   
2. Validating structure
3. Preserving content exactly
4. Handling entities

The HTML processor uses a state machine to track context:

```rust
struct HtmlScanGuard {
    cdata: usize,
    processing: usize, 
    declaration: usize,
    comment: usize,
}
```

## String Handling

Inline processing needs efficient string handling:

1. Copy-on-write strings to avoid allocation
2. Smart handling of escaped characters
3. Entity resolution
4. UTF-8 awareness
5. Line ending normalization

The `CowStr` type provides this:

```rust
enum CowStr<'a> {
    Borrowed(&'a str),
    Boxed(Box<str>),
    Inlined(InlineStr),
}
```

## Event Generation

As inline elements are processed, they generate events:

1. Start/end events for container elements
2. Text events for content
3. Specialized events for atomic elements
4. Source position tracking

Events are yielded in document order:

```rust
enum Event<'a> {
    Start(Tag<'a>),
    End(TagEnd),
    Text(CowStr<'a>),
    Code(CowStr<'a>),
    Html(CowStr<'a>),
    // ...
}
```

## Extension Support

The inline processor supports extensions through:

1. Additional special characters
2. New delimiter types
3. Custom processing rules
4. Extension-specific events

For example, the math extension adds:

```rust
fn scan_math(&mut self) -> Option<(usize, bool)> {
    // Handle $...$ and $$...$$
}
```

## Performance Considerations

Key performance features:

1. SIMD-accelerated character scanning
2. Efficient delimiter matching
3. Minimal string allocation
4. Lazy processing of inlines
5. Smart caching of partial results

The inline processor aims to be:
- Fast for common cases
- Memory efficient  
- Spec compliant
- Easy to extend
