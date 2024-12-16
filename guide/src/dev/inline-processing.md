# Inline Processing

The second pass of pulldown-cmark's parsing process handles inline elements like emphasis, links, and code spans.

## Overview

Inline processing happens during event iteration rather than as a separate full-document pass. When the parser encounters a block that can contain inlines, it processes the inline elements on demand.

The main inline elements handled are:

- Emphasis and strong emphasis (* and _)
- Code spans (`)
- Links and images
- HTML tags and entities
- Autolinks
- Extension elements like strikethrough and math

## Processing Model

The inline processor:

1. Scans text for special characters
2. Identifies potential inline markers
3. Resolves matched pairs (like * for emphasis)
4. Handles nested elements
5. Processes escapes and entities

## Delimiter Handling

Emphasis-type elements use a sophisticated delimiter handling system:

1. Identify delimiter runs (consecutive `*`, `_`, etc)
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

## HTML Processing

HTML blocks have already been recognized by the block parser. What remains is inline HTML tags between normal text. Handling this involves:

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

The `CowStr` type provides this and is documented in detail [here](./string-handling.md).

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
