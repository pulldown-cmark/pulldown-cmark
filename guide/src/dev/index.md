# Developer Guide

pulldown-cmark uses a two-pass parsing strategy with a pull parser architecture to efficiently parse Markdown into HTML. This guide explains the internal workings of the library for developers who want to contribute or better understand how it works.

## High-Level Architecture

The parser operates in two main passes:

1. **First Pass (Block Structure)**: The first pass scans the document and builds a tree structure representing block-level elements like paragraphs, lists, code blocks, etc. This establishes the hierarchical structure of the document.

2. **Second Pass (Inline Processing)**: The second pass processes inline elements like emphasis, links and code spans within the blocks identified by the first pass. This is done in a streaming fashion as events are requested.

The library uses a pull parser design, which means:

- Instead of pushing events to a callback or building a complete AST, it provides an iterator interface that lets consumers pull events as needed
- This allows for memory-efficient processing since only a small part of the document needs to be in memory at any time
- It enables flexible transformation of the event stream before rendering

Key components:

- `Parser`: The main entry point that implements the Iterator trait for Events
- `Tree`: A Vec-based data structure that holds the block structure 
- `Event`: An enum representing the different Markdown elements
- `HtmlWriter`: Renders the event stream as HTML

## Performance Characteristics

The parser is designed for high performance:

- Memory usage is O(n) in the nesting depth of blocks, not document size
- String handling uses copy-on-write semantics to avoid unnecessary allocations
- SIMD optimizations are available for scanning text on x86_64
- The pull parser design allows streaming without buffering the whole document

## Extending the Parser

The parser can be extended in several ways:

- New syntax extensions can be added by implementing new scan functions
- The event stream can be transformed using Iterator adaptors
- Custom renderers can be built by consuming events
- The HTML renderer can be customized through options

## Directory Structure

```
src/
  firstpass.rs   - First pass block structure parsing
  scanners.rs    - Low-level text scanning functions  
  parse.rs       - Main parser implementation
  html.rs        - HTML renderer
  tree.rs        - Tree data structure
  entities.rs    - HTML entity handling
  strings.rs     - String types and utilities
```

Subsequent chapters cover each of these components in detail:

1. [Block Structure Parsing](./dev/block-parsing.md) 
2. [Inline Processing](./dev/inline-processing.md)
3. [String Handling](./dev/string-handling.md)
4. [HTML Generation](./dev/html-generation.md)
5. [Performance Optimizations](./dev/performance.md)
6. [Adding Extensions](./dev/extensions.md)

## Design Principles

The core design principles of pulldown-cmark are:

1. **Correctness**: Strict adherence to the CommonMark spec
2. **Performance**: Efficient parsing with minimal allocations
3. **Memory Safety**: Safe Rust code with no unsafe blocks (except SIMD)
4. **Extensibility**: Clean interfaces for adding new features
5. **Simplicity**: Clear, maintainable code over clever optimizations

These principles guide development decisions and help maintain a high-quality codebase as the project evolves.
