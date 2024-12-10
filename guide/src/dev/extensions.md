# Adding Extensions

This guide explains how to add new extensions to pulldown-cmark. Extensions allow you to parse additional Markdown syntax beyond the CommonMark specification.

## Overview

Adding an extension typically requires:

1. Adding a feature flag in the `Options` bitflags
2. Adding any new data structures needed to represent the extension's AST nodes
3. Implementing block parsing in `firstpass.rs` if the extension adds block-level elements
4. Implementing inline parsing in `parse.rs` if the extension adds inline elements
5. Adding HTML rendering support in `html.rs`
6. Adding tests to verify the extension works correctly

Let's walk through each of these steps in detail.

## Adding the Feature Flag

Extensions are controlled via the `Options` bitflags defined in `lib.rs`. Add a new constant using the next available bit:

```rust
bitflags::bitflags! {
    pub struct Options: u32 {
        // Existing options...
        const ENABLE_MY_EXTENSION = 1 << N; // N is next available bit
    }
}
```

This allows users to enable your extension with:

```rust
let mut options = Options::empty();
options.insert(Options::ENABLE_MY_EXTENSION);
```

## Adding AST Data Structures

Extensions often need new AST node types to represent their syntax. These are defined in several places:

- `Tag` enum in `lib.rs` for container elements 
- `TagEnd` enum in `lib.rs` for end tags
- `Event` enum in `lib.rs` for new event types
- `ItemBody` enum in `parse.rs` for internal AST nodes

For example, the tables extension defines:

```rust
// In lib.rs
pub enum Tag<'a> {
    // ...
    Table(Vec<Alignment>),
    TableHead,
    TableRow,
    TableCell,
}

// In parse.rs
pub(crate) enum ItemBody {
    // ...
    Table(AlignmentIndex),
    TableHead,
    TableRow, 
    TableCell,
}
```

Follow existing patterns for naming and make sure to implement all the necessary traits (`Debug`, `Clone`, etc.).

## Implementing Block Parsing

If your extension adds block-level elements (like tables, footnotes, etc.), you'll need to:

1. Add scanning functions in `scanners.rs` to detect your syntax
2. Add parsing logic in `firstpass.rs` to build the block structure
3. Update the `scan_containers()` function if your blocks can be nested

For example, the tables extension adds:

```rust
// In scanners.rs
pub(crate) fn scan_table_head(data: &[u8]) -> (usize, Vec<Alignment>) {
    // Scan table header row syntax...
}

// In firstpass.rs 
impl<'a> FirstPass<'a, 'b> {
    fn parse_table(&mut self, ...) -> Option<usize> {
        // Parse table structure...
    }
}
```

Follow these guidelines when implementing block parsing:

- Use the `scan_` prefix for low-level scanning functions
- Make scanning functions return the number of bytes consumed
- Handle edge cases like empty lines and indentation
- Properly integrate with the container block structure
- Follow the parsing strategies used by existing extensions

## Implementing Inline Parsing

If your extension adds inline elements (like strikethrough, math, etc.), you'll need to:

1. Add marker detection in `parse_line()` in `parse.rs`
2. Add opener/closer matching logic in `handle_inline()` 
3. Add conversion from internal AST to events

For example, the strikethrough extension adds:

```rust
// In parse.rs
impl<'a, F: BrokenLinkCallback<'a>> Parser<'a, F> {
    fn parse_line(&mut self, ..) -> (usize, Option<Item>) {
        match byte {
            b'~' => {
                // Handle tilde markers...
            }
        }
    }
}
```

Inline parsing tips:

- Use the `MaybeX` pattern for markers that need matching
- Handle backslash escaping correctly
- Support nested inline elements 
- Follow CommonMark rules for flanking conditions
- Reuse existing inline parsing infrastructure

## Adding HTML Rendering

HTML rendering is handled in `html.rs`. You'll need to:

1. Add HTML tag generation for your new elements
2. Update the `body_to_tag_end()` and `item_to_event()` functions
3. Handle any special rendering requirements

For example:

```rust 
// In html.rs
impl<'a, I, W> HtmlWriter<'a, I, W> {
    fn start_tag(&mut self, tag: Tag<'a>) -> Result<(), W::Error> {
        match tag {
            Tag::MyExtension => {
                self.write("<my-extension>")
            }
            // ...
        }
    }
}
```

HTML rendering tips:

- Follow HTML5 standards
- Handle escaping properly
- Consider accessibility 
- Add appropriate CSS classes
- Test in different contexts

## Testing

Add tests to verify your extension works correctly:

1. Unit tests alongside implementation
2. Integration tests in `tests/`
3. round-trip tests
4. Edge case tests
5. Interaction tests with other extensions

For example:

```rust
#[test]
fn test_my_extension() {
    let input = "Test my extension syntax";
    let mut options = Options::empty();
    options.insert(Options::ENABLE_MY_EXTENSION);
    let parser = Parser::new_ext(input, options);
    // Test parsing result...
}
```

Testing tips:

- Test both positive and negative cases
- Test interactions with other syntax
- Test error conditions
- Test HTML output
- Test with different options enabled

## Example: Adding Subscript Extension

Here's a complete example of adding a hypothetical subscript extension that uses `~text~` for subscript:

```rust
// In lib.rs
bitflags::bitflags! {
    pub struct Options: u32 {
        const ENABLE_SUBSCRIPT = 1 << 15;
    }
}

pub enum Tag<'a> {
    Subscript,
}

// In parse.rs
pub(crate) enum ItemBody {
    MaybeSubscript(usize),  // For opener/closer matching
    Subscript,
}

impl<'a, F> Parser<'a, F> {
    fn parse_line(&mut self, ..) -> (usize, Option<Item>) {
        match byte {
            b'~' => {
                // Handle subscript markers...
            }
        }
    }
}

// In html.rs
impl<'a, I, W> HtmlWriter<'a, I, W> {
    fn start_tag(&mut self, tag: Tag<'a>) -> Result<(), W::Error> {
        match tag {
            Tag::Subscript => self.write("<sub>"),
        }
    }
}
```

## Tips and Best Practices

- Study existing extensions for patterns to follow
- Keep parsing efficient 
- Handle edge cases gracefully
- Make error messages helpful
- Document your extension thoroughly
- Consider adding feature flags for subfeatures
- Follow CommonMark principles where possible
- Test extensively
- Consider compatibility with other extensions

## Common Pitfalls

- Not handling nested elements correctly
- Improper escaping in HTML output
- Not following CommonMark precedence rules
- Inefficient parsing of large documents
- Poor error recovery
- Not handling edge cases
- Breaking existing syntax
- Not documenting limitations

## Further Reading

- [CommonMark Spec](https://spec.commonmark.org/)
- [GitHub Flavored Markdown Spec](https://github.github.com/gfm/)
- [Existing pulldown-cmark extensions](https://docs.rs/pulldown-cmark/)
- [HTML5 Spec](https://html.spec.whatwg.org/)

## Getting Help

If you need help implementing an extension:

1. Read the existing code carefully
2. Check the test suite for examples
3. File an issue on GitHub
4. Ask questions in pull requests
5. Read related CommonMark discussions

Remember that extensions should be:

- Well-tested
- Efficient
- Maintainable 
- Compatible with existing syntax
- Following CommonMark principles
- Properly documented
