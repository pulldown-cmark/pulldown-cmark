# String Handling

pulldown-cmark uses a specialized string type system optimized for the specific needs of parsing and representing Markdown content. This chapter explains the key components and design decisions of this system.

## Overview

The library uses two main custom string types:

- `CowStr`: A three-word copy-on-write string type that can be owned, borrowed, or inlined
- `InlineStr`: A small string optimized for very short content that can be stored inline

These types are designed to balance several requirements:

- Efficient memory usage for the many small string fragments in Markdown
- Zero-copy operation where possible by borrowing from input
- Good performance for string operations needed during parsing
- Safety and correct handling of Unicode

## CowStr Type

The `CowStr` enum is the primary string type used throughout the library. It has three variants:

```rust
pub enum CowStr<'a> {
    Boxed(Box<str>),
    Borrowed(&'a str), 
    Inlined(InlineStr),
}
```

Each variant serves a specific purpose:

- `Borrowed`: References strings from the input text with zero copying
- `Inlined`: Stores very short strings (up to ~22 bytes on 64-bit systems) directly in the enum
- `Boxed`: Owns longer strings that need to be heap allocated

The key feature is that `CowStr` is exactly three words in size regardless of which variant is used. This fixed size makes it efficient to store and pass around.

### Example Usage

```rust
// Borrow from input when possible
let borrowed: CowStr = "some text".into();

// Short strings are inlined
let inline: CowStr = "x".into();

// Longer strings are boxed
let boxed: CowStr = "a rather long string...".to_string().into();
```

## InlineStr Type

`InlineStr` is a small string type that can store short strings inline without heap allocation. It consists of:

- A fixed-size byte array sized to three machine words minus 2 bytes
- A length field using the remaining byte

```rust
pub struct InlineStr {
    inner: [u8; MAX_INLINE_STR_LEN],
    len: u8,
}
```

The size is chosen to allow `InlineStr` to be stored directly in `CowStr` without increasing its overall size. On 64-bit systems this allows for strings up to 22 bytes.

Key characteristics:

- Fixed size with no heap allocation
- UTF-8 encoded
- Length limited by available space
- Copy-able since it's a fixed-size type

### Size Optimization

The size of `InlineStr` is carefully chosen based on the size of machine words:

```rust
const MAX_INLINE_STR_LEN: usize = 3 * std::mem::size_of::<isize>() - 2;
```

This ensures:
1. `InlineStr` fits within three words
2. `CowStr` remains three words total
3. Maximum space is available for inline storage

## Converting Between Types

The library provides conversions between various string types:

```rust
// From str
let cow: CowStr = "text".into();

// From String 
let cow: CowStr = string.into();

// From char
let cow: CowStr = 'x'.into();

// From std::borrow::Cow
let cow: CowStr = std_cow.into();
```

It also provides methods to convert into owned types:

```rust
let string: String = cow_str.into_string();
let static_cow: CowStr<'static> = cow_str.into_static();
```

## Performance Considerations

The string system is designed for the performance characteristics needed by a Markdown parser:

- Minimal copying of input text
- Efficient handling of many small string fragments
- Quick concatenation for things like text merging
- Fast comparisons for link matching

The `TextMergeStream` utility showcases these optimizations by efficiently combining consecutive text events while preserving memory efficiency.

## Unicode Handling

All string types maintain proper UTF-8 encoding:

- Input validation occurs when creating `InlineStr`
- String operations preserve valid UTF-8
- Character boundaries are respected when manipulating strings

This ensures the parser handles international text correctly while maintaining performance.

## Implementation Details

When implementing new features that handle strings, follow these guidelines:

1. Use `CowStr` as the primary string type
2. Borrow from input when possible using `Borrowed` variant
3. Use `InlineStr` for short string literals
4. Convert to `String` only when necessary for interfacing with other code
5. Be aware of UTF-8 encoding requirements
6. Consider memory usage patterns when choosing string operations

The string system is designed to be mostly transparent to users of the library while providing significant optimizations internally.
