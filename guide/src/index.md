# Guide

Pulldown-cmark is a [CommonMark](http://commonmark.org/) Markdown parser based
on [pull parsing](https://www.xmlpull.org/history/index.html),
a high-performance, low-memory approach to parsing resursive grammars.

In addition to the base CommonMark language,
it supports tables, task lists, strikethrough, footnotes, admonitions, and LaTeX-style math from GFM.
It also supports [tagging headers with explicit IDs and classes](cheat-sheet.md#other-extensions), 

## Getting started

```rust
// Create parser with example Markdown text.
let markdown_input = "hello world";
let parser = pulldown_cmark::Parser::new(markdown_input);

// Write to a new String buffer.
let mut html_output = String::new();
pulldown_cmark::html::push_html(&mut html_output, parser);
assert_eq!(&html_output, "<p>hello world</p>\n");
```

For more details on the API, see the [Rust API docs](https://docs.rs/pulldown-cmark/).

## Why a pull parser?

There are many parsers for Markdown and its variants, but to my knowledge none
use pull parsing. Pull parsing has become popular for XML, especially for
memory-conscious applications, because it uses dramatically less memory than
constructing a document tree, but is much easier to use than push parsers. Push
parsers are notoriously difficult to use, and also often error-prone because of
the need for user to delicately juggle state in a series of callbacks.

In a clean design, the parsing and rendering stages are neatly separated, but
this is often sacrificed in the name of performance and expedience. Many Markdown
implementations mix parsing and rendering together, and even designs that try
to separate them (such as the popular [hoedown](https://github.com/hoedown/hoedown)),
make the assumption that the rendering process can be fully represented as a
serialized string.

Pull parsing is in some sense the most versatile architecture. It's possible to
drive a push interface, also with minimal memory, and quite straightforward to
construct an AST. Another advantage is that source-map information (the mapping
between parsed blocks and offsets within the source text) is readily available;
you can call `into_offset_iter()` to create an iterator that yields `(Event, Range)`
pairs, where the second element is the event's corresponding range in the source
document.

While manipulating ASTs is the most flexible way to transform documents,
operating on iterators is surprisingly easy, and quite efficient. Here, for
example, is the code to transform soft line breaks into hard breaks:

```rust
let parser = parser.map(|event| match event {
	Event::SoftBreak => Event::HardBreak(HardBreakStyle::DoubleSpace),
	_ => event
});
```

Or expanding an abbreviation in text:

```rust
let parser = parser.map(|event| match event {
	Event::Text(text) => Event::Text(text.replace("abbr", "abbreviation").into()),
	_ => event
});
```

Another simple example is code to determine the max nesting level:

```rust
let mut max_nesting = 0;
let mut level = 0;
for event in parser {
	match event {
		Event::Start(_) => {
			level += 1;
			max_nesting = std::cmp::max(max_nesting, level);
		}
		Event::End(_) => level -= 1,
		_ => ()
	}
}
```

Note that consecutive text events can happen due to the manner in which the
parser evaluates the source. A utility `TextMergeStream` exists to improve
the comfort of iterating the events:

```rust
use pulldown_cmark::{Event, Parser, Options};

let markdown_input = "Hello world, this is a ~~complicated~~ *very simple* example.";

let iterator = TextMergeStream::new(Parser::new(markdown_input));

for event in iterator {
    match event {
        Event::Text(text) => println!("{}", text),
        _ => {}
    }
}
```

More detailed examples can be found in the Rust API docs.

## Using Rust idiomatically

A lot of the internal scanning code is written at a pretty low level (it
pretty much scans byte patterns for the bits of syntax), but the external
interface is designed to be idiomatic Rust.

Pull parsers are at heart an iterator of events (start and end tags, text,
and other bits and pieces). The parser data structure implements the
Rust Iterator trait directly, and Event is an enum. Thus, you can use the
full power and expressivity of Rust's iterator infrastructure, including
for loops and `map` (as in the examples above), collecting the events into
a vector (for recording, playback, and manipulation), and more.

Further, the `Text` event (representing text) is a small copy-on-write string.
The vast majority of text fragments are just
slices of the source document. For these, copy-on-write gives a convenient
representation that requires no allocation or copying, but allocated
strings are available when they're needed. Thus, when rendering text to
HTML, most text is copied just once, from the source document to the
HTML buffer.

When using the pulldown-cmark's own HTML renderer, make sure to write to a buffered
target like a `Vec<u8>` or `String`. Since it performs many (very) small writes, writing
directly to stdout, files, or sockets is detrimental to performance. Such writers can
be wrapped in a [`BufWriter`](https://doc.rust-lang.org/std/io/struct.BufWriter.html).

## Build options

By default, the binary is built as well. If you don't want/need it, then build like this:

```bash
> cargo build --no-default-features
```

Or put in your `Cargo.toml` file:

```toml
pulldown-cmark = { version = "0.10.3", default-features = false }
```

SIMD accelerated scanners are available for the x64 platform from version 0.5 onwards. To
enable them, build with simd feature:

```bash
> cargo build --release --features simd
```

Or add the feature to your project's `Cargo.toml`:

```toml
pulldown-cmark = { version = "0.10.3", default-features = false, features = ["simd"] }
```

For a higher release performance you may want this configuration in your profile release:

```
lto = true
codegen-units = 1
panic = "abort"
```
