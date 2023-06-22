#![no_main]

//! Differential fuzzing of pulldown-cmark and commonmark.js.
//!
//! This fuzzer sends the same input to both Markdown parsers and
//! compares the output. The output from commonmark.js is turned into
//! `pulldown_cmark::Event` values for this purpose.
//!
//! Run the fuzzer like this to only test ASCII input (which is
//! usually enough to find parsing differences):
//!
//!     cargo fuzz run commonmark_js -- -only_ascii=1

use libfuzzer_sys::fuzz_target;
use pretty_assertions::assert_eq;
use pulldown_cmark_fuzz::{commonmark_js, normalize, print_events, pulldown_cmark, xml_to_events};

fuzz_target!(|text: String| {
    // There are some differences in handling of non-UTF-8 input.
    if text.bytes().any(|b| b.is_ascii_control() && b != b'\n') {
        return;
    }

    // https://github.com/raphlinus/pulldown-cmark/issues/659
    if text.contains("<!") {
        return;
    }

    // There are some trivial differences due to trailing whitespace.
    let text = text
        .lines()
        .map(|line| line.trim_end())
        .collect::<Vec<_>>()
        .join("\n");

    let pulldown_cmark_events = normalize(pulldown_cmark(&text));
    let commonmark_js_xml = &commonmark_js(&text).unwrap();

    let raw_events = match xml_to_events(commonmark_js_xml) {
        Ok(raw_events) => raw_events,
        Err(err) => {
            print_events(&text, &pulldown_cmark_events);
            eprintln!("XML from commonmark.js:\n{commonmark_js_xml}");
            panic!("Could not parse XML: {}", err);
        }
    };

    let commonmark_js_events = normalize(raw_events);
    if pulldown_cmark_events != commonmark_js_events {
        eprintln!("Events from pulldown-cmark:\n\n```rust");
        print_events(&text, &pulldown_cmark_events);
        eprintln!("```");
        eprintln!();

        eprintln!("Events from commonmark.js:\n\n```rust");
        print_events(&text, &commonmark_js_events);
        eprintln!("```");
        eprintln!();

        let dingus_url = format!(
            "https://spec.commonmark.org/dingus/?text={}",
            urlencoding::encode(&text)
        );
        eprintln!("XML from [commonmark.js]({dingus_url}):\n\n```xml\n{commonmark_js_xml}```");
        eprintln!();

        assert_eq!(pulldown_cmark_events, commonmark_js_events);
    }
});
