#![no_main]
use libfuzzer_sys::fuzz_target;

use libfuzzer_sys::arbitrary::{self, Arbitrary};
use pulldown_cmark::Options;

#[derive(Debug, Arbitrary)]
struct FuzzingInput<'a> {
    markdown: &'a str,
    tables: bool,
    footnotes: bool,
    strikethrough: bool,
    tasklists: bool,
    smart_punctuation: bool,
    heading_attributes: bool,
}

fuzz_target!(|data: FuzzingInput<'_>| {
    let mut opts = pulldown_cmark::Options::empty();

    if data.tables {
        opts.insert(Options::ENABLE_TABLES);
    }

    if data.footnotes {
        opts.insert(Options::ENABLE_FOOTNOTES);
    }

    if data.strikethrough {
        opts.insert(Options::ENABLE_STRIKETHROUGH);
    }

    if data.tasklists {
        opts.insert(Options::ENABLE_TASKLISTS);
    }

    if data.smart_punctuation {
        opts.insert(Options::ENABLE_SMART_PUNCTUATION);
    }

    if data.heading_attributes {
        opts.insert(Options::ENABLE_HEADING_ATTRIBUTES);
    }

    for _ in pulldown_cmark::Parser::new_ext(data.markdown, opts) {};
});
