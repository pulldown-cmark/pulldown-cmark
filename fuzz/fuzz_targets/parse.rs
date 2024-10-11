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
    metadata_block: bool,
    math: bool,
    gfm: bool,
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

    if data.super_sub {
        opts.insert(Options::ENABLE_SUPER_SUB);
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

    if data.metadata_block {
        opts.insert(Options::ENABLE_YAML_STYLE_METADATA_BLOCKS);
    }

    if data.math {
        opts.insert(Options::ENABLE_MATH);
    }

    if data.gfm {
        opts.insert(Options::ENABLE_GFM);
    }

    for _ in pulldown_cmark::Parser::new_ext(data.markdown, opts) {}
});
