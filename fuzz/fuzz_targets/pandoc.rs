#![no_main]

//! Differential fuzzing of pulldown-cmark and pandoc commonmark.
//!
//! This fuzzer sends the same input to both Markdown parsers and
//! compares the output. Pandoc's JSON AST is turned into
//! `pulldown_cmark::Event` values for this purpose.
//!
//! Run the fuzzer like this to only test ASCII input (which is
//! usually enough to find parsing differences):
//!
//!     cargo fuzz run pandoc -- -only_ascii=1

use libfuzzer_sys::fuzz_target;
use pretty_assertions::assert_eq;
use pulldown_cmark_fuzz::{PandocHandle, normalize_pandoc, print_events, pulldown_cmark_ext, xml_to_events, commonmark_js};
use std::sync::OnceLock;

static PANDOC: OnceLock<PandocHandle> = OnceLock::new();

fuzz_target!(|text: String| {
    // There are some differences in handling of non-UTF-8 input.
    if text.bytes().any(|b| b.is_ascii_control() && b != b'\n') {
        return;
    }

    // https://github.com/jgm/commonmark-hs/issues/149
    if text.contains(r"\&") {
        return;
    }
    if text.contains("[^ ") {
        return;
    }
    if text.contains("[^\t") {
        return;
    }
    if text.contains("[^\n") {
        return;
    }
    if text.contains("[^\r") {
        return;
    }
    if text.contains(" ]") {
        return;
    }
    if text.contains("\t]") {
        return;
    }
    if text.contains("\n]") {
        return;
    }
    if text.contains("\r]") {
        return;
    }
    if text.contains("[ ") {
        return;
    }
    if text.contains("[\t") {
        return;
    }
    if text.contains("[\n") {
        return;
    }
    if text.contains("[\r") {
        return;
    }
    // https://github.com/jgm/commonmark-hs/issues/136
    if text.contains("<") || text.contains("`") {
        return;
    }

    if text.bytes().any(|b| b > 127) {
        return;
    }

    // There are some trivial differences due to trailing whitespace.
    let mut text = text
        .lines()
        .map(|line| line.trim_end())
        .collect::<Vec<_>>()
        .join("\n");
    text.push('\n');

    let pulldown_cmark_events = pulldown_cmark_ext(&text);

    // Make sure there aren't cyclical footnotes.
    // pulldown-cmark supports them, but pandoc does not
    // and making it work would require completely redesigning
    // the pandoc ast.
    //
    // commonmark-hs also trims footnote reference names.
    // I'm not going to bother reporting it as a bug, since it's
    // obviously on purpose and it's not gonna matter.
    let mut footstack = vec![];
    for event in &pulldown_cmark_events {
        use pulldown_cmark::{Event, Tag, TagEnd};
        match event {
            Event::Start(Tag::FootnoteDefinition(id)) => {
                if id.starts_with("\n") || id.ends_with("\n") || id.starts_with("\r") || id.ends_with("\r") || id.starts_with(" ") || id.starts_with("\t") || id.contains("  ") || id.contains("\t ") || id.contains(" \t") || id.contains("\t\t") || id.ends_with(" ") || id.ends_with("\t") { return };
                footstack.push(id.trim().to_string());
            }
            Event::End(TagEnd::FootnoteDefinition) => {
                footstack.pop();
            }
            Event::FootnoteReference(id) => {
                if !footstack.is_empty() {
                //if footstack.contains(&id.trim().to_string()) {
                    return;
                }
            }
            _ => {}
        }
    }
    let pulldown_cmark_events = normalize_pandoc(pulldown_cmark_events);
    let mut footstack = vec![];
    let mut liststack = 0;
    for event in &pulldown_cmark_events {
        use pulldown_cmark::{Event, Tag, TagEnd};
        match event {
            Event::Start(Tag::CodeBlock(..)) => {
                // differences in list tightness
                // https://github.com/jgm/commonmark-hs/issues/144
                if liststack != 0 {
                    return;
                }
            }
            Event::Start(Tag::Item) => {
                // differences in list tightness
                liststack += 1;
            }
            Event::End(TagEnd::Item) => {
                // differences in list tightness
                liststack -= 1;
            }
            Event::Start(Tag::FootnoteDefinition(id)) => {
                footstack.push(id.to_string());
            }
            Event::End(TagEnd::FootnoteDefinition) => {
                footstack.pop();
            }
            Event::FootnoteReference(id) => {
                if footstack.contains(&id.to_string()) {
                    return;
                }
            }
            _ => {}
        }
    }

    let pandoc = PANDOC.get_or_init(PandocHandle::new);

    let pandoc_ast = match pandoc.get_ast(&text) {
        Ok(pandoc_ast) => pandoc_ast,
        Err(err) => {
            print_events(&text, &pulldown_cmark_events);
            if err.to_string().contains("recursion limit") {
                return;
            } else {
                panic!("Could not get Pandoc JSON: {}", err);
            }
        }
    };

    let raw_events = match pandoc_ast.to_events() {
        Ok(raw_events) => raw_events,
        Err(err) => {
            print_events(&text, &pulldown_cmark_events);
            eprintln!("AST from pandoc:\n{pandoc_ast:#?}");
            panic!("Could not convert Pandoc AST: {}", err);
        }
    };

    let pandoc_events = normalize_pandoc(raw_events);
    if pulldown_cmark_events != pandoc_events {
        eprintln!("Events from pulldown-cmark:\n\n```rust");
        print_events(&text, &pulldown_cmark_events);
        eprintln!("```");
        eprintln!();

        eprintln!("Events from pandoc:\n\n```rust");
        print_events(&text, &pandoc_events);
        eprintln!("```");
        eprintln!();

        // For completeness's sake, also include commonmark.js
        // It's not used in this fuzzer, but it's convenient to tell if it's
        // just Pandoc being weird.
        let commonmark_js_xml = &commonmark_js(&text).unwrap();

        match xml_to_events(commonmark_js_xml) {
            Ok(raw_events_cmjs) => {
                let commonmark_js_events = normalize_pandoc(raw_events_cmjs);
                eprintln!("Events from commonmark.js:\n\n```rust");
                print_events(&text, &commonmark_js_events);
                eprintln!("```");
                eprintln!();
            },
            Err(..) => {}
        };

        #[derive(serde::Serialize)]
        struct DingusParams<'a> {
            text: &'a str,
            to: &'a str,
            from: &'a str,
        }
        let dingus_params = serde_json::to_string(&DingusParams {
            text: &text,
            to: "json",
            from: "commonmark",
        }).unwrap();

        let dingus_url = format!(
            "https://pandoc.org/try/?params={}",
            urlencoding::encode(&dingus_params)
        );
        eprintln!("AST from [pandoc]({dingus_url}):\n\n```text\n{pandoc_ast:#?}\n```");
        eprintln!();

        assert_eq!(pulldown_cmark_events, pandoc_events);
    }
});
