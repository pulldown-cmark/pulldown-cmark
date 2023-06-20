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

use std::convert::TryInto;

use anyhow::anyhow;
use libfuzzer_sys::fuzz_target;
use once_cell::sync::OnceCell;
use pretty_assertions::assert_eq;
use pulldown_cmark::{CodeBlockKind, Event, LinkType, Parser, Tag, TagEnd};
use quick_xml::escape::unescape;
use quick_xml::events::Event as XmlEvent;
use quick_xml::reader::Reader;
use rquickjs::{Context, Function, Runtime};
use urlencoding::decode as urldecode;

/// Send Markdown `text` to `pulldown-cmark` and return Markdown
/// events.
fn pulldown_cmark(text: &str) -> Vec<Event<'_>> {
    Parser::new(text).collect()
}

/// Send Markdown `text` to `commonmark.js` and return XML.
fn commonmark_js(text: &str) -> anyhow::Result<String> {
    const COMMONMARK_MIN_JS: &str =
        include_str!("../../third_party/commonmark.js/commonmark.min.js");
    static RUNTIME: OnceCell<Runtime> = OnceCell::new();
    static CONTEXT: OnceCell<Context> = OnceCell::new();

    let runtime = RUNTIME.get_or_try_init(Runtime::new)?;
    let context = CONTEXT.get_or_try_init(|| {
        let context = Context::full(runtime)?;
        context.with(|ctx| {
            ctx.eval(COMMONMARK_MIN_JS)?;
            ctx.eval(
                r#"
                  function render_to_xml(markdown) {
                    var reader = new commonmark.Parser();
                    var xmlwriter = new commonmark.XmlRenderer({ sourcepos: false });
                    return xmlwriter.render(reader.parse(markdown));
                  }
                "#,
            )?;
            rquickjs::Result::Ok(())
        })?;
        rquickjs::Result::Ok(context)
    })?;

    Ok(context.with(|ctx| {
        let render_to_xml: Function = ctx.globals().get("render_to_xml")?;
        render_to_xml.call((text,))
    })?)
}

/// Parse commonmark.js XML and return Markdown events.
fn xml_to_events(xml: &str) -> anyhow::Result<Vec<Event>> {
    let mut list_stack = Vec::new();
    let mut heading_stack = Vec::new();

    let mut reader = Reader::from_str(xml);
    let mut events: Vec<Event> = Vec::new();
    loop {
        match reader.read_event()? {
            XmlEvent::Eof => break,
            XmlEvent::Decl(..) | XmlEvent::DocType(..) => continue,
            XmlEvent::Start(tag) => match tag.name().as_ref() {
                b"document" => continue,
                b"paragraph" => events.push(Event::Start(Tag::Paragraph)),
                b"heading" => match tag.try_get_attribute("level")? {
                    Some(level) => {
                        let level = level
                            .unescape_value()?
                            .parse::<usize>()?
                            .try_into()
                            .map_err(|err| anyhow!("Invalid level: {err:?}"))?;
                        heading_stack.push(level);
                        events.push(Event::Start(Tag::Heading {
                            level,
                            id: None,
                            classes: Vec::new(),
                            attrs: Vec::new(),
                        }));
                    }
                    None => anyhow::bail!("Missing level in heading"),
                },
                b"text" => {
                    events.push(Event::Text(
                        unescape(&reader.read_text(tag.to_end().name())?)?
                            .into_owned()
                            .into(),
                    ));
                }
                b"code_block" => {
                    match tag.try_get_attribute("info")? {
                        Some(info) => events.push(Event::Start(Tag::CodeBlock(
                            CodeBlockKind::Fenced(info.unescape_value()?.into_owned().into()),
                        ))),
                        None => events.push(Event::Start(Tag::CodeBlock(CodeBlockKind::Indented))),
                    }
                    events.push(Event::Text(
                        unescape(&reader.read_text(tag.to_end().name())?)?
                            .into_owned()
                            .into(),
                    ));
                    events.push(Event::End(TagEnd::CodeBlock));
                }
                b"list" => {
                    let start = tag.try_get_attribute("start")?;
                    match &start {
                        Some(start) => events.push(Event::Start(Tag::List(Some(
                            start.unescape_value()?.parse()?,
                        )))),
                        None => events.push(Event::Start(Tag::List(None))),
                    };
                    list_stack.push(start.is_some());
                }
                b"item" => events.push(Event::Start(Tag::Item)),
                b"strong" => events.push(Event::Start(Tag::Strong)),
                b"emph" => events.push(Event::Start(Tag::Emphasis)),
                b"code" => events.push(Event::Code(
                    unescape(&reader.read_text(tag.to_end().name())?)?
                        .into_owned()
                        .into(),
                )),
                name @ (b"link" | b"image") => {
                    let url_encoded_dest_url = tag
                        .try_get_attribute("destination")?
                        .ok_or(anyhow!("Missing destination"))?
                        .unescape_value()?;
                    let dest_url = urldecode(&url_encoded_dest_url)?.into_owned().into();
                    let title = match tag.try_get_attribute("title")? {
                        Some(title) => title.unescape_value()?.into_owned().into(),
                        None => "".into(),
                    };
                    let link_type = LinkType::Inline; // commonmark.js does not distinguish.
                    let id = "".into(); // commonmark.js does not record this.
                    events.push(Event::Start(if name == b"link" {
                        Tag::Link {
                            link_type,
                            dest_url,
                            title,
                            id,
                        }
                    } else {
                        Tag::Image {
                            link_type,
                            dest_url,
                            title,
                            id,
                        }
                    }));
                }
                b"block_quote" => events.push(Event::Start(Tag::BlockQuote)),
                b"html_block" | b"html_inline" => events.push(Event::Html(
                    unescape(&reader.read_text(tag.to_end().name())?)?
                        .into_owned()
                        .into(),
                )),
                name => anyhow::bail!("start tag: {}", String::from_utf8_lossy(name)),
            },
            XmlEvent::End(tag) => match tag.name().as_ref() {
                b"document" => continue,
                b"paragraph" => events.push(Event::End(TagEnd::Paragraph)),
                b"heading" => events.push(Event::End(TagEnd::Heading(
                    heading_stack.pop().ok_or(anyhow!("Heading stack empty"))?,
                ))),
                b"list" => events.push(Event::End(TagEnd::List(
                    list_stack.pop().ok_or(anyhow!("List stack empty"))?,
                ))),
                b"item" => events.push(Event::End(TagEnd::Item)),
                b"emph" => events.push(Event::End(TagEnd::Emphasis)),
                b"strong" => events.push(Event::End(TagEnd::Strong)),
                b"link" => events.push(Event::End(TagEnd::Link)),
                b"image" => events.push(Event::End(TagEnd::Image)),
                b"block_quote" => events.push(Event::End(TagEnd::BlockQuote)),
                name => anyhow::bail!("end tag: {}", String::from_utf8_lossy(name)),
            },
            XmlEvent::Text(_) => continue,
            XmlEvent::Empty(tag) => match tag.name().as_ref() {
                b"thematic_break" => events.push(Event::Rule),
                b"softbreak" => events.push(Event::SoftBreak),
                b"linebreak" => events.push(Event::HardBreak),
                name => anyhow::bail!("empty tag: {}", String::from_utf8_lossy(name)),
            },
            event => anyhow::bail!("event {event:?}"),
        }
    }

    Ok(events)
}

/// Normalize Markdown events
///
/// - Joins adjacent `Event::Text` and `Event::Html` events.
///
/// - Ensures every `Tag::Item` has a `Tag::Paragraph` as its first
///   child (commonmark.js tracks items loseness via an attribute).
///
/// - Adds a final newline to non-empty `Tag::CodeBlock` tags.
///
/// - Resets the link type to `LinkType::Inline`.
///
/// - Resets all code blocks to `CodeBlockKind::Fenced`.
fn normalize(events: Vec<Event<'_>>) -> Vec<Event<'_>> {
    let mut normalized = Vec::with_capacity(events.len());
    for event in events.into_iter() {
        match (normalized.last_mut(), &event) {
            // Join adjacent text events.
            (Some(Event::Text(prev)), Event::Text(next)) => *prev = format!("{prev}{next}").into(),
            // Join adjacent HTML events.
            (Some(Event::Html(prev)), Event::Html(next)) => *prev = format!("{prev}{next}").into(),
            // commonmark.js always wraps list items in a paragraph
            (Some(Event::Start(Tag::Item)), next) if next != &Event::Start(Tag::Paragraph) => {
                normalized.push(Event::Start(Tag::Paragraph));
                normalized.push(event);
            }
            // commonmark.js always wraps list items in a paragraph
            (Some(prev), Event::End(TagEnd::Item)) if prev != &Event::End(TagEnd::Paragraph) => {
                normalized.push(Event::End(TagEnd::Paragraph));
                normalized.push(event);
            }
            // commonmark.js adds an empty text event to an empty code block.
            (Some(Event::Start(Tag::CodeBlock(_))), Event::End(TagEnd::CodeBlock)) => {
                normalized.push(Event::Text("".into()));
                normalized.push(event);
            }
            // commonmark.js always adds a final newline to code blocks.
            (Some(Event::Text(prev)), Event::End(TagEnd::CodeBlock))
                if !prev.is_empty() && !prev.ends_with('\n') =>
            {
                *prev = format!("{prev}\n").into();
                normalized.push(event);
            }
            (_, _) => normalized.push(event),
        }
    }

    normalized
        .into_iter()
        .map(|event| match event {
            // commonmark.js does not record the link type.
            Event::Start(Tag::Link {
                dest_url,
                title,
                id,
                ..
            }) => Event::Start(Tag::Link {
                link_type: LinkType::Inline,
                dest_url: dest_url.clone(),
                title: title.clone(),
                id: id.clone(),
            }),
            // commonmark.js does not record the link type.
            Event::Start(Tag::Image {
                dest_url,
                title,
                id,
                ..
            }) => Event::Start(Tag::Image {
                link_type: LinkType::Inline,
                dest_url: dest_url.clone(),
                title: title.clone(),
                id: id.clone(),
            }),
            // commonmark.js does not distinguish between fenced code
            // blocks with a "" info string and indented code blocks.
            Event::Start(Tag::CodeBlock(CodeBlockKind::Indented)) => {
                Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced("".into())))
            }
            // pulldown-cmark includes trailing newlines in HTML.
            Event::Html(html) => Event::Html(html.trim_end_matches('\n').to_string().into()),
            event => event,
        })
        .collect()
}

fn print_events(text: &str, events: &[Event]) {
    eprintln!("{text:?} -> [");
    let mut width = 0;
    for event in events {
        if let Event::End(_) = event {
            width -= 2;
        }
        eprintln!("  {:width$}{event:?}", "");
        if let Event::Start(_) = event {
            width += 2;
        }
    }
    eprintln!("]");
}

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
