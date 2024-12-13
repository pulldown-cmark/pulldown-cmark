use pulldown_cmark::{html, Event, LinkType, Options, Parser, Tag};
use std::io::Write;

/// This example demonstrates how to prefix wikilinks with a special prefix, to
/// correctly render hrefs.
fn main() {
    // your wiki is stored at "/wiki"
    let prefix: &str = "/wiki";
    let markdown_input: &str = "Wanna go for a [[Wiki Walk]]?";

    let parser = Parser::new_ext(markdown_input, Options::ENABLE_WIKILINKS).map(|event| {
        if let Event::Start(Tag::Link {
            link_type: LinkType::WikiLink,
            dest_url,
            title,
            id,
        }) = event
        {
            // prefix wikilink
            let mut new_link = String::with_capacity(prefix.len() + dest_url.len());
            new_link.push_str(prefix);
            new_link.push_str(&dest_url);
            Event::Start(Tag::Link {
                link_type: LinkType::WikiLink,
                dest_url: new_link.into(),
                title,
                id,
            })
        } else {
            event
        }
    });

    // Write to anything implementing the `Write` trait. This could also be a file
    // or network socket.
    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    handle.write_all(b"\nHTML output:\n").unwrap();
    html::write_html_io(&mut handle, parser).unwrap();
}
