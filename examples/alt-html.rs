use pulldown_cmark::{Event, LinkType, Parser, Tag, html::HtmlWriter};
use pulldown_cmark::escape::{escape_href, escape_html};
use pulldown_cmark::CowStr;
use std::io;
use std::path::Path;

fn is_audio_file(url: &CowStr) -> bool {
    let audio_format = ["mp3", "mp4", "m4a", "wav", "ogg"];
    let path = Path::new(url.as_ref());
    if let Some(ext_osstr) = path.extension() {
        let extension = ext_osstr.to_string_lossy().to_lowercase();
        if audio_format.contains(&extension.as_str()) {
            return true
        }
    }
    false
}

fn main() -> io::Result<()> {
    let markdown_input = r#"
# Example Heading
Example paragraph with **lorem** _ipsum_ text.
Normal link: [example](https://example.com)
Audio link: [](https://file-examples.com/storage/feeb72b10363daaeba4c0c9/2017/11/file_example_MP3_700KB.mp3)
"#;
println!("\nParsing the following markdown string:\n{}\n", markdown_input);

    let parser = Parser::new(markdown_input);
    let mut html_output = String::new();
    let mut html = HtmlWriter::new(parser, &mut html_output);
    
    while let Some(event) = html.iter.next() {
        match &event {
            Event::Start(Tag::Link(LinkType::Inline, dest, title)) => {
                if is_audio_file(dest) {
                    html.write("<a href=\"")?;
                    escape_href(&mut html.writer, &dest)?;
                    html.write("\" title=\"")?;
                    if !title.is_empty() {
                        escape_html(&mut html.writer, &title)?;
                    } else {
                        html.write("Play Audio")?;
                    }
                    html.write("\" class=\"audio\">")?;
                } else {
                    html.render_event(event)?
                }
            },
            _ => html.render_event(event)?,
        };
    };

    println!("\nHTML output:\n{}\n", &html_output);
    Ok(())
}
