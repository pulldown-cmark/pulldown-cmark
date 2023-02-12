// Example illustrates how to write alternate HTML tag
// In this case, when there is an inline link to an audio file
// a class attribute is addeed

use pulldown_cmark::{Event, LinkType, Parser, Tag, html};
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
Audio link with text: [Play Audio](https://file-examples.com/storage/feeb72b10363daaeba4c0c9/2017/11/file_example_MP3_700KB.mp3)
"#;
println!("\nParsing the following markdown string:\n{}\n", markdown_input);

    let mut parser = Parser::new(markdown_input);
    let mut new_event_list: Vec<Event> = Vec::new();
    while let Some(event) = parser.next() {
        match event {
            Event::Start(Tag::Link(LinkType::Inline, dest, title)) => {
                if is_audio_file(&dest) {
                    if let Some(next_event) = parser.next() {
                        let link_text = if let Event::Text(text) = next_event {
                            parser.next();  // skip Event::End
                            text
                        } else {
                            // no text event, just Event::End
                            CowStr::Borrowed("")
                        };
                        new_event_list.push(Event::Html(format!("<a href=\"{}\" title=\"{}\" class=\"audio\">{}</a>", 
                                                                    dest, title, link_text).into()));
                    } 

                } else {
                    new_event_list.push(Event::Start(Tag::Link(LinkType::Inline, dest, title)));
                }
            },
            _ => new_event_list.push(event)
        }
    };

    let mut html_output = String::new();
    html::push_html(&mut html_output, new_event_list.into_iter());

    println!("\nHTML output:\n{}\n", &html_output);
    Ok(())
}
