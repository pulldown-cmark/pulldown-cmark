use std::{collections::HashMap, fmt::Write as _, io::Write as _};
use pulldown_cmark::{html::{write_html_io}, CowStr, Event, Options, Parser, Tag, TagEnd};

/// This example shows how to do footnotes as bottom-notes, in the style of GitHub.
fn main() {
    let markdown_input: &str = "This is an [^a] footnote [^a].\n\n[^a]: footnote contents";
    println!("Parsing the following markdown string:\n{}", markdown_input);

    // To generate this style, you have to collect the footnotes at the end, while parsing.
    // You also need to count usages.
    let mut footnote_filter = FootnoteFilter::new();

    // ENABLE_FOOTNOTES is used in this example, but ENABLE_OLD_FOOTNOTES would work, too.
    let parser = Parser::new_ext(markdown_input, Options::ENABLE_FOOTNOTES)
        .filter_map(|event| {
            footnote_filter.apply(event)
        });

    // Write to anything implementing the `Write` trait. This could also be a file
    // or network socket.
    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    handle.write_all(b"\nHTML output:\n").unwrap();
    write_html_io(&mut handle, parser).unwrap();

    // To make the footnotes look right, we need to sort them by their appearance order, not by
    // the in-tree order of their actual definitions. Unused items are omitted entirely.
    //
    // For example, this code:
    //
    //     test [^1] [^2]
    //     [^2]: second used, first defined
    //     [^1]: test
    //
    // Gets rendered like *this* if you copy it into a GitHub comment box:
    //
    //     <p>test <sup>[1]</sup> <sup>[2]</sup></p>
    //     <hr>
    //     <ol>
    //     <li>test ↩</li>
    //     <li>second used, first defined ↩</li>
    //     </ol>
    if !footnote_filter.is_empty() {
        footnote_filter.retain();
        footnote_filter.sort_by_cached_key();
        println!("After sort_by {:?}", &footnote_filter.footnotes);
        handle
            .write_all(b"<hr><ol class=\"footnotes-list\">\n")
            .unwrap();
        let events = footnote_filter.get_events();
        write_html_io(
            &mut handle,
            events,
        )
        .unwrap();
        handle.write_all(b"</ol>\n").unwrap();
    }
}

/// Reusable footnote filter.
struct FootnoteFilter<'a> {
    footnotes: Vec<Vec<Event<'a>>>,
    in_footnote: Vec<Vec<Event<'a>>>,
    footnote_numbers: HashMap<CowStr<'a>, (usize, usize)>,
}
impl<'a> FootnoteFilter<'a> {
    pub fn new() -> Self {
        Self {
            footnotes: Vec::new(),
            in_footnote: Vec::new(),
            footnote_numbers: HashMap::new(),
        }
    }
    pub fn apply(&mut self, event: Event<'a>) -> Option<Event<'a>> {
        match event {
            Event::Start(Tag::FootnoteDefinition(ref text)) => {
                println!("Start(Tag::FootnoteDefinition = {}", &text);
                self.push_footnote_events(vec![event]);
                None
            }
            Event::End(TagEnd::FootnoteDefinition) => {
                self.move_from_in_to_footnotes(event);
                None
            }
            Event::FootnoteReference(name) => {
                println!("FootnoteReference = {}", &name);
                let n = self.footnote_numbers.len() + 1;
                let (n, mut nr) = self.footnote_numbers.entry(name.clone()).or_insert((n, 0usize));
                nr += 1;
                let html = Event::Html(format!(r##"<sup class="footnote-reference" id="fr-{name}-{nr}"><a href="#fn-{name}">[{n}]</a></sup>"##).into());
                if self.is_empty_in_footnote() {
                    Some(html)
                } else {
                    self.replace_last_in_footnote(html);
                    None
                }
            }
            _ if !self.is_empty_in_footnote() => {
                self.replace_last_in_footnote(event);
                None
            }
            _ => Some(event),
        }

    }
    fn is_empty(&self) -> bool {
        self.footnotes.is_empty()
    }
    fn push_footnote_events(&mut self, footnotes: Vec<Event<'a>>) {
        self.in_footnote.push(footnotes);
    }
    fn is_empty_in_footnote(&self) -> bool {
        self.in_footnote.is_empty()
    }
    fn move_from_in_to_footnotes(&mut self, event: Event<'a>) {
        let popped_vector = self.in_footnote.pop();
        match popped_vector {
            None => {}
            Some(mut vector) => {
                println!("End(TagEnd::FootnoteDefinition... = {:?}", &vector);
                vector.push(event);
                self.footnotes.push(vector);
            }
        }
    }
    fn replace_last_in_footnote(&mut self, event: Event<'a>) {
        if let Some(value) = self.in_footnote.last_mut() { value.push(event) }
    }
    fn retain(&mut self) -> bool {
        let original_len = self.footnotes.len(); // number before retain
        println!("Before retain = {:?}", &self.footnotes);
        println!("Before retain numbers = {:?}", &self.footnote_numbers);
        self.footnotes.retain(|f| match f.first() {
            Some(Event::Start(Tag::FootnoteDefinition(name))) => {
                self.footnote_numbers.get(name).unwrap_or(&(0, 0)).1 != 0
            }
            _ => false,
        });
        println!("After retain {:?}", &self.footnotes);
        self.footnotes.len() != original_len // true, if number has changed
    }
    fn sort_by_cached_key(&mut self) {
        self.footnotes.sort_by_cached_key(|f| match f.first() {
            Some(Event::Start(Tag::FootnoteDefinition(name))) => {
                self.footnote_numbers.get(name).unwrap_or(&(0, 0)).0
            }
            _ => unreachable!(),
        });
    }
    fn _get_footnote_numbers_entry(&mut self, n: usize, name: CowStr<'a>) -> (usize, usize) {
        let (n, nr) = self.footnote_numbers.entry(name).or_insert((n, 0usize));
        (*n, *nr)
    }
    fn _get_footnote_numbers_len(&self) -> usize {
        self.footnote_numbers.len()
    }
    fn get_events(&self) -> impl Iterator<Item = Event> {
        self.footnotes.clone().into_iter().flat_map(move |external_flat_event| {
            // To write backrefs, the name needs kept until the end of the footnote definition.
            let mut name = CowStr::from("");
            println!("external_flat_event {:?}", &external_flat_event);
            // Backrefs are included in the final paragraph of the footnote, if it's normal text.
            // For example, this DOM can be produced:
            //
            // Markdown:
            //
            //     five [^feet].
            //
            //     [^feet]:
            //         A foot is defined, in this case, as 0.3048 m.
            //
            //         Historically, the foot has not been defined this way, corresponding to many
            //         subtly different units depending on the location.
            //
            // HTML:
            //
            //     <p>five <sup class="footnote-reference" id="fr-feet-1"><a href="#fn-feet">[1]</a></sup>.</p>
            //
            //     <ol class="footnotes-list">
            //     <li id="fn-feet">
            //     <p>A foot is defined, in this case, as 0.3048 m.</p>
            //     <p>Historically, the foot has not been defined this way, corresponding to many
            //     subtly different units depending on the location. <a href="#fr-feet-1">↩</a></p>
            //     </li>
            //     </ol>
            //
            // This is mostly a visual hack, so that footnotes use less vertical space.
            //
            // If there is no final paragraph, such as a tabular, list, or image footnote, it gets
            // pushed after the last tag instead.
            let mut has_written_backrefs = false;
            let fl_len = external_flat_event.len();
            // let footnote_numbers_ref = &self.footnote_numbers;
            println!("footnote_numbers = {:?}", &self.footnote_numbers);
            external_flat_event.into_iter().enumerate().map(move |(i, internal_flat_event)| match internal_flat_event {
                Event::Start(Tag::FootnoteDefinition(current_name)) => {
                    name = current_name;
                    has_written_backrefs = false;
                    Event::Html(format!(r##"<li id="fn-{name}">"##).into())
                }
                Event::End(TagEnd::FootnoteDefinition) | Event::End(TagEnd::Paragraph)
                if !has_written_backrefs && i >= fl_len - 2 =>
                    {
                        let usage_count = self.footnote_numbers.get(&name).unwrap().1;
                        let mut end = String::with_capacity(
                            name.len() + (r##" <a href="#fr--1">↩</a></li>"##.len() * usage_count),
                        );
                        for usage in 1..=usage_count {
                            if usage == 1 {
                                write!(&mut end, r##" <a href="#fr-{name}-{usage}">↩</a>"##)
                                    .unwrap();
                            } else {
                                write!(&mut end, r##" <a href="#fr-{name}-{usage}">↩{usage}</a>"##)
                                    .unwrap();
                            }
                        }
                        has_written_backrefs = true;
                        if internal_flat_event == Event::End(TagEnd::FootnoteDefinition) {
                            end.push_str("</li>\n");
                        } else {
                            end.push_str("</p>\n");
                        }
                        Event::Html(end.into())
                    }
                Event::End(TagEnd::FootnoteDefinition) => Event::Html("</li>\n".into()),
                Event::FootnoteReference(_) => unreachable!("converted to HTML earlier"),
                f => f,
            }).collect::<Vec<_>>()
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_holder() {
        let _holder = FootnoteFilter::new();
    }
}
