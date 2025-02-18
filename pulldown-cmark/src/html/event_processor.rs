use crate::CowStr;
use crate::TagEnd;
use std::collections::HashMap;

use crate::html::HtmlElementRenderer;
use crate::Event;

use crate::Tag;

#[derive(Debug)]
pub struct HtmlEventProcessor<'a, I, R> {
    iter: I,
    renderer: R,
    numbers: HashMap<CowStr<'a>, usize>,
}

impl<'a, I, R> HtmlEventProcessor<'a, I, R>
where
    I: Iterator<Item = Event<'a>>,
    R: HtmlElementRenderer,
{
    pub fn new(iter: I, renderer: R) -> Self {
        Self {
            iter,
            renderer,
            numbers: HashMap::new(),
        }
    }

    pub fn run(mut self) -> Result<(), R::Error> {
        while let Some(event) = self.iter.next() {
            match event {
                Event::Start(tag) => self.process_start_tag(tag)?,
                Event::End(tag) => self.process_end_tag(tag)?,
                Event::Text(text) => self.renderer.render_text(&text)?,
                Event::Code(text) => {
                    self.renderer.render_code_inline(&text)?;
                }
                Event::InlineMath(text) => {
                    self.renderer.render_math_inline(&text)?;
                }
                Event::DisplayMath(text) => {
                    self.renderer.render_math_display(&text)?;
                }
                Event::Html(html) | Event::InlineHtml(html) => {
                    self.renderer.render_raw_html(&html)?;
                }
                Event::SoftBreak => {
                    self.renderer.render_soft_break()?;
                }
                Event::HardBreak => {
                    self.renderer.render_hard_break()?;
                }
                Event::Rule => {
                    self.renderer.render_horizontal_rule()?;
                }
                Event::FootnoteReference(name) => {
                    self.process_footnote_reference(name)?;
                }
                Event::TaskListMarker(checked) => {
                    self.renderer.render_task_list_marker(checked)?;
                }
            }
        }
        Ok(())
    }

    fn process_start_tag(&mut self, tag: Tag<'a>) -> Result<(), R::Error> {
        match tag {
            Tag::Paragraph => {
                self.renderer.render_paragraph_start()?;
            }
            Tag::Heading {
                level,
                id,
                classes,
                attrs,
            } => {
                self.renderer.render_heading_start(
                    level as u32,
                    id.as_deref(),
                    &classes,
                    &attrs,
                )?;
            }
            Tag::Table(alignments) => {
                self.renderer.render_table_start(&alignments)?;
            }
            Tag::TableHead => {
                self.renderer.render_table_head_start()?;
            }
            Tag::TableRow => {
                self.renderer.render_table_row_start()?;
            }
            Tag::TableCell => {
                self.renderer.render_table_cell_start()?;
            }
            Tag::BlockQuote(kind) => {
                self.renderer.render_blockquote_start(kind)?;
            }
            Tag::CodeBlock(info) => {
                self.renderer.render_code_block_start(&info)?;
            }
            Tag::List(start) => {
                self.renderer.render_list_start(start)?;
            }
            Tag::Item => {
                self.renderer.render_list_item_start()?;
            }
            Tag::DefinitionList => {
                self.renderer.render_definition_list_start()?;
            }
            Tag::DefinitionListTitle => {
                self.renderer.render_definition_title_start()?;
            }
            Tag::DefinitionListDefinition => {
                self.renderer.render_definition_list_definition_start()?;
            }
            Tag::Superscript => {
                self.renderer.render_superscript_start()?;
            }
            Tag::Subscript => {
                self.renderer.render_subscript_start()?;
            }
            Tag::Emphasis => {
                self.renderer.render_emphasis_start()?;
            }
            Tag::Strong => {
                self.renderer.render_strong_start()?;
            }
            Tag::Strikethrough => {
                self.renderer.render_strikethrough_start()?;
            }
            Tag::Link {
                link_type,
                dest_url,
                title,
                id: _,
            } => {
                self.renderer
                    .render_link_start(link_type, &dest_url, &title)?;
            }
            Tag::Image {
                link_type: _,
                dest_url,
                title,
                id: _,
            } => {
                self.process_image(&dest_url, &title)?;
            }
            Tag::FootnoteDefinition(name) => {
                self.process_footnote_definition(name)?;
            }
            Tag::MetadataBlock(kind) => self.renderer.render_metadata_start(kind)?,
            Tag::HtmlBlock => {}
        }
        Ok(())
    }

    fn process_end_tag(&mut self, tag: TagEnd) -> Result<(), R::Error> {
        match tag {
            TagEnd::Paragraph => self.renderer.render_paragraph_end()?,
            TagEnd::Heading(level) => self.renderer.render_heading_end(level as u32)?,
            TagEnd::Table => self.renderer.render_table_end()?,
            TagEnd::TableHead => self.renderer.render_table_head_end()?,
            TagEnd::TableRow => self.renderer.render_table_row_end()?,
            TagEnd::TableCell => self.renderer.render_table_cell_end()?,
            TagEnd::BlockQuote(kind) => self.renderer.render_blockquote_end(kind)?,
            TagEnd::CodeBlock => self.renderer.render_code_block_end()?,
            TagEnd::List(ordered) => self.renderer.render_list_end(ordered)?,
            TagEnd::Item => self.renderer.render_list_item_end()?,
            TagEnd::DefinitionList => self.renderer.render_definition_list_end()?,
            TagEnd::DefinitionListTitle => self.renderer.render_definition_title_end()?,
            TagEnd::DefinitionListDefinition => {
                self.renderer.render_definition_list_definition_end()?
            }
            TagEnd::Emphasis => self.renderer.render_emphasis_end()?,
            TagEnd::Subscript => self.renderer.render_subscript_end()?,
            TagEnd::Superscript => self.renderer.render_superscript_end()?,
            TagEnd::Strong => self.renderer.render_strong_end()?,
            TagEnd::Strikethrough => self.renderer.render_strikethrough_end()?,
            TagEnd::Link => self.renderer.render_link_end()?,
            TagEnd::Image => {} // Handled in start tag
            TagEnd::FootnoteDefinition => self.renderer.render_footnote_definition_end()?,
            TagEnd::MetadataBlock(kind) => self.renderer.render_metadata_end(kind)?,
            TagEnd::HtmlBlock => {} // No special handling needed
        }
        Ok(())
    }

    fn process_image(&mut self, url: &str, title: &str) -> Result<(), R::Error> {
        // Image tag events will be proceeded by any number of specific events that
        // amount to the alt text. Run the iterator until we consume all of these
        // nested events and accumulate the raw text from said events.
        let alt_text = self.capture_inner_text()?;
        self.renderer.render_image(url, title, &alt_text)
    }

    fn process_footnote_reference(&mut self, name: CowStr<'a>) -> Result<(), R::Error> {
        let len = self.numbers.len() + 1;
        let number = *self.numbers.entry(name.clone()).or_insert(len);
        self.renderer.render_footnote_reference(&name, number)
    }

    fn process_footnote_definition(&mut self, name: CowStr<'a>) -> Result<(), R::Error> {
        let len = self.numbers.len() + 1;
        let number = *self.numbers.entry(name.clone()).or_insert(len);
        self.renderer
            .render_footnote_definition_start(&name, number)
    }

    // Helper method to capture text content between tags
    fn capture_inner_text(&mut self) -> Result<String, R::Error> {
        let mut text = String::new();
        let mut nest = 0;

        for event in self.iter.by_ref() {
            match event {
                Event::Start(_) => nest += 1,
                Event::End(_) => {
                    if nest == 0 {
                        break;
                    }
                    nest -= 1;
                }
                Event::Text(content) | Event::Code(content) | Event::InlineHtml(content) => {
                    text.push_str(&content);
                }
                Event::InlineMath(content) => {
                    text.push('$');
                    text.push_str(&content);
                    text.push('$');
                }
                Event::DisplayMath(content) => {
                    text.push_str("$$");
                    text.push_str(&content);
                    text.push_str("$$");
                }
                Event::SoftBreak | Event::HardBreak | Event::Rule => {
                    text.push(' ');
                }
                Event::FootnoteReference(name) => {
                    let len = self.numbers.len() + 1;
                    let number = *self.numbers.entry(name).or_insert(len);
                    text.push_str(&format!("[{}]", number));
                }
                Event::TaskListMarker(true) => text.push_str("[x]"),
                Event::TaskListMarker(false) => text.push_str("[ ]"),
                Event::Html(_) => {}
            }
        }
        Ok(text)
    }
}
