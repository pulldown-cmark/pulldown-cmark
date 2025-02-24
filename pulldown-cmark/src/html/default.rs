use crate::html::element_renderer::{HtmlElementRenderer, TableState};
use crate::{Alignment, BlockQuoteKind, CodeBlockKind, CowStr, LinkType, MetadataBlockKind};
use pulldown_cmark_escape::{escape_href, escape_html, escape_html_body_text};

#[derive(Debug)]
pub struct HtmlWriter<W: pulldown_cmark_escape::StrWrite> {
    pub writer: W,
    end_newline: bool,
    in_non_writing_block: bool,
    table_state: TableState,
    table_alignments: Vec<Alignment>,
    table_cell_index: usize,
}

impl<W: pulldown_cmark_escape::StrWrite> HtmlWriter<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            end_newline: true,
            in_non_writing_block: false,
            table_state: TableState::Head,
            table_alignments: Vec::new(),
            table_cell_index: 0,
        }
    }

    pub fn write(&mut self, s: &str) -> Result<(), W::Error> {
        self.writer.write_str(s)?;
        if !s.is_empty() {
            self.end_newline = s.ends_with('\n');
        }
        Ok(())
    }

    pub fn write_newline(&mut self) -> Result<(), W::Error> {
        self.end_newline = true;
        self.writer.write_str("\n")
    }
}

impl<W: pulldown_cmark_escape::StrWrite> HtmlElementRenderer for HtmlWriter<W> {
    type Error = W::Error;

    // Table state management
    fn get_table_state(&self) -> TableState {
        self.table_state
    }

    fn set_table_state(&mut self, state: TableState) {
        self.table_state = state;
    }

    fn get_table_alignments(&self) -> &[Alignment] {
        &self.table_alignments
    }

    fn set_table_alignments(&mut self, alignments: Vec<Alignment>) {
        self.table_alignments = alignments;
    }

    fn get_table_cell_index(&self) -> usize {
        self.table_cell_index
    }

    fn set_table_cell_index(&mut self, index: usize) {
        self.table_cell_index = index;
    }

    fn increment_table_cell_index(&mut self) {
        self.table_cell_index += 1;
    }

    // Core rendering methods
    fn render_text(&mut self, text: &CowStr) -> Result<(), Self::Error> {
        if !self.in_non_writing_block {
            escape_html_body_text(&mut self.writer, text)?;
            self.end_newline = text.ends_with('\n');
        }
        Ok(())
    }

    fn render_code_inline(&mut self, text: &str) -> Result<(), Self::Error> {
        self.write("<code>")?;
        escape_html_body_text(&mut self.writer, text)?;
        self.write("</code>")
    }

    fn render_math_inline(&mut self, text: &str) -> Result<(), Self::Error> {
        self.write(r#"<span class="math math-inline">"#)?;
        escape_html(&mut self.writer, text)?;
        self.write("</span>")
    }

    fn render_math_display(&mut self, text: &str) -> Result<(), Self::Error> {
        self.write(r#"<span class="math math-display">"#)?;
        escape_html(&mut self.writer, text)?;
        self.write("</span>")
    }

    fn render_raw_html(&mut self, html: &str) -> Result<(), Self::Error> {
        self.write(html)
    }

    fn render_soft_break(&mut self) -> Result<(), Self::Error> {
        self.write_newline()
    }

    fn render_hard_break(&mut self) -> Result<(), Self::Error> {
        self.write("<br />\n")
    }

    fn render_horizontal_rule(&mut self) -> Result<(), Self::Error> {
        if self.end_newline {
            self.write("<hr />\n")
        } else {
            self.write("\n<hr />\n")
        }
    }

    fn render_task_list_marker(&mut self, checked: bool) -> Result<(), Self::Error> {
        if checked {
            self.write("<input disabled=\"\" type=\"checkbox\" checked=\"\"/>\n")
        } else {
            self.write("<input disabled=\"\" type=\"checkbox\"/>\n")
        }
    }

    // Block element methods
    fn render_paragraph_start(&mut self) -> Result<(), Self::Error> {
        if self.end_newline {
            self.write("<p>")
        } else {
            self.write("\n<p>")
        }
    }

    fn render_paragraph_end(&mut self) -> Result<(), Self::Error> {
        self.write("</p>\n")
    }

    fn render_heading_start(
        &mut self,
        level: u32,
        id: Option<&str>,
        classes: &[CowStr<'_>],
        attrs: &[(CowStr<'_>, Option<CowStr<'_>>)],
    ) -> Result<(), Self::Error> {
        if self.end_newline {
            self.write("<h")?;
        } else {
            self.write("\n<h")?;
        }
        write!(&mut self.writer, "{}", level)?;
        if let Some(id) = id {
            self.write(" id=\"")?;
            escape_html(&mut self.writer, id)?;
            self.write("\"")?;
        }
        let mut classes = classes.iter();
        if let Some(class) = classes.next() {
            self.write(" class=\"")?;
            escape_html(&mut self.writer, class)?;
            for class in classes {
                self.write(" ")?;
                escape_html(&mut self.writer, class)?;
            }
            self.write("\"")?;
        }
        for (attr, value) in attrs {
            self.write(" ")?;
            escape_html(&mut self.writer, attr)?;
            if let Some(val) = value {
                self.write("=\"")?;
                escape_html(&mut self.writer, val)?;
                self.write("\"")?;
            } else {
                self.write("=\"\"")?;
            }
        }
        self.write(">")
    }

    fn render_heading_end(&mut self, level: u32) -> Result<(), Self::Error> {
        self.write("</h")?;
        write!(&mut self.writer, "{}", level)?;
        self.write(">\n")
    }

    // Table methods
    fn render_table_start(&mut self, alignments: &[Alignment]) -> Result<(), Self::Error> {
        self.table_alignments = alignments.to_vec();
        self.write("<table>")
    }

    fn render_table_end(&mut self) -> Result<(), Self::Error> {
        self.write("</tbody></table>\n")
    }

    fn render_table_head_start(&mut self) -> Result<(), Self::Error> {
        self.set_table_state(TableState::Head);
        self.set_table_cell_index(0);
        self.write("<thead><tr>")
    }

    fn render_table_head_end(&mut self) -> Result<(), Self::Error> {
        self.write("</tr></thead><tbody>\n")?;
        self.set_table_state(TableState::Body);
        Ok(())
    }

    fn render_table_row_start(&mut self) -> Result<(), Self::Error> {
        self.set_table_cell_index(0);
        self.write("<tr>")
    }

    fn render_table_row_end(&mut self) -> Result<(), Self::Error> {
        self.write("</tr>\n")
    }

    fn render_table_cell_start(&mut self) -> Result<(), Self::Error> {
        match self.table_state {
            TableState::Head => {
                self.write("<th")?;
            }
            TableState::Body => {
                self.write("<td")?;
            }
        }
        match self.table_alignments.get(self.table_cell_index) {
            Some(&Alignment::Left) => self.write(" style=\"text-align: left\">"),
            Some(&Alignment::Center) => self.write(" style=\"text-align: center\">"),
            Some(&Alignment::Right) => self.write(" style=\"text-align: right\">"),
            _ => self.write(">"),
        }
    }

    fn render_table_cell_end(&mut self) -> Result<(), Self::Error> {
        match self.table_state {
            TableState::Head => {
                self.write("</th>")?;
            }
            TableState::Body => {
                self.write("</td>")?;
            }
        }
        self.table_cell_index += 1;
        Ok(())
    }

    // Block quote methods
    fn render_blockquote_start(&mut self, kind: Option<BlockQuoteKind>) -> Result<(), Self::Error> {
        let class_str = match kind {
            None => "",
            Some(kind) => match kind {
                BlockQuoteKind::Note => " class=\"markdown-alert-note\"",
                BlockQuoteKind::Tip => " class=\"markdown-alert-tip\"",
                BlockQuoteKind::Important => " class=\"markdown-alert-important\"",
                BlockQuoteKind::Warning => " class=\"markdown-alert-warning\"",
                BlockQuoteKind::Caution => " class=\"markdown-alert-caution\"",
            },
        };
        if self.end_newline {
            self.write(&format!("<blockquote{}>\n", class_str))
        } else {
            self.write(&format!("\n<blockquote{}>\n", class_str))
        }
    }

    fn render_blockquote_end(&mut self, _kind: Option<BlockQuoteKind>) -> Result<(), Self::Error> {
        self.write("</blockquote>\n")
    }

    // Code block methods
    fn render_code_block_start(&mut self, info: &CodeBlockKind) -> Result<(), Self::Error> {
        if !self.end_newline {
            self.write_newline()?;
        }
        match info {
            CodeBlockKind::Fenced(info) => {
                let lang = info.split(' ').next().unwrap();
                if lang.is_empty() {
                    self.write("<pre><code>")
                } else {
                    self.write("<pre><code class=\"language-")?;
                    escape_html(&mut self.writer, lang)?;
                    self.write("\">")
                }
            }
            CodeBlockKind::Indented => self.write("<pre><code>"),
        }
    }

    fn render_code_block_end(&mut self) -> Result<(), Self::Error> {
        self.write("</code></pre>\n")
    }

    // List methods
    fn render_list_start(&mut self, start: Option<u64>) -> Result<(), Self::Error> {
        match start {
            None => {
                if self.end_newline {
                    self.write("<ul>\n")
                } else {
                    self.write("\n<ul>\n")
                }
            }
            Some(1) => {
                if self.end_newline {
                    self.write("<ol>\n")
                } else {
                    self.write("\n<ol>\n")
                }
            }
            Some(start) => {
                if self.end_newline {
                    self.write("<ol start=\"")?;
                } else {
                    self.write("\n<ol start=\"")?;
                }
                write!(&mut self.writer, "{}", start)?;
                self.write("\">\n")
            }
        }
    }

    fn render_list_end(&mut self, ordered: bool) -> Result<(), Self::Error> {
        if ordered {
            self.write("</ol>\n")
        } else {
            self.write("</ul>\n")
        }
    }

    fn render_list_item_start(&mut self) -> Result<(), Self::Error> {
        if self.end_newline {
            self.write("<li>")
        } else {
            self.write("\n<li>")
        }
    }

    fn render_list_item_end(&mut self) -> Result<(), Self::Error> {
        self.write("</li>\n")
    }

    // Definition list methods
    fn render_definition_list_start(&mut self) -> Result<(), Self::Error> {
        if self.end_newline {
            self.write("<dl>\n")
        } else {
            self.write("\n<dl>\n")
        }
    }

    fn render_definition_list_end(&mut self) -> Result<(), Self::Error> {
        self.write("</dl>\n")
    }

    fn render_definition_title_start(&mut self) -> Result<(), Self::Error> {
        if self.end_newline {
            self.write("<dt>")
        } else {
            self.write("\n<dt>")
        }
    }

    fn render_definition_title_end(&mut self) -> Result<(), Self::Error> {
        self.write("</dt>\n")
    }

    fn render_definition_list_definition_start(&mut self) -> Result<(), Self::Error> {
        if self.end_newline {
            self.write("<dd>")
        } else {
            self.write("\n<dd>")
        }
    }

    fn render_definition_list_definition_end(&mut self) -> Result<(), Self::Error> {
        self.write("</dd>\n")
    }

    // Inline formatting methods
    fn render_emphasis_start(&mut self) -> Result<(), Self::Error> {
        self.write("<em>")
    }

    fn render_emphasis_end(&mut self) -> Result<(), Self::Error> {
        self.write("</em>")
    }

    fn render_superscript_start(&mut self) -> Result<(), Self::Error> {
        self.write("<sup>")
    }

    fn render_superscript_end(&mut self) -> Result<(), Self::Error> {
        self.write("</sup>")
    }

    fn render_subscript_start(&mut self) -> Result<(), Self::Error> {
        self.write("<sub>")
    }

    fn render_subscript_end(&mut self) -> Result<(), Self::Error> {
        self.write("</sub>")
    }

    fn render_strong_start(&mut self) -> Result<(), Self::Error> {
        self.write("<strong>")
    }

    fn render_strong_end(&mut self) -> Result<(), Self::Error> {
        self.write("</strong>")
    }

    fn render_strikethrough_start(&mut self) -> Result<(), Self::Error> {
        self.write("<del>")
    }

    fn render_strikethrough_end(&mut self) -> Result<(), Self::Error> {
        self.write("</del>")
    }

    // Link methods
    fn render_link_start(
        &mut self,
        link_type: LinkType,
        dest_url: &str,
        title: &str,
    ) -> Result<(), Self::Error> {
        match link_type {
            LinkType::Email => {
                self.write("<a href=\"mailto:")?;
                escape_href(&mut self.writer, dest_url)?;
            }
            _ => {
                self.write("<a href=\"")?;
                escape_href(&mut self.writer, dest_url)?;
            }
        }

        if !title.is_empty() {
            self.write("\" title=\"")?;
            escape_html(&mut self.writer, title)?;
        }
        self.write("\">")
    }

    fn render_link_end(&mut self) -> Result<(), Self::Error> {
        self.write("</a>")
    }

    fn render_image(&mut self, url: &str, title: &str, alt: &str) -> Result<(), Self::Error> {
        self.write("<img src=\"")?;
        escape_href(&mut self.writer, url)?;
        self.write("\" alt=\"")?;
        escape_html(&mut self.writer, alt)?;
        if !title.is_empty() {
            self.write("\" title=\"")?;
            escape_html(&mut self.writer, title)?;
        }
        self.write("\" />")
    }
    fn render_footnote_reference(&mut self, name: &str, number: usize) -> Result<(), Self::Error> {
        self.write("<sup class=\"footnote-reference\"><a href=\"#")?;
        escape_html(&mut self.writer, name)?;
        write!(&mut self.writer, "\">{}</a></sup>", number)
    }

    fn render_footnote_definition_start(
        &mut self,
        name: &str,
        number: usize,
    ) -> Result<(), Self::Error> {
        if self.end_newline {
            self.write("<div class=\"footnote-definition\" id=\"")?;
        } else {
            self.write("\n<div class=\"footnote-definition\" id=\"")?;
        }
        escape_html(&mut self.writer, name)?;
        self.write("\"><sup class=\"footnote-definition-label\">")?;

        write!(&mut self.writer, "{}", number)?;
        self.write("</sup>")
    }

    fn render_footnote_definition_end(&mut self) -> Result<(), Self::Error> {
        self.write("</div>\n")
    }
    fn render_metadata_start(&mut self, _: MetadataBlockKind) -> Result<(), Self::Error> {
        self.in_non_writing_block = true;
        Ok(())
    }
    fn render_metadata_end(&mut self, _: MetadataBlockKind) -> Result<(), Self::Error> {
        self.in_non_writing_block = false;
        Ok(())
    }
}
