use crate::{Alignment, BlockQuoteKind, CodeBlockKind, CowStr, LinkType, MetadataBlockKind};

#[derive(Clone, Copy, Debug)]
pub enum TableState {
    Head,
    Body,
}

pub trait HtmlElementRenderer {
    type Error;

    // Table state tracking
    fn get_table_state(&self) -> TableState;
    fn set_table_state(&mut self, state: TableState);
    fn get_table_alignments(&self) -> &[Alignment];
    fn set_table_alignments(&mut self, alignments: Vec<Alignment>);
    fn get_table_cell_index(&self) -> usize;
    fn set_table_cell_index(&mut self, index: usize);
    fn increment_table_cell_index(&mut self);

    // Core rendering methods
    fn render_text(&mut self, text: &CowStr) -> Result<(), Self::Error>;
    fn render_code_inline(&mut self, text: &str) -> Result<(), Self::Error>;
    fn render_math_inline(&mut self, text: &str) -> Result<(), Self::Error>;
    fn render_math_display(&mut self, text: &str) -> Result<(), Self::Error>;
    fn render_raw_html(&mut self, html: &str) -> Result<(), Self::Error>;
    fn render_soft_break(&mut self) -> Result<(), Self::Error>;
    fn render_hard_break(&mut self) -> Result<(), Self::Error>;
    fn render_horizontal_rule(&mut self) -> Result<(), Self::Error>;
    fn render_task_list_marker(&mut self, checked: bool) -> Result<(), Self::Error>;

    // Block element methods
    fn render_paragraph_start(&mut self) -> Result<(), Self::Error>;
    fn render_paragraph_end(&mut self) -> Result<(), Self::Error>;
    fn render_heading_start(
        &mut self,
        level: u32,
        id: Option<&str>,
        classes: &[CowStr<'_>],
        attrs: &[(CowStr<'_>, Option<CowStr<'_>>)],
    ) -> Result<(), Self::Error>;
    fn render_heading_end(&mut self, level: u32) -> Result<(), Self::Error>;

    // Table methods
    fn render_table_start(&mut self, alignments: &[Alignment]) -> Result<(), Self::Error>;
    fn render_table_end(&mut self) -> Result<(), Self::Error>;
    fn render_table_head_start(&mut self) -> Result<(), Self::Error>;
    fn render_table_head_end(&mut self) -> Result<(), Self::Error>;
    fn render_table_row_start(&mut self) -> Result<(), Self::Error>;
    fn render_table_row_end(&mut self) -> Result<(), Self::Error>;
    fn render_table_cell_start(&mut self) -> Result<(), Self::Error>;
    fn render_table_cell_end(&mut self) -> Result<(), Self::Error>;

    // Other block elements
    fn render_blockquote_start(&mut self, kind: Option<BlockQuoteKind>) -> Result<(), Self::Error>;
    fn render_blockquote_end(&mut self, kind: Option<BlockQuoteKind>) -> Result<(), Self::Error>;
    fn render_code_block_start(&mut self, info: &CodeBlockKind) -> Result<(), Self::Error>;
    fn render_code_block_end(&mut self) -> Result<(), Self::Error>;

    // List methods
    fn render_list_start(&mut self, start: Option<u64>) -> Result<(), Self::Error>;
    fn render_list_end(&mut self, ordered: bool) -> Result<(), Self::Error>;
    fn render_list_item_start(&mut self) -> Result<(), Self::Error>;
    fn render_list_item_end(&mut self) -> Result<(), Self::Error>;

    // Definition list methods
    fn render_definition_list_start(&mut self) -> Result<(), Self::Error>;
    fn render_definition_list_end(&mut self) -> Result<(), Self::Error>;
    fn render_definition_title_start(&mut self) -> Result<(), Self::Error>;
    fn render_definition_title_end(&mut self) -> Result<(), Self::Error>;
    fn render_definition_list_definition_start(&mut self) -> Result<(), Self::Error>;
    fn render_definition_list_definition_end(&mut self) -> Result<(), Self::Error>;

    // Inline formatting methods
    fn render_emphasis_start(&mut self) -> Result<(), Self::Error>;
    fn render_emphasis_end(&mut self) -> Result<(), Self::Error>;
    fn render_superscript_start(&mut self) -> Result<(), Self::Error>;
    fn render_superscript_end(&mut self) -> Result<(), Self::Error>;
    fn render_subscript_start(&mut self) -> Result<(), Self::Error>;
    fn render_subscript_end(&mut self) -> Result<(), Self::Error>;
    fn render_strong_start(&mut self) -> Result<(), Self::Error>;
    fn render_strong_end(&mut self) -> Result<(), Self::Error>;
    fn render_strikethrough_start(&mut self) -> Result<(), Self::Error>;
    fn render_strikethrough_end(&mut self) -> Result<(), Self::Error>;

    // Link and image methods
    fn render_link_start(
        &mut self,
        link_type: LinkType,
        dest_url: &str,
        title: &str,
    ) -> Result<(), Self::Error>;
    fn render_link_end(&mut self) -> Result<(), Self::Error>;
    fn render_image(&mut self, url: &str, title: &str, alt: &str) -> Result<(), Self::Error>;

    // Footnote methods
    fn render_footnote_reference(&mut self, name: &str, number: usize) -> Result<(), Self::Error>;
    fn render_footnote_definition_start(
        &mut self,
        name: &str,
        number: usize,
    ) -> Result<(), Self::Error>;
    fn render_footnote_definition_end(&mut self) -> Result<(), Self::Error>;

    // Metadata
    fn render_metadata_start(&mut self, kind: MetadataBlockKind) -> Result<(), Self::Error>;
    fn render_metadata_end(&mut self, kind: MetadataBlockKind) -> Result<(), Self::Error>;
}
