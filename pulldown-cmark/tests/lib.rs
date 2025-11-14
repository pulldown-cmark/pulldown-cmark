#![cfg(feature = "html")]

use pulldown_cmark::{Options, Parser};

#[rustfmt::skip]
mod suite;

#[derive(Default)]
pub struct TestMarkdownHtmlOptions {
    pub smart_punct: bool,
    pub metadata_blocks: bool,
    pub old_footnotes: bool,
    pub subscript: bool,
    pub wikilinks: bool,
    pub deflists: bool,
    pub container_extensions: bool,
}

#[inline(never)]
pub fn test_markdown_html(input: &str, output: &str, test_opts: TestMarkdownHtmlOptions) {
    let mut s = String::new();

    let mut opts = Options::empty();
    opts.insert(Options::ENABLE_MATH);
    opts.insert(Options::ENABLE_TABLES);
    opts.insert(Options::ENABLE_STRIKETHROUGH);
    opts.insert(Options::ENABLE_SUPERSCRIPT);
    if test_opts.wikilinks {
        opts.insert(Options::ENABLE_WIKILINKS);
    }
    if test_opts.subscript {
        opts.insert(Options::ENABLE_SUBSCRIPT);
    }
    opts.insert(Options::ENABLE_TASKLISTS);
    opts.insert(Options::ENABLE_GFM);
    if test_opts.old_footnotes {
        opts.insert(Options::ENABLE_OLD_FOOTNOTES);
    } else {
        opts.insert(Options::ENABLE_FOOTNOTES);
    }
    if test_opts.metadata_blocks {
        opts.insert(Options::ENABLE_YAML_STYLE_METADATA_BLOCKS);
        opts.insert(Options::ENABLE_PLUSES_DELIMITED_METADATA_BLOCKS);
    }
    if test_opts.smart_punct {
        opts.insert(Options::ENABLE_SMART_PUNCTUATION);
    }
    opts.insert(Options::ENABLE_HEADING_ATTRIBUTES);
    if test_opts.deflists {
        opts.insert(Options::ENABLE_DEFINITION_LIST);
    }
    if test_opts.container_extensions {
        opts.insert(Options::ENABLE_CONTAINER_EXTENSIONS);
    }

    let p = Parser::new_ext(input, opts);
    pulldown_cmark::html::push_html(&mut s, p);

    // normalizing the HTML using html5ever may hide actual errors
    // assert_eq!(html_standardize(output), html_standardize(&s));
    assert_eq!(html_standardize(output), html_standardize(&s));
}

fn html_standardize(s: &str) -> String {
    s.replace("<br>", "<br />")
        .replace("<br/>", "<br />")
        .replace("<hr>", "<hr />")
        .replace("<hr/>", "<hr />")
        // permit extra or missing line breaks only between tags
        .replace(">\n<", "><")
}
