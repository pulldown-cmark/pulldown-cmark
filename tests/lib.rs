#![cfg(feature = "html")]

use pulldown_cmark::{Options, Parser};

mod suite;

#[inline(never)]
pub fn test_markdown_html(
    input: &str,
    output: &str,
    smart_punct: bool,
    metadata_blocks: bool,
    old_footnotes: bool,
) {
    let mut s = String::new();

    let mut opts = Options::empty();
    opts.insert(Options::ENABLE_TABLES);
    opts.insert(Options::ENABLE_STRIKETHROUGH);
    opts.insert(Options::ENABLE_TASKLISTS);
    if old_footnotes {
        opts.insert(Options::ENABLE_OLD_FOOTNOTES);
    } else {
        opts.insert(Options::ENABLE_FOOTNOTES);
    }
    if metadata_blocks {
        opts.insert(Options::ENABLE_YAML_STYLE_METADATA_BLOCKS);
        opts.insert(Options::ENABLE_PLUSES_DELIMITED_METADATA_BLOCKS);
    }
    if smart_punct {
        opts.insert(Options::ENABLE_SMART_PUNCTUATION);
    }
    opts.insert(Options::ENABLE_HEADING_ATTRIBUTES);

    let p = Parser::new_ext(input, opts);
    pulldown_cmark::html::push_html(&mut s, p);

    // normalizing the HTML using html5ever may hide actual errors
    // assert_eq!(html_standardize(output), html_standardize(&s));
    assert_eq!(html_standardize(output), html_standardize(&s));
}

fn html_standardize(s: &str) -> String {
    s.trim()
        .replace("<br>", "<br />")
        .replace("<br/>", "<br />")
        .replace("<hr>", "<hr />")
        .replace("<hr/>", "<hr />")
        // permit extra or missing line breaks only between tags
        .replace(">\n<", "><")
}

#[test]
fn strip_div_newline() {
    assert_eq!("<div></div>", html_standardize("<div>\n</div>"));
}

#[test]
fn strip_end_newline() {
    assert_eq!("test", html_standardize("test\n"));
}

#[test]
fn strip_double_space() {
    assert_eq!("test mess", html_standardize("test  mess"));
}

#[test]
fn strip_inline_internal_text() {
    assert_eq!(
        "<u>a </u>b <u>c</u>",
        html_standardize("<u> a </u> b <u> c </u>")
    )
}

#[test]
fn strip_inline_block_internal_text() {
    assert_eq!(
        "<u>a </u>b <u>c</u>",
        html_standardize(" <u> a </u> b <u> c </u> ")
    )
}

#[test]
fn leaves_necessary_whitespace_alone() {
    assert_eq!(
        "<u>a</u> b <u>c</u>",
        html_standardize("<u>a</u> b <u>c</u>")
    )
}

#[test]
fn leaves_necessary_whitespace_alone_weird() {
    assert_eq!(
        "<u>a </u>b <u>c</u>",
        html_standardize(" <u>a </u>b <u>c</u>")
    )
}

#[test]
fn leaves_necessary_whitespace_all_nested() {
    assert_eq!(
        "<u></u><u></u><u></u><u></u>",
        html_standardize("<u> </u><u> </u><u> </u><u> </u>")
    )
}

#[test]
fn drops_empty_tbody() {
    assert_eq!(
        "<table><thead><tr><td>hi</td></tr></thead></table>",
        html_standardize("<table><thead><tr><td>hi</td></tr></thead><tbody>  </tbody></table>")
    )
}

#[test]
fn leaves_nonempty_tbody() {
    let input = "<table><thead><tr><td>hi</td></tr></thead><tbody><tr></tr></tbody></table>";
    assert_eq!(input, html_standardize(input))
}
