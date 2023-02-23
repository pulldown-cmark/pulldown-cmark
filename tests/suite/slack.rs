use pulldown_cmark::{Options, Parser};

use crate::normalize_html;

use super::test_markdown_html;

#[test]
fn default_is_emphasis() {
    let original = "*hi*";
    let expected = "<p><em>hi</em></p>";

    test_markdown_html(original, expected, false);
}

#[test]
fn with_option_is_strong() {
    let input = "*hi*";
    let output = "<p><strong>hi</strong></p>";

    let mut s = String::new();
    let mut opts = Options::empty();
    opts.insert(Options::ENABLE_SINGLE_EMPHASIS_IS_STRONG);
    let p = Parser::new_ext(input, opts);
    pulldown_cmark::html::push_html(&mut s, p);

    assert_eq!(normalize_html(output), normalize_html(&s));
}
