#![cfg(feature = "html")]

use pulldown_cmark::{Options, Parser};

#[rustfmt::skip]
mod suite;

#[inline(never)]
pub fn test_markdown_html(input: &str, output: &str, options: &str) {
    let mut s = String::new();

    let opts = bitflags::parser::from_str_strict::<Options>(options)
        .unwrap_or_else(|e| panic!("Invalid Options specifier \"{options}\": {e}"));

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
