use pulldown_cmark::Parser;

fn parse(md: &str) {
    let parser = Parser::new(md);

    for _ in parser {}
}

#[test]
fn test_lists_inside_code_spans() {
    parse(
        r"- `
x
**
  *
  `",
    );
}

#[test]
fn test_wrong_code_block() {
    parse(
        r##"```
 * ```
 "##,
    );
}

#[test]
fn test_unterminated_link() {
    parse("[](\\");
}

#[test]
fn test_unterminated_autolink() {
    parse("<a");
}

#[test]
fn test_infinite_loop() {
    parse("[<!W\n\\\n");
}

#[test]
fn test_html_tag() {
    parse("<script\u{feff}");
}

// all of test_bad_slice_* were found in https://github.com/raphlinus/pulldown-cmark/issues/521
#[test]
fn test_bad_slice_a() {
    parse("><a\n");
}

#[test]
fn test_bad_slice_b() {
    parse("><a a\n");
}

#[test]
fn test_bad_slice_unicode() {
    parse("><a a=\n毿>")
}
