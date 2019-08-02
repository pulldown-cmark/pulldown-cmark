extern crate pulldown_cmark;

#[test]
fn test_wrong_code_block() {
    let markdown = r##"```
 * ```
 "##;
    use pulldown_cmark::Parser;

    let _ = Parser::new(&markdown);
}

#[test]
fn test_unterminated_link() {
    let markdown = "[](\\";
    use pulldown_cmark::Parser;

    let parser = Parser::new(&markdown);
    for _ in parser {}
}

#[test]
fn test_unterminated_autolink() {
    use pulldown_cmark::Parser;
    let _ = Parser::new("<a");
}

#[test]
fn test_infinite_loop() {
    let markdown = "[<!W\n\\\n";
    use pulldown_cmark::Parser;

    let parser = Parser::new(&markdown);
    for _ in parser {}
}

#[test]
fn test_html_tag() {
    let markdown = "<script\u{feff}";
    use pulldown_cmark::Parser;

    let parser = Parser::new(&markdown);
    for _ in parser {}
}
