use pulldown_cmark::Parser;

#[test]
fn test_wrong_code_block() {
    let markdown = r##"```
 * ```
 "##;

    let _ = Parser::new(&markdown);
}

#[test]
fn test_unterminated_link() {
    let markdown = "[](\\";

    let parser = Parser::new(&markdown);
    for _ in parser {}
}

#[test]
fn test_unterminated_autolink() {
    let _ = Parser::new("<a");
}

#[test]
fn test_infinite_loop() {
    let markdown = "[<!W\n\\\n";

    let parser = Parser::new(&markdown);
    for _ in parser {}
}

#[test]
fn test_html_tag() {
    let markdown = "<script\u{feff}";

    let parser = Parser::new(&markdown);
    for _ in parser {}
}
