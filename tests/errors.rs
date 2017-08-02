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
    for _ in parser { }
}
