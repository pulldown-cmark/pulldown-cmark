use pulldown_cmark::{html, Options, Parser};

#[test]
fn options_test1() {
    let original = r##"# hello

:: bam: bam
paragraph

"##;
    let expected = "<h1>hello</h1>\n<div class='options'>bam: bam</div>\n<p>paragraph</p>\n";
    let mut s = String::new();
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}

#[test]
fn options_test2() {
    let original = r##"# hello

:: author: "Franz Kafka", number: 42, apo: "(1;2;3)"
paragraph

"##;
    let expected = "<h1>hello</h1>\n<div class='options'>author: &quot;Franz Kafka&quot;, number: 42, apo: &quot;(1;2;3)&quot;</div>\n<p>paragraph</p>\n";
    let mut s = String::new();
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}

// options without newlines become part of the paragraph
#[test]
fn options_test3() {
    let original = r##"# hello
parapara
:: bam: bam
paragraph

"##;
    let expected = "<h1>hello</h1>\n<p>parapara\n:: bam: bam\nparagraph</p>\n";
    let mut s = String::new();
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}

#[test]
fn options_test4() {
    let original = r##"# hello

:: author: bambam
paragraph

"##;
    let expected = "<h1>hello</h1>\n<div class='options'>author: bambam</div>\n<p>paragraph</p>\n";
    let mut s = String::new();
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}