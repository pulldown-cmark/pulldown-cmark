#![cfg(feature = "text")]

use pulldown_cmark::{text, BrokenLink, Options, Parser};

#[test]
fn text_test_1() {
    let original = r##"Little header

<script type="text/js">
function some_func() {
console.log("teeeest");
}


function another_func() {
console.log("fooooo");
}
</script>"##;
    let expected = r##"Little header
<script type="text/js">
function some_func() {
console.log("teeeest");
}


function another_func() {
console.log("fooooo");
}
</script>"##;

    let mut s = String::new();
    text::push_text(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn text_test_2() {
    let original = r##"Little header

<script
type="text/js">
function some_func() {
console.log("teeeest");
}


function another_func() {
console.log("fooooo");
}
</script>"##;
    let expected = r##"Little header
<script
type="text/js">
function some_func() {
console.log("teeeest");
}


function another_func() {
console.log("fooooo");
}
</script>"##;

    let mut s = String::new();
    text::push_text(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn test_test_3() {
    let original = r##"Little header

<?
<div></div>
<p>Useless</p>
?>"##;
    let expected = r##"Little header
<?
<div></div>
<p>Useless</p>
?>"##;

    let mut s = String::new();
    text::push_text(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn text_test_4() {
    let original = r##"Little header

<!--
<div></div>
<p>Useless</p>
-->"##;
    let expected = r##"Little header
<!--
<div></div>
<p>Useless</p>
-->"##;

    let mut s = String::new();
    text::push_text(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn text_test_5() {
    let original = r##"Little header

<![CDATA[
<div></div>
<p>Useless</p>
]]>"##;
    let expected = r##"Little header
<![CDATA[
<div></div>
<p>Useless</p>
]]>"##;

    let mut s = String::new();
    text::push_text(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn text_test_6() {
    let original = r##"Little header

<!X
Some things are here...
>"##;
    let expected = r##"Little header
<!X
Some things are here...
>"##;

    let mut s = String::new();
    text::push_text(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn text_test_7() {
    let original = r##"Little header
-----------

<script>
function some_func() {
console.log("teeeest");
}


function another_func() {
console.log("fooooo");
}
</script>"##;
    let expected = r##"Little header
<script>
function some_func() {
console.log("teeeest");
}


function another_func() {
console.log("fooooo");
}
</script>"##;

    let mut s = String::new();
    text::push_text(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn text_test_8() {
    let original = "A | B\n---|---\nfoo | bar";
    let expected = r##"A B
foo bar

"##;

    let mut s = String::new();
    let mut opts = Options::empty();
    opts.insert(Options::ENABLE_TABLES);
    text::push_text(&mut s, Parser::new_ext(original, opts));
    assert_eq!(expected, s);
}

#[test]
fn text_test_9() {
    let original = "---";
    let expected = "";

    let mut s = String::new();
    text::push_text(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn text_test_10() {
    let original = "* * *";
    let expected = "";

    let mut s = String::new();
    text::push_text(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn text_test_11() {
    let original = "hi ~~no~~";
    let expected = r#"hi ~~no~~
"#;

    let mut s = String::new();
    text::push_text(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn text_test_broken_callback() {
    let original = r##"[foo],
[bar],
[baz],

   [baz]: https://example.org
"##;

    let expected = r##"some title: https://replaced.example.org ,
[ bar ] ,
: https://example.org ,
"##;

    use pulldown_cmark::{text, Options, Parser};

    let mut s = String::new();

    let mut callback = |broken_link: BrokenLink| {
        if &*broken_link.reference == "foo" || &*broken_link.reference == "baz" {
            Some(("https://replaced.example.org".into(), "some title".into()))
        } else {
            None
        }
    };

    let p = Parser::new_with_broken_link_callback(original, Options::empty(), Some(&mut callback));
    text::push_text(&mut s, p);

    assert_eq!(expected, s);
}
