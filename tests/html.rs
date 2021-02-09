// Tests for HTML spec.

use pulldown_cmark::{html, BrokenLink, Options, Parser};

#[test]
fn html_test_1() {
    let original = r##"Little header

<script type="text/js">
function some_func() {
console.log("teeeest");
}


function another_func() {
console.log("fooooo");
}
</script>"##;
    let expected = r##"<p>Little header</p>
<script type="text/js">
function some_func() {
console.log("teeeest");
}


function another_func() {
console.log("fooooo");
}
</script>"##;

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}

#[test]
fn html_test_2() {
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
    let expected = r##"<p>Little header</p>
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
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}

#[test]
fn html_test_3() {
    let original = r##"Little header

<?
<div></div>
<p>Useless</p>
?>"##;
    let expected = r##"<p>Little header</p>
<?
<div></div>
<p>Useless</p>
?>"##;

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}

#[test]
fn html_test_4() {
    let original = r##"Little header

<!--
<div></div>
<p>Useless</p>
-->"##;
    let expected = r##"<p>Little header</p>
<!--
<div></div>
<p>Useless</p>
-->"##;

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}

#[test]
fn html_test_5() {
    let original = r##"Little header

<![CDATA[
<div></div>
<p>Useless</p>
]]>"##;
    let expected = r##"<p>Little header</p>
<![CDATA[
<div></div>
<p>Useless</p>
]]>"##;

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}

#[test]
fn html_test_6() {
    let original = r##"Little header

<!X
Some things are here...
>"##;
    let expected = r##"<p>Little header</p>
<!X
Some things are here...
>"##;

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}

#[test]
fn html_test_7() {
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
    let expected = r##"<h2>Little header</h2>
<script>
function some_func() {
console.log("teeeest");
}


function another_func() {
console.log("fooooo");
}
</script>"##;

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}

#[test]
fn html_test_8() {
    let original = "A | B\n---|---\nfoo | bar";
    let expected = r##"<table><thead><tr><th>A</th><th>B</th></tr></thead><tbody>
<tr><td>foo</td><td>bar</td></tr>
</tbody></table>
"##;

    let mut s = String::new();
    let mut opts = Options::empty();
    opts.insert(Options::ENABLE_TABLES);
    html::push_html(&mut s, Parser::new_ext(&original, opts));
    assert_eq!(expected, s);
}

#[test]
fn html_test_9() {
    let original = "---";
    let expected = "<hr />\n";

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}

#[test]
fn html_test_10() {
    let original = "* * *";
    let expected = "<hr />\n";

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}

#[test]
fn html_test_11() {
    let original = "hi ~~no~~";
    let expected = "<p>hi ~~no~~</p>\n";

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(&original));
    assert_eq!(expected, s);
}

#[test]
fn html_test_broken_callback() {
    let original = r##"[foo],
[bar],
[baz],

   [baz]: https://example.org
"##;

    let expected = r##"<p><a href="https://replaced.example.org" title="some title">foo</a>,
[bar],
<a href="https://example.org">baz</a>,</p>
"##;

    use pulldown_cmark::{html, Options, Parser};

    let mut s = String::new();

    let mut callback = |broken_link: BrokenLink| {
        if &*broken_link.reference == "foo" || &*broken_link.reference == "baz" {
            Some(("https://replaced.example.org".into(), "some title".into()))
        } else {
            None
        }
    };

    let p = Parser::new_with_broken_link_callback(&original, Options::empty(), Some(&mut callback));
    html::push_html(&mut s, p);

    assert_eq!(expected, s);
}
