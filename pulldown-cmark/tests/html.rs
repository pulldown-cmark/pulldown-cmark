// Tests for HTML spec.
#![cfg(feature = "html")]

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
</script>
"##;

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
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
</script>
"##;

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
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
?>
"##;

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
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
-->
"##;

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
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
]]>
"##;

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
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
>
"##;

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
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
</script>
"##;

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
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
    html::push_html(&mut s, Parser::new_ext(original, opts));
    assert_eq!(expected, s);
}

#[test]
fn html_test_9() {
    let original = "---";
    let expected = "<hr />\n";

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn html_test_10() {
    let original = "* * *";
    let expected = "<hr />\n";

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn blockquote_marker_after_tab_is_indented() {
    // A tab expands to the next tab stop, so `\t>` puts the `>` at column 4: too far in for a
    // block quote marker (at most three spaces of indentation).
    let cases = [
        ("> a\n\t>b\n", "<blockquote>\n<p>a\n&gt;b</p>\n</blockquote>\n"),
        (
            "> *\n\t>a\n",
            "<blockquote>\n<ul>\n<li></li>\n</ul>\n</blockquote>\n<pre><code>&gt;a\n</code></pre>\n",
        ),
        // unchanged: fewer than four columns, and a tab after the marker
        ("> a\n   >b\n", "<blockquote>\n<p>a\nb</p>\n</blockquote>\n"),
        (">\t> a\n", "<blockquote>\n<blockquote>\n<p>a</p>\n</blockquote>\n</blockquote>\n"),
    ];

    for (original, expected) in cases {
        let mut s = String::new();
        html::push_html(&mut s, Parser::new(original));
        assert_eq!(expected, s, "input {original:?}");
    }
}

#[test]
fn html_test_11() {
    let original = "hi ~~no~~";
    let expected = "<p>hi ~~no~~</p>\n";

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
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

    let p = Parser::new_with_broken_link_callback(original, Options::empty(), Some(&mut callback));
    html::push_html(&mut s, p);

    assert_eq!(expected, s);
}

#[test]
fn newline_in_code() {
    // The leading space of a continuation line is stripped; a trailing one is kept.
    let cases = [
        ("`\n `x", "<p><code> </code>x</p>\n"),
        ("` \n`x", "<p><code>  </code>x</p>\n"),
    ];

    for (original, expected) in cases {
        let mut s = String::new();
        html::push_html(&mut s, Parser::new(original));
        assert_eq!(expected, s);
    }
}

#[test]
fn newline_start_end_of_code() {
    let original = "`\nx\n`x";
    let expected = "<p><code>x</code>x</p>\n";

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

// https://github.com/raphlinus/pulldown-cmark/issues/715

#[test]
fn trim_space_and_tab_at_end_of_paragraph() {
    let original = "one\ntwo \t";
    let expected = "<p>one\ntwo</p>\n";

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn newline_within_code() {
    let originals = ["`\nx \ny\n`x", "`x \ny`x"];
    let expected = "<p><code>x  y</code>x</p>\n";

    for original in originals {
        let mut s = String::new();
        html::push_html(&mut s, Parser::new(original));
        assert_eq!(expected, s);
    }

    // The leading space of a continuation line is stripped.
    let mut s = String::new();
    html::push_html(&mut s, Parser::new("`x\n y`x"));
    assert_eq!("<p><code>x y</code>x</p>\n", s);
}

#[test]
fn trim_space_tab_nl_at_end_of_paragraph() {
    let original = "one\ntwo \t\n";
    let expected = "<p>one\ntwo</p>\n";

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn trim_space_nl_at_end_of_paragraph() {
    let original = "one\ntwo \n";
    let expected = "<p>one\ntwo</p>\n";

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

#[test]
fn trim_space_before_soft_break() {
    let original = "one \ntwo";
    let expected = "<p>one\ntwo</p>\n";

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));
    assert_eq!(expected, s);
}

// Can't easily use regression.txt due to newline normalization.
#[test]
fn issue_819() {
    let original = [
        "# \\",
        "# \\\n",
        "# \\\n\n",
        "# \\\r\n",
        "# \\\r\n\r\n",
        "# \\\n\r\n",
        "# \\\r\n\n",
    ];
    let expected = "<h1>\\</h1>";

    for orig in original {
        let mut s = String::new();
        html::push_html(&mut s, Parser::new(orig));
        // Trailing newline doesn't matter. Just the actual HTML.
        assert_eq!(expected, s.trim_end_matches('\n'));
    }

    for orig in original {
        let mut s = String::new();
        let mut opts = Options::empty();
        opts.insert(Options::ENABLE_HEADING_ATTRIBUTES);
        html::push_html(&mut s, Parser::new_ext(orig, opts));
        // Trailing newline doesn't matter. Just the actual HTML.
        assert_eq!(expected, s.trim_end_matches('\n'));
    }
}

// Can't easily use regression.txt due to newline normalization.
#[test]
fn issue_1056() {
    let original = "```\rcode\rblock\r\n```\n";
    let expected = "<pre><code>code\nblock\n</code></pre>\n";

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));

    assert_eq!(expected, s);
}

// Can't easily use regression.txt due to newline normalization.
#[test]
fn issue_1131_crlf() {
    let original = "` tag\r\n    *   x\r\n    *   `\r\n";
    let expected = "<p><code>tag *   x *  </code></p>\n";

    let mut s = String::new();
    html::push_html(&mut s, Parser::new(original));

    assert_eq!(expected, s);
}
