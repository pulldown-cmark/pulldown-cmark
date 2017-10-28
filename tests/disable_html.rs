// Tests for OPTION_DISABLE_HTML

extern crate pulldown_cmark;

use pulldown_cmark::{Parser, html, OPTION_DISABLE_HTML};

#[test]
fn disable_html_test_1() {
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
<p>&lt;script type=&quot;text/js&quot;&gt;
function some_func() {
console.log(&quot;teeeest&quot;);
}</p>
<p>function another_func() {
console.log(&quot;fooooo&quot;);
}
&lt;/script&gt;</p>
"##;

    let mut s = String::new();

    let p = Parser::new_ext(&original, OPTION_DISABLE_HTML);
    html::push_html(&mut s, p);

    assert_eq!(expected, s);
}

#[test]
fn disable_html_test_2() {
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
<p>&lt;script
type=&quot;text/js&quot;&gt;
function some_func() {
console.log(&quot;teeeest&quot;);
}</p>
<p>function another_func() {
console.log(&quot;fooooo&quot;);
}
&lt;/script&gt;</p>
"##;

    let mut s = String::new();

    let p = Parser::new_ext(&original, OPTION_DISABLE_HTML);
    html::push_html(&mut s, p);

    assert_eq!(expected, s);
}

#[test]
fn disable_html_test_3() {
    let original = r##"Little header

<?
<div></div>
<p>Useful</p>
?>"##;
    let expected = r##"<p>Little header</p>
<p>&lt;?
&lt;div&gt;&lt;/div&gt;
&lt;p&gt;Useful&lt;/p&gt;
?&gt;</p>
"##;

    let mut s = String::new();

    let p = Parser::new_ext(&original, OPTION_DISABLE_HTML);
    html::push_html(&mut s, p);

    assert_eq!(expected, s);
}

#[test]
fn disable_html_test_4() {
    let original = r##"Little header

<!--
<div></div>
<p>Useful</p>
-->"##;
    let expected = r##"<p>Little header</p>
<p>&lt;!--
&lt;div&gt;&lt;/div&gt;
&lt;p&gt;Useful&lt;/p&gt;
--&gt;</p>
"##;

    let mut s = String::new();

    let p = Parser::new_ext(&original, OPTION_DISABLE_HTML);
    html::push_html(&mut s, p);

    assert_eq!(expected, s);
}

#[test]
fn disable_html_test_5() {
    let original = r##"Little header

<![CDATA[
<div></div>
<p>Useful</p>
]]>"##;
    let expected = r##"<p>Little header</p>
<p>&lt;![CDATA[
&lt;div&gt;&lt;/div&gt;
&lt;p&gt;Useful&lt;/p&gt;
]]&gt;</p>
"##;

    let mut s = String::new();

    let p = Parser::new_ext(&original, OPTION_DISABLE_HTML);
    html::push_html(&mut s, p);

    assert_eq!(expected, s);
}

#[test]
fn disable_html_test_6() {
    let original = r##"Little header

<!X
Some things are here...
>"##;
    let expected = r##"<p>Little header</p>
<p>&lt;!X
Some things are here...</p>
<blockquote>
</blockquote>
"##;

    let mut s = String::new();

    let p = Parser::new_ext(&original, OPTION_DISABLE_HTML);
    html::push_html(&mut s, p);

    assert_eq!(expected, s);
}

#[test]
fn disable_html_test_7() {
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
<p>&lt;script&gt;
function some_func() {
console.log(&quot;teeeest&quot;);
}</p>
<p>function another_func() {
console.log(&quot;fooooo&quot;);
}
&lt;/script&gt;</p>
"##;

    let mut s = String::new();

    let p = Parser::new_ext(&original, OPTION_DISABLE_HTML);
    html::push_html(&mut s, p);

    assert_eq!(expected, s);
}

#[test]
fn disable_html_test_8() {
    let original = r##"<a href=\"this *should* be parsed\">Not a link</a>"##;
    let expected = r##"<p>&lt;a href=&quot;this <em>should</em> be parsed&quot;&gt;Not a link&lt;/a&gt;</p>
"##;

    let mut s = String::new();

    let p = Parser::new_ext(&original, OPTION_DISABLE_HTML);
    html::push_html(&mut s, p);

    assert_eq!(expected, s);
}
