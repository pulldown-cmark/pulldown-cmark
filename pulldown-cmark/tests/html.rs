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
</script>"##;

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
</script>"##;

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
?>"##;

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
-->"##;

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
]]>"##;

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
>"##;

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
</script>"##;

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
    let originals = ["`\n `x", "` \n`x"];
    let expected = "<p><code>  </code>x</p>\n";

    for original in originals {
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
    let originals = ["`\nx \ny\n`x", "`x \ny`x", "`x\n y`x"];
    let expected = "<p><code>x  y</code>x</p>\n";

    for original in originals {
        let mut s = String::new();
        html::push_html(&mut s, Parser::new(original));
        assert_eq!(expected, s);
    }
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

// ============================================================
// CJK-friendly emphasis tests
// ============================================================

/// Helper to render markdown with CJK-friendly emphasis enabled.
fn render_cjk(input: &str) -> String {
    let mut opts = Options::empty();
    opts.insert(Options::ENABLE_CJK_FRIENDLY_EMPHASIS);
    let mut s = String::new();
    html::push_html(&mut s, Parser::new_ext(input, opts));
    s
}

/// Helper to render markdown with default options (no CJK mode).
fn render_default(input: &str) -> String {
    let mut s = String::new();
    html::push_html(&mut s, Parser::new(input));
    s
}

// -- Japanese quotation marks --

#[test]
fn cjk_emphasis_japanese_corner_brackets() {
    assert_eq!(
        render_cjk("これは**「重要」**です"),
        "<p>これは<strong>「重要」</strong>です</p>\n"
    );
}

#[test]
fn cjk_emphasis_japanese_double_corner_brackets() {
    assert_eq!(
        render_cjk("これは**『重要』**です"),
        "<p>これは<strong>『重要』</strong>です</p>\n"
    );
}

#[test]
fn cjk_emphasis_japanese_fullwidth_quotes() {
    assert_eq!(
        render_cjk("これは**\u{201C}重要\u{201D}**です"),
        "<p>これは<strong>\u{201C}重要\u{201D}</strong>です</p>\n"
    );
}

// -- Japanese parentheses and brackets --

#[test]
fn cjk_emphasis_japanese_fullwidth_parens() {
    assert_eq!(
        render_cjk("これは**（仮）**です"),
        "<p>これは<strong>（仮）</strong>です</p>\n"
    );
}

#[test]
fn cjk_emphasis_japanese_lenticular_brackets() {
    assert_eq!(
        render_cjk("これは**【重要】**です"),
        "<p>これは<strong>【重要】</strong>です</p>\n"
    );
}

// -- CJK punctuation at end of emphasis --

#[test]
fn cjk_emphasis_period_inside() {
    assert_eq!(
        render_cjk("これは**重要。**だから"),
        "<p>これは<strong>重要。</strong>だから</p>\n"
    );
}

#[test]
fn cjk_emphasis_comma_inside() {
    assert_eq!(
        render_cjk("これは**重要、**だから"),
        "<p>これは<strong>重要、</strong>だから</p>\n"
    );
}

#[test]
fn cjk_emphasis_exclamation_inside() {
    assert_eq!(
        render_cjk("これは**重要！**だから"),
        "<p>これは<strong>重要！</strong>だから</p>\n"
    );
}

// -- Emphasis at start of text followed by CJK --

#[test]
fn cjk_emphasis_start_corner_brackets() {
    assert_eq!(
        render_cjk("**「注意」**してください"),
        "<p><strong>「注意」</strong>してください</p>\n"
    );
}

#[test]
fn cjk_emphasis_start_period() {
    assert_eq!(
        render_cjk("**重要。**なので"),
        "<p><strong>重要。</strong>なので</p>\n"
    );
}

// -- Emphasis preceded by CJK --

#[test]
fn cjk_emphasis_preceded_by_cjk() {
    assert_eq!(
        render_cjk("彼は**「異常」**と言った"),
        "<p>彼は<strong>「異常」</strong>と言った</p>\n"
    );
}

#[test]
fn cjk_emphasis_preceded_by_cjk_parens() {
    assert_eq!(
        render_cjk("私は**（仮説）**だと思う"),
        "<p>私は<strong>（仮説）</strong>だと思う</p>\n"
    );
}

// -- Chinese examples --

#[test]
fn cjk_emphasis_chinese_fullwidth_quotes() {
    assert_eq!(
        render_cjk("我是**\u{201C}美國人\u{201D}**。"),
        "<p>我是<strong>\u{201C}美國人\u{201D}</strong>。</p>\n"
    );
}

#[test]
fn cjk_emphasis_chinese_period() {
    assert_eq!(
        render_cjk("这是。**强调**吗？"),
        "<p>这是。<strong>强调</strong>吗？</p>\n"
    );
}

// -- Korean examples --

#[test]
fn cjk_emphasis_korean_hangul() {
    assert_eq!(
        render_cjk("**강조**합니다"),
        "<p><strong>강조</strong>합니다</p>\n"
    );
}

// -- Spec example from commonmark-spec#650 --

#[test]
fn cjk_emphasis_spec_example_cat() {
    assert_eq!(
        render_cjk("猫は**「のどか」**という。"),
        "<p>猫は<strong>「のどか」</strong>という。</p>\n"
    );
}

// -- Single emphasis (italic) --

#[test]
fn cjk_emphasis_italic_corner_brackets() {
    assert_eq!(
        render_cjk("これは*「重要」*です"),
        "<p>これは<em>「重要」</em>です</p>\n"
    );
}

// -- Underscore delimiters --

#[test]
fn cjk_emphasis_underscore_with_cjk() {
    // Underscore delimiters also work with CJK in CJK-friendly mode,
    // because CJK characters satisfy the flanking condition.
    assert_eq!(
        render_cjk("これは__重要__です"),
        "<p>これは<strong>重要</strong>です</p>\n"
    );
}

// -- Mixed CJK and Latin --

#[test]
fn cjk_emphasis_mixed_latin() {
    assert_eq!(
        render_cjk("日本語**English**混在"),
        "<p>日本語<strong>English</strong>混在</p>\n"
    );
}

// -- Backward compatibility: default mode (no CJK flag) --

#[test]
fn cjk_default_mode_unchanged() {
    // Without ENABLE_CJK_FRIENDLY_EMPHASIS, CJK emphasis with punctuation
    // should NOT work (preserving backward compatibility)
    let result = render_default("これは**「重要」**です");
    // The emphasis should fail, resulting in literal ** in output
    assert!(
        result.contains("**"),
        "Default mode should not change CJK emphasis behavior"
    );
}

// -- Backward compatibility: non-CJK text with CJK mode --

#[test]
fn cjk_mode_ascii_quotes_unchanged() {
    // CommonMark example 360: a*"foo"* should not be emphasis
    assert_eq!(
        render_cjk("a*\"foo\"*"),
        "<p>a*\"foo\"*</p>\n"
    );
}

#[test]
fn cjk_mode_intraword_underscore_unchanged() {
    // foo_bar_baz should not produce emphasis
    assert_eq!(
        render_cjk("foo_bar_baz"),
        "<p>foo_bar_baz</p>\n"
    );
}

#[test]
fn cjk_mode_strong_emph() {
    assert_eq!(
        render_cjk("***strong emph***"),
        "<p><em><strong>strong emph</strong></em></p>\n"
    );
}

#[test]
fn cjk_mode_basic_emphasis() {
    assert_eq!(
        render_cjk("This is **bold** text"),
        "<p>This is <strong>bold</strong> text</p>\n"
    );
}

#[test]
fn cjk_mode_basic_italic() {
    assert_eq!(
        render_cjk("This is *italic* text"),
        "<p>This is <em>italic</em> text</p>\n"
    );
}
