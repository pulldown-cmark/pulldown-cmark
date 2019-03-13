include!("normalize_html.rs.inc");

#[test]
fn strip_div_newline() {
    assert_eq!("<div></div>", normalize_html("<div>\n</div>"));
}

#[test]
fn strip_end_newline() {
    assert_eq!("test", normalize_html("test\n"));
}

#[test]
fn strip_double_space() {
    assert_eq!("test mess", normalize_html("test  mess"));
}

#[test]
fn strip_inline_internal_text() {
    assert_eq!("<u>a </u>b <u>c</u>", normalize_html("<u> a </u> b <u> c </u>"))
}

#[test]
fn strip_inline_block_internal_text() {
    assert_eq!("<u>a </u>b <u>c</u>", normalize_html(" <u> a </u> b <u> c </u> "))
}

#[test]
fn leaves_necessary_whitespace_alone() {
    assert_eq!("<u>a</u> b <u>c</u>", normalize_html("<u>a</u> b <u>c</u>"))
}

#[test]
fn leaves_necessary_whitespace_alone_weird() {
    assert_eq!("<u>a </u>b <u>c</u>", normalize_html(" <u>a </u>b <u>c</u>"))
}

#[test]
fn leaves_necessary_whitespace_all_nested() {
    assert_eq!("<u></u><u></u><u></u><u></u>", normalize_html("<u> </u><u> </u><u> </u><u> </u>"))
}

#[test]
fn drops_empty_tbody() {
    assert_eq!("<table><thead><tr><td>hi</td></tr></thead></table>", normalize_html("<table><thead><tr><td>hi</td></tr></thead><tbody>  </tbody></table>"))
}

#[test]
fn leaves_nonempty_tbody() {
    assert_eq!("<table><thead><tr><td>hi</td></tr></thead><tbody><tr></tr></tbody></table>", normalize_html("<table><thead><tr><td>hi</td></tr></thead><tbody><tr></tr></tbody></table>"))
}

