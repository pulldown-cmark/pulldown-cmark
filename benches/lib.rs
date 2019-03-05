#![feature(test)]

extern crate pulldown_cmark;
extern crate test;

mod to_html {
    use pulldown_cmark::{Parser, Options, html};
    use pulldown_cmark::escape::{escape_html_safe, escape_html_unsafe};
    use std::str::from_utf8;

    fn render_html(text: &str, opts: Options) -> String {
        let mut s = String::with_capacity(text.len() * 3 / 2);
        let p = Parser::new_ext(text, opts);
        html::push_html(&mut s, p);
        s
    }

    #[bench]
    fn crdt_empty_options(b: &mut test::Bencher) {
        let input_bytes = include_bytes!("crdt.md");
        let input = from_utf8(input_bytes).unwrap();
        let opts = Options::empty();

        b.iter(|| render_html(&input, opts));
    }

    #[bench]
    fn escape_html_dense_unsafe(b: &mut test::Bencher) {
        let test_string = "<html><head><title>our amazing blog!</title><head><body>happy you are here. this is my
        very personal blog. hope u enjoy it. this is me <img alt=\"holiday in greece\" src=\"holiday.jph\"/></body>";
        let expected = "&lt;html&gt;&lt;head&gt;&lt;title&gt;our amazing blog!&lt;/title&gt;&lt;head&gt;&lt;body&gt;happy you are here. this is my
        very personal blog. hope u enjoy it. this is me &lt;img alt=&quot;holiday in greece&quot; src=&quot;holiday.jph&quot;/&gt;&lt;/body&gt;";
        let mut buf = String::with_capacity(1000);

        b.iter(|| {
            escape_html_unsafe(&mut buf, test_string);
            assert_eq!(expected, buf);
            buf.clear();
        });
    }

    #[bench]
    fn escape_html_dense_safe(b: &mut test::Bencher) {
        let test_string = "<html><head><title>our amazing blog!</title><head><body>happy you are here. this is my
        very personal blog. hope u enjoy it. this is me <img alt=\"holiday in greece\" src=\"holiday.jph\"/></body>";
        let expected = "&lt;html&gt;&lt;head&gt;&lt;title&gt;our amazing blog!&lt;/title&gt;&lt;head&gt;&lt;body&gt;happy you are here. this is my
        very personal blog. hope u enjoy it. this is me &lt;img alt=&quot;holiday in greece&quot; src=&quot;holiday.jph&quot;/&gt;&lt;/body&gt;";
        let mut buf = String::with_capacity(1000);

        b.iter(|| {
            escape_html_safe(&mut buf, test_string, false);
            assert_eq!(expected, buf);
            buf.clear();
        });
    }

    #[bench]
    fn escape_html_sparse(b: &mut test::Bencher) {
        let test_string = "On the other hand, we denounce with righteous indignation and dislike men who are so beguiled
         and demoralized by the charms of pleasure of the moment, so blinded by desire, that they cannot foresee the pain
         and trouble that are bound to ensue; and equal blame belongs to those who fail in their duty through weakness
         of will, which is the same as saying through shrinking from toil and pain. These cases are perfectly simple
         and easy to distinguish.";
        let mut buf = String::with_capacity(1000);

        b.iter(|| {
            escape_html_unsafe(&mut buf, test_string);
            assert_eq!(buf, test_string);
            buf.clear();
        });
    }

    #[bench]
    fn escape_html_sparse_safe(b: &mut test::Bencher) {
        let test_string = "On the other hand, we denounce with righteous indignation and dislike men who are so beguiled
         and demoralized by the charms of pleasure of the moment, so blinded by desire, that they cannot foresee the pain
         and trouble that are bound to ensue; and equal blame belongs to those who fail in their duty through weakness
         of will, which is the same as saying through shrinking from toil and pain. These cases are perfectly simple
         and easy to distinguish.";
        let mut buf = String::with_capacity(1000);

        b.iter(|| {
            escape_html_safe(&mut buf, test_string, false);
            assert_eq!(buf, test_string);
            buf.clear();
        });
    }
}