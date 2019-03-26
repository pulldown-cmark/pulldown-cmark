#![feature(test)]

extern crate pulldown_cmark;
extern crate test;

mod to_html {
    use pulldown_cmark::{Parser, Options, html};
    use pulldown_cmark::escape::escape_html;
    use std::str::from_utf8;

    fn render_html(text: &str, opts: Options) -> String {
        let mut s = String::with_capacity(text.len() * 3 / 2);
        let p = Parser::new_ext(text, opts);
        html::push_html(&mut s, p);
        s
    }

    #[bench]
    fn crdt_empty_options(b: &mut test::Bencher) {
        let input_bytes = include_bytes!("../third_party/xi-editor/crdt.md");
        let input = from_utf8(input_bytes).unwrap();

        b.iter(|| render_html(&input, Options::empty()));
    }

    static DENSE_TEST_STRING: &str = "<html><head><title>our amazing blog!</title><head><body>happy you are here. this is my
        very personal blog. hope u enjoy it. this is me <img alt=\"holiday in greece\" src=\"holiday.jph\"/></body>";
    static DENSE_EXPECTED: &str = "&lt;html&gt;&lt;head&gt;&lt;title&gt;our amazing blog!&lt;/title&gt;&lt;head&gt;&lt;body&gt;happy you are here. this is my
        very personal blog. hope u enjoy it. this is me &lt;img alt=&quot;holiday in greece&quot; src=&quot;holiday.jph&quot;/&gt;&lt;/body&gt;";

    #[bench]
    fn escape_html_dense(b: &mut test::Bencher) {
        let mut buf = String::with_capacity(1000);

        b.iter(|| unsafe {
            escape_html(buf.as_mut_vec(), DENSE_TEST_STRING).unwrap();
            assert_eq!(DENSE_EXPECTED, buf);
            buf.clear();
        });
    }

    static SPARSE_TEST_STRING: &str = "On the other hand, we denounce with righteous indignation and dislike men who are so beguiled
         and demoralized by the charms of pleasure of the moment, so blinded by desire, that they cannot foresee the pain
         and trouble that are bound to ensue; and equal blame belongs to those who fail in their duty through weakness
         of will, which is the same as saying through shrinking from toil and pain. These cases are perfectly simple
         and easy to distinguish.";

    #[bench]
    fn escape_html_sparse(b: &mut test::Bencher) {
        let mut buf = String::with_capacity(1000);

        b.iter(|| unsafe {
            escape_html(buf.as_mut_vec(), SPARSE_TEST_STRING).unwrap();
            assert_eq!(buf, SPARSE_TEST_STRING);
            buf.clear();
        });
    }

    #[bench]
    fn paragraph_lots_unescapes(b: &mut test::Bencher) {
        let input = "This is by far my favourite unicode code point: &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;";

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_links(b: &mut test::Bencher) {
        let input = std::iter::repeat("[a](<").take(1000).collect::<String>();

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_codeblocks(b: &mut test::Bencher) {
        // Note that `buf` grows quadratically with number of
        // iterations. The point here is that the render time shouldn't
        // grow faster than that.
        let mut buf = String::new();
        for i in 1..1000 {
            for _ in 0..i {
                buf.push('`');
            }
            buf.push(' ');
        }

        b.iter(|| render_html(&buf, Options::empty()));
    }
}
