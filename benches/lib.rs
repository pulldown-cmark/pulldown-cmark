#![feature(test)]

extern crate pulldown_cmark;
extern crate test;

mod to_html {
    use pulldown_cmark::{Parser, Options, html};
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

    #[bench]
    fn more_pathological_codeblocks(b: &mut test::Bencher) {
        let input = std::iter::repeat("\\``").take(1000).collect::<String>();

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_hrules(b: &mut test::Bencher) {
        let mut input = std::iter::repeat("* ").take(2000).collect::<String>();
        input.push('a');

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn advanced_pathological_codeblocks(b: &mut test::Bencher) {
        // Note that `buf` grows quadratically with number of
        // iterations. The point here is that the render time shouldn't
        // grow faster than that.
        let size = 120;
        let mut buf = String::new();
        for i in 1..size {
            for _ in 0..i {
                buf.push('`');
            }
            buf.push(' ');
        }
        for _ in 1..(size * size) {
            buf.push_str("*a* ");
        }
        eprintln!("str size: {}", buf.len());

        b.iter(|| render_html(&buf, Options::empty()));
    }
}
