#![feature(test)]

extern crate pulldown_cmark;
extern crate test;

mod to_html {
    use pulldown_cmark::{Parser, Options, html};

    fn render_html(text: &str, opts: Options) -> String {
        let mut s = String::with_capacity(text.len() * 3 / 2);
        let p = Parser::new_ext(text, opts);
        html::push_html(&mut s, p);
        s
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
    fn pathological_emphasis(b: &mut test::Bencher) {
        let input = std::iter::repeat("a***").take(1000).collect::<String>();

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_emphasis2(b: &mut test::Bencher) {
        let input = std::iter::repeat("a***_b__").take(1000).collect::<String>();

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_emphasis3(b: &mut test::Bencher) {
        let input = std::iter::repeat("[*_a").take(1000).collect::<String>();

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_strikethrough(b: &mut test::Bencher) {
        let input = std::iter::repeat("a***b~~").take(2000).collect::<String>();

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_codeblocks1(b: &mut test::Bencher) {
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
    fn pathological_codeblocks2(b: &mut test::Bencher) {
        let input = std::iter::repeat("\\``").take(1000).collect::<String>();

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_codeblocks3(b: &mut test::Bencher) {
        let mut input = std::iter::repeat("`a`").take(4000).collect::<String>();
        input.push('`');

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_hrules(b: &mut test::Bencher) {
        let mut input = std::iter::repeat("* ").take(2000).collect::<String>();
        input.push('a');

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_link_titles(b: &mut test::Bencher) {
        let input = std::iter::repeat("[ (](").take(2000).collect::<String>();

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
