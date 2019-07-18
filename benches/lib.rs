#![feature(test)]

extern crate pulldown_cmark;
extern crate test;

mod to_html {
    use pulldown_cmark::{html, Options, Parser};

    fn render_html(text: &str, opts: Options) -> String {
        let mut s = String::with_capacity(text.len() * 3 / 2);
        let p = Parser::new_ext(text, opts);
        html::push_html(&mut s, p);
        s
    }

    #[bench]
    fn pathological_links(b: &mut test::Bencher) {
        let input = "[a](<".repeat(1000);

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_links2(b: &mut test::Bencher) {
        let input = "[[]()".repeat(1000);

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_emphasis1(b: &mut test::Bencher) {
        let input = "a***".repeat(1000);

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_emphasis2(b: &mut test::Bencher) {
        let input = "a***_b__".repeat(1000);

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_emphasis3(b: &mut test::Bencher) {
        let input = "[*_a".repeat(1000);

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_emphasis4(b: &mut test::Bencher) {
        let input = "*~~\u{a0}".repeat(1000);
        let mut opts = Options::empty();
        opts.insert(Options::ENABLE_STRIKETHROUGH);

        b.iter(|| render_html(&input, opts));
    }

    #[bench]
    fn pathological_strikethrough(b: &mut test::Bencher) {
        let input = "a***b~~".repeat(1000);

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
        let input = "\\``".repeat(1000);

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_codeblocks3(b: &mut test::Bencher) {
        let mut input = "`a`".repeat(1000);
        input.push('`');

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_hrules(b: &mut test::Bencher) {
        let mut input = "* ".repeat(1000);
        input.push('a');

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_link_titles(b: &mut test::Bencher) {
        let input = "[ (](".repeat(1000);

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn advanced_pathological_codeblocks(b: &mut test::Bencher) {
        let mut buf = String::new();
        let mut i = 1;
        while buf.len() < 1250 {
            for _ in 0..i {
                buf.push('`');
            }
            buf.push(' ');
            i += 1;
        }
        for _ in 0..buf.len() {
            buf.push_str("*a* ");
        }

        b.iter(|| render_html(&buf, Options::empty()));
    }

    #[bench]
    fn pathological_cdata(b: &mut test::Bencher) {
        let input = "a <![CDATA[ ".repeat(1000);

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_html_processing(b: &mut test::Bencher) {
        let input = "a <? ".repeat(1000);

        b.iter(|| render_html(&input, Options::empty()));
    }

    #[bench]
    fn pathological_html_defs(b: &mut test::Bencher) {
        let input = "a <!A ".repeat(1000);

        b.iter(|| render_html(&input, Options::empty()));
    }
}
