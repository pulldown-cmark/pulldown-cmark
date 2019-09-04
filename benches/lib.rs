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
}
