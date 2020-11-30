use criterion::{criterion_group, criterion_main};

mod to_html {
    use criterion::Criterion;
    use pulldown_cmark::{html, Options, Parser};

    pub fn benchmarks(c: &mut Criterion) {
        c.bench_function("pathological_codeblocks1", |b| {
            // Note that `buf` grows quadratically with number of
            // iterations. The point here is that the render time shouldn't
            // grow faster than that.
            let mut buf = String::new();
            for i in 1..1000 {
                buf.push_str(&"`".repeat(i));
                buf.push(' ');
            }

            b.iter(|| render_html(&buf, Options::empty()));
        });

        c.bench_function("advanced_pathological_codeblocks", |b| {
            let mut buf = String::new();
            let mut i = 1;
            while buf.len() < 1250 {
                buf.push_str(&"`".repeat(i));
                buf.push(' ');
                i += 1;
            }
            buf.push_str(&"*a* ".repeat(buf.len()));

            b.iter(|| render_html(&buf, Options::empty()));
        });
    }

    fn render_html(text: &str, opts: Options) -> String {
        let mut s = String::with_capacity(text.len() * 3 / 2);
        let p = Parser::new_ext(text, opts);
        html::push_html(&mut s, p);
        s
    }
}

criterion_group!(benches, to_html::benchmarks);
criterion_main!(benches);
