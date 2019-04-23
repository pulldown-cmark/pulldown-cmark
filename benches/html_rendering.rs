#[macro_use]
extern crate criterion;
extern crate pulldown_cmark;

use criterion::Criterion;
use pulldown_cmark::{Parser, Options, html};
use std::str::from_utf8;

fn render_html(text: &str, opts: Options) -> String {
    let mut s = String::with_capacity(text.len() * 3 / 2);
    let p = Parser::new_ext(text, opts);
    html::push_html(&mut s, p);
    s
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("crdt empty options", |b| {
        let input_bytes = include_bytes!("../third_party/xi-editor/crdt.md");
        let input = from_utf8(input_bytes).unwrap();

        b.iter(|| render_html(&input, Options::empty()))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
