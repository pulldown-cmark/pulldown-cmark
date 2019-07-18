#[macro_use]
extern crate criterion;
extern crate pulldown_cmark;

use criterion::Criterion;
use pulldown_cmark::{html, Options, Parser};
use std::str::from_utf8;

static CRDT_BYTES: &[u8] = include_bytes!("../third_party/xi-editor/crdt.md");

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("crdt_total", |b| {
        let input = from_utf8(CRDT_BYTES).unwrap();
        let mut buf = String::with_capacity(input.len() * 3 / 2);

        b.iter(|| {
            buf.clear();
            html::push_html(&mut buf, Parser::new_ext(input, Options::empty()));
        })
    });

    c.bench_function("crdt_html", |b| {
        let input = from_utf8(CRDT_BYTES).unwrap();
        let events: Vec<_> = Parser::new_ext(input, Options::empty()).collect();
        let mut buf = String::with_capacity(input.len() * 3 / 2);

        b.iter(|| {
            buf.clear();
            html::push_html(&mut buf, events.clone().into_iter());
        })
    });

    c.bench_function("crdt_parse", |b| {
        let input = from_utf8(CRDT_BYTES).unwrap();

        b.iter(|| Parser::new_ext(input, Options::empty()).count())
    });

    c.bench_function("links_n_emphasis", |b| {
        let input = r#"""This is a [link](example.com). **Cool!**

This is a [link](example.com). **Cool!**

This is a [link](example.com). **Cool!**

This is a [link](example.com). **Cool!**
"""#;

        b.iter(|| Parser::new_ext(input, Options::empty()).count());
    });

    c.bench_function("unescapes", |b| {
        let input = "This is by far my favourite unicode code point: &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
        &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;";

        b.iter(|| Parser::new_ext(input, Options::empty()).count());
    });

    c.bench_function("autolinks_n_html", |b| {
        let input = "Drop me a line at <john@example.com>. <emph font-weight='BOLD'>Thanks!</emph> <otherinline>
        Drop me a line at <john@example.com>. <emph font-weight='BOLD'>Thanks!</emph> <otherinline>
        Drop me a line at <john@example.com>. <emph font-weight='BOLD'>Thanks!</emph> <otherinline>
        Drop me a line at <john@example.com>. <emph font-weight='BOLD'>Thanks!</emph> <otherinline>
        Drop me a line at <john@example.com>. <emph font-weight='BOLD'>Thanks!</emph> <otherinline>";

        b.iter(|| Parser::new_ext(input, Options::empty()).count());
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
