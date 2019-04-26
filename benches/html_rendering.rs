#[macro_use]
extern crate criterion;
extern crate pulldown_cmark;

use criterion::Criterion;
use pulldown_cmark::{Parser, Options, html};
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

//     #[bench]
//     fn crdt_empty_options(b: &mut test::Bencher) {
//         let input_bytes = include_bytes!("../third_party/xi-editor/crdt.md");
//         let input = from_utf8(input_bytes).unwrap();

//         b.iter(|| render_html(&input, Options::empty()));
//     }

//     #[bench]
//     fn links_and_emphasis(b: &mut test::Bencher) {
//         let input = r#"""This is a [link](example.com). **Cool!**

// This is a [link](example.com). **Cool!**

// This is a [link](example.com). **Cool!**

// This is a [link](example.com). **Cool!**
// """#;

//         b.iter(|| render_html(input, Options::empty()));
//     }

//     #[bench]
//     fn paragraph_lots_unescapes(b: &mut test::Bencher) {
//         let input = "This is by far my favourite unicode code point: &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
//         &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
//         &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
//         &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
//         &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
//         &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
//         &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;
//         &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA; &#xAAA;";

//         b.iter(|| render_html(&input, Options::empty()));
//     }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
