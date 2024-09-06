use criterion::{criterion_group, criterion_main, Criterion};
use pulldown_cmark::{html, Options, Parser};
use std::str::from_utf8;

static CRDT_BYTES: &[u8] = include_bytes!("../../pulldown-cmark/third_party/xi-editor/crdt.md");

fn criterion_benchmark(c: &mut Criterion) {
    let mut full_opts = Options::empty();
    full_opts.insert(Options::ENABLE_TABLES);
    full_opts.insert(Options::ENABLE_FOOTNOTES);
    full_opts.insert(Options::ENABLE_STRIKETHROUGH);
    full_opts.insert(Options::ENABLE_TASKLISTS);
    full_opts.insert(Options::ENABLE_SMART_PUNCTUATION);

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

    c.bench_function("crdt_all_options_parse", |b| {
        let input = from_utf8(CRDT_BYTES).unwrap();

        b.iter(|| Parser::new_ext(input, full_opts).count())
    });

    c.bench_function("crdt_parse", |b| {
        let input = from_utf8(CRDT_BYTES).unwrap();

        b.iter(|| Parser::new_ext(input, Options::empty()).count())
    });

    c.bench_function("smart_punctuation", |b| {
        let input = r#"""'This here a real "quote"'

And -- if you're interested -- some em-dashes. Wait --- she actually said that?

Wow... Becky is so 'mean'!
"""#;

        b.iter(|| Parser::new_ext(input, full_opts).count());
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

    c.bench_function("inline_link_to_sample", |b| {
        let input = r###"
        [Playground](https://play.rust-lang.org/?code=%23!%5Ballow(unused)%5D%0Afn+main()+%7B%0A++++let+mut+x+=+Some(42);%0A++++%0A++++let+prev+=+x.take_if(%7Cv%7C+if+*v+==+42+%7B%0A++++++++*v+%2B=+1;%0A++++++++false%0A++++%7D+else+%7B%0A++++++++false%0A++++%7D);%0A++++assert_eq!(x,+Some(43));%0A++++assert_eq!(prev,+None);%0A++++%0A++++let+prev+=+x.take_if(%7Cv%7C+*v+==+43);%0A++++assert_eq!(x,+None);%0A++++assert_eq!(prev,+Some(43));%0A%7D&edition=2021)
        [Playground](https://play.rust-lang.org/?code=%23!%5Ballow(unused)%5D%0Afn+main()+%7B%0A++++let+mut+vec+=+Vec::new();%0A++++vec.push(1);%0A++++vec.push(2);%0A++++%0A++++assert_eq!(vec.len(),+2);%0A++++assert_eq!(vec%5B0%5D,+1);%0A++++%0A++++assert_eq!(vec.pop(),+Some(2));%0A++++assert_eq!(vec.len(),+1);%0A++++%0A++++vec%5B0%5D+=+7;%0A++++assert_eq!(vec%5B0%5D,+7);%0A++++%0A++++vec.extend(%5B1,+2,+3%5D);%0A++++%0A++++for+x+in+%26vec+%7B%0A++++++++println!(%22%7Bx%7D%22);%0A++++%7D%0A++++assert_eq!(vec,+%5B7,+1,+2,+3%5D);%0A%7D&edition=2021)
        [Playground](https://play.rust-lang.org/?code=%23!%5Ballow(unused)%5D%0Afn+main()+%7B%0A++++let+mut+vec+=+Vec::with_capacity(10);%0A++++%0A++++//+The+vector+contains+no+items,+even+though+it+has+capacity+for+more%0A++++assert_eq!(vec.len(),+0);%0A++++assert!(vec.capacity()+%3E=+10);%0A++++%0A++++//+These+are+all+done+without+reallocating...%0A++++for+i+in+0..10+%7B%0A++++++++vec.push(i);%0A++++%7D%0A++++assert_eq!(vec.len(),+10);%0A++++assert!(vec.capacity()+%3E=+10);%0A++++%0A++++//+...but+this+may+make+the+vector+reallocate%0A++++vec.push(11);%0A++++assert_eq!(vec.len(),+11);%0A++++assert!(vec.capacity()+%3E=+11);%0A++++%0A++++//+A+vector+of+a+zero-sized+type+will+always+over-allocate,+since+no%0A++++//+allocation+is+necessary%0A++++let+vec_units+=+Vec::%3C()%3E::with_capacity(10);%0A++++assert_eq!(vec_units.capacity(),+usize::MAX);%0A%7D&edition=2021)
        "###;

        b.iter(|| Parser::new_ext(input, Options::empty()).count());
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
