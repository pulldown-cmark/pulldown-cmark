#![feature(test)]

extern crate pulldown_cmark;
extern crate test;

mod to_html {
    use pulldown_cmark::{Parser, Options, html};
    use std::io::Read;
    use std::path::Path;
    use std::fs::File;

    fn render_html(text: &str, opts: Options) -> String {
        let mut s = String::with_capacity(text.len() * 3 / 2);
        let p = Parser::new_ext(text, opts);
        html::push_html(&mut s, p);
        s
    }

    fn read_file(filename: &str) -> String {
        let path = Path::new(filename);
        let mut file = match File::open(&path) {
            Err(why) => panic!("couldn't open {}: {}", path.display(), why),
            Ok(file) => file
        };
        let mut s = String::new();
        match file.read_to_string(&mut s) {
            Err(why) => panic!("couldn't open {}: {}", path.display(), why),
            Ok(_) => s
        }
    }

    #[bench]
    fn crdt_empty_options(b: &mut test::Bencher) {
        let input = read_file("./benches/crdt.md");
        let mut opts = Options::empty();

        b.iter(|| render_html(&input, opts));
    }
}