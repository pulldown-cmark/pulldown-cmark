#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &str| {
    for _ in pulldown_cmark::Parser::new(data) {};
});
