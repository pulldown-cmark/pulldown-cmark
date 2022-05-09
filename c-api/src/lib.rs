use libc::{c_char, c_int};

use pulldown_cmark::{Parser, Options, html};
use std::ffi::CStr;

/// Render Commonmark to HTML.
#[no_mangle]
pub unsafe extern "C" fn pulldown_cmark_commonmark_to_html(c_text: *const c_char, c_options: c_int) -> *mut c_char {
    // Convert C string to Rust string
    assert!(!c_text.is_null());
    let commonmark_input = CStr::from_ptr(c_text).to_str().unwrap();

    // Set up options and parser
    let mut options = Options::empty();
    options.insert(Options::ENABLE_STRIKETHROUGH);
    let parser = Parser::new_ext(commonmark_input, options);

    // Write to String buffer.
    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);

    // Convert to C string and return
    Box::into_raw(html_output.into_boxed_str()) as *mut c_char
}
