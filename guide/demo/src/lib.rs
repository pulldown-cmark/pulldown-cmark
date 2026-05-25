#![no_std]
#![expect(clippy::missing_safety_doc)]

use alloc::string::String;
use core::fmt::Write;
use dlmalloc::GlobalDlmalloc;

extern crate alloc;

#[global_allocator]
static ALLOC: GlobalDlmalloc = GlobalDlmalloc;

#[panic_handler]
#[cfg(target_arch = "wasm32")]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    core::arch::wasm32::unreachable()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn alloc(size: usize, align: usize) -> *mut u8 {
    let layout = alloc::alloc::Layout::from_size_align(size, align).unwrap();
    unsafe { alloc::alloc::alloc(layout) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn dealloc(ptr: *mut u8, size: usize, align: usize) {
    let layout = alloc::alloc::Layout::from_size_align(size, align).unwrap();
    unsafe { alloc::alloc::dealloc(ptr, layout) };
}

#[repr(C)]
pub struct FfiString {
    ptr: *mut u8,
    len: usize,
    cap: usize,
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn render(
    ptr: *mut u8,
    len: usize,
    capacity: usize,
    options: u32,
    ast: bool,
    out: *mut FfiString,
) {
    let s = unsafe { String::from_raw_parts(ptr, len, capacity) };

    let options = pulldown_cmark::Options::from_bits(options).unwrap();
    let parser = pulldown_cmark::Parser::new_ext(&s, options);

    let mut res = String::new();

    match ast {
        false => {
            pulldown_cmark::html::push_html(&mut res, parser);
        }
        true => {
            res.push_str("<ul>");
            for event in parser {
                match event {
                    pulldown_cmark::Event::End(_) => res.push_str("</ul>"),
                    _ => res.push_str("<li>"),
                }
                write!(HtmlEscape(&mut res), "{event:?}").unwrap();
                match event {
                    pulldown_cmark::Event::Start(_) => res.push_str("<ul>"),
                    _ => res.push_str("</li>"),
                }
            }
            res.push_str("</ul>");
        }
    }

    let (ptr, len, cap) = res.into_raw_parts();
    unsafe { *out = FfiString { ptr, len, cap } };
}

struct HtmlEscape<'a>(&'a mut String);

impl Write for HtmlEscape<'_> {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for c in s.chars() {
            match c {
                '&' => self.0.push_str("&amp;"),
                '<' => self.0.push_str("&lt;"),
                '>' => self.0.push_str("&gt;"),
                c => self.0.push(c),
            }
        }
        Ok(())
    }
}
