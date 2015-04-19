use std::str::from_utf8;

static HREF_SAFE: [u8; 128] = [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1,
        0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
    ];

static HEX_CHARS: &'static [u8] = b"0123456789ABCDEF";

pub fn escape_href(ob: &mut String, s: &str) {
	let mut mark = 0;
	for i in 0..s.len() {
		let c = s.as_bytes()[i];
		if c < 0x80 && HREF_SAFE[c as usize] == 0 {
			// character needing escape

			// write partial substring up to mark
			if mark < i {
				ob.push_str(&s[mark..i]);
			}
			match c {
				b'&' => {
					ob.push_str("&amp;");
				},
				b'\'' => {
					ob.push_str("&#x27;");
				},
				_ => {
					let mut buf = [0u8; 3];
					buf[0] = b'%';
					buf[1] = HEX_CHARS[((c as usize) >> 4) & 0xF];
					buf[2] = HEX_CHARS[(c as usize) & 0xF];
					ob.push_str(from_utf8(&buf).unwrap());
				}
			}
			mark = i + 1;  // all escaped characters are ASCII
		}
	}
	if mark < s.len() {
		ob.push_str(&s[mark..]);
	}
}

static HTML_ESCAPE_TABLE: [u8; 64] = [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 1, 0, 0, 0, 2, 3, 0, 0, 0, 0, 0, 0, 0, 4,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 6, 0,
	];

static HTML_ESCAPES: [&'static str; 7] = [
		"",
		"&quot;",
		"&amp;",
		"&#39;",
		"&#47;",
		"&lt;",
		"&gt;"
	];

pub fn escape_html(ob: &mut String, s: &str, secure: bool) {
	let mut mark = 0;
	for i in 0..s.len() {
		let c = s.as_bytes()[i];
		if c < 0x40 {
			let escape = HTML_ESCAPE_TABLE[c as usize];
			if escape != 0 && (secure || c != b'/') {
				if mark < i {
					ob.push_str(&s[mark..i]);
				}
				ob.push_str(HTML_ESCAPES[escape as usize]);
				mark = i + 1;  // all escaped characters are ASCII
			}
		}
	}
	if mark < s.len() {
		ob.push_str(&s[mark..]);
	}	
}
