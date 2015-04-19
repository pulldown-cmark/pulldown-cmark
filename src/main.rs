mod parse;
mod entities;
mod html;
mod escape;

use parse::Parser;
use parse::Event;

pub fn main() {
	let source = "a b\r\nc &aring; &#x3d; d\n---\n\ne f\n1\t12\t123\t1234\t.";
	//let source = "\\\\\\&aring; break:\\\nbreak (\\\\r\\\\n):\\\r\n\\A\\B";
	let mut p = Parser::new(&source);
	loop {
		print!("{}: ", p.get_offset());
		if let Some(event) = p.next() {
			match event {
				Event::Start(tag) => println!("start {:?}", tag),
				Event::End(tag) => println!("end {:?}", tag),
				Event::Text(text) => println!("text: [{}]", text),
				Event::Entity(text) => println!("entity: [{}]", text),
				Event::LineBreak => println!("line break")
			}
		} else {
			break;
		}
	}
	println!("EOF");
	let p = Parser::new(&source);
	let mut s = String::new();
	html::push_html(&mut s, p);
	print!("{}", s);
}
