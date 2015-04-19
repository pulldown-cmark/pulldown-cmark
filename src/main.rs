mod parse;

use parse::Parser;
use parse::Event;

pub fn main() {
	let source = "a b\nc d\n---\n\ne f";
	let mut p = Parser::new(&source);
	loop {
		print!("{}: ", p.get_offset());
		if let Some(event) = p.next() {
			match event {
				Event::Start(tag) => println!("start {:?}", tag),
				Event::End(tag) => println!("end {:?}", tag),
				Event::Text(text) => println!("text: [{}]", text)
			}
		} else {
			break;
		}
	}
	println!("EOF");
}
