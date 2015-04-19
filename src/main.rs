mod parse;

use parse::Parser;
use parse::Tag;
use parse::Event;

pub fn main() {
	let source = "a b\nc d\n\ne f";
	let p = Parser::new(&source);
	for event in p {
		match event {
			Event::Start(tag) => println!("start {:?}", tag),
			Event::End(tag) => println!("end {:?}", tag),
			Event::Text(text) => println!("text: [{}]", text)
		}
	}
}
