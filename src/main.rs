mod parse;
mod entities;
mod html;
mod escape;

use parse::Parser;
use parse::Event;
use std::io;
use std::io::Read;

pub fn main() {
	let mut input = String::new();
	io::stdin().read_to_string(&mut input);
	let mut p = Parser::new(&input);
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
	let p = Parser::new(&input);
	let mut s = String::new();
	html::push_html(&mut s, p);
	print!("{}", s);
}
