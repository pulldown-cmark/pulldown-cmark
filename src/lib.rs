pub mod parse;
pub mod html;

mod entities;
mod escape;

pub use parse::Parser;
pub use parse::Event;
