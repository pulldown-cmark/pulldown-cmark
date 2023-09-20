//! Miscellaneous utilities to incraese comfort.
//! Special thanks to
//! <https://github.com/BenjaminRi/Redwood-Wiki/blob/master/src/markdown_utils.rs>.
//! Its author authorized the use of this GPL code in this project in
//! <https://github.com/raphlinus/pulldown-cmark/issues/507>.

use crate::{CowStr, Event};

/// Merge consecutive `Event::Text` events into only one.
#[derive(Debug)]
pub struct TextMergeStream<'a, I> {
    iter: I,
    last_event: Option<Event<'a>>,
}

impl<'a, I> TextMergeStream<'a, I>
where
    I: Iterator<Item = Event<'a>>,
{
    pub fn new(iter: I) -> Self {
        Self {
            iter,
            last_event: None,
        }
    }
}

impl<'a, I> Iterator for TextMergeStream<'a, I>
where
    I: Iterator<Item = Event<'a>>,
{
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match (self.last_event.take(), self.iter.next()) {
            (Some(Event::Text(last_text)), Some(Event::Text(next_text))) => {
                // We need to start merging consecutive text events together into one
                let mut string_buf: String = last_text.into_string();
                string_buf.push_str(&next_text);
                loop {
                    // Avoid recursion to avoid stack overflow and to optimize concatenation
                    match self.iter.next() {
                        Some(Event::Text(next_text)) => {
                            string_buf.push_str(&next_text);
                        }
                        next_event => {
                            self.last_event = next_event;
                            if string_buf.is_empty() {
                                // Discard text event(s) altogether if there is no text
                                break self.next();
                            } else {
                                break Some(Event::Text(CowStr::Boxed(
                                    string_buf.into_boxed_str(),
                                )));
                            }
                        }
                    }
                }
            }
            (None, Some(next_event)) => {
                // This only happens once during the first iteration and if there are items
                self.last_event = Some(next_event);
                self.next()
            }
            (None, None) => {
                // This happens when the iterator is depleted
                None
            }
            (last_event, next_event) => {
                // The ordinary case, emit one event after the other without modification
                self.last_event = next_event;
                last_event
            }
        }
    }
}
