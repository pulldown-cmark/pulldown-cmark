use std::ops::Deref;
use std::borrow::Borrow;
use std::str::from_utf8_unchecked;

#[derive(Debug, Clone, Copy)]
pub struct StackStr {
    inner: [u8; 4],
    len: usize,
}

impl From<char> for StackStr {
    fn from(c: char) -> Self {
        let mut inner = [0u8; 4];
        c.encode_utf8(&mut inner);
        Self {
            inner,
            len: c.len_utf8(),
        }
    }
}

impl Deref for StackStr {
    type Target = str;

    fn deref(&self) -> &str {
        unsafe {
            from_utf8_unchecked(&self.inner[..self.len])
        }
    }
}

pub enum SmortStr<'a> {
    Boxed(Box<str>),
    Borrowed(&'a str),
    Stacked(StackStr),
}

impl<'a> From<&'a str> for SmortStr<'a> {
    fn from(s: &'a str) -> Self {
        SmortStr::Borrowed(s)
    }
}

impl<'a> From<String> for SmortStr<'a> {
    fn from(s: String) -> Self {
        SmortStr::Boxed(s.into_boxed_str())
    }
}

impl<'a> From<char> for SmortStr<'a> {
    fn from(c: char) -> Self {
        SmortStr::Stacked(c.into())
    }
}

impl<'a> Deref for SmortStr<'a> {
    type Target = str;

    fn deref(&self) -> &str {
        match self {
            SmortStr::Boxed(ref b) => &*b,
            SmortStr::Borrowed(b) => b,
            SmortStr::Stacked(ref s) => s.deref(),
        }
    }
}

impl<'a> Borrow<str> for SmortStr<'a> {
    fn borrow(&self) -> &str {
        self.deref()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn stackstr_ascii() {
        let s: StackStr = 'a'.into();
        assert_eq!("a", s.deref());
    }

    #[test]
    fn stackstr_unicode() {
        let s: StackStr = '🍔'.into();
        assert_eq!("🍔", s.deref());
    }

    #[test]
    fn smortstr_size() {
        let size = std::mem::size_of::<SmortStr>();
        let word_size = std::mem::size_of::<isize>();
        assert_eq!(3 * word_size, size);
    }

    #[test]
    fn smortstr_char_to_owned() {
        let c = '藏';
        let smort: SmortStr = c.into();
        let owned: String = smort.to_owned();
        let expected = "藏".to_owned();
        assert_eq!(expected, owned);
    }
}
