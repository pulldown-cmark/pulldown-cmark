use std::ops::Deref;
use std::borrow::{ToOwned, Borrow};
use std::str::from_utf8_unchecked;

const DOUBLE_WORD_SIZE: usize = 2 * std::mem::size_of::<isize>();

/// Returned when trying to convert a &str into a StackStr
/// but it fails because it doesn't fit.
#[derive(Debug)]
pub struct StringTooLongError;

#[derive(Debug, Clone, Copy)]
pub struct StackStr {
    inner: [u8; DOUBLE_WORD_SIZE],
}

impl From<char> for StackStr {
    fn from(c: char) -> Self {
        let mut inner = [0u8; DOUBLE_WORD_SIZE];
        c.encode_utf8(&mut inner);
        inner[DOUBLE_WORD_SIZE - 1] = c.len_utf8() as u8;

        Self { inner }
    }
}

impl<'a> std::cmp::PartialEq<StackStr> for StackStr {
    fn eq(&self, other: &StackStr) -> bool {
        self.deref() == other.deref()
    }
}

// This could be an implementation of TryFrom<&str>
// when that trait is stabilized.
impl StackStr {
    pub fn try_from_str(s: &str) -> Result<StackStr, StringTooLongError> {
        let len = s.len();
        if len < DOUBLE_WORD_SIZE {
            let mut inner = [0u8; DOUBLE_WORD_SIZE];
            inner[..len].copy_from_slice(s.as_bytes());
            inner[DOUBLE_WORD_SIZE - 1] = len as u8;
            Ok(Self { inner })
        } else {
            Err(StringTooLongError)
        }
    }
}

impl Deref for StackStr {
    type Target = str;

    fn deref(&self) -> &str {
        let len = self.inner[DOUBLE_WORD_SIZE - 1] as usize;
        unsafe {
            from_utf8_unchecked(&self.inner[..len])
        }
    }
}

#[derive(Debug)]
pub enum SmortStr<'a> {
    Boxed(Box<str>),
    Borrowed(&'a str),
    Stacked(StackStr),
}

impl<'a> std::clone::Clone for SmortStr<'a> {
    fn clone(&self) -> Self {
        match self {
            SmortStr::Boxed(s) if s.len() < DOUBLE_WORD_SIZE
                => SmortStr::Stacked(StackStr::try_from_str(&**s).unwrap()),
            SmortStr::Boxed(s) => SmortStr::Boxed(s.clone()),
            SmortStr::Borrowed(s) => SmortStr::Borrowed(s),
            SmortStr::Stacked(s) => SmortStr::Stacked(*s),
        }
    }
}

impl<'a> std::cmp::PartialEq<SmortStr<'a>> for SmortStr<'a> {
    fn eq(&self, other: &SmortStr) -> bool {
        self.deref() == other.deref()
    }
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

impl<'a> SmortStr<'a> {
    fn to_string(self) -> String {
        match self {
            SmortStr::Boxed(b) => b.into(),
            SmortStr::Borrowed(b) => b.to_owned(),
            SmortStr::Stacked(s) => s.deref().to_owned(),
        }        
    }
}

#[cfg(test)]
mod test_special_string {
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
    fn smortstr_char_to_string() {
        let c = '藏';
        let smort: SmortStr = c.into();
        let owned: String = smort.to_string();
        let expected = "藏".to_owned();
        assert_eq!(expected, owned);
    }

    #[test]
    fn double_word_size_atleast_five() {
        // we need 4 bytes to store a char and then one more to store
        // its length
        assert!(DOUBLE_WORD_SIZE >= 5);
    }

    #[test]
    #[cfg(target_pointer_width = "64")]
    fn stackstr_fits_fifteen() {
        let s = "0123456789abcde";
        let stack_str = StackStr::try_from_str(s).unwrap();
        assert_eq!(stack_str.deref(), s);
    }

    #[test]
    #[cfg(target_pointer_width = "64")]
    fn stackstr_not_fits_sixteen() {
        let s = "0123456789abcdef";
        let stack_str = StackStr::try_from_str(s).unwrap_err();
    }

    #[test]
    #[cfg(target_pointer_width = "64")]
    fn small_boxed_str_clones_to_stack() {
        let s = "0123456789abcde".to_owned();
        let smort: SmortStr = s.into();
        let smort_clone = smort.clone();

        if let SmortStr::Stacked(..) = smort_clone {} else {
            panic!("Expected a stacked variant!");
        }
    }
}

