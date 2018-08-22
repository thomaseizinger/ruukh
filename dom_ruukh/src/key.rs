use std::borrow::Cow;

/// Keys to identify the VNode in Virtual DOM.
/// Only the basic types are supported.
#[derive(Debug, Eq, PartialEq)]
pub enum Key {
    /// An `i64` key
    I64(i64),
    /// An `u64` key
    U64(u64),
    /// A `String` key
    String(String),
}

macro_rules! convert {
    ([$($f:ty),*] to I64) => {
        $(
            impl From<$f> for Key {
                fn from(num: $f) -> Key {
                    Key::I64(num as i64)
                }
            }
        )*
    };
    ([$($f:ty),*] to U64) => {
        $(
            impl From<$f> for Key {
                fn from(num: $f) -> Key {
                    Key::U64(num as u64)
                }
            }
        )*
    };
}

convert!([i8, i16, i32, i64] to I64);
convert!([u8, u16, u32, u64] to U64);

impl<'a> From<&'a str> for Key {
    fn from(string: &'a str) -> Key {
        Key::String(string.to_string())
    }
}

impl<'a> From<Cow<'a, str>> for Key {
    fn from(string: Cow<'a, str>) -> Key {
        Key::String(string.to_string())
    }
}

impl From<String> for Key {
    fn from(string: String) -> Key {
        Key::String(string)
    }
}
