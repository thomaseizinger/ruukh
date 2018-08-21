//! The Virtual DOM library which backs the `ruukh` frontend framework.
#![deny(missing_docs)]

#[cfg(target_arch = "wasm32")]
extern crate wasm_bindgen;

use std::fmt::{self, Display, Formatter};
use vcomponent::VComponent;
use velement::VElement;
use vlist::VList;
use vtext::VText;

#[macro_export]
macro_rules! if_wasm {
    ($($i:item)*) => {
        $(
            #[cfg(target_arch = "wasm32")] $i
        )*
    };
}

mod component;
mod vcomponent;
mod velement;
mod vlist;
mod vtext;
#[cfg(target_arch = "wasm32")]
pub mod web_api;

/// A keyed virtual node in a virtual DOM tree.
#[derive(Debug)]
pub struct KeyedVNodes {
    /// A uniquely identifying key in the list of vnodes.
    pub key: Option<Key>,
    /// A virtual node
    pub node: VNode,
}

/// A virtual node in a virtual DOM tree.
#[derive(Debug)]
pub enum VNode {
    /// A text vnode
    Text(VText),
    /// An element vnode
    Element(VElement),
    /// A list vnode
    List(VList),
    /// A component vnode
    Component(VComponent),
}

/// Keys to identify the VNode in Virtual DOM.
/// Only the basic types are supported.
#[derive(Debug)]
pub enum Key {
    /// An `i8` key
    I8(i8),
    /// An `i16` key
    I16(i16),
    /// An `i32` key
    I32(i32),
    /// An `i64` key
    I64(i64),
    /// An `u8` key
    U8(u8),
    /// An `u16` key
    U16(u16),
    /// An `u32` key
    U32(u32),
    /// An `u64` key
    U64(u64),
    /// An `String` key
    String(String),
}

impl Display for KeyedVNodes {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.node)
    }
}

impl Display for VNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            VNode::Text(inner) => write!(f, "{}", inner),
            VNode::Element(inner) => write!(f, "{}", inner),
            VNode::List(inner) => write!(f, "{}", inner),
            VNode::Component(inner) => write!(f, "{}", inner),
        }
    }
}

#[cfg(test)]
mod test {
    use super::KeyedVNodes;
    use vtext::VText;

    #[test]
    fn should_display_vnode() {
        let node = KeyedVNodes {
            key: None,
            node: VText {
                content: "Hello World!".to_string(),
                is_comment: false,
            }.into(),
        };
        assert_eq!(format!("{}", node), "Hello World!");
    }
}
