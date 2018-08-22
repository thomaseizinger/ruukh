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
    pub vnode: VNode,
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

impl Display for KeyedVNodes {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.vnode)
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
            vnode: VText {
                content: "Hello World!".to_string(),
                is_comment: false,
            }.into(),
        };
        assert_eq!(format!("{}", node), "Hello World!");
    }
}
