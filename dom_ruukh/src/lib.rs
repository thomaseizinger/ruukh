//! The Virtual DOM library which backs the `ruukh` frontend framework.
#![deny(missing_docs)]

extern crate wasm_bindgen;
#[cfg(test)]
extern crate wasm_bindgen_test;

use dom::DOMPatch;
use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;
use vcomponent::VComponent;
use velement::VElement;
use vlist::VList;
use vtext::VText;
use wasm_bindgen::prelude::JsValue;
#[cfg(test)]
use wasm_bindgen_test::*;
use web_api::*;

#[cfg(test)]
wasm_bindgen_test_configure!(run_in_browser);

mod component;
mod dom;
pub mod vcomponent;
pub mod velement;
pub mod vlist;
pub mod vtext;
pub mod web_api;

#[allow(missing_docs)]
pub mod prelude {
    pub use component::{Component, ComponentStatus, Lifecycle, Render};
    pub use vcomponent::VComponent;
    pub use velement::{Attribute, VElement};
    pub use vlist::VList;
    pub use vtext::VText;
    pub use {Key, KeyedVNodes, VNode};
}

/// A keyed virtual node in a virtual DOM tree.
#[derive(Debug)]
pub struct KeyedVNodes {
    /// A uniquely identifying key in the list of vnodes.
    key: Option<Key>,
    /// A virtual node
    vnode: VNode,
}

impl KeyedVNodes {
    /// Constructor for a keyed VNode
    pub fn keyed<K: Into<Key>, T: Into<VNode>>(key: K, vnode: T) -> KeyedVNodes {
        KeyedVNodes {
            key: Some(key.into()),
            vnode: vnode.into(),
        }
    }

    /// Constructor for an unkeyed VNode
    pub fn unkeyed<T: Into<VNode>>(vnode: T) -> KeyedVNodes {
        KeyedVNodes {
            key: None,
            vnode: vnode.into(),
        }
    }
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

type Shared<T> = Rc<RefCell<T>>;

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

impl DOMPatch for KeyedVNodes {
    type Node = Node;

    fn render_walk(&mut self, parent: Self::Node, next: Option<Self::Node>) -> Result<(), JsValue> {
        self.vnode.render_walk(parent, next)
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: Self::Node,
        next: Option<Self::Node>,
    ) -> Result<(), JsValue> {
        if let Some(old) = old {
            if self.key == old.key {
                self.vnode.patch(Some(old.vnode), parent, next)
            } else {
                old.vnode.remove(parent.clone())?;
                self.vnode.patch(None, parent, next)
            }
        } else {
            self.vnode.patch(None, parent, next)
        }
    }

    fn remove(self, parent: Self::Node) -> Result<(), JsValue> {
        self.vnode.remove(parent)
    }

    fn node(&self) -> Option<Node> {
        self.vnode.node()
    }
}

macro_rules! patch {
    ($variant:ident => $this:ident, $old:ident, $parent:ident, $next:ident) => {
        match $old {
            Some(VNode::$variant(old)) => $this.patch(Some(old), $parent, $next),
            Some(old) => {
                old.remove($parent.clone())?;
                $this.patch(None, $parent, $next)
            }
            None => $this.patch(None, $parent, $next),
        }
    };
}

impl DOMPatch for VNode {
    type Node = Node;

    fn render_walk(&mut self, parent: Self::Node, next: Option<Self::Node>) -> Result<(), JsValue> {
        match self {
            VNode::Element(ref mut el) => el.render_walk(parent, next),
            VNode::List(ref mut list) => list.render_walk(parent, next),
            VNode::Component(ref mut comp) => comp.render_walk(parent, next),
            VNode::Text(_) => Ok(()),
        }
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: Self::Node,
        next: Option<Self::Node>,
    ) -> Result<(), JsValue> {
        match self {
            VNode::Element(ref mut new_el) => patch!(Element => new_el, old, parent, next),
            VNode::Text(ref mut new_txt) => patch!(Text => new_txt, old, parent, next),
            VNode::List(ref mut new_li) => patch!(List => new_li, old, parent, next),
            VNode::Component(ref mut new_comp) => patch!(Component => new_comp, old, parent, next),
        }
    }

    fn remove(self, parent: Self::Node) -> Result<(), JsValue> {
        match self {
            VNode::Text(txt) => txt.remove(parent),
            VNode::Element(el) => el.remove(parent),
            VNode::List(li) => li.remove(parent),
            VNode::Component(comp) => comp.remove(parent),
        }
    }

    fn node(&self) -> Option<Node> {
        match self {
            VNode::Text(txt) => txt.node(),
            VNode::Element(el) => el.node(),
            VNode::List(li) => li.node(),
            VNode::Component(comp) => comp.node(),
        }
    }
}

/// Keys to identify a VNode in VDOM.
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

#[cfg(test)]
mod test {
    use super::KeyedVNodes;
    use vtext::VText;

    #[test]
    fn should_display_vnode() {
        let node = KeyedVNodes::unkeyed(VText::text("Hello World!"));
        assert_eq!(format!("{}", node), "Hello World!");
    }
}
