//! The Virtual DOM library which backs the `ruukh` frontend framework.
#![deny(missing_docs)]

if_wasm! {
    extern crate wasm_bindgen;
    #[cfg(test)]
    extern crate wasm_bindgen_test;
}

use key::Key;
use std::fmt::{self, Display, Formatter};
use vcomponent::VComponent;
use velement::VElement;
use vlist::VList;
use vtext::VText;
if_wasm! {
    use web_api::*;
    use dom::DOMPatch;
    use wasm_bindgen::prelude::JsValue;
    #[cfg(test)]
    use wasm_bindgen_test::*;

    #[cfg(test)]
    wasm_bindgen_test_configure!(run_in_browser);
}

#[macro_export]
macro_rules! if_wasm {
    ($($i:item)*) => {
        $(
            #[cfg(target_arch = "wasm32")] $i
        )*
    };
}

mod component;
mod key;
#[allow(missing_docs)]
pub mod vcomponent;
#[allow(missing_docs)]
pub mod velement;
#[allow(missing_docs)]
pub mod vlist;
#[allow(missing_docs)]
pub mod vtext;
if_wasm! {
    mod dom;
    pub mod web_api;
}

#[allow(missing_docs)]
pub mod prelude {
    pub use component::{Component, ComponentStatus, Lifecycle, Render};
    pub use vcomponent::VComponent;
    pub use velement::{Attribute, Attributes, VElement};
    pub use vlist::VList;
    pub use vtext::VText;
    pub use {KeyedVNodes, VNode};
}

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

#[cfg(target_arch = "wasm32")]
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

#[cfg(target_arch = "wasm32")]
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

#[cfg(target_arch = "wasm32")]
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

#[cfg(test)]
#[cfg(not(target_arch = "wasm32"))]
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
