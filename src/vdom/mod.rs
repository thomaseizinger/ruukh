//! The Virtual DOM library which backs the `ruukh` frontend framework.

use component::Render;
use dom::{DOMInfo, DOMPatch, DOMRemove};
use std::borrow::Cow;
use std::fmt::{self, Display, Formatter};
use vdom::vcomponent::VComponent;
use vdom::velement::VElement;
use vdom::vlist::VList;
use vdom::vtext::VText;
use wasm_bindgen::prelude::JsValue;
use web_api::*;
use Shared;

pub mod vcomponent;
pub mod velement;
pub mod vlist;
pub mod vtext;

/// A keyed virtual node in a virtual DOM tree.
pub struct KeyedVNodes<RCTX: Render> {
    /// A uniquely identifying key in the list of vnodes.
    key: Option<Key>,
    /// A virtual node
    vnode: VNode<RCTX>,
}

impl<RCTX: Render> KeyedVNodes<RCTX> {
    /// Constructor for a keyed VNode
    pub fn keyed<K: Into<Key>, T: Into<VNode<RCTX>>>(key: K, vnode: T) -> KeyedVNodes<RCTX> {
        KeyedVNodes {
            key: Some(key.into()),
            vnode: vnode.into(),
        }
    }

    /// Constructor for an unkeyed VNode
    pub fn unkeyed<T: Into<VNode<RCTX>>>(vnode: T) -> KeyedVNodes<RCTX> {
        KeyedVNodes {
            key: None,
            vnode: vnode.into(),
        }
    }
}

/// A virtual node in a virtual DOM tree.
pub enum VNode<RCTX: Render> {
    /// A text vnode
    Text(VText<RCTX>),
    /// An element vnode
    Element(VElement<RCTX>),
    /// A list vnode
    List(VList<RCTX>),
    /// A component vnode
    Component(VComponent<RCTX>),
}

impl<RCTX: Render> Display for KeyedVNodes<RCTX> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.vnode)
    }
}

impl<RCTX: Render> Display for VNode<RCTX> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            VNode::Text(inner) => write!(f, "{}", inner),
            VNode::Element(inner) => write!(f, "{}", inner),
            VNode::List(inner) => write!(f, "{}", inner),
            VNode::Component(inner) => write!(f, "{}", inner),
        }
    }
}

impl<RCTX: Render> DOMPatch<RCTX> for KeyedVNodes<RCTX> {
    type Node = Node;

    fn render_walk(
        &mut self,
        parent: Self::Node,
        next: Option<Self::Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue> {
        self.vnode.render_walk(parent, next, render_ctx)
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: Self::Node,
        next: Option<Self::Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue> {
        if let Some(old) = old {
            if self.key == old.key {
                self.vnode.patch(Some(old.vnode), parent, next, render_ctx)
            } else {
                old.vnode.remove(parent.clone())?;
                self.vnode.patch(None, parent, next, render_ctx)
            }
        } else {
            self.vnode.patch(None, parent, next, render_ctx)
        }
    }
}

impl<RCTX: Render> DOMRemove for KeyedVNodes<RCTX> {
    type Node = Node;

    fn remove(self, parent: Self::Node) -> Result<(), JsValue> {
        self.vnode.remove(parent)
    }
}

impl<RCTX: Render> DOMInfo for KeyedVNodes<RCTX> {
    fn node(&self) -> Option<Node> {
        self.vnode.node()
    }
}

macro_rules! patch {
    ($variant:ident => $this:ident, $old:ident, $parent:ident, $next:ident, $render_ctx:ident) => {
        match $old {
            Some(VNode::$variant(old)) => $this.patch(Some(old), $parent, $next, $render_ctx),
            Some(old) => {
                old.remove($parent.clone())?;
                $this.patch(None, $parent, $next, $render_ctx)
            }
            None => $this.patch(None, $parent, $next, $render_ctx),
        }
    };
}

impl<RCTX: Render> DOMPatch<RCTX> for VNode<RCTX> {
    type Node = Node;

    fn render_walk(
        &mut self,
        parent: Self::Node,
        next: Option<Self::Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue> {
        match self {
            VNode::Element(ref mut el) => el.render_walk(parent, next, render_ctx),
            VNode::List(ref mut list) => list.render_walk(parent, next, render_ctx),
            VNode::Component(ref mut comp) => comp.render_walk(parent, next, render_ctx),
            VNode::Text(_) => Ok(()),
        }
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: Self::Node,
        next: Option<Self::Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue> {
        match self {
            VNode::Element(ref mut new_el) => {
                patch!(Element => new_el, old, parent, next, render_ctx)
            }
            VNode::Text(ref mut new_txt) => patch!(Text => new_txt, old, parent, next, render_ctx),
            VNode::List(ref mut new_li) => patch!(List => new_li, old, parent, next, render_ctx),
            VNode::Component(ref mut new_comp) => {
                patch!(Component => new_comp, old, parent, next, render_ctx)
            }
        }
    }
}

impl<RCTX: Render> DOMRemove for VNode<RCTX> {
    type Node = Node;

    fn remove(self, parent: Self::Node) -> Result<(), JsValue> {
        match self {
            VNode::Text(txt) => txt.remove(parent),
            VNode::Element(el) => el.remove(parent),
            VNode::List(li) => li.remove(parent),
            VNode::Component(comp) => comp.remove(parent),
        }
    }
}

impl<RCTX: Render> DOMInfo for VNode<RCTX> {
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
#[derive(Eq, PartialEq)]
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
    use vdom::vtext::VText;

    #[test]
    fn should_display_vnode() {
        let node = KeyedVNodes::<()>::unkeyed(VText::text("Hello World!"));
        assert_eq!(format!("{}", node), "Hello World!");
    }
}
