//! The Virtual DOM library which backs the `ruukh` frontend framework.

use component::Render;
use dom::{DOMInfo, DOMPatch, DOMRemove, DOMReorder};
use std::borrow::Cow;
use std::fmt::{self, Display, Formatter};
use vdom::vcomponent::VComponent;
use vdom::velement::VElement;
use vdom::vlist::VList;
use vdom::vtext::VText;
use wasm_bindgen::prelude::JsValue;
use web_api::*;
use MessageSender;
use Shared;

pub mod vcomponent;
pub mod velement;
pub mod vlist;
pub mod vtext;
mod conversions;

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
    /// The empty variant
    None
}

impl<RCTX: Render> VNode<RCTX> {
    /// Construct a new VNode from one of its constituent Node types.
    pub fn new<T: Into<VNode<RCTX>>>(node: T) -> VNode<RCTX> {
        node.into()
    }

    /// Whether the VNode is of `None` variant. 
    pub fn is_none(&self) -> bool {
        match self {
            VNode::None => true,
            _ => false
        }
    }
}

impl<RCTX: Render> Display for VNode<RCTX> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            VNode::Text(inner) => write!(f, "{}", inner),
            VNode::Element(inner) => write!(f, "{}", inner),
            VNode::List(inner) => write!(f, "{}", inner),
            VNode::Component(inner) => write!(f, "{}", inner),
            VNode::None => Ok(())
        }
    }
}

macro_rules! patch {
    (
        $variant:ident => $this:ident, 
        $old:ident, 
        $parent:ident, 
        $next:ident, 
        $render_ctx:ident, 
        $rx_sender:ident
    ) => {
        match $old {
            Some(VNode::$variant(old)) => {
                $this.patch(Some(old), $parent, $next, $render_ctx, $rx_sender)
            }
            Some(old) => {
                old.remove($parent)?;
                $this.patch(None, $parent, $next, $render_ctx, $rx_sender)
            }
            None => $this.patch(None, $parent, $next, $render_ctx, $rx_sender),
        }
    };
}

impl<RCTX: Render> DOMPatch<RCTX> for VNode<RCTX> {
    type Node = Node;

    fn render_walk(
        &mut self,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<RCTX>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        match self {
            VNode::Element(ref mut el) => el.render_walk(parent, next, render_ctx, rx_sender),
            VNode::List(ref mut list) => list.render_walk(parent, next, render_ctx, rx_sender),
            VNode::Component(ref mut comp) => comp.render_walk(parent, next, render_ctx, rx_sender),
            VNode::Text(_) => Ok(()),
            VNode::None => Ok(())
        }
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<RCTX>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        match self {
            VNode::Element(ref mut new_el) => {
                patch!(Element => new_el, old, parent, next, render_ctx, rx_sender)
            }
            VNode::Text(ref mut new_txt) => {
                patch!(Text => new_txt, old, parent, next, render_ctx, rx_sender)
            }
            VNode::List(ref mut new_li) => {
                patch!(List => new_li, old, parent, next, render_ctx, rx_sender)
            }
            VNode::Component(ref mut new_comp) => {
                patch!(Component => new_comp, old, parent, next, render_ctx, rx_sender)
            }
            VNode::None => {
                if let Some(old) = old {
                    old.remove(parent)?;
                }
                Ok(())
            }
        }
    }
}

impl<RCTX: Render> DOMReorder for VNode<RCTX> {
    fn reorder(&self, parent: &Node, next: Option<&Node>) -> Result<(), JsValue> {
        match self {
            VNode::Text(txt) => txt.reorder(parent, next),
            VNode::Element(el) => el.reorder(parent, next),
            VNode::List(li) => li.reorder(parent, next),
            VNode::Component(comp) => comp.reorder(parent, next),
            VNode::None => Ok(())
        }
    }
}

impl<RCTX: Render> DOMRemove for VNode<RCTX> {
    type Node = Node;

    fn remove(self, parent: &Self::Node) -> Result<(), JsValue> {
        match self {
            VNode::Text(txt) => txt.remove(parent),
            VNode::Element(el) => el.remove(parent),
            VNode::List(li) => li.remove(parent),
            VNode::Component(comp) => comp.remove(parent),
            VNode::None => Ok(())
        }
    }
}

impl<RCTX: Render> DOMInfo for VNode<RCTX> {
    fn node(&self) -> Option<&Node> {
        match self {
            VNode::Text(txt) => txt.node(),
            VNode::Element(el) => el.node(),
            VNode::List(li) => li.node(),
            VNode::Component(comp) => comp.node(),
            VNode::None => None
        }
    }
}

/// Keys to identify a VNode in VDOM.
/// Only the basic types are supported.
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Key {
    /// An `i64` key
    I64(i64),
    /// An `u64` key
    U64(u64),
    /// A `String` key
    String(String),
}

impl Key {
    /// Construct a new Key by passing any of the `Key` types.
    pub fn new<T: Into<Key>>(val: T) -> Key {
        val.into()
    }
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
    use vdom::vtext::VText;
    use super::VNode;

    #[test]
    fn should_display_vnode() {
        let node = VNode::<()>::new(VText::text("Hello World!"));
        assert_eq!(format!("{}", node), "Hello World!");
    }
}
