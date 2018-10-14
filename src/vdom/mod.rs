//! The Virtual DOM library which backs the `ruukh` frontend framework.

use crate::{
    component::Render,
    dom::DOMPatch,
    vdom::{
        vcomponent::VComponent,
        velement::VElement,
        vlist::VList,
        vtext::VText
    },
    MessageSender,
    Shared
};
use std::{
    borrow::Cow, 
    fmt::{self, Display, Formatter}
};
use wasm_bindgen::prelude::JsValue;
use web_sys::Node;

pub mod vcomponent;
pub mod velement;
pub mod vlist;
pub mod vtext;
mod conversions;

/// A virtual node in a virtual DOM tree.
pub enum VNode<RCTX> {
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

impl<RCTX> VNode<RCTX> {
    /// Whether the VNode is of `None` variant. 
    pub fn is_none(&self) -> bool {
        match self {
            VNode::None => true,
            _ => false
        }
    }
}

impl<RCTX: Render> Display for VNode<RCTX> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
                // If the variant is same patch it.
                $this.patch(Some(old), $parent, $next, $render_ctx, $rx_sender)
            }
            Some(old) => {
                // If it is a different variant, remove the old one.
                old.remove($parent)?;
                $this.patch(None, $parent, $next, $render_ctx, $rx_sender)
            }
            None => $this.patch(None, $parent, $next, $render_ctx, $rx_sender),
        }
    };
}

impl<RCTX: Render> DOMPatch for VNode<RCTX> {
    type RenderContext = RCTX;
    type Node = Node;

    fn render_walk(
        &mut self,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<Self::RenderContext>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        match self {
            VNode::Element(ref mut el) => el.render_walk(parent, next, render_ctx, rx_sender),
            VNode::List(ref mut list) => list.render_walk(parent, next, render_ctx, rx_sender),
            VNode::Component(ref mut comp) => comp.render_walk(parent, next, render_ctx, rx_sender),
            // There is nothing to walk on.
            VNode::Text(_) => Ok(()),
            VNode::None => Ok(())
        }
    }

    fn patch(
        &mut self,
        old: Option<&mut Self>,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<Self::RenderContext>,
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

    fn reorder(&self, parent: &Node, next: Option<&Node>) -> Result<(), JsValue> {
        match self {
            VNode::Text(txt) => txt.reorder(parent, next),
            VNode::Element(el) => el.reorder(parent, next),
            VNode::List(li) => li.reorder(parent, next),
            VNode::Component(comp) => comp.reorder(parent, next),
            VNode::None => Ok(())
        }
    }

    fn remove(&self, parent: &Self::Node) -> Result<(), JsValue> {
        match self {
            VNode::Text(txt) => txt.remove(parent),
            VNode::Element(el) => el.remove(parent),
            VNode::List(li) => li.remove(parent),
            VNode::Component(comp) => comp.remove(parent),
            VNode::None => Ok(())
        }
    }
    
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
/// 
/// Users don't need to explicitly use the `Key` type in html! macro. Any 
/// supported type is automatically converted to it.
/// 
/// Note:
/// WASM only supported 32-bit and 64-bit of the integers.
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Key {
    /// An `i32` key
    I32(i32),
    /// An `i64` key
    I64(i64),
    /// An `u32` key
    U32(u32),
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
    ([$($f:ty),*] to I32) => {
        $(
            impl From<$f> for Key {
                fn from(num: $f) -> Key {
                    Key::I32(i32::from(num))
                }
            }
        )*
    };
    ([$($f:ty),*] to U32) => {
        $(
            impl From<$f> for Key {
                fn from(num: $f) -> Key {
                    Key::U32(u32::from(num))
                }
            }
        )*
    };
}

convert!([i8, i16, i32] to I32);
convert!([u8, u16, u32] to U32);

impl From<i64> for Key {
    fn from(num: i64) -> Key {
        Key::I64(num)
    }
}

impl From<u64> for Key {
    fn from(num: u64) -> Key {
        Key::U64(num)
    }
}

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
    use crate::vdom::{vtext::VText, VNode};
    use web_sys::{window, Element};

    pub fn container() -> Element {
        window().unwrap().document().unwrap().create_element("div").unwrap()
    }

    #[test]
    fn should_display_vnode() {
        let node = VNode::<()>::from(VText::text("Hello World!"));
        assert_eq!(format!("{}", node), "Hello World!");
    }
}
