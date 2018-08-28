//! Representation of a list of nodes in VDOM.

use component::Render;
use dom::{DOMInfo, DOMPatch, DOMRemove};
use std::fmt::{self, Display, Formatter};
use wasm_bindgen::prelude::JsValue;
use web_api::*;
use Shared;
use vdom::{KeyedVNodes, VNode};

/// The representation of a list of vnodes in the vtree.
pub struct VList<RCTX: Render>(Vec<KeyedVNodes<RCTX>>);

impl<RCTX: Render> VList<RCTX> {
    /// Constructor to create a list of VNodes.
    pub fn new(list: Vec<KeyedVNodes<RCTX>>) -> VList<RCTX> {
        VList(list)
    }
}

impl<RCTX: Render> From<VList<RCTX>> for VNode<RCTX> {
    fn from(list: VList<RCTX>) -> VNode<RCTX> {
        VNode::List(list)
    }
}

impl<RCTX: Render> Display for VList<RCTX> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for vnode in self.0.iter() {
            write!(f, "{}", vnode)?;
        }
        Ok(())
    }
}

impl<RCTX: Render> DOMPatch<RCTX> for VList<RCTX> {
    type Node = Node;

    fn render_walk(
        &mut self,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue> {
        let mut next = next;
        for vnode in self.0.iter_mut().rev() {
            vnode.render_walk(parent, next, render_ctx.clone())?;
            next = vnode.node();
        }
        Ok(())
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue> {
        let mut next = next;
        if let Some(mut old) = old {
            let old_len = old.0.len();
            for (index, vnode) in self.0.iter_mut().enumerate().rev() {
                let old = if index < old_len {
                    Some(old.0.remove(index))
                } else {
                    None
                };
                vnode.patch(old, parent, next, render_ctx.clone())?;
                next = vnode.node();
            }
            old.remove(parent)?;
        } else {
            for vnode in self.0.iter_mut().rev() {
                vnode.patch(None, parent, next, render_ctx.clone())?;
                next = vnode.node();
            }
        }
        Ok(())
    }
}

impl<RCTX: Render> DOMRemove for VList<RCTX> {
    type Node = Node;

    fn remove(self, parent: &Self::Node) -> Result<(), JsValue> {
        for vnode in self.0 {
            vnode.remove(parent)?;
        }
        Ok(())
    }
}

impl<RCTX: Render> DOMInfo for VList<RCTX> {
    fn node(&self) -> Option<&Node> {
        self.0.get(0).and_then(|first| first.node())
    }
}

#[cfg(test)]
mod test {
    use super::VList;
    use vdom::velement::VElement;
    use vdom::vtext::VText;
    use vdom::KeyedVNodes;

    #[test]
    fn should_display_a_list_of_vnodes() {
        let list = VList::<()>::new(vec![
            KeyedVNodes::unkeyed(VText::text("First of the node")),
            KeyedVNodes::unkeyed(VElement::childless("input", vec![], vec![])),
        ]);
        assert_eq!(format!("{}", list), "First of the node<input>");
    }
}

#[cfg(test)]
pub mod wasm_test {
    use dom::*;
    use prelude::*;
    use wasm_bindgen_test::*;
    use web_api::*;
    use component::root_render_ctx;

    fn container() -> Element {
        html_document.create_element("div").unwrap()
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_list_of_vnodes() {
        let mut list = VList::new(vec![
            KeyedVNodes::unkeyed(VText::text("Hello World!")),
            KeyedVNodes::unkeyed(VElement::childless("div", vec![], vec![])),
        ]);
        let div = container();
        list.patch(None, div.as_ref(), None, root_render_ctx())
            .expect("To patch div");

        assert_eq!(div.inner_html(), "Hello World!<div></div>");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_updated_list() {
        let mut list = VList::new(vec![
            KeyedVNodes::unkeyed(VText::text("Hello World!")),
            KeyedVNodes::unkeyed(VElement::childless("div", vec![], vec![])),
        ]);
        let div = container();
        list.patch(None, div.as_ref(), None, root_render_ctx())
            .expect("To patch div");

        assert_eq!(div.inner_html(), "Hello World!<div></div>");

        let mut new_list = VList::new(vec![
            KeyedVNodes::unkeyed(VElement::childless("div", vec![], vec![])),
            KeyedVNodes::unkeyed(VText::text("Hello World!")),
            KeyedVNodes::unkeyed(VText::text("How are you?")),
        ]);
        new_list
            .patch(Some(list), div.as_ref(), None, root_render_ctx())
            .expect("To patch div");

        assert_eq!(div.inner_html(), "<div></div>Hello World!How are you?");
    }
}
