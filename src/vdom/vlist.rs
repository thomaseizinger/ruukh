//! Representation of a list of nodes in VDOM.

use component::Render;
use dom::{DOMInfo, DOMPatch, DOMRemove, DOMReorder};
use fnv::FnvBuildHasher;
use indexmap::IndexMap;
use std::collections::HashSet;
use std::fmt::{self, Display, Formatter};
use vdom::Key;
use vdom::VNode;
use wasm_bindgen::prelude::JsValue;
use web_api::*;
use MessageSender;
use Shared;

/// The representation of a list of vnodes in the vtree.
pub struct VList<RCTX: Render>(IndexMap<Key, VNode<RCTX>, FnvBuildHasher>);

impl<RCTX: Render> VList<RCTX> {
    /// Constructor to create a list of VNodes.
    pub fn new<T: Into<VList<RCTX>>>(list: T) -> VList<RCTX> {
        list.into()
    }
}

impl<RCTX: Render> From<VList<RCTX>> for VNode<RCTX> {
    fn from(list: VList<RCTX>) -> VNode<RCTX> {
        VNode::List(list)
    }
}

impl<RCTX: Render> From<Vec<VNode<RCTX>>> for VList<RCTX> {
    fn from(children: Vec<VNode<RCTX>>) -> Self {
        VList(
            children
                .into_iter()
                .enumerate()
                .map(|(k, v)| (Key::U64(k as u64), v))
                .collect(),
        )
    }
}

impl<RCTX: Render> From<IndexMap<Key, VNode<RCTX>, FnvBuildHasher>> for VList<RCTX> {
    fn from(map: IndexMap<Key, VNode<RCTX>, FnvBuildHasher>) -> Self {
        VList(map)
    }
}

impl<RCTX: Render> Display for VList<RCTX> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for (_, vnode) in self.0.iter() {
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
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        let mut next = next;
        for (_, vnode) in self.0.iter_mut().rev() {
            vnode.render_walk(parent, next, render_ctx.clone(), rx_sender.clone())?;
            next = vnode.node();
        }
        Ok(())
    }

    fn patch(
        &mut self,
        old: Option<&mut Self>,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<RCTX>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        let mut next = next;
        if let Some(old) = old {
            // Collect the keys of alive nodes from old vlist.
            let mut alive_keys = HashSet::with_hasher(FnvBuildHasher::default());

            for (index, (key, vnode)) in self.0.iter_mut().enumerate().rev() {
                // Patch the old vnode if found.
                if let Some((old_index, _, old)) = old.0.get_full_mut(key) {
                    vnode.patch(
                        Some(old),
                        parent,
                        next,
                        render_ctx.clone(),
                        rx_sender.clone(),
                    )?;

                    // If the order changed, update it in the DOM.
                    if index != old_index {
                        vnode.reorder(parent, next)?;
                    }

                    alive_keys.insert(key);
                } else {
                    vnode.patch(None, parent, next, render_ctx.clone(), rx_sender.clone())?;
                }

                next = vnode.node().or(next);
            }

            // Remove all the remaining ones.
            for (key, vnode) in old.0.iter() {
                if !alive_keys.contains(key) {
                    vnode.remove(parent)?;
                }
            }
        } else {
            for (_, vnode) in self.0.iter_mut().rev() {
                vnode.patch(None, parent, next, render_ctx.clone(), rx_sender.clone())?;
                next = vnode.node().or(next);
            }
        }
        Ok(())
    }
}

impl<RCTX: Render> DOMReorder for VList<RCTX> {
    fn reorder(&self, parent: &Node, next: Option<&Node>) -> Result<(), JsValue> {
        for (_, node) in self.0.iter() {
            node.reorder(parent, next)?;
        }
        Ok(())
    }
}

impl<RCTX: Render> DOMRemove for VList<RCTX> {
    type Node = Node;

    fn remove(&self, parent: &Self::Node) -> Result<(), JsValue> {
        for (_, vnode) in self.0.iter() {
            vnode.remove(parent)?;
        }
        Ok(())
    }
}

impl<RCTX: Render> DOMInfo for VList<RCTX> {
    fn node(&self) -> Option<&Node> {
        self.0.get_index(0).and_then(|(_, first)| first.node())
    }
}

#[cfg(test)]
mod test {
    use super::VList;
    use vdom::velement::VElement;
    use vdom::vtext::VText;
    use vdom::VNode;

    #[test]
    fn should_display_a_list_of_vnodes() {
        let list = VList::<()>::new(vec![
            VNode::new(VText::text("First of the node")),
            VNode::new(VElement::childless("input", vec![], vec![])),
        ]);
        assert_eq!(format!("{}", list), "First of the node<input>");
    }
}

#[cfg(test)]
pub mod wasm_test {
    use component::root_render_ctx;
    use dom::*;
    use prelude::*;
    use wasm_bindgen_test::*;
    use web_api::*;

    fn container() -> Element {
        html_document.create_element("div").unwrap()
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_list_of_vnodes() {
        let mut list = VList::new(vec![
            VNode::new(VText::text("Hello World!")),
            VNode::new(VElement::childless("div", vec![], vec![])),
        ]);
        let div = container();
        list.patch(
            None,
            div.as_ref(),
            None,
            root_render_ctx(),
            ::message_sender(),
        ).expect("To patch div");

        assert_eq!(div.inner_html(), "Hello World!<div></div>");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_updated_list() {
        let mut list = VList::new(vec![
            VNode::new(VText::text("Hello World!")),
            VNode::new(VElement::childless("div", vec![], vec![])),
        ]);
        let div = container();
        list.patch(
            None,
            div.as_ref(),
            None,
            root_render_ctx(),
            ::message_sender(),
        ).expect("To patch div");

        assert_eq!(div.inner_html(), "Hello World!<div></div>");

        let mut new_list = VList::new(vec![
            VNode::new(VElement::childless("div", vec![], vec![])),
            VNode::new(VText::text("Hello World!")),
            VNode::new(VText::text("How are you?")),
        ]);
        new_list
            .patch(
                Some(list),
                div.as_ref(),
                None,
                root_render_ctx(),
                ::message_sender(),
            ).expect("To patch div");

        assert_eq!(div.inner_html(), "<div></div>Hello World!How are you?");
    }
}
