use crate::{component::Render, web_api::Node, MessageSender, Shared};
use wasm_bindgen::prelude::JsValue;

/// Trait to patch the DOM to reflect the VDOM structure.
pub(crate) trait DOMPatch<RCTX: Render>
where
    Self: Sized,
{
    /// The type of the Node the VDOM works upon.
    type Node;

    /// Walks through the VDOM till it has finished walking or an uninitialized
    /// or a dirty component is found and does the lifecycle bits, renders
    /// and invokes the patches on DOM.
    fn render_walk(
        &mut self,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<RCTX>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue>;

    /// Patches the DOM by diffing the VDOM `Self` with Older VDOM.
    fn patch(
        &mut self,
        old: Option<&mut Self>,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<RCTX>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue>;
}

/// Trait to reorder the VNode in the DOM. Why is this needed? It is required
/// when a VDOM changes its position in a VList. We need to reorder that DOM
/// node accordingly to reflect the current state.
pub(crate) trait DOMReorder {
    /// Reappends already existing Node in its correct place to reflect the
    /// current VDOM.
    fn reorder(&self, parent: &Node, next: Option<&Node>) -> Result<(), JsValue>;
}

/// Trait to remove the VNode from the DOM.
pub(crate) trait DOMRemove {
    type Node;

    /// Removes the VDOM from the actual DOM.
    fn remove(&self, parent: &Self::Node) -> Result<(), JsValue>;
}

/// Glean out the info from the DOM attached VDOM.
pub(crate) trait DOMInfo {
    /// Gets the node value of the DOM attached VDOM.
    fn node(&self) -> Option<&Node>;
}
