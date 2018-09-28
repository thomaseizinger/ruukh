use crate::{component::Render, MessageSender, Shared};
use wasm_bindgen::prelude::JsValue;
use web_sys::Node;

/// Trait to patch the DOM to reflect the VDOM structure.
pub(crate) trait DOMPatch
where
    Self: Sized,
{
    /// The render context of this Patch.
    type RenderContext: Render;
    /// The type of the Node the VDOM works upon.
    type Node;

    /// Walks through the VDOM till it has finished walking or an uninitialized
    /// or a dirty component is found and does the lifecycle bits, renders
    /// and invokes the patches on DOM.
    fn render_walk(
        &mut self,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<Self::RenderContext>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue>;

    /// Patches the DOM by diffing the VDOM `Self` with Older VDOM.
    fn patch(
        &mut self,
        old: Option<&mut Self>,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<Self::RenderContext>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue>;

    /// Reappends already existing Node in its correct place to reflect the
    /// current VDOM.
    fn reorder(&self, parent: &Self::Node, next: Option<&Self::Node>) -> Result<(), JsValue>;

    /// Removes the VDOM from the actual DOM.
    fn remove(&self, parent: &Self::Node) -> Result<(), JsValue>;

    /// Gets the node value of the DOM attached VDOM.
    fn node(&self) -> Option<&Node>;
}
