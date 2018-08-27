use component::RenderableComponent;
use wasm_bindgen::prelude::JsValue;
use web_api::Node;
use Shared;

/// Trait to patch the DOM to reflect the VDOM structure.
pub trait DOMPatch<RCTX: RenderableComponent>
where
    Self: Sized,
{
    /// The type of the Node the VDOM works upon.
    type Node;

    /// Walks through the VDOM till an uninitialized or a dirty Component is found and renders it recursively.
    fn render_walk(
        &mut self,
        parent: Self::Node,
        next: Option<Self::Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue>;

    /// Patch the DOM by diffing the VDOM `Self` with Older VDOM.
    fn patch(
        &mut self,
        old: Option<Self>,
        parent: Self::Node,
        next: Option<Self::Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue>;
}

/// Trait to remove the VNode from the DOM.
pub trait DOMRemove {
    type Node;

    /// Remove the VDOM from the actual DOM.
    fn remove(self, parent: Self::Node) -> Result<(), JsValue>;
}

/// Glean out the info from the DOM attached VDOM.
pub trait DOMInfo {
    /// The node value of the DOM attached VDOM.
    fn node(&self) -> Option<Node>;
}
