//! Representation of text/comment in virtual dom tree.

use component::Render;
use dom::{DOMInfo, DOMPatch, DOMRemove};
use std::fmt::{self, Display, Formatter};
use std::marker::PhantomData;
use vdom::VNode;
use wasm_bindgen::prelude::JsValue;
use web_api::*;
use Shared;

/// The representation of text/comment in virtual dom tree.
pub struct VText<RCTX: Render> {
    /// The content of a text string
    content: String,
    /// Whether the content is a comment
    is_comment: bool,
    /// Text/Comment reference to the DOM
    node: Option<Node>,
    /// Render context
    _phantom: PhantomData<RCTX>,
}

impl<RCTX: Render> VText<RCTX> {
    /// Constructor to create a textual VNode.
    pub fn text<T: Into<String>>(content: T) -> VText<RCTX> {
        VText {
            content: content.into(),
            is_comment: false,
            node: None,
            _phantom: PhantomData,
        }
    }

    /// Constructor to create a comment VNode.
    pub fn comment<T: Into<String>>(content: T) -> VText<RCTX> {
        VText {
            content: content.into(),
            is_comment: true,
            node: None,
            _phantom: PhantomData,
        }
    }
}

impl<RCTX: Render> From<VText<RCTX>> for VNode<RCTX> {
    fn from(text: VText<RCTX>) -> VNode<RCTX> {
        VNode::Text(text)
    }
}

impl<RCTX: Render> Display for VText<RCTX> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.is_comment {
            write!(f, "<!--{}-->", self.content)
        } else {
            write!(f, "{}", self.content)
        }
    }
}

impl<RCTX: Render> VText<RCTX> {
    fn patch_new(&mut self, parent: &Node, next: Option<&Node>) -> Result<(), JsValue> {
        let node: Node = if self.is_comment {
            html_document.create_comment(&self.content).into()
        } else {
            html_document.create_text_node(&self.content).into()
        };

        if let Some(next) = next {
            parent.insert_before(&node, next)?;
        } else {
            parent.append_child(&node)?;
        }
        self.node = Some(node);
        Ok(())
    }
}

impl<RCTX: Render> DOMPatch<RCTX> for VText<RCTX> {
    type Node = Node;

    fn render_walk(&mut self, _: &Node, _: Option<&Node>, _: Shared<RCTX>) -> Result<(), JsValue> {
        unreachable!("There is nothing to render in a VText");
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: &Node,
        next: Option<&Node>,
        _: Shared<RCTX>,
    ) -> Result<(), JsValue> {
        if let Some(old) = old {
            if self.is_comment == old.is_comment {
                let old_node = old
                    .node
                    .expect("The old node is expected to be attached to the DOM");
                if self.content != old.content {
                    old_node.set_text_content(&self.content);
                }
                self.node = Some(old_node);
                Ok(())
            } else {
                old.remove(parent)?;
                self.patch_new(parent, next)
            }
        } else {
            self.patch_new(parent, next)
        }
    }
}

impl<RCTX: Render> DOMRemove for VText<RCTX> {
    type Node = Node;

    fn remove(self, parent: &Node) -> Result<(), JsValue> {
        parent.remove_child(
            self.node
                .as_ref()
                .expect("The old node is expected to be attached to the DOM"),
        )?;
        Ok(())
    }
}

impl<RCTX: Render> DOMInfo for VText<RCTX> {
    fn node(&self) -> Option<&Node> {
        self.node.as_ref()
    }
}

#[cfg(test)]
mod test {
    use super::VText;

    #[test]
    fn should_display_text() {
        let text = VText::<()>::text("This is a very fine day!");
        assert_eq!(format!("{}", text), "This is a very fine day!");
    }

    #[test]
    fn should_display_comment() {
        let comment = VText::<()>::comment("Something to remind the hacky users.");
        assert_eq!(
            format!("{}", comment),
            "<!--Something to remind the hacky users.-->"
        );
    }
}

#[cfg(test)]

pub mod wasm_test {
    use component::root_render_ctx;
    use dom::*;
    use prelude::*;
    use wasm_bindgen_test::*;
    use web_api::*;

    #[wasm_bindgen_test]
    fn should_patch_container_with_new_text() {
        let mut vtext = VText::text("Hello World! It is nice to render.");
        let div = html_document.create_element("div").unwrap();
        vtext
            .patch(None, div.as_ref(), None, root_render_ctx())
            .expect("To patch the div");

        assert_eq!(div.inner_html(), "Hello World! It is nice to render.");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_text_update() {
        let mut vtext = VText::text("Hello World! It is nice to render.");
        let div = html_document.create_element("div").unwrap();
        vtext
            .patch(None, div.as_ref(), None, root_render_ctx())
            .expect("To patch div");

        assert_eq!(div.inner_html(), "Hello World! It is nice to render.");

        let mut updated = VText::text("How you doing?");
        updated
            .patch(Some(vtext), div.as_ref(), None, root_render_ctx())
            .expect("To patch div");

        assert_eq!(div.inner_html(), "How you doing?");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_new_comment() {
        let mut comment = VText::comment("This is a comment");
        let div = html_document.create_element("div").unwrap();
        comment
            .patch(None, div.as_ref(), None, root_render_ctx())
            .expect("To patch div");

        assert_eq!(div.inner_html(), "<!--This is a comment-->");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_new_text_on_comment() {
        let mut comment = VText::comment("This is a comment");
        let div = html_document.create_element("div").unwrap();
        comment
            .patch(None, div.as_ref(), None, root_render_ctx())
            .expect("To patch div");

        assert_eq!(div.inner_html(), "<!--This is a comment-->");

        let mut text = VText::text("This is a text");
        text.patch(Some(comment), div.as_ref(), None, root_render_ctx())
            .expect("To patch div");

        assert_eq!(div.inner_html(), "This is a text");
    }

}
