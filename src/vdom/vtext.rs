//! Representation of text/comment in virtual dom tree.

use crate::{component::Render, dom::DOMPatch, vdom::VNode, MessageSender, Shared};
use std::{
    fmt::{self, Display, Formatter},
    marker::PhantomData,
};
use wasm_bindgen::prelude::JsValue;
use web_sys::{window, Node};

/// The representation of text/comment in virtual dom tree.
pub struct VText<RCTX> {
    /// The content of a text string
    content: String,
    /// Whether the content is a comment
    is_comment: bool,
    /// Text/Comment reference to the DOM
    node: Option<Node>,
    /// Render context
    _phantom: PhantomData<RCTX>,
}

impl<RCTX> VText<RCTX> {
    /// Create a textual VText.
    pub fn text(content: impl Into<String>) -> VText<RCTX> {
        VText {
            content: content.into(),
            is_comment: false,
            node: None,
            _phantom: PhantomData,
        }
    }

    /// Create a comment VText.
    pub fn comment(content: impl Into<String>) -> VText<RCTX> {
        VText {
            content: content.into(),
            is_comment: true,
            node: None,
            _phantom: PhantomData,
        }
    }
}

impl<RCTX> VText<RCTX> {
    fn patch_new(&mut self, parent: &Node, next: Option<&Node>) -> Result<(), JsValue> {
        let node: Node = if self.is_comment {
            window()
                .unwrap()
                .document()
                .unwrap()
                .create_comment(&self.content)
                .into()
        } else {
            window()
                .unwrap()
                .document()
                .unwrap()
                .create_text_node(&self.content)
                .into()
        };
        parent.insert_before(&node, next)?;
        self.node = Some(node);
        Ok(())
    }
}

impl<RCTX> From<VText<RCTX>> for VNode<RCTX> {
    fn from(text: VText<RCTX>) -> VNode<RCTX> {
        VNode::Text(text)
    }
}

impl<RCTX> Display for VText<RCTX> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_comment {
            write!(f, "<!--{}-->", self.content)
        } else {
            write!(f, "{}", self.content)
        }
    }
}

impl<RCTX: Render> DOMPatch for VText<RCTX> {
    type RenderContext = RCTX;
    type Node = Node;

    fn render_walk(
        &mut self,
        _: &Node,
        _: Option<&Node>,
        _: Shared<Self::RenderContext>,
        _: MessageSender,
    ) -> Result<(), JsValue> {
        unreachable!("There is nothing to render in a VText");
    }

    fn patch(
        &mut self,
        old: Option<&mut Self>,
        parent: &Node,
        next: Option<&Node>,
        _: Shared<Self::RenderContext>,
        _: MessageSender,
    ) -> Result<(), JsValue> {
        if let Some(old) = old {
            if self.is_comment == old.is_comment {
                let old_node = old
                    .node
                    .as_ref()
                    .expect("The old node is expected to be attached to the DOM");
                if self.content != old.content {
                    old_node.set_text_content(Some(&self.content));
                }
                self.node = Some(old_node.clone());
                Ok(())
            } else {
                old.remove(parent)?;
                self.patch_new(parent, next)
            }
        } else {
            self.patch_new(parent, next)
        }
    }

    fn reorder(&self, parent: &Node, next: Option<&Node>) -> Result<(), JsValue> {
        let node = self.node.as_ref().unwrap();
        parent.insert_before(node, next)?;
        Ok(())
    }

    fn remove(&self, parent: &Node) -> Result<(), JsValue> {
        parent.remove_child(
            self.node
                .as_ref()
                .expect("The old node is expected to be attached to the DOM"),
        )?;
        Ok(())
    }

    fn node(&self) -> Option<&Node> {
        self.node.as_ref()
    }
}

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::{component::root_render_ctx, vdom::test::container};
    use wasm_bindgen_test::*;

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

    #[wasm_bindgen_test]
    fn should_patch_container_with_new_text() {
        let mut vtext = VText::text("Hello World! It is nice to render.");
        let div = container();
        vtext
            .patch(
                None,
                div.as_ref(),
                None,
                root_render_ctx(),
                crate::message_sender(),
            ).expect("To patch the div");

        assert_eq!(div.inner_html(), "Hello World! It is nice to render.");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_text_update() {
        let mut vtext = VText::text("Hello World! It is nice to render.");
        let div = container();
        vtext
            .patch(
                None,
                div.as_ref(),
                None,
                root_render_ctx(),
                crate::message_sender(),
            ).expect("To patch div");

        assert_eq!(div.inner_html(), "Hello World! It is nice to render.");

        let mut updated = VText::text("How you doing?");
        updated
            .patch(
                Some(&mut vtext),
                div.as_ref(),
                None,
                root_render_ctx(),
                crate::message_sender(),
            ).expect("To patch div");

        assert_eq!(div.inner_html(), "How you doing?");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_new_comment() {
        let mut comment = VText::comment("This is a comment");
        let div = container();
        comment
            .patch(
                None,
                div.as_ref(),
                None,
                root_render_ctx(),
                crate::message_sender(),
            ).expect("To patch div");

        assert_eq!(div.inner_html(), "<!--This is a comment-->");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_new_text_on_comment() {
        let mut comment = VText::comment("This is a comment");
        let div = container();
        comment
            .patch(
                None,
                div.as_ref(),
                None,
                root_render_ctx(),
                crate::message_sender(),
            ).expect("To patch div");

        assert_eq!(div.inner_html(), "<!--This is a comment-->");

        let mut text = VText::text("This is a text");
        text.patch(
            Some(&mut comment),
            div.as_ref(),
            None,
            root_render_ctx(),
            crate::message_sender(),
        ).expect("To patch div");

        assert_eq!(div.inner_html(), "This is a text");
    }

}
