//! Representation of text/comment in virtual dom tree.

use std::fmt::{self, Display, Formatter};
use VNode;
if_wasm! {
    use web_api::*;
    use wasm_bindgen::prelude::JsValue;
}

/// The representation of text/comment in virtual dom tree.
#[derive(Debug)]
pub struct VText {
    /// The content of a text string
    content: String,
    /// Whether the content is a comment
    is_comment: bool,
    /// Text/Comment reference to the DOM
    #[cfg(target_arch = "wasm32")]
    node: Option<Node>,
}

impl VText {
    /// Constructor to create a textual VNode.
    pub fn text<T: Into<String>>(content: T) -> VText {
        VText {
            content: content.into(),
            is_comment: false,
            #[cfg(target_arch = "wasm32")]
            node: None,
        }
    }

    /// Constructor to create a comment VNode.
    pub fn comment<T: Into<String>>(content: T) -> VText {
        VText {
            content: content.into(),
            is_comment: true,
            #[cfg(target_arch = "wasm32")]
            node: None,
        }
    }
}

impl From<VText> for VNode {
    fn from(text: VText) -> VNode {
        VNode::Text(text)
    }
}

impl Display for VText {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.is_comment {
            write!(f, "<!-- {} -->", self.content)
        } else {
            write!(f, "{}", self.content)
        }
    }
}

#[cfg(target_arch = "wasm32")]
impl VText {
    fn patch_new(&mut self, parent: Node, next: Option<Node>) -> Result<(), JsValue> {
        let node: Node = if self.is_comment {
            html_document.create_comment(&self.content).into()
        } else {
            html_document.create_text_node(&self.content).into()
        };

        if let Some(next) = next {
            parent.insert_before(node.clone(), next)?;
        } else {
            parent.append_child(node.clone())?;
        }
        self.node = Some(node);
        Ok(())
    }
}

#[cfg(target_arch = "wasm32")]
impl ::dom::DOMPatch for VText {
    type Node = Node;

    fn render_walk(&mut self, _: Node, _: Option<Node>) -> Result<(), JsValue> {
        unreachable!("There is nothing to render in a VText");
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: Node,
        next: Option<Node>,
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
                old.remove(parent.clone())?;
                self.patch_new(parent, next)
            }
        } else {
            self.patch_new(parent, next)
        }
    }

    fn remove(self, parent: Node) -> Result<(), JsValue> {
        parent.remove_child(
            self.node
                .expect("The old node is expected to be attached to the DOM"),
        )?;
        Ok(())
    }

    fn node(&self) -> Option<Node> {
        self.node.as_ref().map(|n| n.clone())
    }
}

#[cfg(test)]
#[cfg(not(target_arch = "wasm32"))]
mod test {
    use super::VText;

    #[test]
    fn should_display_text() {
        let text = VText {
            content: "This is a very fine day!".to_string(),
            is_comment: false,
        };
        assert_eq!(format!("{}", text), "This is a very fine day!");
    }

    #[test]
    fn should_display_comment() {
        let comment = VText {
            content: "Something to remind the hacky users.".to_string(),
            is_comment: true,
        };
        assert_eq!(
            format!("{}", comment),
            "<!-- Something to remind the hacky users. -->"
        );
    }
}

#[cfg(test)]
#[cfg(target_arch = "wasm32")]
pub mod wasm_test {
    use dom::*;
    use prelude::*;
    use wasm_bindgen_test::*;
    use web_api::*;

    #[wasm_bindgen_test]
    fn should_patch_container_with_new_text() {
        let mut vtext = VText {
            content: "Hello World! It is nice to render.".to_string(),
            is_comment: false,
            node: None,
        };
        let div = html_document.create_element("div").unwrap();
        vtext
            .patch(None, div.clone().into(), None)
            .expect("To patch the div");

        assert_eq!(div.inner_html(), "Hello World! It is nice to render.");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_text_update() {
        let mut vtext = VText {
            content: "Hello World! It is nice to render.".to_string(),
            is_comment: false,
            node: None,
        };
        let div = html_document.create_element("div").unwrap();
        vtext
            .patch(None, div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(div.inner_html(), "Hello World! It is nice to render.");

        let mut updated = VText {
            content: "How you doing?".to_string(),
            is_comment: false,
            node: None,
        };
        updated
            .patch(Some(vtext), div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(div.inner_html(), "How you doing?");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_new_comment() {
        let mut comment = VText {
            content: "This is a comment".to_string(),
            is_comment: true,
            node: None,
        };
        let div = html_document.create_element("div").unwrap();
        comment
            .patch(None, div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(div.inner_html(), "<!--This is a comment-->");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_new_text_on_comment() {
        let mut comment = VText {
            content: "This is a comment".to_string(),
            is_comment: true,
            node: None,
        };
        let div = html_document.create_element("div").unwrap();
        comment
            .patch(None, div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(div.inner_html(), "<!--This is a comment-->");

        let mut text = VText {
            content: "This is a text".to_string(),
            is_comment: false,
            node: None,
        };
        text.patch(Some(comment), div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(div.inner_html(), "This is a text");
    }

}
