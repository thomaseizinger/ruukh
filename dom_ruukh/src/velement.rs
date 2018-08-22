use std::fmt::{self, Display, Formatter};
use {KeyedVNodes, VNode};
if_wasm! {
    use web_api::*;
    use wasm_bindgen::prelude::JsValue;
    use dom::DOMPatch;
}

/// The representation of an element in virtual DOM.
#[derive(Debug)]
pub struct VElement {
    /// The tag of the element. Eg: h, p, div, ...
    pub tag: String,
    /// The attributes of the given element
    pub attributes: Attributes,
    /// The child node of the given element
    pub child: Option<Box<KeyedVNodes>>,
    /// Element reference to the DOM
    #[cfg(target_arch = "wasm32")]
    pub node: Option<Element>,
}

/// A list of attributes.
#[derive(Debug)]
pub struct Attributes(pub Vec<Attribute>);

/// The key, value pair of the attributes on an element.
#[derive(Debug)]
pub struct Attribute {
    /// The key of the attribute
    pub key: String,
    /// The value pair of the attribute key
    pub value: String,
}

impl From<VElement> for VNode {
    fn from(el: VElement) -> VNode {
        VNode::Element(el)
    }
}

#[cfg(target_arch = "wasm32")]
impl VElement {
    fn patch_new(&mut self, parent: Node, next: Option<Node>) -> Result<(), JsValue> {
        let el = html_document.create_element(&self.tag)?;
        let el_node: Node = el.clone().into();
        self.attributes.patch(None, el.clone(), None)?;
        if let Some(ref mut child) = self.child {
            child.patch(None, el_node.clone(), None)?;
        }
        if let Some(next) = next {
            parent.insert_before(el_node, next)?;
        } else {
            parent.append_child(el_node)?;
        }
        self.node = Some(el);
        Ok(())
    }
}

#[cfg(target_arch = "wasm32")]
impl DOMPatch for VElement {
    type Node = Node;

    fn render_walk(&mut self, _: Node, _: Option<Node>) -> Result<(), JsValue> {
        if let Some(ref mut child) = self.child {
            let node = self
                .node
                .as_ref()
                .expect("The element itself must be patched before rendering the child")
                .clone();
            child.render_walk(node.into(), None)?;
        }
        Ok(())
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: Node,
        next: Option<Node>,
    ) -> Result<(), JsValue> {
        if let Some(old) = old {
            if self.tag == old.tag {
                let old_el = old
                    .node
                    .expect("The old node is expected to be attached to the DOM");
                let old_node: Node = old_el.clone().into();
                self.attributes
                    .patch(Some(old.attributes), old_el.clone(), None)?;
                if let Some(ref mut child) = self.child {
                    child.patch(old.child.map(|bx| *bx), old_node, None)?;
                }
                self.node = Some(old_el);
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
        let el = self
            .node
            .expect("The old node is expected to be attached to the DOM");
        if let Some(child) = self.child {
            child.remove(el.clone().into())?;
        }
        self.attributes.remove(el.clone())?;
        parent.remove_child(el.into())?;
        Ok(())
    }

    fn node(&self) -> Option<Node> {
        self.node.as_ref().map(|n| n.clone().into())
    }
}

const VOID_TAGS: [&'static str; 14] = [
    "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source",
    "track", "wbr",
];

impl Display for VElement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if VOID_TAGS.contains(&self.tag.as_str()) {
            write!(f, "<{}{}>", self.tag, self.attributes)?;
            if self.child.is_some() {
                panic!(
                    "Element with a void tag `{}` cannot have a child.",
                    self.tag
                );
            } else {
                Ok(())
            }
        } else {
            if let Some(ref child) = self.child {
                write!(
                    f,
                    "<{tag}{attributes}>{child}</{tag}>",
                    tag = self.tag,
                    attributes = self.attributes,
                    child = child
                )
            } else {
                write!(
                    f,
                    "<{tag}{attributes}></{tag}>",
                    tag = self.tag,
                    attributes = self.attributes
                )
            }
        }
    }
}

impl Display for Attributes {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for attr in self.0.iter() {
            write!(f, " {}", attr)?;
        }
        Ok(())
    }
}

#[cfg(target_arch = "wasm32")]
impl DOMPatch for Attributes {
    type Node = Element;

    fn render_walk(&mut self, _: Element, _: Option<Element>) -> Result<(), JsValue> {
        unreachable!("Attributes do not have nested Components");
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: Element,
        next: Option<Element>,
    ) -> Result<(), JsValue> {
        debug_assert!(next.is_none());
        if let Some(old) = old {
            for attr in old.0 {
                attr.remove(parent.clone())?;
            }
        }
        for attr in self.0.iter_mut() {
            attr.patch(None, parent.clone(), None)?;
        }
        Ok(())
    }

    fn remove(self, parent: Element) -> Result<(), JsValue> {
        for attr in self.0 {
            attr.remove(parent.clone())?;
        }
        Ok(())
    }

    fn node(&self) -> Option<Element> {
        unreachable!("There is no node for an attribute.")
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}=\"{}\"", self.key, self.value)
    }
}

#[cfg(target_arch = "wasm32")]
impl DOMPatch for Attribute {
    type Node = Element;

    fn render_walk(&mut self, _: Element, _: Option<Element>) -> Result<(), JsValue> {
        unreachable!("Attribute does not have nested Components");
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: Element,
        next: Option<Element>,
    ) -> Result<(), JsValue> {
        debug_assert!(old.is_none());
        debug_assert!(next.is_none());
        parent.set_attribute(&self.key, &self.value)
    }

    fn remove(self, parent: Element) -> Result<(), JsValue> {
        parent.remove_attribute(&self.key)
    }

    fn node(&self) -> Option<Element> {
        unreachable!("There is no node for an attribute.")
    }
}

#[cfg(test)]
#[cfg(not(target_arch = "wasm32"))]
mod test {
    use super::{Attribute, Attributes, VElement};
    use vtext::VText;
    use KeyedVNodes;

    #[test]
    fn should_display_an_attribute() {
        let attribute = Attribute {
            key: "class".to_string(),
            value: "font-large bg-white".to_string(),
        };
        assert_eq!(format!("{}", attribute), "class=\"font-large bg-white\"");
    }

    #[test]
    fn should_display_a_div() {
        let div = VElement {
            tag: "div".to_string(),
            attributes: Attributes(vec![]),
            child: None,
        };
        assert_eq!(format!("{}", div), "<div></div>");
    }

    #[test]
    fn should_display_a_button_with_text() {
        let button = VElement {
            tag: "button".to_string(),
            attributes: Attributes(vec![]),
            child: Some(Box::new(KeyedVNodes {
                key: None,
                vnode: VText {
                    content: "Click".to_string(),
                    is_comment: false,
                }.into(),
            })),
        };
        assert_eq!(format!("{}", button), "<button>Click</button>");
    }

    #[test]
    fn should_display_an_attributed_p() {
        let p = VElement {
            tag: "p".to_string(),
            attributes: Attributes(vec![
                Attribute {
                    key: "class".to_string(),
                    value: "mt-3".to_string(),
                },
                Attribute {
                    key: "style".to_string(),
                    value: "background-color: grey;".to_string(),
                },
            ]),
            child: None,
        };
        assert_eq!(
            format!("{}", p),
            "<p class=\"mt-3\" style=\"background-color: grey;\"></p>"
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

    fn container() -> Element {
        html_document.create_element("div").unwrap()
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_button_element() {
        let mut button_el = VElement {
            tag: "button".to_string(),
            attributes: Attributes(vec![]),
            child: None,
            node: None,
        };
        let div = container();
        button_el
            .patch(None, div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(div.inner_html(), "<button></button>");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_button_with_attrs() {
        let mut button_el = VElement {
            tag: "button".to_string(),
            attributes: Attributes(vec![
                Attribute {
                    key: "disabled".to_string(),
                    value: "true".to_string(),
                },
                Attribute {
                    key: "class".to_string(),
                    value: "bg-white txt-black".to_string(),
                },
            ]),
            child: None,
            node: None,
        };
        let div = container();
        button_el
            .patch(None, div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<button disabled="true" class="bg-white txt-black"></button>"#
        );
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_anchor_nested_in_div() {
        let mut div_el = VElement {
            tag: "div".to_string(),
            attributes: Attributes(vec![]),
            child: Some(Box::new(KeyedVNodes {
                key: None,
                vnode: VElement {
                    tag: "a".to_string(),
                    attributes: Attributes(vec![Attribute {
                        key: "href".to_string(),
                        value: "http://www.rust-lang.org/".to_string(),
                    }]),
                    child: None,
                    node: None,
                }.into(),
            })),
            node: None,
        };
        let div = container();
        div_el
            .patch(None, div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<div><a href="http://www.rust-lang.org/"></a></div>"#
        );
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_button_on_div() {
        let mut div_el = VElement {
            tag: "div".to_string(),
            attributes: Attributes(vec![]),
            child: None,
            node: None,
        };
        let div = container();
        div_el
            .patch(None, div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(div.inner_html(), "<div></div>");

        let mut button_el = VElement {
            tag: "button".to_string(),
            attributes: Attributes(vec![]),
            child: None,
            node: None,
        };
        button_el
            .patch(Some(div_el), div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(div.inner_html(), "<button></button>");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_div_of_diff_attributes() {
        let mut div_el = VElement {
            tag: "div".to_string(),
            attributes: Attributes(vec![Attribute {
                key: "class".to_string(),
                value: "bg-white".to_string(),
            }]),
            child: None,
            node: None,
        };
        let div = container();
        div_el
            .patch(None, div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(div.inner_html(), r#"<div class="bg-white"></div>"#);

        let mut div_diff = VElement {
            tag: "div".to_string(),
            attributes: Attributes(vec![
                Attribute {
                    key: "class".to_string(),
                    value: "bg-white txt-black".to_string(),
                },
                Attribute {
                    key: "id".to_string(),
                    value: "main".to_string(),
                },
            ]),
            child: None,
            node: None,
        };
        div_diff
            .patch(Some(div_el), div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<div class="bg-white txt-black" id="main"></div>"#
        )
    }

}
