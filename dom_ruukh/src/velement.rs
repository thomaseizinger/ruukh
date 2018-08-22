//! Element representation in a VDOM.

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
    tag: String,
    /// The attributes of the given element
    attributes: Attributes,
    /// The child node of the given element
    child: Option<Box<KeyedVNodes>>,
    /// Element reference to the DOM
    #[cfg(target_arch = "wasm32")]
    node: Option<Element>,
}

impl VElement {
    /// Constructor to create a VElement.
    pub fn new<T: Into<String>>(
        tag: T,
        attributes: Vec<Attribute>,
        child: KeyedVNodes,
    ) -> VElement {
        VElement {
            tag: tag.into(),
            attributes: Attributes(attributes),
            child: Some(Box::new(child)),
            #[cfg(target_arch = "wasm32")]
            node: None,
        }
    }

    /// Constructor to create a VElement without a child.
    pub fn childless<T: Into<String>>(tag: T, attributes: Vec<Attribute>) -> VElement {
        VElement {
            tag: tag.into(),
            attributes: Attributes(attributes),
            child: None,
            #[cfg(target_arch = "wasm32")]
            node: None,
        }
    }
}

/// A list of attributes.
#[derive(Debug)]
struct Attributes(Vec<Attribute>);

/// The key, value pair of the attributes on an element.
#[derive(Debug)]
pub struct Attribute {
    /// The key of the attribute
    key: String,
    /// The value pair of the attribute key
    value: String,
}

impl Attribute {
    /// Constructor to create an Attribute for a VElement.
    pub fn new<K: Into<String>, V: Into<String>>(key: K, value: V) -> Attribute {
        Attribute {
            key: key.into(),
            value: value.into(),
        }
    }
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
    use super::*;
    use vtext::VText;
    use KeyedVNodes;

    #[test]
    fn should_display_an_attribute() {
        let attribute = Attribute::new("class", "font-large bg-white");
        assert_eq!(format!("{}", attribute), "class=\"font-large bg-white\"");
    }

    #[test]
    fn should_display_a_div() {
        let div = VElement::childless("div", vec![]);
        assert_eq!(format!("{}", div), "<div></div>");
    }

    #[test]
    fn should_display_a_button_with_text() {
        let button = VElement::new("button", vec![], KeyedVNodes::unkeyed(VText::text("Click")));
        assert_eq!(format!("{}", button), "<button>Click</button>");
    }

    #[test]
    fn should_display_an_attributed_p() {
        let p = VElement::childless(
            "p",
            vec![
                Attribute::new("class", "mt-3"),
                Attribute::new("style", "background-color: grey;"),
            ],
        );
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
        let mut button_el = VElement::childless("button", vec![]);
        let div = container();
        button_el
            .patch(None, div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(div.inner_html(), "<button></button>");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_button_with_attrs() {
        let mut button_el = VElement::childless(
            "button",
            vec![
                Attribute::new("disabled", "true"),
                Attribute::new("class", "bg-white txt-black"),
            ],
        );
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
        let mut div_el = VElement::new(
            "div",
            vec![],
            KeyedVNodes::unkeyed(VElement::childless(
                "a",
                vec![Attribute::new("href", "http://www.rust-lang.org/")],
            )),
        );
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
        let mut div_el = VElement::childless("div", vec![]);
        let div = container();
        div_el
            .patch(None, div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(div.inner_html(), "<div></div>");

        let mut button_el = VElement::childless("button", vec![]);
        button_el
            .patch(Some(div_el), div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(div.inner_html(), "<button></button>");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_div_of_diff_attributes() {
        let mut div_el = VElement::childless("div", vec![Attribute::new("class", "bg-white")]);
        let div = container();
        div_el
            .patch(None, div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(div.inner_html(), r#"<div class="bg-white"></div>"#);

        let mut div_diff = VElement::childless(
            "div",
            vec![
                Attribute::new("class", "bg-white txt-black"),
                Attribute::new("id", "main"),
            ],
        );
        div_diff
            .patch(Some(div_el), div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<div class="bg-white txt-black" id="main"></div>"#
        )
    }
}
