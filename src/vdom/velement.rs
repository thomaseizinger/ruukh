//! Element representation in a VDOM.

use crate::{component::Render, dom::DOMPatch, vdom::VNode, MessageSender, Shared};
use indexmap::IndexMap;
use std::{
    borrow::Cow,
    cell::RefCell,
    fmt::{self, Display, Formatter},
    rc::Rc,
};
use wasm_bindgen::{prelude::*, JsCast};
use web_sys::{window, Element, Event, EventTarget, Node};

/// The representation of an element in virtual DOM.
pub struct VElement<RCTX: Render> {
    /// The tag of the element. Eg: h, p, div, ...
    tag: &'static str,
    /// The attributes of the given element
    attributes: Attributes,
    /// Event listeners to the DOM events
    event_listeners: EventListeners<RCTX>,
    /// The child node of the given element
    child: Box<VNode<RCTX>>,
    /// Element reference to the DOM
    node: Option<Element>,
}

/// A list of attributes.
struct Attributes(IndexMap<&'static str, AttributeValue>);

/// The key, value pair of the attributes on an element.
pub struct Attribute {
    /// The key of the attribute
    key: &'static str,
    /// The value pair of the attribute key
    value: AttributeValue,
}

/// Either a string or a bool
pub enum AttributeValue {
    /// A string attribute value
    String(String),
    /// A boolean attribute value
    Bool(bool),
    /// An optional attribute value
    None,
}

struct EventListeners<RCTX: Render>(Vec<Box<dyn EventManager<RenderContext = RCTX>>>);

/// Event listener to be invoked on a DOM event.
pub struct EventListener<RCTX: Render> {
    type_: &'static str,
    listener: Option<Box<dyn Fn(&RCTX, Event)>>,
    dom_listener: Option<Closure<dyn Fn(Event)>>,
}

impl<RCTX: Render> VElement<RCTX> {
    /// Create a VElement.
    pub fn new(
        tag: &'static str,
        attributes: Vec<Attribute>,
        event_listeners: Vec<EventListener<RCTX>>,
        child: VNode<RCTX>,
    ) -> VElement<RCTX> {
        VElement {
            tag,
            attributes: Attributes::from(attributes),
            event_listeners: EventListeners(
                event_listeners
                    .into_iter()
                    .map(|listener| {
                        let listener: Box<
                            dyn EventManager<RenderContext = RCTX>,
                        > = Box::new(listener);
                        listener
                    })
                    .collect(),
            ),
            child: Box::new(child),
            node: None,
        }
    }

    /// Create a VElement without a child.
    pub fn childless(
        tag: &'static str,
        attributes: Vec<Attribute>,
        event_listeners: Vec<EventListener<RCTX>>,
    ) -> VElement<RCTX> {
        VElement {
            tag,
            attributes: Attributes::from(attributes),
            event_listeners: EventListeners(
                event_listeners
                    .into_iter()
                    .map(|listener| {
                        let listener: Box<
                            dyn EventManager<RenderContext = RCTX>,
                        > = Box::new(listener);
                        listener
                    })
                    .collect(),
            ),
            child: Box::new(VNode::None),
            node: None,
        }
    }
}

impl Attribute {
    /// Create an Attribute for a VElement.
    pub fn new(key: &'static str, value: impl Into<AttributeValue>) -> Attribute {
        Attribute {
            key,
            value: value.into(),
        }
    }
}

impl<RCTX: Render> EventListener<RCTX> {
    /// Create a EventListener.
    pub fn new(type_: &'static str, listener: Box<dyn Fn(&RCTX, Event)>) -> EventListener<RCTX> {
        EventListener {
            type_,
            listener: Some(listener),
            dom_listener: None,
        }
    }
}

impl<RCTX: Render> From<VElement<RCTX>> for VNode<RCTX> {
    fn from(el: VElement<RCTX>) -> VNode<RCTX> {
        VNode::Element(el)
    }
}

const VOID_TAGS: [&str; 14] = [
    "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source",
    "track", "wbr",
];

impl<RCTX: Render> Display for VElement<RCTX> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if VOID_TAGS.contains(&self.tag) {
            write!(f, "<{}{}>", self.tag, self.attributes)?;
            if !self.child.is_none() {
                panic!(
                    "Element with a void tag `{}` cannot have a child.",
                    self.tag
                );
            } else {
                Ok(())
            }
        } else {
            write!(
                f,
                "<{tag}{attributes}>{child}</{tag}>",
                tag = self.tag,
                attributes = self.attributes,
                child = self.child
            )
        }
    }
}

impl Display for Attributes {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (k, v) in self.0.iter() {
            match v {
                AttributeValue::String(ref v) => {
                    write!(f, " {}=\"{}\"", k, v);
                }
                AttributeValue::Bool(truthy) => {
                    if *truthy {
                        write!(f, " {}=\"\"", k)?;
                    }
                }
                AttributeValue::None => {}
            }
        }
        Ok(())
    }
}

impl<RCTX: Render> VElement<RCTX> {
    fn patch_new(
        &mut self,
        parent: &Node,
        next: Option<&Node>,
        render_ctx: Shared<RCTX>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        let el = window()
            .unwrap()
            .document()
            .unwrap()
            .create_element(&self.tag)?;
        self.attributes.patch(
            None,
            &el,
            None,
            Rc::new(RefCell::new(())),
            rx_sender.clone(),
        )?;
        self.event_listeners
            .patch(None, &el, None, render_ctx.clone(), rx_sender.clone())?;
        self.child
            .patch(None, el.as_ref(), None, render_ctx, rx_sender)?;
        parent.insert_before(el.as_ref(), next)?;
        self.node = Some(el);
        Ok(())
    }
}

impl<RCTX: Render> DOMPatch for VElement<RCTX> {
    type RenderContext = RCTX;
    type Node = Node;

    fn render_walk(
        &mut self,
        _: &Node,
        _: Option<&Node>,
        render_ctx: Shared<Self::RenderContext>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        let node = self
            .node
            .as_ref()
            .expect("The element itself must be patched before rendering the child");
        self.child
            .render_walk(node.as_ref(), None, render_ctx, rx_sender)
    }

    fn patch(
        &mut self,
        old: Option<&mut Self>,
        parent: &Node,
        next: Option<&Node>,
        render_ctx: Shared<Self::RenderContext>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        if let Some(old) = old {
            if self.tag == old.tag {
                let old_el = old
                    .node
                    .as_ref()
                    .expect("The old node is expected to be attached to the DOM");
                self.attributes.patch(
                    Some(&mut old.attributes),
                    &old_el,
                    None,
                    Rc::new(RefCell::new(())),
                    rx_sender.clone(),
                )?;
                self.event_listeners.patch(
                    Some(&mut old.event_listeners),
                    &old_el,
                    None,
                    render_ctx.clone(),
                    rx_sender.clone(),
                )?;
                self.child.patch(
                    Some(&mut *old.child),
                    old_el.as_ref(),
                    None,
                    render_ctx.clone(),
                    rx_sender,
                )?;

                self.node = Some(old_el.clone());
                Ok(())
            } else {
                old.remove(parent)?;
                self.patch_new(parent, next, render_ctx, rx_sender)
            }
        } else {
            self.patch_new(parent, next, render_ctx, rx_sender)
        }
    }

    fn reorder(&self, parent: &Node, next: Option<&Node>) -> Result<(), JsValue> {
        let el = self.node.as_ref().unwrap();
        parent.insert_before(el.as_ref(), next)?;
        Ok(())
    }

    fn remove(&self, parent: &Node) -> Result<(), JsValue> {
        let el = self
            .node
            .as_ref()
            .expect("The old node is expected to be attached to the DOM");
        self.child.remove(el.as_ref())?;
        self.attributes.remove(&el)?;
        parent.remove_child(el.as_ref())?;
        Ok(())
    }

    fn node(&self) -> Option<&Node> {
        self.node.as_ref().map(|el| el.as_ref())
    }
}

impl DOMPatch for Attributes {
    type RenderContext = ();
    type Node = Element;

    fn render_walk(
        &mut self,
        _: &Element,
        _: Option<&Element>,
        _: Shared<Self::RenderContext>,
        _: MessageSender,
    ) -> Result<(), JsValue> {
        unreachable!("Attributes do not have nested Components");
    }

    fn patch(
        &mut self,
        mut old: Option<&mut Self>,
        parent: &Element,
        next: Option<&Element>,
        _: Shared<Self::RenderContext>,
        _: MessageSender,
    ) -> Result<(), JsValue> {
        debug_assert!(next.is_none());
        for (k, v) in self.0.iter() {
            // Remove the key from old as it exists in the newer.
            let existed = if let Some(ref mut old) = old {
                match old.0.swap_remove(k) {
                    Some(AttributeValue::None) | None => false,
                    _ => true,
                }
            } else {
                false
            };
            match v {
                AttributeValue::String(val) => {
                    parent.set_attribute(&k, &val)?;
                }
                AttributeValue::Bool(truthy) => {
                    if *truthy {
                        parent.set_attribute(&k, "")?;
                    } else if existed {
                        parent.remove_attribute(&k)?;
                    }
                }
                AttributeValue::None => {
                    if existed {
                        parent.remove_attribute(&k)?;
                    }
                }
            }
        }
        // Remove the remaining keys.
        if let Some(old) = old {
            old.remove(parent)?;
        }
        Ok(())
    }

    fn reorder(&self, _: &Self::Node, _: Option<&Self::Node>) -> Result<(), JsValue> {
        unreachable!("Cannot reorder Attributes");
    }

    fn remove(&self, parent: &Element) -> Result<(), JsValue> {
        for (k, _) in self.0.iter() {
            parent.remove_attribute(&k)?;
        }
        Ok(())
    }

    fn node(&self) -> Option<&Node> {
        unreachable!("Attributes have no nodes");
    }
}

impl<RCTX: Render> DOMPatch for EventListeners<RCTX> {
    type RenderContext = RCTX;
    type Node = Element;

    fn render_walk(
        &mut self,
        _: &Element,
        _: Option<&Element>,
        _: Shared<Self::RenderContext>,
        _: MessageSender,
    ) -> Result<(), JsValue> {
        unreachable!("EventListeners does not have nested Components");
    }

    fn patch(
        &mut self,
        old: Option<&mut Self>,
        parent: &Element,
        _: Option<&Element>,
        render_ctx: Shared<Self::RenderContext>,
        _: MessageSender,
    ) -> Result<(), JsValue> {
        if let Some(old) = old {
            old.remove(parent)?;
        }
        for listener in self.0.iter_mut() {
            listener.start_listening(parent.as_ref(), render_ctx.clone())?;
        }
        Ok(())
    }

    fn reorder(&self, _: &Self::Node, _: Option<&Self::Node>) -> Result<(), JsValue> {
        unreachable!("Cannot reorder EventListeners");
    }

    fn remove(&self, parent: &Element) -> Result<(), JsValue> {
        for listener in self.0.iter() {
            listener.stop_listening(parent.as_ref())?;
        }
        Ok(())
    }

    fn node(&self) -> Option<&Node> {
        unreachable!("EventListeners have no nodes");
    }
}

trait EventManager {
    type RenderContext;

    fn start_listening(
        &mut self,
        parent: &EventTarget,
        render_ctx: Shared<Self::RenderContext>,
    ) -> Result<(), JsValue>;

    fn stop_listening(&self, parent: &EventTarget) -> Result<(), JsValue>;
}

impl<RCTX: Render> EventManager for EventListener<RCTX> {
    type RenderContext = RCTX;

    fn start_listening(
        &mut self,
        parent: &EventTarget,
        render_ctx: Shared<Self::RenderContext>,
    ) -> Result<(), JsValue> {
        let listener = self.listener.take().unwrap();
        let js_closure: Closure<dyn Fn(Event)> = Closure::wrap(Box::new(move |event| {
            listener(&*render_ctx.borrow(), event)
        }));
        parent
            .add_event_listener_with_callback(&self.type_, js_closure.as_ref().unchecked_ref())?;
        self.dom_listener = Some(js_closure);
        Ok(())
    }

    fn stop_listening(&self, parent: &EventTarget) -> Result<(), JsValue> {
        if let Some(ref dom_listener) = self.dom_listener {
            parent.remove_event_listener_with_callback(
                &self.type_,
                dom_listener.as_ref().unchecked_ref(),
            )?;
        }
        Ok(())
    }
}

impl From<bool> for AttributeValue {
    fn from(val: bool) -> AttributeValue {
        AttributeValue::Bool(val)
    }
}

impl<'a> From<&'a str> for AttributeValue {
    fn from(val: &'a str) -> AttributeValue {
        AttributeValue::String(val.to_string())
    }
}

impl From<String> for AttributeValue {
    fn from(val: String) -> AttributeValue {
        AttributeValue::String(val)
    }
}

impl<'a> From<Cow<'a, str>> for AttributeValue {
    fn from(val: Cow<'a, str>) -> AttributeValue {
        AttributeValue::String(val.into())
    }
}

impl<'a, T: Into<AttributeValue>> From<Option<T>> for AttributeValue {
    fn from(val: Option<T>) -> AttributeValue {
        match val {
            Some(val) => val.into(),
            None => AttributeValue::None,
        }
    }
}

impl From<Vec<Attribute>> for Attributes {
    fn from(val: Vec<Attribute>) -> Attributes {
        let attrs = val.into_iter().map(|attr| (attr.key, attr.value)).collect();
        Attributes(attrs)
    }
}

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::{
        component::root_render_ctx,
        vdom::{test::container, vtext::VText},
    };
    use wasm_bindgen_test::*;

    #[test]
    fn should_display_a_div() {
        let div = VElement::<()>::childless("div", vec![], vec![]);
        assert_eq!(format!("{}", div), "<div></div>");
    }

    #[test]
    fn should_display_a_button_with_text() {
        let button =
            VElement::<()>::new("button", vec![], vec![], VNode::from(VText::text("Click")));
        assert_eq!(format!("{}", button), "<button>Click</button>");
    }

    #[test]
    fn should_display_an_attributed_p() {
        let p = VElement::<()>::childless(
            "p",
            vec![
                Attribute::new("class", "mt-3"),
                Attribute::new("style", "background-color: grey;"),
            ],
            vec![],
        );
        assert_eq!(
            format!("{}", p),
            "<p class=\"mt-3\" style=\"background-color: grey;\"></p>"
        );
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_button_element() {
        let mut button_el = VElement::childless("button", vec![], vec![]);
        let div = container();
        button_el
            .patch(
                None,
                div.as_ref(),
                None,
                root_render_ctx(),
                crate::message_sender(),
            )
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
            vec![],
        );
        let div = container();
        button_el
            .patch(
                None,
                div.as_ref(),
                None,
                root_render_ctx(),
                crate::message_sender(),
            )
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
            vec![],
            VNode::from(VElement::childless(
                "a",
                vec![Attribute::new("href", "http://www.rust-lang.org/")],
                vec![],
            )),
        );
        let div = container();
        div_el
            .patch(
                None,
                div.as_ref(),
                None,
                root_render_ctx(),
                crate::message_sender(),
            )
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<div><a href="http://www.rust-lang.org/"></a></div>"#
        );
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_button_on_div() {
        let mut div_el = VElement::childless("div", vec![], vec![]);
        let div = container();
        div_el
            .patch(
                None,
                div.as_ref(),
                None,
                root_render_ctx(),
                crate::message_sender(),
            )
            .expect("To patch div");

        assert_eq!(div.inner_html(), "<div></div>");

        let mut button_el = VElement::childless("button", vec![], vec![]);
        button_el
            .patch(
                Some(&mut div_el),
                div.as_ref(),
                None,
                root_render_ctx(),
                crate::message_sender(),
            )
            .expect("To patch div");

        assert_eq!(div.inner_html(), "<button></button>");
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_div_of_diff_attributes() {
        let mut div_el =
            VElement::childless("div", vec![Attribute::new("class", "bg-white")], vec![]);
        let div = container();
        div_el
            .patch(
                None,
                div.as_ref(),
                None,
                root_render_ctx(),
                crate::message_sender(),
            )
            .expect("To patch div");

        assert_eq!(div.inner_html(), r#"<div class="bg-white"></div>"#);

        let mut div_diff = VElement::childless(
            "div",
            vec![
                Attribute::new("class", "bg-white txt-black"),
                Attribute::new("id", "main"),
            ],
            vec![],
        );
        div_diff
            .patch(
                Some(&mut div_el),
                div.as_ref(),
                None,
                root_render_ctx(),
                crate::message_sender(),
            )
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<div class="bg-white txt-black" id="main"></div>"#
        )
    }
}
