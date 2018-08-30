#![allow(missing_docs)]
//! Provides only the minimal Web API required to run a framework.
//! Complete API will be provided by `js-sys` & `web-sys` crates by rustwasm
//! and when those crates arrive, this module will be replaced.

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[derive(Debug)]
    pub type HtmlDocument;
    #[wasm_bindgen(js_name = document)]
    pub static html_document: HtmlDocument;

    #[wasm_bindgen(method, catch, js_name = createElement, structural)]
    pub fn create_element(this: &HtmlDocument, tag: &str) -> Result<Element, JsValue>;

    #[wasm_bindgen(method, js_name = createTextNode, structural)]
    pub fn create_text_node(this: &HtmlDocument, data: &str) -> Text;

    #[wasm_bindgen(method, js_name = createComment, structural)]
    pub fn create_comment(this: &HtmlDocument, data: &str) -> Comment;

    #[wasm_bindgen(method, js_name = getElementById, structural)]
    pub fn get_element_by_id(this: &HtmlDocument, id: &str) -> Option<Element>;

    #[wasm_bindgen(method, getter, structural)]
    pub fn body(this: &HtmlDocument) -> Option<Element>;

    #[wasm_bindgen(extends = Node)]
    #[derive(Debug, Clone)]
    pub type Element;

    #[wasm_bindgen(method, catch, js_name = setAttribute)]
    pub fn set_attribute(this: &Element, name: &str, value: &str) -> Result<(), JsValue>;

    #[wasm_bindgen(method, js_name = getAttribute)]
    pub fn get_attribute(this: &Element, name: &str) -> Option<String>;

    #[wasm_bindgen(method, catch, js_name = removeAttribute)]
    pub fn remove_attribute(this: &Element, name: &str) -> Result<(), JsValue>;

    #[wasm_bindgen(method, getter = innerHTML)]
    pub fn inner_html(this: &Element) -> String;

    #[wasm_bindgen(method, catch, js_name = addEventListener)]
    pub fn add_event_listener(
        this: &Element,
        type_: &str,
        listener: &Closure<Fn(Event)>,
    ) -> Result<(), JsValue>;

    #[wasm_bindgen(method, catch, js_name = removeEventListener)]
    pub fn remove_event_listener(
        this: &Element,
        type_: &str,
        listener: &Closure<Fn(Event)>,
    ) -> Result<(), JsValue>;

    #[wasm_bindgen(method, catch, js_name = dispatchEvent)]
    pub fn dispatch_event(this: &Element, event: &Event) -> Result<bool, JsValue>;

    #[wasm_bindgen(extends = Node)]
    #[derive(Debug, Clone)]
    pub type Text;

    #[wasm_bindgen(extends = Node)]
    #[derive(Debug, Clone)]
    pub type Comment;

    #[derive(Debug, Clone)]
    pub type Node;

    #[wasm_bindgen(method, catch, js_name = insertBefore)]
    pub fn insert_before(
        this: &Node,
        new_node: &Node,
        reference_node: &Node,
    ) -> Result<Node, JsValue>;

    #[wasm_bindgen(method, catch, js_name = appendChild)]
    pub fn append_child(this: &Node, new_node: &Node) -> Result<Node, JsValue>;

    #[wasm_bindgen(method, catch, js_name = removeChild)]
    pub fn remove_child(this: &Node, child: &Node) -> Result<Node, JsValue>;

    #[wasm_bindgen(method, getter = textContent)]
    pub fn text_content(this: &Node) -> String;

    #[wasm_bindgen(method, setter = textContent)]
    pub fn set_text_content(this: &Node, val: &str);

    #[derive(Debug, Clone)]
    pub type Event;

    #[wasm_bindgen(constructor)]
    pub fn new(type_: &str) -> Event;

    #[derive(Debug)]
    pub type MessageChannel;

    #[wasm_bindgen(constructor)]
    pub fn new() -> MessageChannel;

    #[wasm_bindgen(method, getter)]
    pub fn port1(this: &MessageChannel) -> MessagePort;

    #[wasm_bindgen(method, getter)]
    pub fn port2(this: &MessageChannel) -> MessagePort;

    #[derive(Debug, Clone)]
    pub type MessagePort;

    #[wasm_bindgen(method, catch, js_name = postMessage)]
    pub fn post_message(this: &MessagePort, any: &JsValue) -> Result<(), JsValue>;

    #[wasm_bindgen(method, setter = onmessage)]
    pub fn on_message(this: &MessagePort, handler: &Closure<FnMut(JsValue)>);

    #[wasm_bindgen(js_namespace = console, js_name = log)]
    pub fn console_log(s: &str);
}

#[cfg(test)]
pub mod wasm_test {
    use super::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn should_create_a_div_element() {
        let div = html_document.create_element("div");
        assert!(div.is_ok());
    }

    #[wasm_bindgen_test]
    fn should_find_element_with_id() {
        let div = html_document.create_element("div").unwrap();
        div.set_attribute("id", "app").unwrap();
        let body: Node = html_document.body().unwrap().into();
        body.append_child(div.as_ref()).unwrap();
        let el = html_document.get_element_by_id("app");
        assert!(el.is_some());
    }

    #[wasm_bindgen_test]
    fn should_append_element_to_document() {
        let body: Node = html_document.body().unwrap().into();
        let el = html_document.create_element("div").unwrap();
        body.append_child(el.as_ref()).expect("To append element");
    }

    #[wasm_bindgen_test]
    fn should_create_text_node() {
        let txt_node = html_document.create_text_node("Hello World!");
        let node: &Node = txt_node.as_ref();
        assert_eq!(node.text_content(), "Hello World!");
    }

    #[wasm_bindgen_test]
    fn should_create_comment_node() {
        let cmnt = html_document.create_comment("This is a comment");
        let node: &Node = cmnt.as_ref();
        assert_eq!(node.text_content(), "This is a comment");
    }

    #[wasm_bindgen_test]
    fn should_set_attribute() {
        let div = html_document.create_element("div").unwrap();
        div.set_attribute("class", "bg-white txt-black").unwrap();
        let class = div.get_attribute("class").unwrap();
        assert_eq!(class, "bg-white txt-black");
    }

    #[wasm_bindgen_test]
    fn should_delete_attribute() {
        let div = html_document.create_element("div").unwrap();
        div.set_attribute("class", "bg-white txt-black").unwrap();
        let class = div.get_attribute("class").unwrap();
        assert_eq!(class, "bg-white txt-black");
        div.remove_attribute("class").unwrap();
        let class = div.get_attribute("class");
        assert!(class.is_none());
    }

    #[wasm_bindgen_test]
    fn should_append_element() {
        let div = html_document.create_element("div").unwrap();
        let span = html_document.create_element("span").unwrap();
        let div: &Node = div.as_ref();
        div.append_child(span.as_ref()).expect("To append span");
    }

    #[wasm_bindgen_test]
    fn should_insert_before_element() {
        let div = html_document.create_element("div").unwrap();
        let span = html_document.create_element("span").unwrap();
        let div: &Node = div.as_ref();
        div.append_child(span.as_ref()).unwrap();
        let anchor = html_document.create_text_node("Hello World!");
        div.insert_before(anchor.as_ref(), span.as_ref())
            .expect("To insert anchor before span");
    }

    #[wasm_bindgen_test]
    fn should_remove_inserted_child() {
        let div = html_document
            .create_element("div")
            .expect("To create div element");
        let span = html_document
            .create_element("span")
            .expect("To create span element");
        let div: &Node = div.as_ref();
        div.append_child(span.as_ref()).unwrap();
        div.remove_child(span.as_ref()).expect("To remove span");
    }

    #[wasm_bindgen_test]
    fn should_set_text_content() {
        let txt: Node = html_document.create_text_node("Hello World!").into();
        txt.set_text_content("Hi World!");
        assert_eq!(txt.text_content(), "Hi World!");
    }

    #[wasm_bindgen_test]
    fn should_get_inner_html() {
        let div = html_document.create_element("div").unwrap();
        let span = html_document.create_element("span").unwrap();
        let div_node: &Node = div.as_ref();
        div_node.append_child(span.as_ref()).unwrap();
        assert_eq!(div.inner_html(), "<span></span>");
    }
}
