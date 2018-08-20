//! Provides only the minimal Web API required to run a framework.
//! Complete API will be provided by `js-sys` & `web-sys` crates by rustwasm
//! and when those crates arrive, this module will be replaced.

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
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
    #[derive(Clone)]
    pub type Element;

    #[wasm_bindgen(method, catch, js_name = setAttribute)]
    pub fn set_attribute(this: &Element, name: &str, value: &str) -> Result<(), JsValue>;

    #[wasm_bindgen(method, js_name = getAttribute)]
    pub fn get_attribute(this: &Element, name: &str) -> Option<String>;

    #[wasm_bindgen(method, catch, js_name = removeAttribute)]
    pub fn remove_attribute(this: &Element, name: &str) -> Result<(), JsValue>;

    #[wasm_bindgen(extends = Node)]
    #[derive(Clone)]
    pub type Text;

    #[wasm_bindgen(extends = Node)]
    #[derive(Clone)]
    pub type Comment;

    #[derive(Clone)]
    pub type Node;

    #[wasm_bindgen(method, catch, js_name = insertBefore)]
    pub fn insert_before(this: &Node, new_node: Node, reference_node: Node) -> Result<Node, JsValue>;

    #[wasm_bindgen(method, catch, js_name = appendChild)]
    pub fn append_child(this: &Node, new_node: Node) -> Result<Node, JsValue>;

    #[wasm_bindgen(method, catch, js_name = removeChild)]
    pub fn remove_child(this: &Node, child: Node) -> Result<Node, JsValue>;

    #[wasm_bindgen(method, getter = textContent)]
    pub fn text_content(this: &Node) -> String;

    #[wasm_bindgen(method, setter = textContent)]
    pub fn set_text_content(this: &Node, val: &str);
}
