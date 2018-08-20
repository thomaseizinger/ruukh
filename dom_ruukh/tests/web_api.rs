#![cfg(target_arch = "wasm32")]

extern crate dom_ruukh;
extern crate wasm_bindgen;
extern crate wasm_bindgen_test;

use dom_ruukh::web_api::*;
use wasm_bindgen_test::*;

wasm_bindgen_test_configure!(run_in_browser);

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
    body.append_child(div.into()).unwrap();
    let el = html_document.get_element_by_id("app");
    assert!(el.is_some());
}

#[wasm_bindgen_test]
fn should_append_element_to_document() {
    let body: Node = html_document.body().unwrap().into();
    let el = html_document.create_element("div").unwrap();
    body.append_child(el.into()).expect("To append element");
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
    let span: Node = span.into();
    div.append_child(span).expect("To append span");
}

#[wasm_bindgen_test]
fn should_insert_before_element() {
    let div = html_document.create_element("div").unwrap();
    let span = html_document.create_element("span").unwrap();
    let div: &Node = div.as_ref();
    let span: Node = span.into();
    div.append_child(span.clone()).unwrap();
    let anchor: Node = html_document.create_text_node("Hello World!").into();
    div.insert_before(anchor, span)
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
    let span: Node = span.into();
    div.append_child(span.clone()).unwrap();
    div.remove_child(span).expect("To remove span");
}

#[wasm_bindgen_test]
fn should_set_text_content() {
    let txt: Node = html_document.create_text_node("Hello World!").into();
    txt.set_text_content("Hi World!");
    assert_eq!(txt.text_content(), "Hi World!");
}
