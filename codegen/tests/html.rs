#![feature(proc_macro_non_items)]

extern crate ruukh;
extern crate ruukh_codegen;

use ruukh::prelude::*;
use ruukh::web_api::Event;

#[test]
fn should_expand_single_element() {
    let _: Markup<()> = html! {
        <div>Hello World!</div>
    };
}

#[test]
fn should_expand_list_of_elements() {
    let _: Markup<()> = html! {
        <div>Hello</div>
        <div>World</div>
        <div>How are you?</div>
    };
}

#[test]
fn should_expand_text() {
    let _: Markup<()> = html! {
        Hello World!
    };
}

#[test]
fn should_expand_multiline_text() {
    let _: Markup<()> = html! {
        Hello World!
        How are you?
    };
}

#[test]
fn should_expand_element_with_props() {
    let _: Markup<()> = html! {
        <button disabled={true}>Click</button>
    };
}

fn on_click(_: &(), _: Event) {}

#[test]
fn should_expand_element_with_event_listener() {
    let _: Markup<()> = html! {
        <button @click={on_click}>Click</button>
    };
}

#[test]
fn should_expand_element_with_attributes() {
    let _: Markup<()> = html! {
        <button
            disabled={true}
            @click={on_click}
            @doubleclick={on_click}
            name={"btn"}
        >Click
        </button>
    };
}
