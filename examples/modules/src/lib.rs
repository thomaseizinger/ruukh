#![feature(proc_macro_gen, proc_macro_non_items, decl_macro)]

use crate::paragraph::*;
use ruukh::prelude::*;
use wasm_bindgen::prelude::*;

mod paragraph;

#[component]
#[derive(Lifecycle)]
struct MyApp;

impl Render for MyApp {
    fn render(&self) -> Markup<Self> {
        html! {
            <h1>Lorem Post</h1>
            <Paragraph></Paragraph>
        }
    }
}

#[wasm_bindgen]
pub fn run() {
    App::<MyApp>::new().mount("app");
}
