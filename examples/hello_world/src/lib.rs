#![feature(proc_macro_hygiene, decl_macro)]

use ruukh::prelude::*;
use wasm_bindgen::prelude::*;

#[component]
#[derive(Lifecycle)]
struct MainApp;

impl Render for MainApp {
    fn render(&self) -> Markup<Self> {
        html! {
            "Hello World!"
        }
    }
}

#[wasm_bindgen]
pub fn run() {
    App::<MainApp>::new().mount("app");
}
