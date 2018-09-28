#![feature(proc_macro_gen, proc_macro_non_items, decl_macro)]

use ruukh::prelude::*;
use wasm_bindgen::prelude::*;

#[component]
#[derive(Lifecycle)]
struct MainApp {
    #[state]
    count: i32,
}

impl Render for MainApp {
    fn render(&self) -> Markup<Self> {
        html! {
            The count is: { self.count }.
            <button @click={|this: &Self, _ev| {
                this.set_state(|state| {
                    state.count += 1;
                });
            }}>Increment</button>
        }
    }
}

#[wasm_bindgen]
pub fn run() {
    App::<MainApp>::new().mount("app");
}
