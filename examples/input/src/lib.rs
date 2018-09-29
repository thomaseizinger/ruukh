#![feature(proc_macro_gen, proc_macro_non_items, decl_macro)]

use ruukh::prelude::*;
use wasm_bindgen::{prelude::*, JsCast};
use web_sys::{Event, HtmlInputElement};

#[component]
#[derive(Lifecycle)]
struct MainApp {
    #[state]
    input: String,
}

impl Render for MainApp {
    fn render(&self) -> Markup<Self> {
        html! {
            Name: <input @input={Self::on_input}/>
            {
                if !self.input.is_empty() {
                    html! {
                        <div>
                            Your name is { &self.input }.
                        </div>
                    }
                } else {
                    html!()
                }
            }
        }
    }
}

impl MainApp {
    fn on_input(&self, ev: Event) {
        let ev_target = ev.current_target().unwrap();
        let input_el: &HtmlInputElement = ev_target.unchecked_ref();
        self.set_state(|state| {
            state.input = input_el.value();
        });
    }
}

#[wasm_bindgen]
pub fn run() {
    App::<MainApp>::new().mount("app");
}
