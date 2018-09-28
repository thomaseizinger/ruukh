#![feature(proc_macro_gen, proc_macro_non_items, decl_macro)]

use ruukh::prelude::*;
use wasm_bindgen::prelude::*;
use web_sys::Event;

#[component]
#[derive(Lifecycle)]
struct MainApp {
    #[state]
    toggle: bool,
}

impl Render for MainApp {
    fn render(&self) -> Markup<Self> {
        html! {
            <div style={self.style()}>{ if self.toggle { "On" } else { "Off" } }</div>
            <Button @click={Self::toggle}></Button>
        }
    }
}

impl MainApp {
    fn style(&self) -> &'static str {
        if self.toggle {
            "width: 100px; height: 100px; background-color: red;"
        } else {
            "width: 100px; height: 100px; background-color: yellow;"
        }
    }

    fn toggle(&self, _: Event) {
        self.set_state(|state| {
            state.toggle = !state.toggle;
        });
    }
}

#[component]
#[derive(Lifecycle)]
#[events(
    fn click(&self, event: Event);
)]
struct Button;

impl Render for Button {
    fn render(&self) -> Markup<Self> {
        html! {
            <button @click={Self::click}>Toggle</button>
        }
    }
}

#[wasm_bindgen]
pub fn run() {
    App::<MainApp>::new().mount("app");
}
