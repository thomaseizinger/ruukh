#![feature(proc_macro_gen, proc_macro_non_items, decl_macro)]

use ruukh::prelude::*;
use wasm_bindgen::{prelude::*, JsCast};
use web_sys::window;

#[component]
struct MainApp {
    #[state]
    seconds: i32,
}

impl Lifecycle for MainApp {
    fn created(&self) {
        // Get a setter to mutate the state within the closure.
        let setter = self.state_setter();

        let closure = Closure::wrap(Box::new(move || {
            setter.set_state(|state| {
                state.seconds += 1;
            });
        }) as Box<Fn()>);

        window()
            .unwrap()
            .set_interval_with_callback_and_timeout_and_arguments_0(
                closure.as_ref().unchecked_ref(),
                1000,
            ).unwrap();

        // Forget the closure so that it lives for the lifetime of the program.
        closure.forget();
    }
}

impl Render for MainApp {
    fn render(&self) -> Markup<Self> {
        html! {
            "Timer: " { self.seconds }
        }
    }
}

#[wasm_bindgen]
pub fn run() {
    App::<MainApp>::new().mount("app");
}
