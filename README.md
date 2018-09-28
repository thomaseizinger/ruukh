# Ruukh - Frontend Web Framework

An experimental next-gen frontend framework for the Web in Rust. Write your web app in 
pure Rust.

#### !! THIS IS HIGHLY UNSTABLE !!
Don't depend on this library for any of your production code. Also, this project does not
make a guarantee that it will ever stabilize as there are a lot of issues to iron out.
So, for the time being, this project is in a state of continuous experimentation.

# Usage

Create a new library project as binary projects are not supported to run on WASM.

Add the following dependencies in your `Cargo.toml`:
```toml
[dependencies]
ruukh = { git = "https://github.com/csharad/cargo-ruukh" }
wasm-bindgen = "0.2.21"

[patch.crates-io]
proc-macro2 = { git = "https://github.com/csharad/proc-macro2", branch = "feature_flagged" }
```

Also, your `Cargo.toml` needs to set the `crate-type` to `cdylib`:
```toml
[lib]
crate-type = ["cdylib"]
```

## Ruukh CLI

You may use [`cargo-ruukh`](https://github.com/csharad/cargo-ruukh) to make your life easier
to build and run Ruukh projects. It supports running your webapp in a development server.

To install it:
```shell
cargo install cargo-ruukh --git https://github.com/csharad/cargo-ruukh
```

For more information: read up the [README](https://github.com/csharad/cargo-ruukh) at its own repo.

## Code structure

The CLI to run your project effortlessly expects your code to be structured in a particular way.
Also, mind that this library requires latest nightly to work.

In `lib.rs`:

```rust
#![feature(proc_macro_gen, proc_macro_non_items, decl_macro)]

extern crate wasm_bindgen;
extern crate ruukh;

use wasm_bindgen::prelude::*;
use ruukh::prelude::*;

#[component]
#[derive(Lifecycle)]
struct MyApp;

impl Render for MyApp {
    fn render(&self) -> Markup<Self> {
        html! {
            Hello World!
        }
    }
}

#[wasm_bindgen]
pub fn run() {
    App::<MyApp>::new().mount("app");
}
```

To check & see how the Ruukh app works, go to the [Examples](https://github.com/csharad/ruukh/tree/master/examples) section and run those projects.

---
The project is licensed under [MIT](https://github.com/csharad/ruukh/blob/master/LICENSE).
