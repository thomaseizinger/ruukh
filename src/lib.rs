#![deny(missing_docs)]
//! The Ruukh framework

extern crate wasm_bindgen;
#[cfg(test)]
extern crate wasm_bindgen_test;

use std::cell::RefCell;
use std::rc::Rc;
#[cfg(test)]
use wasm_bindgen_test::*;

#[cfg(test)]
wasm_bindgen_test_configure!(run_in_browser);

mod component;
mod dom;
pub mod vdom;
pub mod web_api;

#[allow(missing_docs)]
pub mod prelude {
    pub use component::{Component, Lifecycle, Render, Status};
    pub use vdom::{
        vcomponent::VComponent,
        velement::{Attribute, VElement},
        vlist::VList,
        vtext::VText,
        {Key, KeyedVNodes, VNode},
    };
}

type Shared<T> = Rc<RefCell<T>>;
