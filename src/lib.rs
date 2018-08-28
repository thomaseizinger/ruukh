#![deny(missing_docs)]
//! The Ruukh framework

extern crate wasm_bindgen;
#[cfg(test)]
extern crate wasm_bindgen_test;

use std::cell::{Ref, RefCell, RefMut};
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

/// A Shared Value
pub struct Shared<T>(Rc<RefCell<T>>);

impl<T> Shared<T> {
    fn new(val: T) -> Shared<T> {
        Shared(Rc::new(RefCell::new(val)))
    }

    fn borrow(&self) -> Ref<T> {
        self.0.borrow()
    }

    fn borrow_mut(&self) -> RefMut<T> {
        self.0.borrow_mut()
    }
}

impl<T> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Shared(self.0.clone())
    }
}
