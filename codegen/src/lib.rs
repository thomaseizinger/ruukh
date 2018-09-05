#![recursion_limit = "256"]
//! The crate which removes all the boilerplate from `ruukh` apps.

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use component::ComponentMeta;
use syn::{DeriveInput, Item};

mod component;

/// A convenient auto derive for `Lifecycle` trait. It could be simply written
/// as `impl Lifecycle for MyComponent {}` instead, but why not save some chars.
#[proc_macro_derive(Lifecycle)]
pub fn derive_lifecycle(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();

    let ident = input.ident;
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();

    let expanded = quote! {
        impl #impl_gen Lifecycle for #ident #ty_gen #where_clause {}
    };

    expanded.into()
}

/// `#[component]` macro to derive `Component` trait as well as to do modifications
/// to the struct. It does all the heavy lifting which the user would have to do,
/// to make the component work.
///
/// # Example
/// ```ignore,compile_fail
/// #[component]
/// struct MyButton {
///     disabled: bool
/// }
/// ```
#[proc_macro_attribute]
pub fn component(
    metadata: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    if !metadata.is_empty() {
        panic!("`#[component]` does not support attribute arguments.");
    }

    let input: Item = syn::parse(input).unwrap();

    let parsed = match input {
        Item::Struct(struct_) => ComponentMeta::parse(struct_),
        _ => panic!("Only structs are allowed to be Component"),
    };

    parsed.expand().into()
}
