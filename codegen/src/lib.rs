//! The crate which removes all the boilerplate from `ruukh` apps.

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use syn::DeriveInput;

/// A convenient auto derive for `Lifecycle` trait. It could be simply written
/// as `impl Lifecycle for MyComponent {}` instead, but why not save some chars.
#[proc_macro_derive(Lifecycle)]
pub fn derive_lifecycle(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();

    let ident = input.ident;

    let expanded = quote! {
        impl Lifecycle for #ident {}
    };

    expanded.into()
}
