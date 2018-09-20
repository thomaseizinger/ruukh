#![recursion_limit = "256"]
#![cfg_attr(feature = "cargo-clippy", feature(tool_lints))]
#![cfg_attr(feature = "cargo-clippy", warn(clippy::all))]
//! The crate which removes most of the boilerplate from Ruukh apps.
//!
//! This lib defines `#[component]`, `#[derive(Lifecycle)]` and `html!` macros.
extern crate proc_macro;

use crate::{component::ComponentMeta, html::HtmlRoot};
use proc_macro2::Span;
use quote::quote;
use syn::{parse::Error, parse_macro_input, spanned::Spanned, DeriveInput, Item};

mod component;
mod html;
mod suffix;

/// A convenient auto derive for `Lifecycle` trait. It could be simply written
/// as `impl Lifecycle for MyComponent {}` instead, but why not save some chars.
/// You may use it like:
///
/// # Example
/// ```ignore,compile_fail
/// #[derive(Lifecycle)]
/// struct Button;
/// ```
#[proc_macro_derive(Lifecycle)]
pub fn derive_lifecycle(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = input.ident;

    let expanded = quote! {
        impl Lifecycle for #ident {}
    };

    expanded.into()
}

/// `#[component]` macro to derive `Component` trait as well as to do
/// modifications to the struct. It does all the heavy lifting which the user
/// would have to do, to make the component work.
///
/// # Example
/// ```ignore,compile_fail
/// #[component]
/// struct MyButton {
///     disabled: bool,
/// }
/// ```
///
/// You may declare events available on the component by placing `#[events]`
/// attribute. Like:
/// # Example
/// ```ignore,compile_fail
/// #[component]
/// #[events(
///     fn event_name(&self, arg: type) -> return_type;  
/// )]
/// struct MyButton {
///     disabled: bool,
/// }
/// ```
/// You may place multiple event declarations. Also, these event declarations
/// require that the event handlers be passed compulsorily.
///
/// `#[component]` also allows to annotate the struct fields with additional
/// attributes. Such as:
///
/// 1. `#[prop]` attribute: This attribute defines that a field is a prop
/// field, though this attribute is optional. This attribute allows passing a
/// default value for the prop field. Like `#[prop(default)]` which delegates
/// it to the types `Default` implementation or `#[prop(default = val)]` which
/// uses the `val` as its default value.
/// Any fields which has a given `default` value or any `Option` type is
/// optional while passing props.
///
/// 2. `#[state]` attribute: This attributes is required to define a field as a
/// state field. If a `#[state]` or `#[state(default)]` is specified then the
/// `Default` value of the field is used. If you want to provide a more
/// specific value, then pass it by using `#[state(default = val)]` attribute.
#[proc_macro_attribute]
#[cfg_attr(
    feature = "cargo-clippy",
    allow(clippy::needless_pass_by_value)
)]
pub fn component(
    metadata: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    if !metadata.is_empty() {
        return Error::new(
            Span::call_site(),
            "`#[component]` does not support attribute arguments.",
        ).to_compile_error()
        .into();
    }

    let input = parse_macro_input!(input as Item);

    let expanded = match input {
        Item::Struct(struct_) => ComponentMeta::parse(struct_)
            .map(|s| s.expand())
            .unwrap_or_else(|e| e.to_compile_error()),
        _ => {
            Error::new(input.span(), "Only structs are allowed to be Component").to_compile_error()
        }
    };

    expanded.into()
}

/// `html!` macro to parse `vue`-inspired syntax to generate Markup.
///
/// The basics of using html! macro:
///
/// ## Text
/// ```ignore,compile_fail
/// html! {
///     This is a sample text.
/// }
/// ```
///
/// ## Empty Markup
/// ```ignore,compile_fail
/// html!()
/// ```
///
/// ## Self-closing tags
/// Only html specified self-closing tags can be self-closing tags.
///
/// ```ignore,compile_fail
/// html! {
///     <br>
/// }
/// ```
///
/// ## Normal tags
/// ```ignore,compile_fail
/// html! {
///     <div></div>
///     <my-custom-tag></my-custom-tag>
/// }
/// ```
///
/// ## Component tags
/// ```ignore,compile_fail
/// html! {
///     <MyComponent></MyComponent>
/// }
/// ```
///
/// ## List of tags
/// ```ignore,compile_fail
/// html! {
///     <div></div>
///     <span></span>
///     <button></button>
/// }
/// ```
///
/// ## Nested markup
/// ```ignore,compile_fail
/// html! {
///     <div>
///         <span></span>
///     </div>
/// }
/// ```
///
/// ## Expressions in between
/// ```ignore,compile_fail
/// html! {
///     There are { count } people.
/// }
/// ```
#[proc_macro]
pub fn html(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed = parse_macro_input!(input as HtmlRoot);
    parsed.expand().into()
}
