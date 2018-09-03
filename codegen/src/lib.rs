#![recursion_limit = "256"]
//! The crate which removes all the boilerplate from `ruukh` apps.

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro2::{Span, TokenStream};
use syn::synom::Synom;
use syn::{
    Attribute, DeriveInput, Expr, Field, Fields, Generics, Ident, Item, ItemStruct, Type,
    Visibility,
};

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
        Item::Struct(struct_) => parse_struct(struct_),
        _ => panic!("Only structs are allowed to be Component"),
    };

    parsed.expand().into()
}

/// Bundling all the information required to recreate a Component struct
/// (along with Component trait impl), as well as to produce the accompanying
/// Prop struct and State struct.
struct Component {
    attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Ident,
    generics: Generics,
    props: Vec<ComponentField>,
    state: Vec<ComponentField>,
}

/// There are two kinds of component fields: Prop and State. This struct
/// parses the required metadata for either prop/state field.
///
/// A prop field can be in either of the following format:
/// ```ignore,compile_fail
/// prop_a: type,
///
/// #[prop]
/// prop_b: type,
///
/// #[prop()]
/// prop_c: type,
///
/// #[prop(default = value)]
/// prop_d: type
///
/// ```
///
/// Likewise, a state field can be in the following format:
/// ```ignore,compile_fail
/// #[state]
/// state_a: type,
///
/// #[state()]
/// state_b: type,
///
/// #[state(default = value)]
/// state_c: type
/// ```
struct ComponentField {
    attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Ident,
    ty: Type,
    args: FieldAttributeArgs,
}

/// Parses ``|`()`|`(default = expr)`
enum FieldAttributeArgs {
    None,
    Empty,
    Some { default: Expr },
}

impl Synom for FieldAttributeArgs {
    named!(parse -> Self, alt!(
        input_end!() => {|_| FieldAttributeArgs::None}
        |
        parens!(input_end!()) => {|_| FieldAttributeArgs::Empty}
        |
        parens!(do_parse!(
            custom_keyword!(default) >>
            syn!(Token!(=)) >>
            expr: syn!(Expr) >>
            ( expr )
        )) => {|(_, expr)| FieldAttributeArgs::Some { default: expr }}
    ));
}

fn parse_struct(struct_: ItemStruct) -> Component {
    let (props, state) = match struct_.fields {
        Fields::Named(named_fields) => {
            let fields = named_fields.named.into_iter().collect();
            parse_struct_fields(fields)
        }
        Fields::Unnamed(_) => {
            panic!("Only unit structs and structs with named fields can be Components.");
        }
        Fields::Unit => (vec![], vec![]),
    };
    let remaining_attrs = remaining_struct_attrs(struct_.attrs);
    Component {
        attrs: remaining_attrs,
        vis: struct_.vis,
        ident: struct_.ident,
        generics: struct_.generics,
        props,
        state,
    }
}

fn parse_struct_fields(fields: Vec<Field>) -> (Vec<ComponentField>, Vec<ComponentField>) {
    let mut props = vec![];
    let mut state = vec![];
    for field in fields {
        if is_state_field(&field) {
            let (args, remaining_attrs) = parse_field_arg(field.attrs, true);
            state.push(ComponentField {
                attrs: remaining_attrs,
                vis: field.vis,
                ident: field.ident.unwrap(),
                ty: field.ty,
                args,
            });
        } else {
            let (args, remaining_attrs) = parse_field_arg(field.attrs, false);
            props.push(ComponentField {
                attrs: remaining_attrs,
                vis: field.vis,
                ident: field.ident.unwrap(),
                ty: field.ty,
                args,
            });
        }
    }
    (props, state)
}

fn is_state_field(field: &Field) -> bool {
    let state_ident = Ident::new("state", Span::call_site()).into();
    let prop_ident = Ident::new("prop", Span::call_site()).into();
    let mut is_state = false;
    let mut is_prop = false;

    for attr in field.attrs.iter() {
        if attr.path == state_ident {
            if !is_state {
                is_state = true;
            } else {
                panic!("Multiple instances of `#[state]` attribute are not allowed.");
            }
        }

        if attr.path == prop_ident {
            if !is_prop {
                is_prop = true;
            } else {
                panic!("Multiple instances of `#[prop]` attribute are not allowed.");
            }
        }
    }

    if is_state && is_prop {
        panic!("A field cannot be both prop and state.");
    }

    is_state
}

fn parse_field_arg(attrs: Vec<Attribute>, is_state: bool) -> (FieldAttributeArgs, Vec<Attribute>) {
    let ident = if is_state {
        Ident::new("state", Span::call_site()).into()
    } else {
        Ident::new("prop", Span::call_site()).into()
    };
    let mut remaining_attrs = vec![];
    let mut default = None;
    for attr in attrs {
        if attr.path == ident {
            default = Some(syn::parse2(attr.tts).expect("Could not parse the attribute arguments"));
        } else {
            remaining_attrs.push(attr);
        }
    }

    (default.unwrap_or(FieldAttributeArgs::None), remaining_attrs)
}

fn remaining_struct_attrs(attrs: Vec<Attribute>) -> Vec<Attribute> {
    let ident = Ident::new("component", Span::call_site()).into();
    let mut present = false;
    let mut remaining_attrs = vec![];
    for attr in attrs {
        if attr.path == ident {
            if !present {
                present = true;
            } else {
                panic!("Multiple instances of `#[component]` attribute not allowed.");
            }
        } else {
            remaining_attrs.push(attr);
        }
    }
    remaining_attrs
}

impl Component {
    fn expand(&self) -> TokenStream {
        let props_empty = self.props.is_empty();
        let state_empty = self.state.is_empty();

        if props_empty && state_empty {
            self.expand_with_none()
        } else {
            self.expand_with_some()
        }
    }

    fn expand_with_some(&self) -> TokenStream {
        let vis = &self.vis;
        let comp_ident = &self.ident;
        let generics = &self.generics;
        let comp_attrs = &self.attrs;
        let (impl_gen, ty_gen, where_clause) = generics.split_for_impl();

        let props_typed_fields: &Vec<_> =
            &self.props.iter().map(|field| field.as_field()).collect();
        let state_typed_fields: &Vec<_> =
            &self.state.iter().map(|field| field.as_field()).collect();

        // The state type and its default implementation based on the default val provided.
        let (state_ty, state_struct) = if !self.state.is_empty() {
            let state_ident = Ident::new(&format!("{}State", comp_ident), Span::call_site());

            let field_idents: &Vec<_> = &self.state.iter().map(|field| &field.ident).collect();
            let default_vals: &Vec<_> =
                &self.state.iter().map(|field| field.as_default()).collect();
            (
                quote!(#state_ident),
                // States cannot be generic as they are private to the component and the user
                // has no way to select a concrete type from its usage ergonomically.
                quote!{
                    struct #state_ident {
                        #(#state_typed_fields),*
                    }

                    impl Default for #state_ident {
                        fn default() -> Self {
                            #state_ident {
                                #(#field_idents: #default_vals),*
                            }
                        }
                    }
                },
            )
        } else {
            (quote!(()), quote!())
        };

        let (props_ty, props_struct) = if !self.props.is_empty() {
            let props_ident = Ident::new(&format!("{}Props", comp_ident), Span::call_site());
            let props_args: &Vec<_> = &self.props.iter().map(|field| field.as_arg()).collect();
            let field_idents: &Vec<_> = &self.props.iter().map(|field| &field.ident).collect();

            (
                quote!(#props_ident),
                quote!{
                    struct #props_ident #generics {
                        #(#props_typed_fields),*
                    }

                    impl #props_ident {
                        pub fn new(#(#props_args),*) -> Self {
                            #props_ident {
                                #(#field_idents),*
                            }
                        }
                    }
                },
            )
        } else {
            (quote!(()), quote!())
        };

        let status_field = if self.props.is_empty() && self.state.is_empty() {
            quote!()
        } else {
            quote! {
                __status: ruukh::Shared<ruukh::component::Status<#state_ty>>
            }
        };

        let init_impl = self.init_impl();
        let update_impl = self.update_impl();
        let refresh_state_impl = self.refresh_state_impl();
        let is_state_dirty_impl = self.is_state_dirty_impl();
        let is_props_dirty_impl = self.expand_is_props_dirty_impl();
        let set_state_impl = self.expand_set_state_impl();

        let comp_struct = quote! {
            #(#comp_attrs)*
            #vis struct #comp_ident #generics {
                #(#props_typed_fields ,)*
                #(#state_typed_fields ,)*
                #status_field
            }

            impl #impl_gen Component for #comp_ident #ty_gen #where_clause {
                type Props = #props_ty;
                type State = #state_ty;
                type Events = ();

                #init_impl

                #update_impl

                #refresh_state_impl

                #is_state_dirty_impl

                #is_props_dirty_impl

                #set_state_impl
            }
        };

        quote! {
            #comp_struct

            #state_struct

            #props_struct
        }
    }

    fn init_impl(&self) -> TokenStream {
        let props_field_idents: &Vec<_> = &self.props.iter().map(|field| &field.ident).collect();
        let props_field_idents2 = props_field_idents;

        let state_field_idents: &Vec<_> = &self.state.iter().map(|field| &field.ident).collect();
        let state_field_idents2 = state_field_idents;

        let comp_ident = &self.ident;

        quote! {
            fn init<RCTX: Render>(
                props: Self::Props,
                _: <Self::Events as ruukh::component::EventsPair<RCTX>>::Other,
                status: ruukh::Shared<ruukh::component::Status<Self::State>>,
                _: ruukh::Shared<RCTX>,
            ) -> Self
            where
                Self::Events: ruukh::component::EventsPair<RCTX>
            {
                let (#(#state_field_idents),*) = {
                    let status = status.borrow();
                    let state = status.state_as_ref();
                    (#(state.#state_field_idents.clone()),*)
                };

                #comp_ident {
                    #(#props_field_idents: props.#props_field_idents2 ,)*
                    #(#state_field_idents: #state_field_idents2 ,)*
                    __status: status
                }
            }
        }
    }

    fn update_impl(&self) -> TokenStream {
        let props_field_idents: &Vec<_> = &self.props.iter().map(|field| &field.ident).collect();
        let props_field_idents2 = props_field_idents;
        let props_field_idents3 = props_field_idents;
        let props_field_idents4 = props_field_idents;

        quote! {
            fn update<RCTX: Render>(
                &mut self,
                mut props: Self::Props,
                _: <Self::Events as ruukh::component::EventsPair<RCTX>>::Other,
                _: ruukh::Shared<RCTX>,
            ) -> Option<Self::Props>
            where
                Self::Events: ruukh::component::EventsPair<RCTX>
            {
                use std::mem;

                let mut updated = false;
                #(
                    if self.#props_field_idents != props.#props_field_idents2 {
                        mem::swap(&mut self.#props_field_idents3, &mut props.#props_field_idents4);

                        if !updated {
                            updated = true;
                        }
                    }
                )*
                if updated {
                    self.__status.borrow_mut().mark_props_dirty();
                    Some(props)
                } else {
                    None
                }
            }
        }
    }

    fn refresh_state_impl(&self) -> TokenStream {
        if self.state.is_empty() {
            quote! {
                fn refresh_state(&mut self) -> bool {
                    false
                }
            }
        } else {
            let state_field_idents: &Vec<_> =
                &self.state.iter().map(|field| &field.ident).collect();
            let state_field_idents2 = state_field_idents;
            let state_field_idents3 = state_field_idents;
            let state_field_idents4 = state_field_idents;

            quote! {
                fn refresh_state(&mut self) -> bool {
                    let status = self.__status.borrow();
                    let state = status.state_as_ref();
                    #(
                        if self.#state_field_idents != state.#state_field_idents2 {
                            self.#state_field_idents3 = state.#state_field_idents4.clone();
                        }
                    )*
                    // always going to be true. cuz, set_state only makes state dirty
                    // when it is dirty.
                    //
                    // TODO: FIX THIS
                    true
                }
            }
        }
    }

    fn is_state_dirty_impl(&self) -> TokenStream {
        if self.state.is_empty() {
            quote! {
                fn is_state_dirty(&mut self) -> bool {
                   false
                }
            }
        } else {
            quote! {
                fn is_state_dirty(&mut self) -> bool {
                    self.__status.borrow_mut().is_state_dirty()
                }
            }
        }
    }

    fn expand_is_props_dirty_impl(&self) -> TokenStream {
        if self.props.is_empty() {
            quote! {
                fn is_props_dirty(&mut self) -> bool {
                    false
                }
            }
        } else {
            quote! {
                fn is_props_dirty(&mut self) -> bool {
                    self.__status.borrow_mut().is_props_dirty()
                }
            }
        }
    }

    fn expand_set_state_impl(&self) -> TokenStream {
        if self.state.is_empty() {
            quote! {
                fn set_state<F>(&self, mut mutator: F)
                where
                    F: FnMut(&mut Self::State)
                {
                    mutator(&mut ());
                }
            }
        } else {
            let state_field_idents: &Vec<_> =
                &self.state.iter().map(|field| &field.ident).collect();
            let state_field_idents2 = state_field_idents;

            quote! {
                fn set_state<F>(&self, mut mutator: F)
                where
                    F: FnMut(&mut Self::State)
                {
                    let mut status = self.__status.borrow_mut();
                    mutator(status.state_as_mut());
                    let mut changed = false;
                    {
                        let state = status.state_as_ref();
                        #(
                            if !changed && self.#state_field_idents != state.#state_field_idents2 {
                                changed = true;
                            }
                        )*
                    }
                    if changed {
                        status.mark_state_dirty();
                        status.do_react();
                    }
                }
            }
        }
    }

    // When there are none of the props and state fields.
    fn expand_with_none(&self) -> TokenStream {
        let vis = &self.vis;
        let comp_ident = &self.ident;
        let generics = &self.generics;
        let comp_attrs = &self.attrs;
        let (impl_gen, ty_gen, where_clause) = generics.split_for_impl();

        quote! {
            #(#comp_attrs)*
            #vis struct #comp_ident #generics;

            impl #impl_gen Component for #comp_ident #ty_gen #where_clause {
                type Props = ();
                type State = ();
                type Events = ();

                fn init<RCTX: Render>(
                    _: Self::Props,
                    _: <Self::Events as ruukh::component::EventsPair<RCTX>>::Other,
                    _: ruukh::Shared<ruukh::component::Status<Self::State>>,
                    _: ruukh::Shared<RCTX>,
                ) -> Self
                where
                    Self::Events: ruukh::component::EventsPair<RCTX>
                {
                    #comp_ident
                }


                fn update<RCTX: Render>(
                    &mut self,
                    _: Self::Props,
                    _: <Self::Events as ruukh::component::EventsPair<RCTX>>::Other,
                    _: ruukh::Shared<RCTX>,
                ) -> Option<Self::Props>
                where
                    Self::Events: ruukh::component::EventsPair<RCTX>
                {
                    None
                }

                fn refresh_state(&mut self) -> bool { false }

                fn is_state_dirty(&mut self) -> bool { false }

                fn is_props_dirty(&mut self) -> bool { false }

                fn set_state<F>(&self, mut mutator: F)
                where
                    F: FnMut(&mut Self::State)
                {
                    mutator(&mut ());
                }
            }
        }
    }
}

impl ComponentField {
    fn as_field(&self) -> TokenStream {
        let attrs = &self.attrs;
        let vis = &self.vis;
        let ident = &self.ident;
        let ty = &self.ty;

        quote!{
            #(#attrs)*
            #vis #ident: #ty
        }
    }

    fn as_arg(&self) -> TokenStream {
        let ident = &self.ident;
        let ty = &self.ty;

        quote! {
            #ident: #ty
        }
    }

    fn as_default(&self) -> TokenStream {
        match self.args {
            FieldAttributeArgs::Empty | FieldAttributeArgs::None => quote! { Default::default() },
            FieldAttributeArgs::Some { ref default } => quote! { #default },
        }
    }
}
