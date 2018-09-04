#![recursion_limit = "256"]
//! The crate which removes all the boilerplate from `ruukh` apps.

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro2::{Span, TokenStream};
use syn::punctuated::Punctuated;
use syn::synom::Synom;
use syn::{
    Attribute, DeriveInput, Expr, Field, Fields, FnArg, Generics, Ident, Item, ItemStruct,
    ReturnType, Type, Visibility,
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
    events: Vec<EventField>,
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

/// Parses `fn fn_name(&self, arg0: ty, arg1: ty, ...) -> ty;`
struct EventField {
    ident: Ident,
    args: Vec<FnArg>,
    return_type: ReturnType,
}

/// Parses ``|`()`|`(default = expr)`
enum FieldAttributeArgs {
    None,
    Empty,
    Some { default: Expr },
}

impl Synom for EventField {
    named!(parse -> Self, do_parse!(
        keyword!(fn) >>
        ident: syn!(Ident) >>
        args: parens!(call!(Punctuated::<FnArg, Token!(,)>::parse_terminated)) >>
        return_type: syn!(ReturnType) >>
        syn!(Token!(;)) >>
        (EventField { ident, args: args.1.into_iter().collect(), return_type })
    ));
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
    let (events, remaining_attrs) = take_events(remaining_attrs);
    Component {
        attrs: remaining_attrs,
        vis: struct_.vis,
        ident: struct_.ident,
        generics: struct_.generics,
        props,
        state,
        events,
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

fn take_events(attrs: Vec<Attribute>) -> (Vec<EventField>, Vec<Attribute>) {
    let mut remaining = vec![];
    let mut events = vec![];
    let ident = Ident::new("events", Span::call_site()).into();

    for attr in attrs {
        if attr.path == ident {
            events.append(&mut parse_event(attr.tts));
        } else {
            remaining.push(attr);
        }
    }

    (events, remaining)
}

struct EventList(Vec<EventField>);

impl Synom for EventList {
    named!(parse -> Self, do_parse!(
        list: parens!(many0!(syn!(EventField))) >>
        (EventList(list.1))
    ));
}

fn parse_event(tts: TokenStream) -> Vec<EventField> {
    let events: EventList = syn::parse2(tts).expect(
        r#"The event list is written as: 
        `#[events(
            fn event_name(&self, args: type) -> type;

            fn other_event_name(&self);
        )]`"#,
    );

    events.0.iter().for_each(|event| {
        if event.args.is_empty() {
            panic!(
                "The event `{}` does not have any argument. \
                 Any event must have atleast one argument `&self`.",
                event.ident
            );
        } else {
            if let FnArg::SelfRef(_) = event.args[0] {
                // Is fine
            } else {
                panic!(
                    "The first argument of event `{}` must be `&self`.",
                    event.ident
                );
            }
        }
    });

    events.0
}

impl Component {
    fn expand(&self) -> TokenStream {
        if self.props.is_empty() && self.state.is_empty() && self.events.is_empty() {
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

        let (events_ty, events_structs) = if !self.events.is_empty() {
            let events_ty = Ident::new(&format!("{}Events", comp_ident), Span::call_site());
            let rctx_events_ty = Ident::new(&format!("{}Gen", events_ty), Span::call_site());
            let event_list: &Vec<_> = &self.events.iter().map(|event| event.as_field()).collect();
            let rctx_event_list: &Vec<_> = &self
                .events
                .iter()
                .map(|event| event.as_gen_field())
                .collect();
            let event_idents: &Vec<_> = &self.events.iter().map(|event| &event.ident).collect();
            let event_idents2 = event_idents;
            let event_list_args: &Vec<Vec<_>> = &self
                .events
                .iter()
                .map(|event| event.as_args_idents())
                .collect();
            let event_list_args2 = event_list_args;
            let event_list_typed_args: &Vec<Vec<_>> = &self
                .events
                .iter()
                .map(|event| event.as_typed_args())
                .collect();
            let event_list_ret_types: &Vec<_> =
                &self.events.iter().map(|event| &event.return_type).collect();

            (
                quote!(#events_ty),
                quote! {
                    struct #events_ty {
                        #(#event_list),*
                    }

                    impl #events_ty {
                        fn build<RCTX: Render>(
                            rctx_events: <Self as ruukh::component::EventsPair<RCTX>>::Other,
                            render_ctx: ruukh::Shared<RCTX>
                        ) -> Self
                        {
                            #(
                                let #event_idents = rctx_events.#event_idents2;
                            )*
                            #events_ty {
                                #(
                                    #event_idents: {
                                        let rctx = render_ctx.clone();
                                        Box::new(move |#(#event_list_args),*| {
                                            (#event_idents2)(&*rctx.borrow(), #(#event_list_args2),*)
                                        })
                                    }
                                ),*
                            }
                        }
                    }

                    struct #rctx_events_ty <RCTX: Render> {
                        #(#rctx_event_list),*
                    }


                    impl #impl_gen #comp_ident #ty_gen #where_clause {
                        #(
                            fn #event_idents (&self, #(#event_list_typed_args),*) #event_list_ret_types {
                                (self.__events.#event_idents2)(#(#event_list_args),*)
                            }
                        )*
                    }

                    impl<RCTX: Render> ruukh::component::EventsPair<RCTX> for #events_ty {
                        type Other = #rctx_events_ty<RCTX>;
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
                __status: ruukh::Shared<ruukh::component::Status<#state_ty>>,
            }
        };

        let events_field = if self.events.is_empty() {
            quote!()
        } else {
            quote! {
                __events: #events_ty,
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
                #events_field
            }

            impl #impl_gen Component for #comp_ident #ty_gen #where_clause {
                type Props = #props_ty;
                type State = #state_ty;
                type Events = #events_ty;

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

            #events_structs
        }
    }

    fn init_impl(&self) -> TokenStream {
        let props_field_idents: &Vec<_> = &self.props.iter().map(|field| &field.ident).collect();
        let props_field_idents2 = props_field_idents;

        let state_field_idents: &Vec<_> = &self.state.iter().map(|field| &field.ident).collect();
        let state_field_idents2 = state_field_idents;

        let comp_ident = &self.ident;

        let events_field = if self.events.is_empty() {
            quote!()
        } else {
            let events_ident = Ident::new(&format!("{}Events", comp_ident), Span::call_site());
            quote! {
                __events: #events_ident::build(events, render_ctx)
            }
        };

        let state_assignment = if self.state.is_empty() {
            quote!()
        } else if self.state.len() == 1 {
            quote! {
                let #(#state_field_idents)* = {
                    let status = status.borrow();
                    let state = status.state_as_ref();
                    #(state.#state_field_idents.clone())*
                };
            }
        } else {
            quote! {
                let (#(#state_field_idents),*) = {
                    let status = status.borrow();
                    let state = status.state_as_ref();
                    (#(state.#state_field_idents.clone()),*)
                };
            }
        };

        let status_field = if self.props.is_empty() && self.state.is_empty() {
            quote!()
        } else {
            quote!( __status: status, )
        };

        quote! {
            #[allow(unused_variables)]
            fn init<RCTX: Render>(
                props: Self::Props,
                events: <Self::Events as ruukh::component::EventsPair<RCTX>>::Other,
                status: ruukh::Shared<ruukh::component::Status<Self::State>>,
                render_ctx: ruukh::Shared<RCTX>,
            ) -> Self
            where
                Self::Events: ruukh::component::EventsPair<RCTX>
            {
                #state_assignment

                #comp_ident {
                    #(#props_field_idents: props.#props_field_idents2 ,)*
                    #(#state_field_idents: #state_field_idents2 ,)*
                    #status_field
                    #events_field
                }
            }
        }
    }

    fn update_impl(&self) -> TokenStream {
        let props_field_idents: &Vec<_> = &self.props.iter().map(|field| &field.ident).collect();
        let props_field_idents2 = props_field_idents;
        let props_field_idents3 = props_field_idents;
        let props_field_idents4 = props_field_idents;

        let events_assignment = if self.events.is_empty() {
            quote!()
        } else {
            let events_ident = Ident::new(&format!("{}Events", &self.ident), Span::call_site());
            quote! {
                // The events need to be updated regardless, there is no checking them.
                self.__events = #events_ident::build(events, render_ctx);
            }
        };

        if self.props.is_empty() {
            quote! {
                #[allow(unused_variables)]
                fn update<RCTX: Render>(
                    &mut self,
                    _: Self::Props,
                    events: <Self::Events as ruukh::component::EventsPair<RCTX>>::Other,
                    render_ctx: ruukh::Shared<RCTX>,
                ) -> Option<Self::Props>
                where
                    Self::Events: ruukh::component::EventsPair<RCTX>
                {
                    #events_assignment

                    None
                }
            }
        } else {
            quote! {
                #[allow(unused_variables)]
                fn update<RCTX: Render>(
                    &mut self,
                    mut props: Self::Props,
                    events: <Self::Events as ruukh::component::EventsPair<RCTX>>::Other,
                    render_ctx: ruukh::Shared<RCTX>,
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

                    #events_assignment

                    if updated {
                        self.__status.borrow_mut().mark_props_dirty();
                        Some(props)
                    } else {
                        None
                    }
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

impl EventField {
    fn types_only(&self) -> Vec<TokenStream> {
        let mut types = vec![];
        for (index, arg) in self.args.iter().enumerate() {
            match arg {
                FnArg::SelfRef(_) if index == 0 => {}
                FnArg::Captured(ref captured) if index > 0 => {
                    let ty = &captured.ty;
                    types.push(quote!( #ty ));
                }
                _ => unreachable!("The argument can only be in the format of `pattern: type`"),
            }
        }
        types
    }

    fn as_field(&self) -> TokenStream {
        let ident = &self.ident;
        let types = self.types_only();
        let ret_type = &self.return_type;

        match ret_type {
            ReturnType::Default => quote! {
                #ident: Box<Fn(#(#types),*)>
            },
            ReturnType::Type(_, ty) => quote! {
                #ident: Box<Fn(#(#types),*) -> #ty>
            },
        }
    }

    fn as_gen_field(&self) -> TokenStream {
        let ident = &self.ident;
        let types = self.types_only();
        let ret_type = &self.return_type;

        match ret_type {
            ReturnType::Default => quote! {
                #ident: Box<Fn(&RCTX, #(#types),*)>
            },
            ReturnType::Type(_, ty) => quote! {
                #ident: Box<Fn(&RCTX, #(#types),*) -> #ty>
            },
        }
    }

    fn as_args_idents(&self) -> Vec<TokenStream> {
        let mut args_idents = vec![];
        for (index, arg) in self.args.iter().enumerate() {
            match arg {
                FnArg::SelfRef(_) if index == 0 => {}
                FnArg::Captured(ref captured) if index > 0 => {
                    let pat = &captured.pat;
                    args_idents.push(quote!( #pat ));
                }
                _ => unreachable!("The argument can only be in the format of `pattern: type`"),
            }
        }
        args_idents
    }

    fn as_typed_args(&self) -> Vec<TokenStream> {
        let mut typed_args = vec![];
        for (index, arg) in self.args.iter().enumerate() {
            match arg {
                FnArg::SelfRef(_) if index == 0 => {}
                FnArg::Captured(ref captured) if index > 0 => {
                    let pat = &captured.pat;
                    let ty = &captured.ty;
                    typed_args.push(quote!( #pat: #ty ));
                }
                _ => unreachable!("The argument can only be in the format of `pattern: type`"),
            }
        }
        typed_args
    }
}
