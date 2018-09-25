use crate::suffix::{EVENT_PROPS_SUFFIX, EVENT_SUFFIX, PROPS_SUFFIX, STATE_SUFFIX};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    custom_keyword, parenthesized,
    parse::{Error, Parse, ParseStream, Result as ParseResult},
    spanned::Spanned,
    Attribute, Expr, Field, Fields, FnArg, Ident, ItemStruct, Pat, ReturnType, Token, Type,
    TypePath, TypeReference, Visibility,
};

/// All the necessary metadata taken from the struct declaration to construct
/// a working Component.
pub struct ComponentMeta {
    /// Attributes on a component.
    attrs: Vec<Attribute>,
    /// Visibility specifier on a component.
    vis: Visibility,
    /// Ident of component.
    ident: Ident,
    /// Props metadata if any prop fields.
    props_meta: Option<PropsMeta>,
    /// State metadata if any state fields.
    state_meta: Option<StateMeta>,
    /// Events metadata if any events declaration.
    events_meta: Option<EventsMeta>,
}

impl ComponentMeta {
    pub fn parse(item: ItemStruct) -> ParseResult<ComponentMeta> {
        if item.generics != Default::default() {
            return Err(Error::new(
                item.ident.span(),
                "Generic parameters not allowed on a component.",
            ));
        }

        // Sort out the struct fields into state and props fields.
        let (props_meta, state_meta) = match item.fields {
            Fields::Named(fields) => {
                let fields: Vec<_> = fields.named.into_iter().collect();

                for field in fields.iter() {
                    check_supported_attributes(field)?;
                }

                let (props_meta, fields) = PropsMeta::take_prop_fields(&item.ident, fields)?;
                let (state_meta, fields) = StateMeta::take_state_fields(&item.ident, fields)?;
                assert!(
                    fields.is_empty(),
                    "There are no fields left other than prop and state fields."
                );
                Ok((props_meta, state_meta))
            }
            Fields::Unnamed(_) => Err(Error::new(
                item.ident.span(),
                "Only unit and named field structs can be components.",
            )),
            Fields::Unit => Ok((None, None)),
        }?;
        let attrs = Self::filter_out_component_attributes(item.attrs);
        let (events_meta, attrs) = EventsMeta::take_events_attributes(&item.ident, attrs)?;

        Ok(ComponentMeta {
            attrs,
            vis: item.vis,
            ident: item.ident,
            props_meta,
            state_meta,
            events_meta,
        })
    }

    fn filter_out_component_attributes(attrs: Vec<Attribute>) -> Vec<Attribute> {
        let component_name = Ident::new("component", Span::call_site()).into();
        attrs
            .into_iter()
            .filter(|attr| attr.path != component_name)
            .collect()
    }

    pub fn expand(&self) -> TokenStream {
        let component_struct = self.create_component_struct();
        let component_impl = self.impl_component_trait_on_component_struct();
        let state_struct = self.state_meta.as_ref().map(StateMeta::create_state_struct);
        let props_struct = self
            .props_meta
            .as_ref()
            .map(|m| m.create_props_struct_and_macro(&self.ident, &self.vis))
            .unwrap_or_else(|| PropsMeta::create_void_props_macro(&self.ident, &self.vis));
        let events_structs = self
            .events_meta
            .as_ref()
            .map(|m| m.create_events_and_event_props_struct_and_macro(&self.ident, &self.vis))
            .unwrap_or_else(|| EventsMeta::create_void_events_macro(&self.ident, &self.vis));

        quote! {
            #component_struct

            #state_struct

            #props_struct

            #events_structs

            #component_impl
        }
    }

    fn create_component_struct(&self) -> TokenStream {
        let attrs = &self.attrs;
        let ident = &self.ident;
        let vis = &self.vis;

        if self.props_meta.is_none() && self.state_meta.is_none() && self.events_meta.is_none() {
            quote! {
                #(#attrs)*
                #vis struct #ident;
            }
        } else {
            let state_fields = self
                .state_meta
                .as_ref()
                .map(|s| s.expand_fields_with(ComponentField::to_struct_field))
                .unwrap_or_default();
            let props_fields = self
                .props_meta
                .as_ref()
                .map(|p| p.expand_fields_with(ComponentField::to_struct_field))
                .unwrap_or_default();
            let status_field = self.create_status_field();
            let events_field = self.create_events_field();

            quote! {
                #(#attrs)*
                #vis struct #ident {
                    #(#state_fields ,)*
                    #(#props_fields ,)*
                    #status_field
                    #events_field
                }
            }
        }
    }

    fn create_status_field(&self) -> TokenStream {
        if self.props_meta.is_none() && self.state_meta.is_none() {
            quote!()
        } else {
            let ident = if let Some(ref state_meta) = self.state_meta {
                let ident = &state_meta.ident;
                quote! { #ident }
            } else {
                quote! { () }
            };
            quote! {
                __status__: std::rc::Rc<std::cell::RefCell<ruukh::component::Status<#ident>>>,
            }
        }
    }

    fn create_events_field(&self) -> TokenStream {
        if self.events_meta.is_none() {
            quote!()
        } else {
            let ident = &self.events_meta.as_ref().unwrap().ident;
            quote! {
                __events__: #ident,
            }
        }
    }

    fn expand_state_field_idents(&self) -> Vec<TokenStream> {
        self.state_meta
            .as_ref()
            .map(|s| s.expand_fields_with(ComponentField::to_ident))
            .unwrap_or_default()
    }

    fn get_props_type(&self) -> TokenStream {
        if let Some(ref props_meta) = self.props_meta {
            let ident = &props_meta.ident;
            quote!(#ident)
        } else {
            quote!(())
        }
    }

    fn get_state_type(&self) -> TokenStream {
        if let Some(ref state_meta) = self.state_meta {
            let ident = &state_meta.ident;
            quote!(#ident)
        } else {
            quote!(())
        }
    }

    fn get_events_type(&self) -> TokenStream {
        if let Some(ref events_meta) = self.events_meta {
            let ident = &events_meta.ident;
            quote!(#ident)
        } else {
            quote!(())
        }
    }

    fn impl_component_trait_on_component_struct(&self) -> TokenStream {
        let ident = &self.ident;
        let props_type = &self.get_props_type();
        let state_type = &self.get_state_type();
        let events_type = &self.get_events_type();
        let state_clone = self.impl_state_clone_from_status();
        let event_assignment = self.impl_event_assignment();
        let state_field_idents = &self
            .state_meta
            .as_ref()
            .map(|m| m.fields.iter().map(|f| &f.ident).collect::<Vec<_>>())
            .unwrap_or_default();
        let props_field_idents = &self
            .props_meta
            .as_ref()
            .map(|m| m.fields.iter().map(|f| &f.ident).collect::<Vec<_>>())
            .unwrap_or_default();
        let props_field_idents2 = props_field_idents;
        let status_assignment = self.impl_status_assignment();
        let props_updation = self.impl_props_updation(props_field_idents);
        let updation_ret_block = self.impl_return_block_after_updation();
        let events_updation = self.impl_events_updation();
        let refresh_state_body = self.impl_fn_refresh_state_body(state_field_idents);
        let take_state_dirty_body = self.impl_fn_take_state_dirty_body();
        let take_props_dirty_body = self.impl_fn_take_props_dirty_body();
        let set_state_body = self.impl_fn_set_state_body(state_field_idents);

        quote! {
            impl Component for #ident {
                type Props = #props_type;
                type State = #state_type;
                type Events = #events_type;

                fn init(
                    __props__: Self::Props,
                    __events__: Self::Events,
                    __status__: std::rc::Rc<std::cell::RefCell<ruukh::component::Status<Self::State>>>,
                ) -> Self {
                    #state_clone

                    #ident {
                        #(#props_field_idents: __props__.#props_field_idents2 ,)*
                        #(#state_field_idents ,)*
                        #event_assignment
                        #status_assignment
                    }
                }

                fn update(
                    &mut self,
                    mut __props__: Self::Props,
                    __events__: Self::Events,
                ) -> Option<Self::Props> {
                    #props_updation

                    #events_updation

                    #updation_ret_block
                }

                fn refresh_state(&mut self) {
                    #refresh_state_body
                }

                fn take_state_dirty(&self) -> bool {
                    #take_state_dirty_body
                }

                fn take_props_dirty(&self) -> bool {
                    #take_props_dirty_body
                }

                fn set_state<F>(&self, mut mutator: F)
                where
                    F: FnMut(&mut Self::State)
                {
                    #set_state_body
                }
            }
        }
    }

    fn impl_fn_set_state_body(&self, idents: &[&Ident]) -> TokenStream {
        let idents2 = idents;
        if self.state_meta.is_some() {
            quote! {
                let mut status = self.__status__.borrow_mut();
                mutator(status.state_as_mut());
                let mut changed = false;
                {
                    let state = status.state_as_ref();
                    #(
                        if !changed && self.#idents != state.#idents2 {
                            changed = true;
                        }
                    )*
                }
                if changed {
                    status.mark_state_dirty();
                    status.do_react();
                }
            }
        } else {
            quote! {
                mutator(&mut ());
            }
        }
    }

    fn impl_fn_take_props_dirty_body(&self) -> TokenStream {
        if self.props_meta.is_some() {
            quote! {
                self.__status__.borrow_mut().take_props_dirty()
            }
        } else {
            quote! { false }
        }
    }

    fn impl_fn_take_state_dirty_body(&self) -> TokenStream {
        if self.state_meta.is_some() {
            quote! {
                self.__status__.borrow_mut().take_state_dirty()
            }
        } else {
            quote! { false }
        }
    }

    fn impl_fn_refresh_state_body(&self, idents: &[&Ident]) -> TokenStream {
        let idents2 = idents;
        let idents3 = idents;
        let idents4 = idents;

        if self.state_meta.is_some() {
            quote! {
                let status = self.__status__.borrow();
                let state = status.state_as_ref();
                #(
                    if self.#idents != state.#idents2 {
                        self.#idents3 = state.#idents4.clone();
                    }
                )*
            }
        } else {
            quote!()
        }
    }

    fn impl_props_updation(&self, idents: &[&Ident]) -> TokenStream {
        let idents2 = idents;
        let idents3 = idents;
        let idents4 = idents;

        if self.props_meta.is_some() {
            quote! {
                use std::mem;

                let mut updated = false;
                #(
                    if self.#idents != __props__.#idents2 {
                        mem::swap(&mut self.#idents3, &mut __props__.#idents4);

                        if !updated {
                            updated = true;
                        }
                    }
                )*
            }
        } else {
            quote!()
        }
    }

    fn impl_return_block_after_updation(&self) -> TokenStream {
        if self.props_meta.is_some() {
            quote! {
                if updated {
                    self.__status__.borrow_mut().mark_props_dirty();
                    Some(__props__)
                } else {
                    None
                }
            }
        } else {
            quote! { None }
        }
    }

    fn impl_events_updation(&self) -> TokenStream {
        if self.events_meta.is_some() {
            quote! {
                // The events need to be updated regardless, there is no checking them.
                self.__events__ = __events__;
            }
        } else {
            quote!()
        }
    }

    fn impl_state_clone_from_status(&self) -> TokenStream {
        if self.state_meta.is_none() {
            quote!()
        } else {
            let state_field_idents = &self.expand_state_field_idents();

            if state_field_idents.len() == 1 {
                quote! {
                    let #(#state_field_idents)* = {
                        let status = __status__.borrow();
                        let state = status.state_as_ref();
                        #(state.#state_field_idents.clone())*
                    };
                }
            } else {
                quote! {
                    let (#(#state_field_idents),*) = {
                        let status = __status__.borrow();
                        let state = status.state_as_ref();
                        (#(state.#state_field_idents.clone()),*)
                    };
                }
            }
        }
    }

    fn impl_status_assignment(&self) -> TokenStream {
        if self.props_meta.is_none() && self.state_meta.is_none() {
            quote!()
        } else {
            quote!(__status__: __status__,)
        }
    }

    fn impl_event_assignment(&self) -> TokenStream {
        if self.events_meta.is_some() {
            quote! {
                __events__,
            }
        } else {
            quote!()
        }
    }
}

/// Stores the list of prop fields.
struct PropsMeta {
    /// Ident of props struct.
    ident: Ident,
    /// List of prop fields.
    fields: Vec<ComponentField>,
}

impl PropsMeta {
    fn take_prop_fields(
        component_ident: &Ident,
        fields: Vec<Field>,
    ) -> ParseResult<(Option<PropsMeta>, Vec<Field>)> {
        let (rest, prop_fields): (Vec<_>, Vec<_>) = fields.into_iter().partition(is_state);
        let prop_fields: ParseResult<Vec<_>> = prop_fields
            .into_iter()
            .map(ComponentField::parse_prop_field)
            .collect();
        let mut prop_fields = prop_fields?;
        prop_fields.sort_by(|l, r| l.ident.cmp(&r.ident));
        if prop_fields.is_empty() {
            Ok((None, rest))
        } else {
            let meta = PropsMeta {
                ident: Ident::new(
                    &format!("{}{}", component_ident, PROPS_SUFFIX),
                    Span::call_site(),
                ),
                fields: prop_fields,
            };
            Ok((Some(meta), rest))
        }
    }

    fn expand_fields_with<F>(&self, map_fn: F) -> Vec<TokenStream>
    where
        F: Fn(&ComponentField) -> TokenStream,
    {
        self.fields.iter().map(map_fn).collect()
    }

    fn create_props_struct_and_macro(&self, comp_ident: &Ident, vis: &Visibility) -> TokenStream {
        let ident = &self.ident;
        let fields = self.expand_fields_with(ComponentField::to_struct_field);
        let field_idents = self.expand_fields_with(ComponentField::to_ident);
        let field_default_vals =
            self.expand_fields_with(ComponentField::to_default_argument_for_macro);
        let mut next_idents = field_idents.clone();
        let first = next_idents.remove(0);
        next_idents.push(quote!(@finish));
        let internal_macro_ident =
            &Ident::new(&format!("__new_{}_internal__", ident), Span::call_site());

        let mut match_hands = vec![];
        for ((cur, next), default) in field_idents
            .iter()
            .zip(next_idents.iter())
            .zip(field_default_vals.iter())
        {
            match_hands.push(quote!{
                (
                    @#cur
                    arguments = [{ $($args:tt)* }]
                    tokens = [{ [#cur = $val:expr] $($rest:tt)* }]
                ) => {
                    #internal_macro_ident!(
                        @#next
                        arguments = [{ $($args)* [#cur = $val] }]
                        tokens = [{ $($rest)* }]
                    );
                },
                (
                    @#cur
                    arguments = [{ $($args:tt)* }]
                    tokens = [{ $($rest:tt)* }]
                ) => {
                    #internal_macro_ident!(
                        @#next
                        arguments = [{ $($args)* #default }]
                        tokens = [{ $($rest)* }]
                    );
                },
            });
        }
        quote! {
            #vis struct #ident {
                #(#fields),*
            }

            macro #internal_macro_ident {
                #(#match_hands)*
                (
                    @@finish
                    arguments = [{ $([$key:ident = $val:expr])* }]
                    tokens = [{ }]
                ) => {
                    #ident {
                        $($key: $val),*
                    }
                },
                (
                    @@finish
                    arguments = [{ $($tt:tt)* }]
                    tokens = [{ [$key:ident = $val:expr] $($rem:tt)* }]
                ) => {
                    compile_error!(concat!("There is no prop `", stringify!($key), "` on `", stringify!(#comp_ident), "`."));
                }
            }

            #vis macro #ident($($key:ident: $val:expr),*) {
                #internal_macro_ident!(
                    @#first
                    arguments = [{ }]
                    tokens = [{ $([$key = $val])* }]
                );
            }
        }
    }

    fn create_void_props_macro(comp_ident: &Ident, vis: &Visibility) -> TokenStream {
        let prop_ident = Ident::new(
            &format!("{}{}", comp_ident, PROPS_SUFFIX),
            Span::call_site(),
        );
        let internal_macro_ident = Ident::new(
            &format!("__new_{}_internal__", prop_ident),
            Span::call_site(),
        );
        quote! {
            macro #internal_macro_ident {
                (
                    tokens = [{ }]
                ) => {
                    // A void prop.
                    ()
                },
                (
                    tokens = [{ $($tt:tt)+ }]
                ) => {
                    compile_error!(concat!("`", stringify!(#comp_ident), "` has no props."));
                }
            }

            #vis macro #prop_ident($($key:ident: $val:expr),*) {
                #internal_macro_ident!(
                    tokens = [{ $([$key = $val])* }]
                );
            }
        }
    }
}

/// Stores the list of state fields.
struct StateMeta {
    /// Ident of state struct.
    ident: Ident,
    /// List of state fields.
    fields: Vec<ComponentField>,
}

impl StateMeta {
    fn take_state_fields(
        component_ident: &Ident,
        fields: Vec<Field>,
    ) -> ParseResult<(Option<StateMeta>, Vec<Field>)> {
        let (state_fields, rest): (Vec<_>, Vec<_>) = fields.into_iter().partition(is_state);
        let state_fields: ParseResult<Vec<_>> = state_fields
            .into_iter()
            .map(ComponentField::parse_state_field)
            .collect();
        let state_fields = state_fields?;
        if state_fields.is_empty() {
            Ok((None, rest))
        } else {
            let meta = StateMeta {
                ident: Ident::new(
                    &format!("{}{}", component_ident, STATE_SUFFIX),
                    Span::call_site(),
                ),
                fields: state_fields,
            };
            Ok((Some(meta), rest))
        }
    }

    fn expand_fields_with<F>(&self, map_fn: F) -> Vec<TokenStream>
    where
        F: Fn(&ComponentField) -> TokenStream,
    {
        self.fields.iter().map(map_fn).collect()
    }

    fn create_state_struct(&self) -> TokenStream {
        let ident = &self.ident;
        let fields = self.expand_fields_with(ComponentField::to_struct_field);
        let def_fields = self.expand_fields_with(ComponentField::to_field_assignment_as_default);

        quote! {
            struct #ident {
                #(#fields),*
            }

            impl Default for #ident {
                fn default() -> Self {
                    #ident {
                        #(#def_fields),*
                    }
                }
            }
        }
    }
}

custom_keyword!(default);

/// The argument passed with `#[prop]` or `#[state]` attributes.
///
/// Can be `#[prop]`, `#[prop(default)]` or `#[prop(default = expr)]`.
#[derive(Default)]
struct AttrArg {
    default: Option<DefaultArg>,
}

impl Parse for AttrArg {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        if input.is_empty() {
            return Ok(AttrArg::default());
        }

        let content;
        parenthesized!(content in input);

        let default = if content.peek(default) {
            Some(content.parse()?)
        } else {
            None
        };

        if !input.is_empty() {
            return Err(input.error("expected `)`."));
        }
        Ok(AttrArg { default })
    }
}

enum DefaultArg {
    Expr(Box<Expr>),
    Default,
}

impl Parse for DefaultArg {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        input.parse::<default>()?;
        if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            Ok(DefaultArg::Expr(Box::new(input.parse()?)))
        } else {
            Ok(DefaultArg::Default)
        }
    }
}

/// A field of a component. Stores all the struct metadata along with additional
/// default value expression.
struct ComponentField {
    attrs: Vec<Attribute>,
    attr_arg: AttrArg,
    is_optional: bool,
    vis: Visibility,
    ident: Ident,
    ty: Type,
}

impl ComponentField {
    fn is_optional(field: &Field) -> ParseResult<bool> {
        match field.ty {
            Type::Path(TypePath { ref path, .. }) => {
                let tokens = quote! { #path };
                let tokens = tokens.to_string().replace(' ', "");
                Ok(tokens.starts_with("Option<") && tokens.ends_with('>'))
            }
            Type::Reference(TypeReference { ref lifetime, .. }) => {
                let lifetime = lifetime.as_ref().expect("Lifetimes are always provided");
                Ok(lifetime.ident == Ident::new("static", Span::call_site()))
            }
            _ => Err(Error::new(field.ty.span(), "Type not supported")),
        }
    }

    fn parse_prop_field(field: Field) -> ParseResult<ComponentField> {
        let state_kind = Ident::new("state", Span::call_site()).into();
        let is_optional = Self::is_optional(&field)?;

        let (mut prop_attr, rest): (Vec<_>, Vec<_>) = field
            .attrs
            .into_iter()
            .partition(|attr| attr.path != state_kind);

        let attr_arg = if !prop_attr.is_empty() {
            syn::parse2(prop_attr.swap_remove(0).tts)?
        } else {
            AttrArg::default()
        };

        Ok(ComponentField {
            attrs: rest,
            attr_arg,
            is_optional,
            vis: field.vis,
            ident: field.ident.unwrap(),
            ty: field.ty,
        })
    }

    fn parse_state_field(field: Field) -> ParseResult<ComponentField> {
        let state_kind = Ident::new("state", Span::call_site()).into();
        let is_optional = Self::is_optional(&field)?;

        let (mut state_attr, rest): (Vec<_>, Vec<_>) = field
            .attrs
            .into_iter()
            .partition(|attr| attr.path == state_kind);

        let attr_arg = syn::parse2(state_attr.swap_remove(0).tts)?;

        Ok(ComponentField {
            attrs: rest,
            attr_arg,
            is_optional,
            vis: field.vis,
            ident: field.ident.unwrap(),
            ty: field.ty,
        })
    }

    fn to_struct_field(&self) -> TokenStream {
        let attrs = &self.attrs;
        let vis = &self.vis;
        let ident = &self.ident;
        let ty = &self.ty;
        quote! {
            #(#attrs)*
            #vis #ident: #ty
        }
    }

    fn to_field_assignment_as_default(&self) -> TokenStream {
        let ident = &self.ident;
        if let AttrArg {
            default: Some(DefaultArg::Expr(ref default)),
        } = self.attr_arg
        {
            quote! {
                #ident: #default
            }
        } else {
            quote! {
                #ident: Default::default()
            }
        }
    }

    fn to_ident(&self) -> TokenStream {
        let ident = &self.ident;
        quote! {
            #ident
        }
    }

    fn to_default_argument_for_macro(&self) -> TokenStream {
        let ident = &self.ident;
        if let AttrArg {
            default: Some(DefaultArg::Expr(ref default)),
        } = self.attr_arg
        {
            quote! {
                [ #ident = #default ]
            }
        } else if let Some(DefaultArg::Default) = self.attr_arg.default {
            quote! {
                [ #ident = Default::default() ]
            }
        } else if self.is_optional {
            quote! {
                [ #ident = Default::default() ]
            }
        } else {
            quote!()
        }
    }
}

/// The syntax for the `#[events]` attribute TokenStream.
///
/// i.e. Parses ```ignore,compile_fail
/// #[events(
///     fn event_name(&self, arg: type, ...) -> type;
///     fn event_name(&self, arg: type, ...) -> type;
///     fn event_name(&self, arg: type, ...) -> type;
/// )]```
struct EventsSyntax {
    events: Vec<EventSyntax>,
}

impl Parse for EventsSyntax {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let content;
        parenthesized!(content in input);

        let mut events = vec![];
        while !content.is_empty() {
            let event: EventSyntax = content.parse()?;
            events.push(event);
        }
        Ok(EventsSyntax { events })
    }
}

/// The syntax of a single event.
///
/// ```ignore,compile_fail
/// #[optional]
/// fn event_name(&self, arg: type, ...) -> type;
/// ```
struct EventSyntax {
    attrs: Vec<Attribute>,
    ident: Ident,
    args: Vec<FnArg>,
    return_type: ReturnType,
}

impl Parse for EventSyntax {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        input.parse::<Token![fn]>()?;
        let ident = input.parse()?;

        let content;
        parenthesized!(content in input);
        let args = content
            .parse_terminated::<_, Token![,]>(FnArg::parse)?
            .into_iter()
            .collect();

        let return_type = input.parse()?;
        // The `#[component]` macro was pointed to when the last one errored.
        if input.peek(Token![;]) {
            input.parse::<Token![;]>()?;
        } else {
            Err(input.error("expected `;`"))?;
        }

        Ok(EventSyntax {
            attrs,
            ident,
            args,
            return_type,
        })
    }
}

impl EventSyntax {
    fn attribute_span(&self) -> Option<Span> {
        self.attrs.get(0).map(|attr| attr.span())
    }
}

/// Stores all the events declared on a component.
struct EventsMeta {
    /// Ident of Event type stored in the component itself.
    ident: Ident,
    /// Ident of Event type which stores actual events passed from parent. It
    /// serves as an intermediate event type before converting it to the
    /// above event type.
    event_props_ident: Ident,
    /// All the event declarations on the component.
    events: Vec<EventMeta>,
}

impl EventsMeta {
    fn take_events_attributes(
        component_ident: &Ident,
        attrs: Vec<Attribute>,
    ) -> ParseResult<(Option<EventsMeta>, Vec<Attribute>)> {
        let events_kind = Ident::new("events", Span::call_site()).into();
        let (event_attrs, rest): (Vec<_>, Vec<_>) =
            attrs.into_iter().partition(|attr| attr.path == events_kind);

        let event_metas: ParseResult<Vec<Vec<EventMeta>>> = event_attrs
            .into_iter()
            .map(|attr| {
                let parsed: EventsSyntax = syn::parse2(attr.tts)?;
                let mut event_metas = vec![];

                for event in parsed.events {
                    // Grab the arguments pat and type from the event declaration while
                    // skipping the first `&self` argument.
                    let mut arguments = vec![];
                    for (index, arg) in event.args.iter().enumerate() {
                        match arg {
                            FnArg::SelfRef(_) if index == 0 => {}
                            FnArg::Captured(ref captured) if index > 0 => {
                                arguments.push((captured.pat.clone(), captured.ty.clone()));
                            }
                            _ if index == 0 => {
                                Err(Error::new(arg.span(), "expected `&self`"))?;
                            }
                            _ => {
                                Err(Error::new(arg.span(), "expected `: type`"))?;
                            }
                        }
                    }

                    if event.attrs.len() > 1 {
                        Err(Error::new(
                            event.attribute_span().unwrap(),
                            "Multiple attributes found. Only one allowed.",
                        ))?;
                    }

                    let is_optional = if let Some(ref attr) = event.attrs.get(0) {
                        if attr.path != Ident::new("optional", Span::call_site()).into() {
                            Err(Error::new(
                                attr.span(),
                                "Only `#[optional]` attribute allowed here.",
                            ))?;
                        };
                        true
                    } else {
                        false
                    };

                    let meta = EventMeta {
                        ident: event.ident,
                        arguments,
                        return_type: event.return_type,
                        is_optional,
                    };
                    event_metas.push(meta);
                }

                Ok(event_metas)
            }).collect();

        let mut event_metas: Vec<EventMeta> = event_metas?.into_iter().flatten().collect();
        event_metas.sort_by(|l, r| l.ident.cmp(&r.ident));

        if event_metas.is_empty() {
            Ok((None, rest))
        } else {
            let meta = EventsMeta {
                ident: Ident::new(
                    &format!("{}{}", component_ident, EVENT_SUFFIX),
                    Span::call_site(),
                ),
                event_props_ident: Ident::new(
                    &format!("{}{}", component_ident, EVENT_PROPS_SUFFIX),
                    Span::call_site(),
                ),
                events: event_metas,
            };
            Ok((Some(meta), rest))
        }
    }

    fn expand_events_with<F>(&self, map_fn: F) -> Vec<TokenStream>
    where
        F: Fn(&EventMeta) -> TokenStream,
    {
        self.events.iter().map(map_fn).collect()
    }

    fn create_events_and_event_props_struct_and_macro(
        &self,
        component_ident: &Ident,
        vis: &Visibility,
    ) -> TokenStream {
        let ident = &self.ident;
        let event_props_ident = &self.event_props_ident;
        let fields = self.expand_events_with(EventMeta::to_event_field);
        let gen_fields = self.expand_events_with(EventMeta::to_event_prop_field);
        let event_names = &self.expand_events_with(EventMeta::to_event_ident);
        let event_conversion =
            self.expand_events_with(EventMeta::impl_event_conversion_from_event_prop);
        let event_wrappers = self.expand_events_with(|e| e.impl_event_wrapper(component_ident));

        let mut next_event_names = event_names.clone();
        let first = next_event_names.remove(0);
        next_event_names.push(quote!(@finish));

        let events_assignment = self.expand_events_with(EventMeta::to_event_assignment_for_macro);
        let events_default_val =
            self.expand_events_with(EventMeta::to_event_assignment_as_default_value_for_macro);

        let macro_internal_ident =
            &Ident::new(&format!("__new_{}_internal__", ident), Span::call_site());

        let mut match_hands = vec![];
        for ((cur, next), (assignment, default)) in event_names
            .iter()
            .zip(next_event_names.iter())
            .zip(events_assignment.iter().zip(events_default_val.iter()))
        {
            match_hands.push(quote!{
                (
                    @#cur
                    arguments = [{ $($args:tt)* }]
                    tokens = [{ [#cur = $val:expr] $($rest:tt)* }]
                ) => {
                    #macro_internal_ident!(
                        @#next
                        arguments = [{ $($args)* #assignment }]
                        tokens = [{ $($rest)* }]
                    );
                },
                (
                    @#cur
                    arguments = [{ $($args:tt)* }]
                    tokens = [{ $($rest:tt)* }]
                ) => {
                    #macro_internal_ident!(
                        @#next
                        arguments = [{ $($args)* #default }]
                        tokens = [{ $($rest)* }]
                    );
                },
            });
        }

        quote! {
            #vis struct #ident {
                #(#fields),*
            }

            #vis struct #event_props_ident<RCTX: Render> {
                #(#gen_fields),*
            }

            impl<RCTX: Render> ruukh::component::FromEventProps<RCTX> for #ident {
                type From = #event_props_ident<RCTX>;

                fn from(
                    __rctx_events__: Self::From,
                    __render_ctx__: std::rc::Rc<std::cell::RefCell<RCTX>>
                ) -> Self {
                    #(#event_conversion)*

                    #ident {
                        #(#event_names),*
                    }
                }
            }

            #(#event_wrappers)*

            macro #macro_internal_ident {
                #(#match_hands)*
                (
                    @@finish
                    arguments = [{ $([$key:ident = $val:expr])* }]
                    tokens = [{ }]
                ) => {
                    #event_props_ident {
                        $($key: $val),*
                    }
                },
                (
                    @@finish
                    arguments = [{ $($tt:tt)* }]
                    tokens = [{ [$key:ident = $val:expr] $($rem:tt)* }]
                ) => {
                    compile_error!(concat!("There is no event `", stringify!($key), "` on `", stringify!(#component_ident), "`."));
                }
            }

            #vis macro #ident($($key:ident: $val:expr),*) {
                #macro_internal_ident!(
                    @#first
                    arguments = [{ }]
                    tokens = [{ $([$key = $val])* }]
                );
            }
        }
    }

    fn create_void_events_macro(comp_ident: &Ident, vis: &Visibility) -> TokenStream {
        let event_ident = Ident::new(
            &format!("{}{}", comp_ident, EVENT_SUFFIX),
            Span::call_site(),
        );
        let internal_macro_ident = Ident::new(
            &format!("__new_{}_internal__", event_ident),
            Span::call_site(),
        );
        quote! {
            macro #internal_macro_ident {
                (
                    tokens = [{ }]
                ) => {
                    // A void event.
                    ()
                },
                (
                    tokens = [{ $($tt:tt)+ }]
                ) => {
                    compile_error!(concat!("`", stringify!(#comp_ident), "` has no events."));
                }
            }

            #vis macro #event_ident($($key:ident: $val:expr),*) {
                #internal_macro_ident!(
                    tokens = [{ $([$key = $val])* }]
                );
            }
        }
    }
}

/// A single event metadata.
struct EventMeta {
    /// Event name.
    ident: Ident,
    /// Name and type pair of the argument list.
    arguments: Vec<(Pat, Type)>,
    /// Return type of the event.
    return_type: ReturnType,
    /// Whether the event is optional.
    is_optional: bool,
}

impl EventMeta {
    fn arg_types(&self) -> Vec<TokenStream> {
        self.arguments.iter().map(|(_, ty)| quote!(#ty)).collect()
    }

    fn fn_type(&self) -> TokenStream {
        let arg_types = self.arg_types();
        let ret_type = &self.return_type;
        quote! {
            Fn(#(#arg_types),*) #ret_type
        }
    }

    fn fn_type_with_opt_ret(&self) -> TokenStream {
        let arg_types = self.arg_types();
        match self.return_type {
            ReturnType::Default => quote! {
                Fn(#(#arg_types),*) -> Option<()>
            },
            ReturnType::Type(_, ref ty) => quote! {
                Fn(#(#arg_types),*) -> Option<#ty>
            },
        }
    }

    fn prop_fn_type(&self) -> TokenStream {
        let arg_types = self.arg_types();
        let ret_type = &self.return_type;
        quote! {
            Fn(&RCTX, #(#arg_types),*) #ret_type
        }
    }

    fn to_event_ident(&self) -> TokenStream {
        let ident = &self.ident;
        quote! {
            #ident
        }
    }

    fn to_event_field(&self) -> TokenStream {
        let ident = &self.ident;

        let fn_type = if self.is_optional {
            self.fn_type_with_opt_ret()
        } else {
            self.fn_type()
        };
        quote! {
            #ident: Box<#fn_type>
        }
    }

    fn to_event_prop_field(&self) -> TokenStream {
        let ident = &self.ident;
        let fn_type = self.prop_fn_type();
        if self.is_optional {
            quote! {
                #ident: Option<Box<#fn_type>>
            }
        } else {
            quote! {
                #ident: Box<#fn_type>
            }
        }
    }

    fn to_arg_fields(&self) -> Vec<TokenStream> {
        self.arguments
            .iter()
            .map(|(pat, ty)| quote!( #pat: #ty ))
            .collect()
    }

    fn to_arg_idents(&self) -> Vec<TokenStream> {
        self.arguments
            .iter()
            .map(|(pat, _)| quote!( #pat ))
            .collect()
    }

    fn impl_event_conversion_from_event_prop(&self) -> TokenStream {
        let ident = &self.ident;
        let event_arg_idents = &self.to_arg_idents();

        let converter = if self.is_optional {
            quote! {
                if let Some(ref #ident) = #ident {
                    Some((#ident)(&*__rctx__.borrow(), #(#event_arg_idents),*))
                } else {
                    None
                }
            }
        } else {
            quote! {
                (#ident)(&*__rctx__.borrow(), #(#event_arg_idents),*)
            }
        };

        quote! {
            let #ident = {
                let __rctx__ = __render_ctx__.clone();
                let #ident = __rctx_events__.#ident;
                Box::new(move |#(#event_arg_idents),*| {
                    #converter
                })
            };
        }
    }

    fn to_return_type(&self) -> TokenStream {
        match self.return_type {
            ReturnType::Default => if self.is_optional {
                quote! {
                    -> Option<()>
                }
            } else {
                quote!()
            },
            ReturnType::Type(_, ref ty) => if self.is_optional {
                quote! {
                    -> Option<#ty>
                }
            } else {
                quote! {
                    -> #ty
                }
            },
        }
    }

    fn impl_event_wrapper(&self, component_ident: &Ident) -> TokenStream {
        let ident = &self.ident;
        let arg_fields = self.to_arg_fields();
        let arg_idents = self.to_arg_idents();
        let ret_type = self.to_return_type();

        quote! {
            impl #component_ident {
                fn #ident (&self, #(#arg_fields),*) #ret_type {
                    (self.__events__.#ident)(#(#arg_idents),*)
                }
            }
        }
    }

    fn to_event_assignment_as_default_value_for_macro(&self) -> TokenStream {
        let ident = &self.ident;
        if self.is_optional {
            quote! {
                [#ident = None]
            }
        } else {
            quote!()
        }
    }

    fn to_event_assignment_for_macro(&self) -> TokenStream {
        let ident = &self.ident;
        if self.is_optional {
            quote! {
                [#ident = Some(Box::new($val))]
            }
        } else {
            quote! {
                [#ident = Box::new($val)]
            }
        }
    }
}

fn is_state(field: &Field) -> bool {
    let state_kind = Ident::new("state", Span::call_site()).into();
    let state_attrs = field
        .attrs
        .iter()
        .filter(|attr| attr.path == state_kind)
        .collect::<Vec<_>>();

    !state_attrs.is_empty()
}

fn check_supported_attributes(field: &Field) -> ParseResult<()> {
    let prop_kind = Ident::new("prop", Span::call_site()).into();
    let state_kind = Ident::new("state", Span::call_site()).into();
    let prop_attrs = field
        .attrs
        .iter()
        .filter(|attr| attr.path == prop_kind)
        .collect::<Vec<_>>();
    let state_attrs = field
        .attrs
        .iter()
        .filter(|attr| attr.path == state_kind)
        .collect::<Vec<_>>();

    if !state_attrs.is_empty() && !prop_attrs.is_empty() {
        Err(Error::new(
            field.ident.span(),
            "Cannot be both `#[prop]` and `#[state]` at once.",
        ))?;
    }
    if prop_attrs.len() > 1 {
        Err(Error::new(
            field.ident.span(),
            "Cannot have multiple `#[prop]` attributes.",
        ))?;
    }
    if state_attrs.len() > 1 {
        Err(Error::new(
            field.ident.span(),
            "Cannot have multiple `#[state]` attributes.",
        ))?;
    }

    Ok(())
}
