use proc_macro2::{Span, TokenStream};
use syn;
use syn::parse::{Error, Parse, ParseStream, Result as ParseResult};
use syn::spanned::Spanned;
use syn::{
    Attribute, Expr, Field, Fields, FnArg, Generics, Ident, ItemStruct, Pat, ReturnType, Type,
    TypePath, Visibility,
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
    /// Generics part of component declaration.
    generics: Generics,
    /// Props metadata if any prop fields.
    props_meta: Option<PropsMeta>,
    /// State metadata if any state fields.
    state_meta: Option<StateMeta>,
    /// Events metadata if any events declaration.
    events_meta: Option<EventsMeta>,
}

impl ComponentMeta {
    pub fn parse(item: ItemStruct) -> ParseResult<ComponentMeta> {
        // Sort out the struct fields into state and props fields.
        let (props_meta, state_meta) = match item.fields {
            Fields::Named(fields) => {
                let fields: Vec<_> = fields.named.into_iter().collect();

                for field in fields.iter() {
                    check_supported_attributes(field)?;
                }

                let (props_meta, fields) = PropsMeta::parse(&item.ident, fields)?;
                let (state_meta, fields) = StateMeta::parse(&item.ident, fields)?;
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

        let component_name = Ident::new("component", Span::call_site()).into();
        let attrs: Vec<_> = item
            .attrs
            .into_iter()
            .filter(|attr| attr.path != component_name)
            .collect();

        let (events_meta, attrs) = EventsMeta::parse(&item.ident, attrs)?;

        Ok(ComponentMeta {
            attrs,
            vis: item.vis,
            ident: item.ident,
            generics: item.generics,
            props_meta,
            state_meta,
            events_meta,
        })
    }

    pub fn expand(&self) -> TokenStream {
        let component_struct = self.expand_component_struct();
        let component_impl = self.expand_component_impl();
        let state_struct = self.state_meta.as_ref().map(StateMeta::expand_struct);
        let prop_struct = self
            .props_meta
            .as_ref()
            .map(|m| m.expand_struct(&self.vis, &self.generics));
        let events_structs = self
            .events_meta
            .as_ref()
            .map(|m| m.expand_structs(&self.ident, &self.vis, &self.generics));

        quote! {
            #component_struct

            #state_struct

            #prop_struct

            #events_structs

            #component_impl
        }
    }

    fn expand_component_struct(&self) -> TokenStream {
        let attrs = &self.attrs;
        let ident = &self.ident;
        let vis = &self.vis;
        let generics = &self.generics;

        if self.props_meta.is_none() && self.state_meta.is_none() && self.events_meta.is_none() {
            quote! {
                #(#attrs)*
                #vis struct #ident #generics;
            }
        } else {
            let state_fields = self
                .state_meta
                .as_ref()
                .map(|s| s.expand_fields_with(ComponentField::expand_as_struct_field))
                .unwrap_or_default();
            let props_fields = self
                .props_meta
                .as_ref()
                .map(|p| p.expand_fields_with(ComponentField::expand_as_struct_field))
                .unwrap_or_default();
            let status_field = self.expand_status_field();
            let events_field = self.expand_events_field();

            quote! {
                #(#attrs)*
                #vis struct #ident #generics {
                    #(#state_fields ,)*
                    #(#props_fields ,)*
                    #status_field
                    #events_field
                }
            }
        }
    }

    fn expand_status_field(&self) -> TokenStream {
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
                __status__: ruukh::Shared<ruukh::component::Status<#ident>>,
            }
        }
    }

    fn expand_events_field(&self) -> TokenStream {
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
            .map(|s| s.expand_fields_with(ComponentField::expand_as_ident))
            .unwrap_or_default()
    }

    fn expand_props_field_idents(&self) -> Vec<TokenStream> {
        self.props_meta
            .as_ref()
            .map(|p| p.expand_fields_with(ComponentField::expand_as_ident))
            .unwrap_or_default()
    }

    fn expand_component_impl(&self) -> TokenStream {
        let ident = &self.ident;
        let (impl_gen, ty_gen, where_clause) = self.generics.split_for_impl();
        let props_ident = self.expand_props_ident();
        let state_ident = self.expand_state_ident();
        let events_ident = self.expand_events_ident();
        let state_clone = self.expand_state_clone();
        let build_event = self.expand_build_event();
        let state_field_idents = &self.expand_state_field_idents();
        let props_field_idents = &self.expand_props_field_idents();
        let props_field_idents2 = props_field_idents;
        let status_assignment = self.expand_status_assignment();
        let props_updation = self.expand_props_updation(props_field_idents);
        let updation_ret_block = self.expand_updation_return_block();
        let events_updation = self.expand_events_updation();
        let refresh_state_body = self.expand_refresh_state_body(state_field_idents);
        let is_state_dirty_body = self.expand_is_state_dirty_body();
        let is_props_dirty_body = self.expand_is_props_dirty_body();
        let set_state_body = self.expand_set_state_body(state_field_idents);

        quote! {
            impl #impl_gen Component for #ident #ty_gen #where_clause {
                type Props = #props_ident #ty_gen;
                type State = #state_ident;
                type Events = #events_ident;

                fn init<RCTX: Render>(
                    __props__: Self::Props,
                    __events__: <Self::Events as ruukh::component::EventsPair<RCTX>>::Other,
                    __status__: ruukh::Shared<ruukh::component::Status<Self::State>>,
                    __render_ctx__: ruukh::Shared<RCTX>,
                ) -> Self
                where
                    Self::Events: ruukh::component::EventsPair<RCTX>
                {
                    #state_clone

                    #ident {
                        #(#props_field_idents: __props__.#props_field_idents2 ,)*
                        #(#state_field_idents ,)*
                        #build_event
                        #status_assignment
                    }
                }

                fn update<RCTX: Render>(
                    &mut self,
                    mut __props__: Self::Props,
                    __events__: <Self::Events as ruukh::component::EventsPair<RCTX>>::Other,
                    __render_ctx__: ruukh::Shared<RCTX>,
                ) -> Option<Self::Props>
                where
                    Self::Events: ruukh::component::EventsPair<RCTX>
                {

                    #props_updation

                    #events_updation

                    #updation_ret_block
                }

                fn refresh_state(&mut self) {
                    #refresh_state_body
                }

                fn is_state_dirty(&self) -> bool {
                    #is_state_dirty_body
                }

                fn is_props_dirty(&self) -> bool {
                    #is_props_dirty_body
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

    fn expand_set_state_body(&self, idents: &Vec<TokenStream>) -> TokenStream {
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

    fn expand_is_props_dirty_body(&self) -> TokenStream {
        if self.props_meta.is_some() {
            quote! {
                self.__status__.borrow_mut().is_props_dirty()
            }
        } else {
            quote! { false }
        }
    }

    fn expand_is_state_dirty_body(&self) -> TokenStream {
        if self.state_meta.is_some() {
            quote! {
                self.__status__.borrow_mut().is_state_dirty()
            }
        } else {
            quote! { false }
        }
    }

    fn expand_refresh_state_body(&self, idents: &Vec<TokenStream>) -> TokenStream {
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

    fn expand_props_updation(&self, idents: &Vec<TokenStream>) -> TokenStream {
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

    fn expand_updation_return_block(&self) -> TokenStream {
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

    fn expand_events_updation(&self) -> TokenStream {
        if let Some(ref events_meta) = self.events_meta {
            let ident = &events_meta.ident;
            quote! {
                // The events need to be updated regardless, there is no checking them.
                self.__events__ = #ident::build(__events__, __render_ctx__);
            }
        } else {
            quote!()
        }
    }

    fn expand_state_clone(&self) -> TokenStream {
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

    fn expand_status_assignment(&self) -> TokenStream {
        if self.props_meta.is_none() && self.state_meta.is_none() {
            quote!()
        } else {
            quote!(__status__: __status__,)
        }
    }

    fn expand_build_event(&self) -> TokenStream {
        if let Some(ref events_meta) = self.events_meta {
            let ident = &events_meta.ident;
            quote! {
                __events__: #ident::build(__events__, __render_ctx__),
            }
        } else {
            quote!()
        }
    }

    fn expand_props_ident(&self) -> TokenStream {
        self.props_meta
            .as_ref()
            .map(|m| {
                let ident = &m.ident;
                quote!( #ident )
            }).unwrap_or(quote!(()))
    }

    fn expand_state_ident(&self) -> TokenStream {
        self.state_meta
            .as_ref()
            .map(|m| {
                let ident = &m.ident;
                quote!( #ident )
            }).unwrap_or(quote!(()))
    }

    fn expand_events_ident(&self) -> TokenStream {
        self.events_meta
            .as_ref()
            .map(|m| {
                let ident = &m.ident;
                quote!( #ident )
            }).unwrap_or(quote!(()))
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
    fn parse(
        component_ident: &Ident,
        fields: Vec<Field>,
    ) -> ParseResult<(Option<PropsMeta>, Vec<Field>)> {
        let (rest, prop_fields): (Vec<_>, Vec<_>) = fields.into_iter().partition(is_state);
        let prop_fields: ParseResult<Vec<_>> = prop_fields
            .into_iter()
            .map(ComponentField::parse_prop)
            .collect();
        let prop_fields = prop_fields?;
        if prop_fields.is_empty() {
            Ok((None, rest))
        } else {
            let meta = PropsMeta {
                ident: Ident::new(&format!("{}Props", component_ident), Span::call_site()),
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

    fn expand_struct(&self, vis: &Visibility, generics: &Generics) -> TokenStream {
        let ident = &self.ident;
        let fields = self.expand_fields_with(ComponentField::expand_as_struct_field);

        quote! {
            #vis struct #ident #generics {
                #(#fields),*
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
    fn parse(
        component_ident: &Ident,
        fields: Vec<Field>,
    ) -> ParseResult<(Option<StateMeta>, Vec<Field>)> {
        let (state_fields, rest): (Vec<_>, Vec<_>) = fields.into_iter().partition(is_state);
        let state_fields: ParseResult<Vec<_>> = state_fields
            .into_iter()
            .map(ComponentField::parse_state)
            .collect();
        let state_fields = state_fields?;
        if state_fields.is_empty() {
            Ok((None, rest))
        } else {
            let meta = StateMeta {
                ident: Ident::new(&format!("{}State", component_ident), Span::call_site()),
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

    fn expand_struct(&self) -> TokenStream {
        let ident = &self.ident;
        let fields = self.expand_fields_with(ComponentField::expand_as_struct_field);
        let def_fields = self.expand_fields_with(ComponentField::expand_as_default_field);

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

/// The argument passed with `#[prop]` or `#[state]` attributes.
///
/// Can be `#[prop]`, `#[prop(default)]` or `#[prop(default = expr)]`.
#[derive(Default)]
struct AttrArg {
    use_default: bool,
    default: Option<Expr>,
}

impl Parse for AttrArg {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        if input.is_empty() {
            return Ok(AttrArg::default());
        }

        let content;
        parenthesized!(content in input);

        custom_keyword!(default);
        content.parse::<default>()?;

        if content.peek(Token![=]) {
            content.parse::<Token![=]>()?;
            let default = content.parse::<Expr>()?;
            Ok(AttrArg {
                use_default: true,
                default: Some(default),
            })
        } else {
            if content.is_empty() {
                Ok(AttrArg {
                    use_default: true,
                    default: None,
                })
            } else {
                Err(content.error("expected ')'."))
            }
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
                Ok(tokens.starts_with("Option<") && tokens.ends_with(">"))
            }
            _ => Err(Error::new(field.ty.span(), "Type not supported")),
        }
    }

    fn parse_prop(field: Field) -> ParseResult<ComponentField> {
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

    fn parse_state(field: Field) -> ParseResult<ComponentField> {
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

    fn expand_as_struct_field(&self) -> TokenStream {
        let attrs = &self.attrs;
        let vis = &self.vis;
        let ident = &self.ident;
        let ty = &self.ty;
        quote! {
            #(#attrs)*
            #vis #ident: #ty
        }
    }

    fn expand_as_default_field(&self) -> TokenStream {
        let ident = &self.ident;
        if let AttrArg {
            default: Some(ref default),
            ..
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

    fn expand_as_ident(&self) -> TokenStream {
        let ident = &self.ident;
        quote! {
            #ident
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
    fn parse(input: ParseStream) -> ParseResult<Self> {
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
    fn parse(input: ParseStream) -> ParseResult<Self> {
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
    gen_ident: Ident,
    /// Ident of the builder type which builds `gen_ident`.
    builder_ident: Ident,
    /// All the event declarations on the component.
    events: Vec<EventMeta>,
}

impl EventsMeta {
    fn parse(
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

        let event_metas: Vec<EventMeta> = event_metas?.into_iter().flatten().collect();

        if event_metas.is_empty() {
            Ok((None, rest))
        } else {
            let meta = EventsMeta {
                ident: Ident::new(&format!("{}Events", component_ident), Span::call_site()),
                gen_ident: Ident::new(&format!("{}EventsGen", component_ident), Span::call_site()),
                builder_ident: Ident::new(
                    &format!("{}EventsBuilder", component_ident),
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

    fn expand_structs(
        &self,
        component_ident: &Ident,
        vis: &Visibility,
        generics: &Generics,
    ) -> TokenStream {
        let ident = &self.ident;
        let gen_ident = &self.gen_ident;
        let fields = self.expand_events_with(EventMeta::expand_as_struct_field);
        let gen_fields = self.expand_events_with(EventMeta::expand_as_gen_struct_field);
        let event_names = &self.expand_events_with(EventMeta::expand_as_ident);
        let event_conversion = self.expand_events_with(EventMeta::expand_event_conversion);
        let event_wrappers =
            self.expand_events_with(|e| e.expand_event_wrapper(component_ident, generics));

        let builder_ident = &self.builder_ident;
        let builder_fields = self.expand_events_with(EventMeta::expand_as_builder_struct_field);
        let builder_assignment = self.expand_events_with(EventMeta::expand_builder_assignment);
        let builder_finish_assignment =
            self.expand_events_with(EventMeta::expand_builder_finish_assignment);

        quote! {
            #vis struct #ident {
                #(#fields),*
            }

            impl #ident {
                fn build<RCTX: Render>(
                    __rctx_events__: <Self as ruukh::component::EventsPair<RCTX>>::Other,
                    __render_ctx__: ruukh::Shared<RCTX>,
                ) -> Self
                {
                    #(#event_conversion)*

                    #ident {
                        #(#event_names),*
                    }
                }
            }

            #vis struct #gen_ident<RCTX: Render> {
                #(#gen_fields),*
            }

            impl<RCTX: Render> ruukh::component::BuilderCreator for #gen_ident<RCTX> {
                type Builder = #builder_ident<RCTX>;

                fn builder() -> Self::Builder {
                    Default::default()
                }
            }

            impl<RCTX: Render> ruukh::component::EventsPair<RCTX> for #ident {
                type Other = #gen_ident<RCTX>;
            }

            #(#event_wrappers)*

            #vis struct #builder_ident<RCTX: Render> {
                #(#builder_fields),*
            }

            impl<RCTX: Render> Default for #builder_ident<RCTX> {
                fn default() -> Self {
                    #builder_ident {
                        #(#event_names: None),*
                    }
                }
            }

            impl<RCTX: Render> #builder_ident<RCTX> {
                #(#builder_assignment)*
            }

            impl<RCTX: Render> ruukh::component::BuilderFinisher for #builder_ident<RCTX> {
                type Built = #gen_ident<RCTX>;

                fn finish(self) -> Self::Built {
                    #gen_ident {
                        #(#builder_finish_assignment),*
                    }
                }
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

    fn gen_fn_type(&self) -> TokenStream {
        let arg_types = self.arg_types();
        let ret_type = &self.return_type;
        quote! {
            Fn(&RCTX, #(#arg_types),*) #ret_type
        }
    }

    fn expand_as_ident(&self) -> TokenStream {
        let ident = &self.ident;
        quote! {
            #ident
        }
    }

    fn expand_as_struct_field(&self) -> TokenStream {
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

    fn expand_as_gen_struct_field(&self) -> TokenStream {
        let ident = &self.ident;
        let fn_type = self.gen_fn_type();
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

    fn expand_as_builder_struct_field(&self) -> TokenStream {
        let ident = &self.ident;
        let fn_type = self.gen_fn_type();
        quote! {
            #ident: Option<Box<#fn_type>>
        }
    }

    fn expand_as_arg_fields(&self) -> Vec<TokenStream> {
        self.arguments
            .iter()
            .map(|(pat, ty)| quote!( #pat: #ty ))
            .collect()
    }

    fn expand_as_arg_idents(&self) -> Vec<TokenStream> {
        self.arguments
            .iter()
            .map(|(pat, _)| quote!( #pat ))
            .collect()
    }

    fn expand_event_conversion(&self) -> TokenStream {
        let ident = &self.ident;
        let event_arg_idents = &self.expand_as_arg_idents();

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

    fn expand_as_return_type(&self) -> TokenStream {
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

    fn expand_event_wrapper(&self, component_ident: &Ident, generics: &Generics) -> TokenStream {
        let ident = &self.ident;
        let (impl_gen, ty_gen, where_clause) = generics.split_for_impl();
        let arg_fields = self.expand_as_arg_fields();
        let arg_idents = self.expand_as_arg_idents();
        let ret_type = self.expand_as_return_type();

        quote! {
            impl #impl_gen #component_ident #ty_gen #where_clause {
                fn #ident (&self, #(#arg_fields),*) #ret_type {
                    (self.__events__.#ident)(#(#arg_idents),*)
                }
            }
        }
    }

    fn expand_builder_assignment(&self) -> TokenStream {
        let ident = &self.ident;
        let fn_type = self.gen_fn_type();
        quote! {
            pub fn #ident(mut self, val: Box<#fn_type>) -> Self {
                self.#ident = Some(val);
                self
            }
        }
    }

    fn expand_builder_finish_assignment(&self) -> TokenStream {
        let ident = &self.ident;

        if self.is_optional {
            quote! {
                #ident: self.#ident
            }
        } else {
            quote! {
                #ident: self.#ident.expect(&format!("The event `{}` is required.", stringify!(#ident)))
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
