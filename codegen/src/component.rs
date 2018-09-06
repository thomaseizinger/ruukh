use proc_macro2::{Span, TokenStream};
use syn;
use syn::punctuated::Punctuated;
use syn::synom::Synom;
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
    pub fn parse(item: ItemStruct) -> ComponentMeta {
        // Sort out the struct fields into state and props fields.
        let (props_meta, state_meta) = match item.fields {
            Fields::Named(fields) => {
                let fields: Vec<_> = fields.named.into_iter().collect();
                let (props_meta, fields) = PropsMeta::parse(&item.ident, fields);
                let (state_meta, fields) = StateMeta::parse(&item.ident, fields);
                assert!(
                    fields.is_empty(),
                    "There are no fields left other than prop and state fields."
                );
                (props_meta, state_meta)
            }
            Fields::Unnamed(_) => {
                panic!("Only unit structs and structs with named fields can be components.");
            }
            Fields::Unit => (None, None),
        };

        let component_name = Ident::new("component", Span::call_site()).into();
        let attrs: Vec<_> = item
            .attrs
            .into_iter()
            .filter(|attr| attr.path != component_name)
            .collect();

        let (events_meta, attrs) = EventsMeta::parse(&item.ident, attrs);

        ComponentMeta {
            attrs,
            vis: item.vis,
            ident: item.ident,
            generics: item.generics,
            props_meta,
            state_meta,
            events_meta,
        }
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
                .map(StateMeta::expand_as_struct_fields)
                .unwrap_or_default();
            let props_fields = self
                .props_meta
                .as_ref()
                .map(PropsMeta::expand_as_struct_fields)
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

    fn expand_state_field_idents(&self) -> Vec<&Ident> {
        self.state_meta
            .as_ref()
            .map(StateMeta::expand_as_field_idents)
            .unwrap_or_default()
    }

    fn expand_props_field_idents(&self) -> Vec<&Ident> {
        self.props_meta
            .as_ref()
            .map(PropsMeta::expand_as_field_idents)
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
                type Props = #props_ident;
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

    fn expand_set_state_body(&self, idents: &Vec<&Ident>) -> TokenStream {
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

    fn expand_refresh_state_body(&self, idents: &Vec<&Ident>) -> TokenStream {
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

    fn expand_props_updation(&self, idents: &Vec<&Ident>) -> TokenStream {
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
    /// Ident of props builder struct.
    builder_ident: Ident,
    /// List of prop fields.
    fields: Vec<ComponentField>,
}

impl PropsMeta {
    fn parse(component_ident: &Ident, fields: Vec<Field>) -> (Option<PropsMeta>, Vec<Field>) {
        let (prop_fields, rest): (Vec<_>, Vec<_>) = fields.into_iter().partition(is_prop);
        let prop_fields: Vec<_> = prop_fields
            .into_iter()
            .map(ComponentField::parse_prop)
            .collect();
        if prop_fields.is_empty() {
            (None, rest)
        } else {
            let meta = PropsMeta {
                ident: Ident::new(&format!("{}Props", component_ident), Span::call_site()),
                builder_ident: Ident::new(
                    &format!("{}PropsBuilder", component_ident),
                    Span::call_site(),
                ),
                fields: prop_fields,
            };
            (Some(meta), rest)
        }
    }

    fn expand_as_struct_fields(&self) -> Vec<TokenStream> {
        self.fields
            .iter()
            .map(ComponentField::expand_as_struct_field)
            .collect()
    }

    fn expand_as_builder_struct_fields(&self) -> Vec<TokenStream> {
        self.fields
            .iter()
            .map(ComponentField::expand_as_builder_struct_field)
            .collect()
    }

    fn expand_as_field_idents(&self) -> Vec<&Ident> {
        self.fields.iter().map(|field| &field.ident).collect()
    }

    fn expand_builder_assignment(&self) -> Vec<TokenStream> {
        self.fields
            .iter()
            .map(ComponentField::expand_builder_assignment)
            .collect()
    }

    fn expand_builder_finish_assignment(&self) -> Vec<TokenStream> {
        self.fields
            .iter()
            .map(ComponentField::expand_builder_finish_assignment)
            .collect()
    }

    fn expand_struct(&self, vis: &Visibility, generics: &Generics) -> TokenStream {
        let ident = &self.ident;
        let fields = self.expand_as_struct_fields();
        let (impl_gen, ty_gen, where_clause) = generics.split_for_impl();

        let builder_ident = &self.builder_ident;
        let builder_fields = self.expand_as_builder_struct_fields();
        let builder_assignment = self.expand_builder_assignment();
        let builder_finish_assignment = self.expand_builder_finish_assignment();

        quote! {
            #vis struct #ident #generics {
                #(#fields),*
            }

            impl #impl_gen #ident #ty_gen #where_clause {
                pub fn builder() -> #builder_ident #ty_gen {
                    Default::default()
                }
            }

            #[derive(Default)]
            #vis struct #builder_ident #generics {
                #(#builder_fields),*
            }

            impl #impl_gen #builder_ident #ty_gen #where_clause {
                #(#builder_assignment)*

                // Underscored, so it is unlikely to colide with field names.
                pub fn __finish__(self) -> #ident #ty_gen {
                    #ident {
                        #(#builder_finish_assignment),*
                    }
                }
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
    fn parse(component_ident: &Ident, fields: Vec<Field>) -> (Option<StateMeta>, Vec<Field>) {
        let (rest, state_fields): (Vec<_>, Vec<_>) = fields.into_iter().partition(is_prop);
        let state_fields: Vec<_> = state_fields
            .into_iter()
            .map(ComponentField::parse_state)
            .collect();
        if state_fields.is_empty() {
            (None, rest)
        } else {
            let meta = StateMeta {
                ident: Ident::new(&format!("{}State", component_ident), Span::call_site()),
                fields: state_fields,
            };
            (Some(meta), rest)
        }
    }

    fn expand_as_struct_fields(&self) -> Vec<TokenStream> {
        self.fields
            .iter()
            .map(ComponentField::expand_as_struct_field)
            .collect()
    }

    fn expand_as_default_fields(&self) -> Vec<TokenStream> {
        self.fields
            .iter()
            .map(ComponentField::expand_as_default_field)
            .collect()
    }

    fn expand_as_field_idents(&self) -> Vec<&Ident> {
        self.fields.iter().map(|field| &field.ident).collect()
    }

    fn expand_struct(&self) -> TokenStream {
        let ident = &self.ident;
        let fields = self.expand_as_struct_fields();
        let def_fields = self.expand_as_default_fields();

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
/// Can be `#[prop]` or `#[prop(default = expr)]`.
enum AttrArg {
    None,
    Some { default: Expr },
}

impl Synom for AttrArg {
    named!(parse -> Self, alt!(
        input_end!() => {|_| AttrArg::None}
        |
        parens!(do_parse!(
            custom_keyword!(default) >>
            syn!(Token!(=)) >>
            expr: syn!(Expr) >>
            ( expr )
        )) => {|(_, expr)| AttrArg::Some { default: expr }}
    ));
}

/// A field of a component. Stores all the struct metadata along with additional
/// default value expression.
struct ComponentField {
    attrs: Vec<Attribute>,
    default: Option<Expr>,
    is_optional: bool,
    vis: Visibility,
    ident: Ident,
    ty: Type,
}

impl ComponentField {
    fn parse_default(tts: TokenStream, kind: &str) -> Option<Expr> {
        let attr_arg: AttrArg = syn::parse2(tts).expect(&format!(
            "The attribute can only be one of these: `#[{kind}]` or `#[{kind}(default = val)]`",
            kind = kind,
        ));
        match attr_arg {
            AttrArg::None => None,
            AttrArg::Some { default } => Some(default),
        }
    }

    fn is_optional(field: &Field) -> bool {
        match field.ty {
            Type::Path(TypePath { ref path, .. }) => {
                let tokens = quote! { #path };
                let tokens = tokens.to_string().replace(' ', "");
                tokens.starts_with("Option<") && tokens.ends_with(">")
            }
            _ => panic!(
                "Type `{:?}` of field `{}` not supported",
                field.ty,
                field.ident.as_ref().unwrap()
            ),
        }
    }

    fn parse_prop(field: Field) -> ComponentField {
        let state_kind = Ident::new("state", Span::call_site()).into();
        let is_optional = Self::is_optional(&field);

        let (mut prop_attr, rest): (Vec<_>, Vec<_>) = field
            .attrs
            .into_iter()
            .partition(|attr| attr.path != state_kind);

        let default = if !prop_attr.is_empty() {
            Self::parse_default(prop_attr.swap_remove(0).tts, "prop")
        } else {
            None
        };

        ComponentField {
            attrs: rest,
            default,
            is_optional,
            vis: field.vis,
            ident: field.ident.unwrap(),
            ty: field.ty,
        }
    }

    fn parse_state(field: Field) -> ComponentField {
        let state_kind = Ident::new("state", Span::call_site()).into();
        let is_optional = Self::is_optional(&field);

        let (mut state_attr, rest): (Vec<_>, Vec<_>) = field
            .attrs
            .into_iter()
            .partition(|attr| attr.path == state_kind);

        // The `#[state]` check was already done, so its safe.
        let default = Self::parse_default(state_attr.swap_remove(0).tts, "state");

        ComponentField {
            attrs: rest,
            default,
            is_optional,
            vis: field.vis,
            ident: field.ident.unwrap(),
            ty: field.ty,
        }
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

    fn expand_as_builder_struct_field(&self) -> TokenStream {
        let ident = &self.ident;
        let ty = &self.ty;
        if self.is_optional {
            quote! {
                #ident: #ty
            }
        } else {
            quote! {
                #ident: Option<#ty>
            }
        }
    }

    fn expand_as_default_field(&self) -> TokenStream {
        let ident = &self.ident;
        if let Some(ref default) = self.default {
            quote! {
                #ident: #default
            }
        } else {
            quote! {
                #ident: Default::default()
            }
        }
    }

    fn expand_as_arg_field(&self) -> TokenStream {
        let ident = &self.ident;
        let ty = &self.ty;
        quote! {
            #ident: #ty
        }
    }

    fn expand_builder_assignment(&self) -> TokenStream {
        let ident = &self.ident;
        let arg_field = self.expand_as_arg_field();

        if self.is_optional {
            quote! {
                pub fn #ident(mut self, #arg_field) -> Self {
                    self.#ident = #ident;
                    self
                }
            }
        } else {
            quote! {
                pub fn #ident(mut self, #arg_field) -> Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        }
    }

    fn expand_builder_finish_assignment(&self) -> TokenStream {
        let ident = &self.ident;
        if let Some(ref default) = self.default {
            if self.is_optional {
                quote! {
                    #ident: self.#ident.or(#default)
                }
            } else {
                quote! {
                    #ident: self.#ident.unwrap_or(#default)
                }
            }
        } else {
            if self.is_optional {
                quote! {
                    #ident: self.#ident
                }
            } else {
                quote! {
                    #ident: self.#ident.expect(&format!("The field `{}` is required.", stringify!(#ident)))
                }
            }
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

impl Synom for EventsSyntax {
    named!(parse -> Self, do_parse!(
        events: parens!(many0!(syn!(EventSyntax))) >>
        (EventsSyntax { events: events.1 })
    ));
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

impl Synom for EventSyntax {
    named!(parse -> Self, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        keyword!(fn) >>
        ident: syn!(Ident) >>
        args: parens!(call!(Punctuated::<FnArg, Token!(,)>::parse_terminated)) >>
        return_type: syn!(ReturnType) >>
        syn!(Token!(;)) >>
        (EventSyntax { attrs, ident, args: args.1.into_iter().collect(), return_type })
    ));
}

/// Stores all the events declared on a component.
struct EventsMeta {
    /// Ident of Event type stored in the component itself.
    ident: Ident,
    /// Ident of Event type which stores actual events passed from parent. It serves as
    /// an intermediate event type before converting it to the above event type.
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
    ) -> (Option<EventsMeta>, Vec<Attribute>) {
        let events_kind = Ident::new("events", Span::call_site()).into();
        let (event_attrs, rest): (Vec<_>, Vec<_>) =
            attrs.into_iter().partition(|attr| attr.path == events_kind);

        let event_metas: Vec<_> = event_attrs
            .into_iter()
            // Parse each event declaration from each `#[events]` attribute found
            // and join them. 
            .flat_map(|attr| {
                let parsed: EventsSyntax = syn::parse2(attr.tts).expect(
                    r#"The events syntax is: 
                    #[events(
                        fn event_name(&self, args: type) -> type;

                        fn event_name2(&self);
                    )]
                    "#,
                );
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
                                panic!(
                                    "The first argument of event `{}` must be `&self`.",
                                    &event.ident
                                );
                            }
                            _ => {
                                panic!(
                                    "The argument of the event `{}` must be in the format `name: type`.",
                                    &event.ident
                                );
                            }
                        }
                    }

                    if event.attrs.len() > 1 {
                        panic!("The event declaration `{}` does not support multiple attributes.", &event.ident);
                    }

                    let is_optional = if let Some(ref attr) = event.attrs.get(0) {
                        if attr.path != Ident::new("optional", Span::call_site()).into() {
                            panic!("Did you want to annotate the event `{}` with `#[optional]`.", &event.ident);
                        };
                        true
                    } else {
                        false
                    };

                    let meta = EventMeta {
                        ident: event.ident,
                        arguments,
                        return_type: event.return_type,
                        is_optional
                    };
                    event_metas.push(meta);
                }

                event_metas
            }).collect();

        if event_metas.is_empty() {
            (None, rest)
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
            (Some(meta), rest)
        }
    }

    fn expand_as_struct_fields(&self) -> Vec<TokenStream> {
        self.events
            .iter()
            .map(EventMeta::expand_as_struct_field)
            .collect()
    }

    fn expand_as_gen_struct_fields(&self) -> Vec<TokenStream> {
        self.events
            .iter()
            .map(EventMeta::expand_as_gen_struct_field)
            .collect()
    }

    fn expand_as_builder_struct_fields(&self) -> Vec<TokenStream> {
        self.events
            .iter()
            .map(EventMeta::expand_as_builder_struct_field)
            .collect()
    }

    fn expand_as_event_names(&self) -> Vec<&Ident> {
        self.events.iter().map(|event| &event.ident).collect()
    }

    fn expand_event_conversions(&self) -> Vec<TokenStream> {
        self.events
            .iter()
            .map(EventMeta::expand_event_conversion)
            .collect()
    }

    fn expand_event_wrappers(
        &self,
        component_ident: &Ident,
        generics: &Generics,
    ) -> Vec<TokenStream> {
        self.events
            .iter()
            .map(|e| e.expand_event_wrapper(component_ident, generics))
            .collect()
    }

    fn expand_builder_assignment(&self) -> Vec<TokenStream> {
        self.events
            .iter()
            .map(EventMeta::expand_builder_assignment)
            .collect()
    }

    fn expand_builder_finish_assignment(&self) -> Vec<TokenStream> {
        self.events
            .iter()
            .map(EventMeta::expand_builder_finish_assignment)
            .collect()
    }

    fn expand_structs(
        &self,
        component_ident: &Ident,
        vis: &Visibility,
        generics: &Generics,
    ) -> TokenStream {
        let ident = &self.ident;
        let gen_ident = &self.gen_ident;
        let fields = self.expand_as_struct_fields();
        let gen_fields = self.expand_as_gen_struct_fields();
        let event_names = &self.expand_as_event_names();
        let event_conversion = self.expand_event_conversions();
        let event_wrappers = self.expand_event_wrappers(component_ident, generics);

        let builder_ident = &self.builder_ident;
        let builder_fields = self.expand_as_builder_struct_fields();
        let builder_assignment = self.expand_builder_assignment();
        let builder_finish_assignment = self.expand_builder_finish_assignment();

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

            impl<RCTX: Render> #gen_ident<RCTX> {
                pub fn builder() -> #builder_ident<RCTX> {
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

                // Underscored, so it is unlikely to colide with event names.
                pub fn __finish__(self) -> #gen_ident<RCTX> {
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
    fn expand_as_struct_field(&self) -> TokenStream {
        let ident = &self.ident;
        let arg_types: Vec<_> = self.arguments.iter().map(|arg| &arg.1).collect();

        match self.return_type {
            ReturnType::Default => if self.is_optional {
                quote! {
                    #ident: Box<Fn(#(#arg_types),*) -> Option<()>>
                }
            } else {
                quote! {
                    #ident: Box<Fn(#(#arg_types),*)>
                }
            },
            ReturnType::Type(_, ref ty) => if self.is_optional {
                quote! {
                    #ident: Box<Fn(#(#arg_types),*) -> Option<#ty>>
                }
            } else {
                quote! {
                    #ident: Box<Fn(#(#arg_types),*) -> #ty>
                }
            },
        }
    }

    fn expand_as_gen_struct_field(&self) -> TokenStream {
        let ident = &self.ident;
        let arg_types: Vec<_> = self.arguments.iter().map(|arg| &arg.1).collect();

        match self.return_type {
            ReturnType::Default => if self.is_optional {
                quote! {
                    #ident: Option<Box<Fn(&RCTX, #(#arg_types),*)>>
                }
            } else {
                quote! {
                    #ident: Box<Fn(&RCTX, #(#arg_types),*)>
                }
            },
            ReturnType::Type(_, ref ty) => if self.is_optional {
                quote! {
                    #ident: Option<Box<Fn(&RCTX, #(#arg_types),*) -> #ty>>
                }
            } else {
                quote! {
                    #ident: Box<Fn(&RCTX, #(#arg_types),*) -> #ty>
                }
            },
        }
    }

    fn expand_as_builder_struct_field(&self) -> TokenStream {
        let ident = &self.ident;
        let arg_types: Vec<_> = self.arguments.iter().map(|arg| &arg.1).collect();

        match self.return_type {
            ReturnType::Default => quote! {
                #ident: Option<Box<Fn(&RCTX, #(#arg_types),*)>>
            },
            ReturnType::Type(_, ref ty) => quote! {
                #ident: Option<Box<Fn(&RCTX, #(#arg_types),*) -> #ty>>
            },
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

    fn gen_fn_type(&self) -> TokenStream {
        let arg_types: Vec<_> = self.arguments.iter().map(|arg| &arg.1).collect();
        let ret_type = &self.return_type;
        quote! {
            Fn(&RCTX, #(#arg_types),*) #ret_type
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

fn is_prop(field: &Field) -> bool {
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
        panic!(
            "Component field `{}` cannot be both a prop and a state.",
            field.ident.as_ref().unwrap()
        );
    }
    if prop_attrs.len() > 1 {
        panic!(
            "Component field `{}` cannot have multiple prop attributes.",
            field.ident.as_ref().unwrap()
        );
    }

    state_attrs.is_empty()
}
