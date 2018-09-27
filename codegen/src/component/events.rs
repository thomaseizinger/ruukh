use crate::suffix::{EVENT_PROPS_SUFFIX, EVENT_SUFFIX};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    parenthesized,
    parse::{Error, Parse, ParseStream, Result as ParseResult},
    spanned::Spanned,
    Attribute, FnArg, Ident, Pat, ReturnType, Token, Type, Visibility,
};

/// Stores all the events declared on a component.
pub struct EventsMeta {
    /// Ident of Event type stored in the component itself.
    pub ident: Ident,
    /// Ident of Event type which stores actual events passed from parent. It
    /// serves as an intermediate event type before converting it to the
    /// above event type.
    pub event_props_ident: Ident,
    /// All the event declarations on the component.
    pub events: Vec<EventMeta>,
}

impl EventsMeta {
    pub fn take_events_attributes(
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

    pub fn create_events_and_event_props_struct_and_macro(
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

    pub fn create_void_events_macro(comp_ident: &Ident, vis: &Visibility) -> TokenStream {
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
pub struct EventMeta {
    /// Event name.
    pub ident: Ident,
    /// Name and type pair of the argument list.
    pub arguments: Vec<(Pat, Type)>,
    /// Return type of the event.
    pub return_type: ReturnType,
    /// Whether the event is optional.
    pub is_optional: bool,
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
