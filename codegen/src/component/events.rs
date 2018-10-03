use self::parser::{EventDeclaration, EventDeclarations};
use crate::suffix::{EVENT_PROPS_SUFFIX, EVENT_SUFFIX};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use std::mem;
use syn::{
    parse::{Error, Result as ParseResult},
    spanned::Spanned,
    Attribute, FnArg, Ident, ItemStruct, Pat, ReturnType, Type, Visibility,
};

mod parser;

/// Stores all the events declared on a component.
pub struct EventsMeta {
    /// Ident of Event type stored in the component itself.
    pub ident: Ident,
    /// Ident of Event type which stores actual events passed from parent. It
    /// serves as an intermediate event type before converting it to the
    /// above event type.
    pub event_props_ident: Ident,
    /// Ident of the component.
    pub component_ident: Ident,
    /// Visibility of the component.
    pub vis: Visibility,
    /// All the event declarations on the component.
    pub events: Vec<EventMeta>,
}

impl EventsMeta {
    pub fn parse(component: &mut ItemStruct) -> ParseResult<EventsMeta> {
        let mut attrs = vec![];
        mem::swap(&mut component.attrs, &mut attrs);
        let (event_attrs, rest) = Self::partition_event_attributes(attrs);
        // Reassign the remaining attributes to the struct for other uses.
        component.attrs = rest;

        let event_metas: ParseResult<Vec<Vec<EventMeta>>> = event_attrs
            .into_iter()
            .map(EventMeta::parse_event_metas)
            .collect();
        let mut event_metas: Vec<EventMeta> = event_metas?.into_iter().flatten().collect();
        event_metas.sort_by(|l, r| l.ident.cmp(&r.ident));

        Ok(EventsMeta {
            ident: Ident::new(
                &format!("{}{}", component.ident, EVENT_SUFFIX),
                Span::call_site(),
            ),
            event_props_ident: Ident::new(
                &format!("{}{}", component.ident, EVENT_PROPS_SUFFIX),
                Span::call_site(),
            ),
            component_ident: component.ident.clone(),
            vis: component.vis.clone(),
            events: event_metas,
        })
    }

    fn partition_event_attributes(attrs: Vec<Attribute>) -> (Vec<Attribute>, Vec<Attribute>) {
        let events = Ident::new("events", Span::call_site()).into();
        attrs.into_iter().partition(|attr| attr.path == events)
    }

    fn expand_events_with<F>(&self, map_fn: F) -> Vec<TokenStream>
    where
        F: Fn(&EventMeta) -> TokenStream,
    {
        self.events.iter().map(map_fn).collect()
    }

    pub fn create_events_and_event_props_struct_and_macro(&self) -> TokenStream {
        if self.events.is_empty() {
            self.create_void_events_macro()
        } else {
            self._create_events_and_event_props_struct_and_macro()
        }
    }

    fn _create_events_and_event_props_struct_and_macro(&self) -> TokenStream {
        let ident = &self.ident;
        let vis = &self.vis;
        let fields = self.expand_events_with(EventMeta::to_struct_field);
        let event_names = &self.expand_events_with(EventMeta::to_event_name);

        let event_props_struct = self.create_event_props_struct(event_names);
        let events_macro = self.create_events_macro(event_names);

        quote! {
            #vis struct #ident {
                #(#fields),*
            }

            #event_props_struct

            #events_macro
        }
    }

    fn create_events_macro(&self, event_names: &[TokenStream]) -> TokenStream {
        let vis = &self.vis;
        let ident = &self.ident;
        let event_props_ident = &self.event_props_ident;
        let component_ident = &self.component_ident;
        let macro_internal_ident = self.internal_macro_ident();

        let mut next_event_names = event_names.to_owned();
        let first = next_event_names.remove(0);
        next_event_names.push(quote!(@finish));
        let events_assignment = self.expand_events_with(EventMeta::to_event_assignment_for_macro);
        let events_default_val =
            self.expand_events_with(EventMeta::to_event_assignment_as_default_value_for_macro);

        let match_hands = event_names
            .iter()
            .zip(next_event_names.iter())
            .zip(events_assignment.iter().zip(events_default_val.iter()))
            .map(|((cur, next), (assignment, default))| {
                quote!{
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
                }
            });

        quote! {
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

    fn create_event_props_struct(&self, event_names: &[TokenStream]) -> TokenStream {
        let vis = &self.vis;
        let ident = &self.ident;
        let event_props_ident = &self.event_props_ident;
        let gen_fields = self.expand_events_with(EventMeta::to_event_prop_field);
        let event_conversion =
            self.expand_events_with(EventMeta::impl_event_conversion_from_event_prop);
        let event_wrappers =
            self.expand_events_with(|e| e.impl_event_wrapper(&self.component_ident));

        quote! {
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
        }
    }

    fn create_void_events_macro(&self) -> TokenStream {
        let ident = &self.ident;
        let comp_ident = &self.component_ident;
        let vis = &self.vis;
        let internal_macro_ident = self.internal_macro_ident();

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

            #vis macro #ident($($key:ident: $val:expr),*) {
                #internal_macro_ident!(
                    tokens = [{ $([$key = $val])* }]
                );
            }
        }
    }

    fn internal_macro_ident(&self) -> Ident {
        Ident::new(
            &format!("__new_{}_internal__", self.ident),
            Span::call_site(),
        )
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
    fn parse_event_metas(attr: Attribute) -> ParseResult<Vec<EventMeta>> {
        let parsed: EventDeclarations = syn::parse2(attr.tts)?;
        parsed
            .events
            .into_iter()
            .map(Self::parse_each_event_meta)
            .collect()
    }

    fn parse_each_event_meta(event: EventDeclaration) -> ParseResult<EventMeta> {
        let arguments: ParseResult<Vec<_>> = event
            .args
            .into_iter()
            .enumerate()
            .skip_while(Self::is_first_arg_self_ref)
            .map(Self::parse_event_argument)
            .collect();

        let is_optional = if let Some(ref attr) = event.attr {
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

        Ok(EventMeta {
            ident: event.ident,
            arguments: arguments?,
            return_type: event.return_type,
            is_optional,
        })
    }

    fn is_first_arg_self_ref((index, arg): &(usize, FnArg)) -> bool {
        match arg {
            FnArg::SelfRef(_) if *index == 0 => true,
            _ => false,
        }
    }

    fn parse_event_argument((index, arg): (usize, FnArg)) -> ParseResult<(Pat, Type)> {
        match arg {
            FnArg::Captured(ref captured) => Ok((captured.pat.clone(), captured.ty.clone())),
            _ if index == 0 => Err(Error::new(arg.span(), "expected `&self`")),
            _ => Err(Error::new(arg.span(), "expected `: type`")),
        }
    }

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

    fn to_event_name(&self) -> TokenStream {
        let ident = &self.ident;
        quote! {
            #ident
        }
    }

    fn to_struct_field(&self) -> TokenStream {
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
