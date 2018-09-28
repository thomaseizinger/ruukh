use crate::suffix::SLOTS_SUFFIX;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    custom_keyword, parenthesized,
    parse::{Error, Parse, ParseStream, Result as ParseResult},
    punctuated::Punctuated,
    spanned::Spanned,
    token, Attribute, FnArg, Ident, Pat, Token, Type, Visibility,
};

/// Stores all the slotes declared on a component.
pub struct SlotsMeta {
    /// Ident of the slots type.
    pub ident: Ident,
    /// List of slot descriptions.
    pub slots: Vec<SlotMeta>,
}

impl SlotsMeta {
    pub fn take_slots_attributes(
        component_ident: &Ident,
        attrs: Vec<Attribute>,
    ) -> ParseResult<(Option<SlotsMeta>, Vec<Attribute>)> {
        // Separate the attributes from `slots` attributes.
        let slots = Ident::new("slots", Span::call_site()).into();
        let (slots_attrs, rest): (Vec<Attribute>, Vec<Attribute>) =
            attrs.into_iter().partition(|attr| attr.path == slots);

        let slot_metas: ParseResult<Vec<Vec<SlotMeta>>> = slots_attrs
            .into_iter()
            .map(Self::take_one_slots_attribute)
            .collect();

        let mut slot_metas: Vec<SlotMeta> = slot_metas?.into_iter().flatten().collect();
        slot_metas.sort_by(|l, r| l.ident.cmp(&r.ident));

        if slot_metas.is_empty() {
            Ok((None, rest))
        } else {
            let slots_ident = Ident::new(
                &format!("{}{}", component_ident, SLOTS_SUFFIX),
                Span::call_site(),
            );
            let slots_meta = SlotsMeta {
                ident: slots_ident,
                slots: slot_metas,
            };
            Ok((Some(slots_meta), rest))
        }
    }

    fn take_one_slots_attribute(attr: Attribute) -> ParseResult<Vec<SlotMeta>> {
        let slots_args: SlotsAttributeArgs = syn::parse2(attr.tts)?;
        let slot_metas = slots_args
            .arguments
            .into_iter()
            .map(|decl| {
                let arguments: Vec<_> = decl
                    .arguments
                    .map(|args| {
                        args.into_iter()
                            .map(|arg| match arg {
                                FnArg::Captured(captured) => (captured.pat, captured.ty),
                                _ => unreachable!(),
                            }).collect()
                    }).unwrap_or_default();

                SlotMeta {
                    ident: decl.ident,
                    arguments,
                }
            }).collect();

        Ok(slot_metas)
    }

    fn expand_slots_with(&self, map_fn: impl Fn(&SlotMeta) -> TokenStream) -> Vec<TokenStream> {
        self.slots.iter().map(map_fn).collect()
    }

    pub fn create_slots_struct_and_macro(
        &self,
        component_ident: &Ident,
        vis: &Visibility,
    ) -> TokenStream {
        let ident = &self.ident;
        let internal_macro_ident =
            Ident::new(&format!("__new_{}_internal__", ident), Span::call_site());

        let fields = self.expand_slots_with(SlotMeta::to_struct_field);
        let slot_names = self.expand_slots_with(SlotMeta::to_slot_ident);
        let mut next_slot_names = slot_names.clone();
        let first = next_slot_names.remove(0);
        next_slot_names.push(quote!(@finish));

        let match_hands = slot_names
            .iter()
            .zip(next_slot_names.iter())
            .map(|(cur, next)| {
                quote! {
                    (
                        @#cur
                        arguments = [{ $($args:tt)* }]
                        tokens = [{ [#cur = $val:expr] $($rest:tt)* }]
                    ) => {
                        #internal_macro_ident!(
                            @#next
                            arguments = [{
                                $($args)*
                                [ #cur = std::rc::Rc::new(std::cell::RefCell::new(Some($val))) ]
                            }]
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
                            arguments = [{
                                $($args)*
                                [ #cur = std::rc::Rc::new(std::cell::RefCell::new(None)) ]
                            }]
                            tokens = [{ $($rest)* }]
                        );
                    },
                }
            });

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
                    compile_error!(concat!(
                        "There is no slot `",
                        stringify!($key),
                        "` on `",
                        stringify!(#component_ident), "`."
                    ));
                }
            }

            #vis macro #ident($($key:ident : $val:expr),*) {
                #internal_macro_ident!(
                    @#first
                    arguments = [{ }]
                    tokens = [{ $([$key = $val])* }]
                )
            }
        }
    }

    pub fn create_void_slots_macro(component_ident: &Ident, vis: &Visibility) -> TokenStream {
        let slots_ident = Ident::new(
            &format!("{}{}", component_ident, SLOTS_SUFFIX),
            Span::call_site(),
        );
        let internal_macro_ident = Ident::new(
            &format!("__new_{}_internal__", slots_ident),
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
                    compile_error!(concat!("`", stringify!(#component_ident), "` has no slots."));
                }
            }

            #vis macro #slots_ident($($key:ident: $val:expr),*) {
                #internal_macro_ident!(
                    tokens = [{ $([$key = $val])* }]
                );
            }
        }
    }
}

/// A single slot description.
pub struct SlotMeta {
    /// Name of the slot.
    pub ident: Ident,
    /// Arguments of the slot.
    pub arguments: Vec<(Pat, Type)>,
}

impl SlotMeta {
    fn to_struct_field(&self) -> TokenStream {
        let ident = &self.ident;
        let args_types = self.args_type_tuple_form();

        quote! {
            #ident: std::rc::Rc<std::cell::RefCell<ruukh::vdom::vslot::SlotUse<#args_types>>>
        }
    }

    fn args_type_tuple_form(&self) -> TokenStream {
        let types: Vec<_> = self.arguments.iter().map(|(_, ty)| quote!(#ty)).collect();

        quote! {
            (#(#types),*)
        }
    }

    fn to_slot_ident(&self) -> TokenStream {
        let ident = &self.ident;
        quote!(#ident)
    }
}

/// Parses the arguments provided to the `#[slots]` attribute.
///
/// Looks like:
/// ```ignore,compile_fail
/// (
///     slot default;
///
///     slot named(arg: i32);
/// )
/// ```
struct SlotsAttributeArgs {
    paren_token: token::Paren,
    arguments: Vec<SlotDeclaration>,
}

impl Parse for SlotsAttributeArgs {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let content;
        let paren_token = parenthesized!(content in input);
        let mut arguments = vec![];

        while !content.is_empty() {
            arguments.push(content.parse()?);
        }

        Ok(SlotsAttributeArgs {
            paren_token,
            arguments,
        })
    }
}

custom_keyword!(slot);

/// Parses a single slot declaration.
///
/// Looks like one of the following kinds:
/// ```ignore,compile_fail
/// slot slot_name;
///
/// slot slot_name(arg: type, ...);
/// ```
struct SlotDeclaration {
    slot: slot,
    ident: Ident,
    paren_token: Option<token::Paren>,
    arguments: Option<Punctuated<FnArg, Token![,]>>,
    semi_token: Token![;],
}

impl Parse for SlotDeclaration {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let slot = input.parse()?;
        let ident: Ident = input.parse()?;

        let (paren_token, arguments) = if input.peek(token::Paren) {
            let content;
            let paren_token = parenthesized!(content in input);
            let arguments = content.parse_terminated(FnArg::parse)?;

            for arg in arguments.iter() {
                match arg {
                    FnArg::Captured(_) => {}
                    _ => {
                        return Err(Error::new(arg.span(), "Only `var: type` argument allowed."));
                    }
                }
            }

            (Some(paren_token), Some(arguments))
        } else {
            (None, None)
        };

        if ident == "default" && paren_token.is_some() {
            return Err(Error::new(
                ident.span(),
                "`default` slot cannot have arguments. Use a different name.",
            ));
        }

        let semi_token = input.parse()?;

        Ok(SlotDeclaration {
            slot,
            ident,
            paren_token,
            arguments,
            semi_token,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn should_parse_slots_argument() {
        let slots_args: SlotsAttributeArgs = syn::parse_str(
            r#" ( 
                slot default;
                
                slot named(arg: i32, arg2: &'static str);
            ) "#,
        ).unwrap();

        assert_eq!(slots_args.arguments.len(), 2);
    }

    #[test]
    fn should_parse_argless_slot_declaration() {
        let slot_desc: SlotDeclaration = syn::parse_str(
            r#"
                slot default;
            "#,
        ).unwrap();

        assert_eq!(slot_desc.ident, "default");
        assert!(slot_desc.paren_token.is_none());
        assert!(slot_desc.arguments.is_none());
    }

    #[test]
    fn should_parse_argfull_slot_declaration() {
        let slot_desc: SlotDeclaration = syn::parse_str(
            r#"
                slot named(arg: i32, arg1: &'static str);
            "#,
        ).unwrap();

        assert_eq!(slot_desc.ident, "named");
        assert!(slot_desc.paren_token.is_some());
        assert_eq!(slot_desc.arguments.unwrap().len(), 2);
    }

    #[test]
    fn should_not_parse_args_on_default_declaration() {
        let slot_desc = syn::parse_str::<SlotDeclaration>(
            r#"
                slot default(arg: i32);
            "#,
        );

        assert!(slot_desc.is_err());
    }
}
