use super::fields::{is_state, ComponentField};
use crate::suffix::PROPS_SUFFIX;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{parse::Result as ParseResult, Field, Ident, Visibility};

/// Stores the list of prop fields.
pub struct PropsMeta {
    /// Ident of props struct.
    pub ident: Ident,
    /// List of prop fields.
    pub fields: Vec<ComponentField>,
}

impl PropsMeta {
    pub fn take_prop_fields(
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

    pub fn expand_fields_with<F>(&self, map_fn: F) -> Vec<TokenStream>
    where
        F: Fn(&ComponentField) -> TokenStream,
    {
        self.fields.iter().map(map_fn).collect()
    }

    pub fn create_props_struct_and_macro(
        &self,
        comp_ident: &Ident,
        vis: &Visibility,
    ) -> TokenStream {
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

    pub fn create_void_props_macro(comp_ident: &Ident, vis: &Visibility) -> TokenStream {
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
