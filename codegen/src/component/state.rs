use super::fields::{is_state, ComponentField};
use crate::suffix::STATE_SUFFIX;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{parse::Result as ParseResult, Field, Ident};

/// Stores the list of state fields.
pub struct StateMeta {
    /// Ident of state struct.
    pub ident: Ident,
    /// List of state fields.
    pub fields: Vec<ComponentField>,
}

impl StateMeta {
    pub fn take_state_fields(
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

    pub fn expand_fields_with<F>(&self, map_fn: F) -> Vec<TokenStream>
    where
        F: Fn(&ComponentField) -> TokenStream,
    {
        self.fields.iter().map(map_fn).collect()
    }

    pub fn create_state_struct(&self) -> TokenStream {
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
