use super::fields::ComponentField;
use crate::suffix::STATE_SUFFIX;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Ident, ItemStruct};

/// Stores the list of state fields.
pub struct StateMeta {
    /// Ident of state struct.
    pub ident: Ident,
    /// List of state fields.
    pub fields: Vec<ComponentField>,
}

impl StateMeta {
    pub fn parse(component: &ItemStruct, fields: Vec<ComponentField>) -> StateMeta {
        StateMeta {
            ident: Ident::new(
                &format!("{}{}", component.ident, STATE_SUFFIX),
                Span::call_site(),
            ),
            fields,
        }
    }

    pub fn expand_fields_with<F>(&self, map_fn: F) -> Vec<TokenStream>
    where
        F: Fn(&ComponentField) -> TokenStream,
    {
        self.fields.iter().map(map_fn).collect()
    }

    pub fn to_struct_fields(&self) -> Vec<TokenStream> {
        self.fields
            .iter()
            .map(ComponentField::to_struct_field)
            .collect()
    }

    pub fn to_field_idents(&self) -> Vec<TokenStream> {
        self.fields.iter().map(ComponentField::to_ident).collect()
    }

    pub fn create_state_struct(&self) -> TokenStream {
        if !self.fields.is_empty() {
            let ident = &self.ident;
            let fields = self.expand_fields_with(ComponentField::to_struct_field);
            let def_fields =
                self.expand_fields_with(ComponentField::to_field_assignment_as_default);

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
        } else {
            quote!()
        }
    }
}
