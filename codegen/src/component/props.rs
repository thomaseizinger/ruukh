use super::fields::ComponentField;
use crate::suffix::PROPS_SUFFIX;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Ident, ItemStruct, Visibility};

/// Stores the list of prop fields.
pub struct PropsMeta {
    /// Ident of props struct.
    pub ident: Ident,
    /// Ident of the component.
    pub component_ident: Ident,
    /// Visiblilty of the component.
    pub vis: Visibility,
    /// List of prop fields.
    pub fields: Vec<ComponentField>,
}

impl PropsMeta {
    pub fn parse(component: &ItemStruct, fields: Vec<ComponentField>) -> PropsMeta {
        PropsMeta {
            ident: Ident::new(
                &format!("{}{}", component.ident, PROPS_SUFFIX),
                Span::call_site(),
            ),
            component_ident: component.ident.clone(),
            vis: component.vis.clone(),
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

    pub fn create_props_struct_and_macro(&self) -> TokenStream {
        if self.fields.is_empty() {
            self.create_void_props_macro()
        } else {
            self._create_props_struct_and_macro()
        }
    }

    fn _create_props_struct_and_macro(&self) -> TokenStream {
        let ident = &self.ident;
        let vis = &self.vis;
        let fields = self.expand_fields_with(ComponentField::to_struct_field);

        let props_macro = self.create_props_macro();

        quote! {
            #vis struct #ident {
                #(#fields),*
            }

            #props_macro
        }
    }

    fn create_props_macro(&self) -> TokenStream {
        let ident = &self.ident;
        let vis = &self.vis;
        let comp_ident = &self.component_ident;

        let field_idents = self.expand_fields_with(ComponentField::to_ident);
        let field_default_vals =
            self.expand_fields_with(ComponentField::to_default_argument_for_macro);
        let mut next_idents = field_idents.clone();
        let first = next_idents.remove(0);
        next_idents.push(quote!(@finish));
        let internal_macro_ident = self.internal_macro_ident();

        let match_hands = field_idents
            .iter()
            .zip(next_idents.iter())
            .zip(field_default_vals.iter())
            .map(|((cur, next), default)| {
                quote!{
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
                }
            });

        quote! {
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

    fn create_void_props_macro(&self) -> TokenStream {
        let ident = &self.ident;
        let vis = &self.vis;
        let comp_ident = &self.component_ident;
        let internal_macro_ident = self.internal_macro_ident();
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
