use self::{events::EventsMeta, fields::ComponentField, props::PropsMeta, state::StateMeta};
use crate::suffix::STATUS_SUFFIX;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use std::mem;
use syn::{
    parse::{Error, Result as ParseResult},
    Attribute, Ident, ItemStruct, Visibility,
};

mod events;
mod fields;
mod props;
mod state;

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
    props_meta: PropsMeta,
    /// State metadata if any state fields.
    state_meta: StateMeta,
    /// Events metadata if any events declaration.
    events_meta: EventsMeta,
}

impl ComponentMeta {
    pub fn parse(mut item: ItemStruct) -> ParseResult<ComponentMeta> {
        // Remove `#[component]` attribute.
        Self::filter_out_component_attribute(&mut item);

        if item.generics != Default::default() {
            return Err(Error::new(
                item.ident.span(),
                "Generic parameters not allowed on a component.",
            ));
        }

        let (props_meta, state_meta) = ComponentField::parse_into_prop_and_state_meta(&mut item)?;
        let events_meta = EventsMeta::parse(&mut item)?;

        Ok(ComponentMeta {
            attrs: item.attrs,
            vis: item.vis,
            ident: item.ident,
            props_meta,
            state_meta,
            events_meta,
        })
    }

    fn filter_out_component_attribute(item: &mut ItemStruct) {
        let mut attrs = vec![];
        mem::swap(&mut attrs, &mut item.attrs);
        let component = Ident::new("component", Span::call_site()).into();
        let attrs: Vec<_> = attrs
            .into_iter()
            .filter(|attr| attr.path != component)
            .collect();
        item.attrs = attrs;
    }

    pub fn expand(&self) -> TokenStream {
        let component_struct = self.create_component_struct();
        let component_impl = self.impl_component_trait_on_component_struct();
        let set_state_impl = self.impl_set_state_trait_on_component_struct();
        let state_setter_impl = self.impl_state_setter_trait_on_component_struct();
        let state_struct = self.state_meta.create_state_struct(&self.vis);
        let props_struct = self.props_meta.create_props_struct_and_macro();
        let events_structs = self
            .events_meta
            .create_events_and_event_props_struct_and_macro();
        let status_wrapper_struct = self.create_status_wrapper_struct();

        quote! {
            #component_struct

            #state_struct

            #props_struct

            #events_structs

            #component_impl

            #set_state_impl

            #status_wrapper_struct

            #state_setter_impl
        }
    }

    fn create_component_struct(&self) -> TokenStream {
        let attrs = &self.attrs;
        let ident = &self.ident;
        let vis = &self.vis;

        if self.props_meta.fields.is_empty()
            && self.state_meta.fields.is_empty()
            && self.events_meta.events.is_empty()
        {
            quote! {
                #(#attrs)*
                #vis struct #ident;
            }
        } else {
            let state_fields = self.state_meta.to_struct_fields();
            let props_fields = self.props_meta.to_struct_fields();
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

    fn create_status_field(&self) -> Option<TokenStream> {
        if self.props_meta.fields.is_empty() && self.state_meta.fields.is_empty() {
            None
        } else {
            let status_ty = self.get_status_type();
            Some(quote! {
                __status__: #status_ty,
            })
        }
    }

    fn create_events_field(&self) -> Option<TokenStream> {
        if self.events_meta.events.is_empty() {
            None
        } else {
            let ident = &self.events_meta.ident;
            Some(quote! {
                __events__: #ident,
            })
        }
    }

    fn get_props_type(&self) -> TokenStream {
        if self.props_meta.fields.is_empty() {
            quote!(())
        } else {
            let ident = &self.props_meta.ident;
            quote!(#ident)
        }
    }

    fn get_state_type(&self) -> TokenStream {
        if self.state_meta.fields.is_empty() {
            quote!(())
        } else {
            let ident = &self.state_meta.ident;
            quote!(#ident)
        }
    }

    fn get_events_type(&self) -> TokenStream {
        if self.events_meta.events.is_empty() {
            quote!(())
        } else {
            let ident = &self.events_meta.ident;
            quote!(#ident)
        }
    }

    fn get_status_type(&self) -> Ident {
        Ident::new(
            &format!("{}{}", self.ident, STATUS_SUFFIX),
            Span::call_site(),
        )
    }

    fn create_status_wrapper_struct(&self) -> Option<TokenStream> {
        if self.props_meta.fields.is_empty() && self.state_meta.fields.is_empty() {
            None
        } else {
            let ident = self.get_status_type();
            let vis = &self.vis;
            let state_ty = self.get_state_type();
            let status_set_state = self.impl_set_state_trait_for_status_wrapper();

            Some(quote! {
                #[derive(Clone)]
                #vis struct #ident(std::rc::Rc<std::cell::RefCell<ruukh::component::Status<#state_ty>>>);

                #status_set_state
            })
        }
    }

    fn impl_set_state_trait_for_status_wrapper(&self) -> Option<TokenStream> {
        if self.state_meta.fields.is_empty() {
            None
        } else {
            let ident = self.get_status_type();
            let state_ty = self.get_state_type();
            Some(quote! {
                impl SetState for #ident {
                    type State = #state_ty;

                    fn set_state(&self, mut mutator: impl FnMut(&mut Self::State)) {
                        let mut status = self.0.borrow_mut();
                        mutator(status.state_as_mut());
                        status.set_state_dirty(true);
                        status.do_react();
                    }
                }
            })
        }
    }

    fn impl_component_trait_on_component_struct(&self) -> TokenStream {
        let ident = &self.ident;
        let props_type = &self.get_props_type();
        let state_type = &self.get_state_type();
        let events_type = &self.get_events_type();
        let init_body = self.impl_fn_init_body();
        let update_body = self.impl_fn_update_body();
        let refresh_state_body = self.impl_fn_refresh_state_body();
        let status_body = self.impl_fn_status_body();

        quote! {
            impl Component for #ident {
                type Props = #props_type;
                type State = #state_type;
                type Events = #events_type;

                fn init(
                    __props__: Self::Props,
                    __events__: Self::Events,
                    __status__: ruukh::component::Status<Self::State>,
                ) -> Self {
                    #init_body
                }

                fn update(
                    &mut self,
                    mut __props__: Self::Props,
                    __events__: Self::Events,
                ) -> Option<Self::Props> {
                    #update_body
                }

                fn refresh_state(&mut self) -> bool {
                    #refresh_state_body
                }

                fn status(&self) -> Option<&std::rc::Rc<
                                        std::cell::RefCell<
                                            ruukh::component::Status<
                                                Self::State>>>>
                {
                    #status_body
                }
            }
        }
    }

    fn impl_set_state_trait_on_component_struct(&self) -> Option<TokenStream> {
        if self.state_meta.fields.is_empty() {
            None
        } else {
            let ident = &self.ident;
            let state_ident = &self.state_meta.ident;
            let set_state_body = self.impl_fn_set_state_body();

            Some(quote! {
                impl SetState for #ident {
                    type State = #state_ident;

                    fn set_state(&self, mut mutator: impl FnMut(&mut Self::State)) {
                        #set_state_body
                    }
                }
            })
        }
    }

    fn impl_state_setter_trait_on_component_struct(&self) -> Option<TokenStream> {
        if self.state_meta.fields.is_empty() {
            None
        } else {
            let ident = &self.ident;
            let status_ty = self.get_status_type();

            Some(quote! {
                impl StateSetter for #ident {
                    type Setter = #status_ty;

                    fn state_setter(&self) -> Self::Setter {
                        self.__status__.clone()
                    }
                }
            })
        }
    }

    fn impl_fn_set_state_body(&self) -> Option<TokenStream> {
        if self.state_meta.fields.is_empty() {
            None
        } else {
            let idents = &self.state_meta.to_field_idents();
            let idents2 = idents;

            Some(quote! {
                let mut status = self.__status__.0.borrow_mut();
                mutator(status.state_as_mut());
                let changed = {
                    let state = status.state_as_ref();
                    #(
                        self.#idents != state.#idents2
                    ) ||*
                };
                if changed {
                    status.set_state_dirty(true);
                    status.do_react();
                }
            })
        }
    }

    fn impl_fn_init_body(&self) -> TokenStream {
        let state_clone = if self.state_meta.fields.is_empty() {
            None
        } else {
            let state_field_idents = &self.state_meta.to_field_idents();
            let expanded = if state_field_idents.len() == 1 {
                quote! {
                    let #(#state_field_idents)* = {
                        let state = __status__.state_as_ref();
                        #(state.#state_field_idents.clone())*
                    };
                }
            } else {
                quote! {
                    let (#(#state_field_idents),*) = {
                        let state = __status__.state_as_ref();
                        (#(state.#state_field_idents.clone()),*)
                    };
                }
            };
            Some(expanded)
        };

        let event_assignment = if self.events_meta.events.is_empty() {
            None
        } else {
            Some(quote! {
                __events__,
            })
        };

        let status_assignment =
            if self.props_meta.fields.is_empty() && self.state_meta.fields.is_empty() {
                None
            } else {
                let status_ty = self.get_status_type();
                Some(quote! {
                    __status__: #status_ty(
                                    std::rc::Rc::new(
                                        std::cell::RefCell::new(__status__))),
                })
            };

        let ident = &self.ident;
        let state_field_idents = &self.state_meta.to_field_idents();
        let props_field_idents = &self.props_meta.to_field_idents();
        let props_field_idents2 = props_field_idents;

        quote! {
            #state_clone

            #ident {
                #(#props_field_idents: __props__.#props_field_idents2 ,)*
                #(#state_field_idents ,)*
                #event_assignment
                #status_assignment
            }
        }
    }

    fn impl_fn_status_body(&self) -> TokenStream {
        if self.props_meta.fields.is_empty() && self.state_meta.fields.is_empty() {
            quote!(None)
        } else {
            quote! {
                Some(&self.__status__.0)
            }
        }
    }

    fn impl_fn_refresh_state_body(&self) -> TokenStream {
        if self.state_meta.fields.is_empty() {
            quote! {
                false
            }
        } else {
            let idents = &self.state_meta.to_field_idents();
            let idents2 = idents;
            let idents3 = idents;
            let idents4 = idents;

            quote! {
                let status = self.__status__.0.borrow();
                let state = status.state_as_ref();
                let mut changed = false;
                #(
                    if self.#idents != state.#idents2 {
                        self.#idents3 = state.#idents4.clone();

                        if !changed {
                            changed = true;
                        }
                    }
                )*
                changed
            }
        }
    }

    fn impl_fn_update_body(&self) -> TokenStream {
        let events_assignment = if self.events_meta.events.is_empty() {
            None
        } else {
            Some(quote! {
                // The events need to be updated regardless, there is no checking them.
                self.__events__ = __events__;
            })
        };

        if self.props_meta.fields.is_empty() {
            quote! {
                #events_assignment

                None
            }
        } else {
            let idents = &self.props_meta.to_field_idents();
            let idents2 = idents;

            quote! {
                #events_assignment

                use std::mem;
                #(
                    mem::swap(&mut self.#idents, &mut __props__.#idents2);
                )*

                if #(self.#idents != __props__.#idents2) || * {
                    self.__status__.0.borrow_mut().set_props_dirty(true);
                    Some(__props__)
                } else {
                    None
                }
            }
        }
    }
}
