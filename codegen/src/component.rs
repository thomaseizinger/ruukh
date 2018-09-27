use self::{
    events::EventsMeta,
    fields::{check_supported_attributes, ComponentField},
    props::PropsMeta,
    slots::SlotsMeta,
    state::StateMeta,
};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    parse::{Error, Result as ParseResult},
    Attribute, Fields, Ident, ItemStruct, Visibility,
};

mod events;
mod fields;
mod props;
mod slots;
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
    props_meta: Option<PropsMeta>,
    /// State metadata if any state fields.
    state_meta: Option<StateMeta>,
    /// Events metadata if any events declaration.
    events_meta: Option<EventsMeta>,
    /// Slots metadata if any slots declaration.
    slots_meta: Option<SlotsMeta>,
}

impl ComponentMeta {
    pub fn parse(item: ItemStruct) -> ParseResult<ComponentMeta> {
        if item.generics != Default::default() {
            return Err(Error::new(
                item.ident.span(),
                "Generic parameters not allowed on a component.",
            ));
        }

        // Sort out the struct fields into state and props fields.
        let (props_meta, state_meta) = match item.fields {
            Fields::Named(fields) => {
                let fields: Vec<_> = fields.named.into_iter().collect();

                for field in fields.iter() {
                    check_supported_attributes(field)?;
                }

                let (props_meta, fields) = PropsMeta::take_prop_fields(&item.ident, fields)?;
                let (state_meta, fields) = StateMeta::take_state_fields(&item.ident, fields)?;
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
        let attrs = Self::filter_out_component_attributes(item.attrs);
        let (events_meta, attrs) = EventsMeta::take_events_attributes(&item.ident, attrs)?;
        let (slots_meta, attrs) = SlotsMeta::take_slots_attributes(&item.ident, attrs)?;

        Ok(ComponentMeta {
            attrs,
            vis: item.vis,
            ident: item.ident,
            props_meta,
            state_meta,
            events_meta,
            slots_meta,
        })
    }

    fn filter_out_component_attributes(attrs: Vec<Attribute>) -> Vec<Attribute> {
        let component_name = Ident::new("component", Span::call_site()).into();
        attrs
            .into_iter()
            .filter(|attr| attr.path != component_name)
            .collect()
    }

    pub fn expand(&self) -> TokenStream {
        let component_struct = self.create_component_struct();
        let component_impl = self.impl_component_trait_on_component_struct();
        let state_struct = self.state_meta.as_ref().map(StateMeta::create_state_struct);
        let props_struct = self
            .props_meta
            .as_ref()
            .map(|m| m.create_props_struct_and_macro(&self.ident, &self.vis))
            .unwrap_or_else(|| PropsMeta::create_void_props_macro(&self.ident, &self.vis));
        let events_structs = self
            .events_meta
            .as_ref()
            .map(|m| m.create_events_and_event_props_struct_and_macro(&self.ident, &self.vis))
            .unwrap_or_else(|| EventsMeta::create_void_events_macro(&self.ident, &self.vis));

        quote! {
            #component_struct

            #state_struct

            #props_struct

            #events_structs

            #component_impl
        }
    }

    fn create_component_struct(&self) -> TokenStream {
        let attrs = &self.attrs;
        let ident = &self.ident;
        let vis = &self.vis;

        if self.props_meta.is_none() && self.state_meta.is_none() && self.events_meta.is_none() {
            quote! {
                #(#attrs)*
                #vis struct #ident;
            }
        } else {
            let state_fields = self
                .state_meta
                .as_ref()
                .map(|s| s.expand_fields_with(ComponentField::to_struct_field))
                .unwrap_or_default();
            let props_fields = self
                .props_meta
                .as_ref()
                .map(|p| p.expand_fields_with(ComponentField::to_struct_field))
                .unwrap_or_default();
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

    fn create_status_field(&self) -> TokenStream {
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
                __status__: std::rc::Rc<std::cell::RefCell<ruukh::component::Status<#ident>>>,
            }
        }
    }

    fn create_events_field(&self) -> TokenStream {
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
            .map(|s| s.expand_fields_with(ComponentField::to_ident))
            .unwrap_or_default()
    }

    fn get_props_type(&self) -> TokenStream {
        if let Some(ref props_meta) = self.props_meta {
            let ident = &props_meta.ident;
            quote!(#ident)
        } else {
            quote!(())
        }
    }

    fn get_state_type(&self) -> TokenStream {
        if let Some(ref state_meta) = self.state_meta {
            let ident = &state_meta.ident;
            quote!(#ident)
        } else {
            quote!(())
        }
    }

    fn get_events_type(&self) -> TokenStream {
        if let Some(ref events_meta) = self.events_meta {
            let ident = &events_meta.ident;
            quote!(#ident)
        } else {
            quote!(())
        }
    }

    fn impl_component_trait_on_component_struct(&self) -> TokenStream {
        let ident = &self.ident;
        let props_type = &self.get_props_type();
        let state_type = &self.get_state_type();
        let events_type = &self.get_events_type();
        let state_clone = self.impl_state_clone_from_status();
        let event_assignment = self.impl_event_assignment();
        let state_field_idents = &self
            .state_meta
            .as_ref()
            .map(|m| m.fields.iter().map(|f| &f.ident).collect::<Vec<_>>())
            .unwrap_or_default();
        let props_field_idents = &self
            .props_meta
            .as_ref()
            .map(|m| m.fields.iter().map(|f| &f.ident).collect::<Vec<_>>())
            .unwrap_or_default();
        let props_field_idents2 = props_field_idents;
        let status_assignment = self.impl_status_assignment();
        let props_updation = self.impl_props_updation(props_field_idents);
        let updation_ret_block = self.impl_return_block_after_updation();
        let events_updation = self.impl_events_updation();
        let refresh_state_body = self.impl_fn_refresh_state_body(state_field_idents);
        let take_state_dirty_body = self.impl_fn_take_state_dirty_body();
        let take_props_dirty_body = self.impl_fn_take_props_dirty_body();
        let set_state_body = self.impl_fn_set_state_body(state_field_idents);

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
                    #state_clone

                    #ident {
                        #(#props_field_idents: __props__.#props_field_idents2 ,)*
                        #(#state_field_idents ,)*
                        #event_assignment
                        #status_assignment
                    }
                }

                fn update(
                    &mut self,
                    mut __props__: Self::Props,
                    __events__: Self::Events,
                ) -> Option<Self::Props> {
                    #props_updation

                    #events_updation

                    #updation_ret_block
                }

                fn refresh_state(&mut self) {
                    #refresh_state_body
                }

                fn take_state_dirty(&self) -> bool {
                    #take_state_dirty_body
                }

                fn take_props_dirty(&self) -> bool {
                    #take_props_dirty_body
                }

                fn set_state(&self, mut mutator: impl FnMut(&mut Self::State)) {
                    #set_state_body
                }
            }
        }
    }

    fn impl_fn_set_state_body(&self, idents: &[&Ident]) -> TokenStream {
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

    fn impl_fn_take_props_dirty_body(&self) -> TokenStream {
        if self.props_meta.is_some() {
            quote! {
                self.__status__.borrow_mut().take_props_dirty()
            }
        } else {
            quote! { false }
        }
    }

    fn impl_fn_take_state_dirty_body(&self) -> TokenStream {
        if self.state_meta.is_some() {
            quote! {
                self.__status__.borrow_mut().take_state_dirty()
            }
        } else {
            quote! { false }
        }
    }

    fn impl_fn_refresh_state_body(&self, idents: &[&Ident]) -> TokenStream {
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

    fn impl_props_updation(&self, idents: &[&Ident]) -> TokenStream {
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

    fn impl_return_block_after_updation(&self) -> TokenStream {
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

    fn impl_events_updation(&self) -> TokenStream {
        if self.events_meta.is_some() {
            quote! {
                // The events need to be updated regardless, there is no checking them.
                self.__events__ = __events__;
            }
        } else {
            quote!()
        }
    }

    fn impl_state_clone_from_status(&self) -> TokenStream {
        if self.state_meta.is_none() {
            quote!()
        } else {
            let state_field_idents = &self.expand_state_field_idents();

            if state_field_idents.len() == 1 {
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
            }
        }
    }

    fn impl_status_assignment(&self) -> TokenStream {
        if self.props_meta.is_none() && self.state_meta.is_none() {
            quote!()
        } else {
            quote!(__status__: std::rc::Rc::new(std::cell::RefCell::new(__status__)),)
        }
    }

    fn impl_event_assignment(&self) -> TokenStream {
        if self.events_meta.is_some() {
            quote! {
                __events__,
            }
        } else {
            quote!()
        }
    }
}
