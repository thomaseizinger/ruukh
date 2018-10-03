use super::{props::PropsMeta, state::StateMeta};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use std::mem;
use syn::{
    custom_keyword, parenthesized,
    parse::{Error, Parse, ParseStream, Result as ParseResult},
    punctuated::Punctuated,
    spanned::Spanned,
    Attribute, Expr, Field, Fields, Ident, ItemStruct, Path, Token, Type, TypePath, TypeReference,
    Visibility,
};

/// A field of a component. Stores all the struct metadata along with additional
/// default value expression.
pub struct ComponentField {
    pub attrs: Vec<Attribute>,
    pub attr_arg: AttrArg,
    pub is_optional: bool,
    pub vis: Visibility,
    pub ident: Ident,
    pub ty: Type,
    pub field_type: FieldType,
}

/// Type of field.
#[derive(PartialEq, Eq)]
pub enum FieldType {
    /// A prop field.
    Prop,
    /// A state field.
    State,
}

impl ComponentField {
    pub fn parse_into_prop_and_state_meta(
        component: &mut ItemStruct,
    ) -> ParseResult<(PropsMeta, StateMeta)> {
        let fields = Self::parse_all(component)?;
        let (prop_fields, state_fields) = fields
            .into_iter()
            .partition(|f| f.field_type == FieldType::Prop);

        Ok((
            PropsMeta::parse(component, prop_fields),
            StateMeta::parse(component, state_fields),
        ))
    }

    fn parse_all(component: &mut ItemStruct) -> ParseResult<Vec<ComponentField>> {
        let fields = Self::normalize_fields(component)?;
        let state = Ident::new("state", Span::call_site()).into();
        let prop = Ident::new("prop", Span::call_site()).into();
        fields
            .into_iter()
            .map(|field| ComponentField::parse_one(field, &state, &prop))
            .collect()
    }

    fn parse_one(field: Field, state: &Path, prop: &Path) -> ParseResult<ComponentField> {
        let is_optional = Self::is_optional(&field)?;
        let (mut field_type_attr, rest): (Vec<_>, Vec<_>) = field
            .attrs
            .into_iter()
            .partition(|attr| &attr.path == state || &attr.path == prop);

        let field_type = if field_type_attr.is_empty() || &field_type_attr[0].path == prop {
            FieldType::Prop
        } else {
            FieldType::State
        };

        let attr_arg = if field_type_attr.is_empty() {
            AttrArg::default()
        } else {
            syn::parse2(field_type_attr.remove(0).tts)?
        };

        Ok(ComponentField {
            attrs: rest,
            attr_arg,
            is_optional,
            vis: field.vis,
            ident: field.ident.unwrap(),
            ty: field.ty,
            field_type,
        })
    }

    fn normalize_fields(component: &mut ItemStruct) -> ParseResult<Vec<Field>> {
        match component.fields {
            Fields::Named(ref mut fields) => {
                let mut named = Punctuated::new();
                // Take the named fields.
                mem::swap(&mut fields.named, &mut named);

                let fields: Vec<_> = named.into_iter().collect();
                ComponentField::verify_valid_fields(&fields)?;
                Ok(fields)
            }
            Fields::Unnamed(_) => Err(Error::new(
                component.ident.span(),
                "Only unit and named field structs can be components.",
            )),
            Fields::Unit => Ok(vec![]),
        }
    }

    fn verify_valid_fields(fields: &[Field]) -> ParseResult<()> {
        let prop_kind = Ident::new("prop", Span::call_site()).into();
        let state_kind = Ident::new("state", Span::call_site()).into();

        for field in fields.iter() {
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
                Err(Error::new(
                    field.ident.span(),
                    "Cannot be both `#[prop]` and `#[state]` at once.",
                ))?;
            }
            if prop_attrs.len() > 1 {
                Err(Error::new(
                    field.ident.span(),
                    "Cannot have multiple `#[prop]` attributes.",
                ))?;
            }
            if state_attrs.len() > 1 {
                Err(Error::new(
                    field.ident.span(),
                    "Cannot have multiple `#[state]` attributes.",
                ))?;
            }
        }
        Ok(())
    }

    fn is_optional(field: &Field) -> ParseResult<bool> {
        match field.ty {
            Type::Path(TypePath { ref path, .. }) => {
                let tokens = quote! { #path };
                let tokens = tokens.to_string().replace(' ', "");
                Ok(tokens.starts_with("Option<") && tokens.ends_with('>'))
            }
            Type::Reference(TypeReference { ref lifetime, .. }) => {
                let lifetime = lifetime.as_ref().expect("Lifetimes are always provided");
                Ok(lifetime.ident == Ident::new("static", Span::call_site()))
            }
            _ => Err(Error::new(field.ty.span(), "Type not supported")),
        }
    }

    pub fn to_struct_field(&self) -> TokenStream {
        let attrs = &self.attrs;
        let vis = &self.vis;
        let ident = &self.ident;
        let ty = &self.ty;
        quote! {
            #(#attrs)*
            #vis #ident: #ty
        }
    }

    pub fn to_field_assignment_as_default(&self) -> TokenStream {
        let ident = &self.ident;
        if let AttrArg {
            default: Some(DefaultArg::Expr(ref default)),
        } = self.attr_arg
        {
            quote! {
                #ident: #default
            }
        } else {
            quote! {
                #ident: Default::default()
            }
        }
    }

    pub fn to_ident(&self) -> TokenStream {
        let ident = &self.ident;
        quote! {
            #ident
        }
    }

    pub fn to_default_argument_for_macro(&self) -> TokenStream {
        let ident = &self.ident;
        if let AttrArg {
            default: Some(DefaultArg::Expr(ref default)),
        } = self.attr_arg
        {
            quote! {
                [ #ident = #default ]
            }
        } else if let Some(DefaultArg::Default) = self.attr_arg.default {
            quote! {
                [ #ident = Default::default() ]
            }
        } else if self.is_optional {
            quote! {
                [ #ident = Default::default() ]
            }
        } else {
            quote!()
        }
    }
}

custom_keyword!(default);

/// The argument passed with `#[prop]` or `#[state]` attributes.
///
/// Can be `#[prop]`, `#[prop(default)]` or `#[prop(default = expr)]`.
#[derive(Default)]
pub struct AttrArg {
    pub default: Option<DefaultArg>,
}

impl Parse for AttrArg {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        if input.is_empty() {
            return Ok(AttrArg::default());
        }

        let content;
        parenthesized!(content in input);

        let default = if content.peek(default) {
            Some(content.parse()?)
        } else {
            None
        };

        if !input.is_empty() {
            return Err(input.error("expected `)`."));
        }
        Ok(AttrArg { default })
    }
}

pub enum DefaultArg {
    Expr(Box<Expr>),
    Default,
}

impl Parse for DefaultArg {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        input.parse::<default>()?;
        if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            Ok(DefaultArg::Expr(Box::new(input.parse()?)))
        } else {
            Ok(DefaultArg::Default)
        }
    }
}
