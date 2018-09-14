use super::kw;
use super::HtmlRoot;
use heck::{CamelCase, KebabCase};
use suffix::{EVENT_SUFFIX, PROPS_SUFFIX};
use proc_macro2::{Span, TokenStream};
use syn::parse::{Error, Parse, ParseStream, Result as ParseResult};
use syn::punctuated::Punctuated;
use syn::token;
use syn::{Expr, Ident};

pub enum HtmlElement {
    Normal(NormalHtmlElement),
    SelfClosing(SelfClosingHtmlElement),
}

impl Parse for HtmlElement {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        if kw::is_self_closing(&input) {
            Ok(HtmlElement::SelfClosing(input.parse()?))
        } else {
            Ok(HtmlElement::Normal(input.parse()?))
        }
    }
}

impl HtmlElement {
    pub fn expand(&self) -> TokenStream {
        match self {
            HtmlElement::Normal(ref normal) => normal.expand(),
            HtmlElement::SelfClosing(ref self_closing) => self_closing.expand(),
        }
    }

    pub fn key(&self) -> Option<&KeyAttribute> {
        match self {
            HtmlElement::Normal(ref el) => el.key(),
            HtmlElement::SelfClosing(ref el) => el.key(),
        }
    }
}

pub struct NormalHtmlElement {
    pub opening_tag: OpeningTag,
    pub child: Box<HtmlRoot>,
    pub closing_tag: ClosingTag,
}

impl Parse for NormalHtmlElement {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let opening_tag: OpeningTag = input.parse()?;
        let child = input.parse()?;
        let closing_tag: ClosingTag = input.parse()?;

        let err_span = match (&opening_tag.tag_name, &closing_tag.tag_name) {
            (
                TagName::Tag {
                    name: ref op,
                    span: ref op_span,
                },
                TagName::Tag {
                    name: ref cl,
                    span: ref cl_span,
                },
            ) => {
                if op != cl {
                    op_span.join(cl_span.clone())
                } else {
                    None
                }
            }
            (TagName::Component { ident: ref op }, TagName::Component { ident: ref cl }) => {
                if op != cl {
                    op.span().join(cl.span())
                } else {
                    None
                }
            }
            (TagName::Component { ref ident }, TagName::Tag { ref span, .. })
            | (TagName::Tag { ref span, .. }, TagName::Component { ref ident }) => {
                span.join(ident.span())
            }
        };

        if let Some(span) = err_span {
            return Err(Error::new(span, "opening and closing tag must be same."));
        }

        Ok(NormalHtmlElement {
            opening_tag,
            child,
            closing_tag,
        })
    }
}

impl NormalHtmlElement {
    fn expand(&self) -> TokenStream {
        let child_expanded = self.child.expand();
        self.opening_tag.expand_with(&child_expanded)
    }

    pub fn key(&self) -> Option<&KeyAttribute> {
        self.opening_tag.key.as_ref()
    }
}

pub struct SelfClosingHtmlElement {
    pub tag: SelfClosingTag,
}

impl Parse for SelfClosingHtmlElement {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        Ok(SelfClosingHtmlElement {
            tag: input.parse()?,
        })
    }
}

impl SelfClosingHtmlElement {
    fn expand(&self) -> TokenStream {
        self.tag.expand()
    }

    pub fn key(&self) -> Option<&KeyAttribute> {
        self.tag.key.as_ref()
    }
}

pub struct OpeningTag {
    pub lt: Token![<],
    pub tag_name: TagName,
    pub key: Option<KeyAttribute>,
    pub prop_attributes: Vec<HtmlAttribute>,
    pub event_attributes: Vec<HtmlAttribute>,
    pub gt: Token![>],
}

impl Parse for OpeningTag {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let lt = input.parse()?;
        let tag_name = input.parse()?;
        let mut key = None;

        let mut attributes: Vec<HtmlAttribute> = vec![];
        while !input.peek(Token![>]) {
            if input.peek(kw::key) {
                key = Some(input.parse()?);
            } else {
                attributes.push(input.parse()?);
            }
        }

        let gt = input.parse()?;

        let (mut prop_attributes, event_attributes) = attributes
            .into_iter()
            .partition::<Vec<_>, _>(|attr| attr.at.is_none());

        prop_attributes.sort_by(|l, r| l.key.name.cmp(&r.key.name));

        Ok(OpeningTag {
            lt,
            tag_name,
            key,
            prop_attributes,
            event_attributes,
            gt,
        })
    }
}

impl OpeningTag {
    fn expand_with(&self, child: &TokenStream) -> TokenStream {
        match self.tag_name {
            TagName::Tag { ref name, .. } => {
                let prop_attributes: Vec<_> = self
                    .prop_attributes
                    .iter()
                    .map(|p| p.expand_as_prop_attribute().unwrap())
                    .collect();
                let event_attributes: Vec<_> = self
                    .event_attributes
                    .iter()
                    .map(|e| e.expand_as_event_attribute().unwrap())
                    .collect();

                quote! {
                    ruukh::vdom::velement::VElement::new(
                        #name,
                        vec![#(#prop_attributes),*],
                        vec![#(#event_attributes),*],
                        #child
                    )
                }
            }
            TagName::Component { ref ident } => {
                let prop_attributes: Vec<_> = self
                    .prop_attributes
                    .iter()
                    .map(|p| p.expand_as_named_arg())
                    .collect();

                let event_attributes: Vec<_> = self
                    .event_attributes
                    .iter()
                    .map(|e| e.expand_as_named_arg())
                    .collect();

                let props_ident = Ident::new(&format!("{}{}", ident, PROPS_SUFFIX), ident.span());
                let event_ident = Ident::new(&format!("{}{}", ident, EVENT_SUFFIX), ident.span());
                let span = ident.span();
                quote_spanned!{span=>
                    ruukh::vdom::vcomponent::VComponent::new::<#ident>(
                        #props_ident!(#(#prop_attributes),*),
                        #event_ident!(#(#event_attributes),*),
                    )
                }
            }
        }
    }
}

pub struct ClosingTag {
    pub lt: Token![<],
    pub slash: Token![/],
    pub tag_name: TagName,
    pub gt: Token![>],
}

impl Parse for ClosingTag {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        Ok(ClosingTag {
            lt: input.parse()?,
            slash: input.parse()?,
            tag_name: input.parse()?,
            gt: input.parse()?,
        })
    }
}

pub struct SelfClosingTag {
    pub lt: Token![<],
    pub tag_name: TagName,
    pub key: Option<KeyAttribute>,
    pub prop_attributes: Vec<HtmlAttribute>,
    pub event_attributes: Vec<HtmlAttribute>,
    pub slash: Option<Token![/]>,
    pub gt: Token![>],
}

impl Parse for SelfClosingTag {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let lt = input.parse()?;
        let tag_name = input.parse()?;
        let mut key = None;

        let mut attributes: Vec<HtmlAttribute> = vec![];
        while !input.peek(Token![/]) && !input.peek(Token![>]) {
            if input.peek(kw::key) {
                key = Some(input.parse()?);
            } else {
                attributes.push(input.parse()?);
            }
        }

        let slash = input.parse()?;
        let gt = input.parse()?;

        let (prop_attributes, event_attributes) =
            attributes.into_iter().partition(|attr| attr.at.is_none());

        Ok(SelfClosingTag {
            lt,
            tag_name,
            key,
            prop_attributes,
            event_attributes,
            slash,
            gt,
        })
    }
}

impl SelfClosingTag {
    fn expand(&self) -> TokenStream {
        match self.tag_name {
            TagName::Tag { ref name, .. } => {
                let prop_attributes: Vec<_> = self
                    .prop_attributes
                    .iter()
                    .map(|p| p.expand_as_prop_attribute().unwrap())
                    .collect();
                let event_attributes: Vec<_> = self
                    .event_attributes
                    .iter()
                    .map(|e| e.expand_as_event_attribute().unwrap())
                    .collect();

                quote! {
                    ruukh::vdom::velement::VElement::childless(
                        #name,
                        vec![#(#prop_attributes),*],
                        vec![#(#event_attributes),*]
                    )
                }
            }
            _ => unreachable!("The spec specified self-closing tags are the only ones allowed."),
        }
    }
}

pub struct KeyAttribute {
    pub key: kw::key,
    pub eq: Token![=],
    pub brace: token::Brace,
    pub value: Expr,
}

impl Parse for KeyAttribute {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let key = input.parse()?;
        let eq = input.parse()?;
        let content;
        let brace = braced!(content in input);
        let value = content.parse()?;
        Ok(KeyAttribute {
            key,
            eq,
            brace,
            value,
        })
    }
}

impl KeyAttribute {
    pub fn expand(&self) -> TokenStream {
        let value = &self.value;
        quote! {
            ruukh::vdom::Key::new(#value)
        }
    }
}

pub struct HtmlAttribute {
    pub at: Option<Token![@]>,
    pub key: AttributeName,
    pub eq: Token![=],
    pub brace: token::Brace,
    pub value: Expr,
}

impl Parse for HtmlAttribute {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let at = input.parse()?;
        let key = input.parse()?;
        let eq = input.parse()?;
        let content;
        let brace = braced!(content in input);
        let value = content.parse()?;
        Ok(HtmlAttribute {
            at,
            key,
            eq,
            brace,
            value,
        })
    }
}

impl HtmlAttribute {
    fn expand_as_prop_attribute(&self) -> Option<TokenStream> {
        if self.at.is_some() {
            return None;
        }
        let key = &self.key.name;
        let value = &self.value;

        Some(quote! {
            ruukh::vdom::velement::Attribute::new(#key, #value)
        })
    }

    fn expand_as_event_attribute(&self) -> Option<TokenStream> {
        self.at?;
        let key = &self.key.name;
        let value = &self.value;

        Some(quote! {
            ruukh::vdom::velement::EventListener::new(#key, Box::new(#value))
        })
    }

    fn expand_as_named_arg(&self) -> TokenStream {
        let key = Ident::new(&self.key.name, Span::call_site());
        let value = &self.value;

        quote! {
            #key: #value
        }
    }
}

pub enum TagName {
    Tag { name: String, span: Span },
    Component { ident: Ident },
}

impl Parse for TagName {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let first_span = input.cursor().span();
        let idents = input.call(Punctuated::<Ident, Token![-]>::parse_separated_nonempty)?;
        let span = idents
            .iter()
            .map(|ident| ident.span())
            .fold(first_span, |acc, span| acc.join(span).unwrap());
        let mut idents = idents.into_iter().collect::<Vec<_>>();

        let ident = idents.get(0).as_ref().unwrap().to_string();
        if ident == ident.to_camel_case() {
            if idents.len() != 1 {
                return Err(Error::new(span, "no dashes in a component tag allowed."));
            }
            return Ok(TagName::Component {
                ident: idents.swap_remove(0),
            });
        }

        let tag_name = idents
            .into_iter()
            .map(|ident| ident.to_string())
            .collect::<Vec<_>>()
            .join("-");

        let kebab_tag_name = tag_name.to_kebab_case();
        if tag_name != kebab_tag_name {
            return Err(Error::new(
                span,
                &format!("tag name in kebab case only like {}.", kebab_tag_name),
            ));
        }

        Ok(TagName::Tag {
            name: tag_name,
            span,
        })
    }
}

pub struct AttributeName {
    name: String,
}

impl Parse for AttributeName {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let first_span = input.cursor().span();
        let idents = input.call(Punctuated::<Ident, Token![-]>::parse_separated_nonempty)?;
        let span = idents
            .iter()
            .map(|ident| ident.span())
            .fold(first_span, |acc, span| acc.join(span).unwrap());
        let name = idents
            .into_iter()
            .map(|id| id.to_string())
            .collect::<Vec<_>>()
            .join("-");

        let kebab_name = name.to_kebab_case();
        if name != kebab_name {
            return Err(Error::new(
                span,
                &format!("attribute name in kebab case only like {}.", kebab_name),
            ));
        }

        Ok(AttributeName { name })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use syn;

    #[test]
    fn should_parse_an_html_element() {
        let _: HtmlElement = syn::parse_str("<div></div>").unwrap();
    }

    #[test]
    fn should_parse_normal_html_element() {
        let _: NormalHtmlElement = syn::parse_str("<div></div>").unwrap();
    }

    #[test]
    fn should_parse_normal_html_element_with_child() {
        let _: NormalHtmlElement = syn::parse_str("<div>Hello</div>").unwrap();
    }

    #[test]
    fn should_parse_opening_tag() {
        let _: OpeningTag = syn::parse_str("<div>").unwrap();
    }

    #[test]
    fn should_parse_closing_tag() {
        let _: ClosingTag = syn::parse_str("</div>").unwrap();
    }

    #[test]
    fn should_parse_self_closing_html_element() {
        let _: SelfClosingHtmlElement = syn::parse_str("<input>").unwrap();
    }

    #[test]
    fn should_parse_self_closing_tag() {
        let _: SelfClosingTag = syn::parse_str("<input>").unwrap();
    }

    #[test]
    fn should_parse_self_closing_tag_with_slash() {
        let _: SelfClosingTag = syn::parse_str("<input/>").unwrap();
    }

    #[test]
    fn should_parse_normal_attribute() {
        let attr: HtmlAttribute = syn::parse_str(r#"name={"value"}"#).unwrap();
        assert!(attr.at.is_none());
    }

    #[test]
    fn should_parse_event_attribute() {
        let attr: HtmlAttribute = syn::parse_str(r#"@input={fn_name}"#).unwrap();
        assert!(attr.at.is_some());
    }

    #[test]
    fn should_parse_single_tag_name() {
        let parsed: TagName = syn::parse_str("Identifier").unwrap();
        match parsed {
            TagName::Component { ident } => {
                assert_eq!(ident, "Identifier");
            }
            _ => {}
        }
    }

    #[test]
    fn should_parse_dashed_tag_name() {
        let parsed: TagName = syn::parse_str("first-second-third").unwrap();
        match parsed {
            TagName::Tag { name, .. } => {
                assert_eq!(name, "first-second-third");
            }
            _ => {}
        }
    }
}
