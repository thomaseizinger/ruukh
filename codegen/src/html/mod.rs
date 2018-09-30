//! The pseudo grammar for the html! macro.
//!
//! ROOT -> ITEM ITEM
//!
//! ITEM -> ELEMENT ITEM | EXPR_BLOCK ITEM | TEXT ITEM | EPS
//!
//! ELEMENT ->
//! <TAGNAME ATTRIBUTES>
//!     ROOT
//! </TAGNAME> |
//! <TAGNAME ATTRIBUTES/> |
//! <TAGNAME ATTRIBUTES>
//!
//! TEXT -> ".*"
//!
//! EXPR_BLOCK -> { EXPR }
//!
//! TAGNAME -> DASHED_IDENT
//!
//! ATTRIBUTES -> ATTRIBUTE ATTRIBUTES | EPS
//!
//! ATTRIBUTE -> OPTIONAL_AT DASHED_IDENT = { EXPR }
//!
//! OPTIONAL_AT -> @ | EPS
//!
//! DASHED_IDENT -> IDENT-DASHED_IDENT | IDENT
//!
//! N.B. EPS is Epsilon and IDENT & EXPR are Rust constructs.

use self::element::{HtmlElement, KeyAttribute};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream, Result as ParseResult},
    token, Block as RustExpressionBlock, LitStr, Token,
};

mod element;
mod kw;

pub struct HtmlRoot {
    pub items: Vec<HtmlItems>,
    pub flat_len: usize,
    pub keyed_only: bool,
}

impl Parse for HtmlRoot {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let mut ungrouped_items: Vec<HtmlItem> = vec![];
        while !input.is_empty() {
            // Encounters an end tag.
            if input.peek(Token![<]) && input.peek2(Token![/]) {
                break;
            }
            ungrouped_items.push(input.parse()?);
        }

        let flat_len = ungrouped_items.len();
        let mut keyed_only = true;
        // Unflatten the items into their own seggregated list of keyed & unkeyed
        // items while still maintaining order.
        let mut items = vec![];
        let mut keyed = vec![];
        let mut unkeyed = vec![];
        for item in ungrouped_items {
            if item.key().is_some() {
                // If there were unkeyed ones before push them first to maintain order.
                if !unkeyed.is_empty() {
                    items.push(HtmlItems::Unkeyed(unkeyed));
                    unkeyed = vec![];
                }

                keyed.push(item);
            } else {
                if keyed_only {
                    keyed_only = false;
                }

                // If there were keyed ones before push them first to maintain order.
                if !keyed.is_empty() {
                    items.push(HtmlItems::Keyed(keyed));
                    keyed = vec![];
                }

                unkeyed.push(item);
            }
        }

        // Push in the remaining ones.
        if !keyed.is_empty() {
            items.push(HtmlItems::Keyed(keyed));
        }
        if !unkeyed.is_empty() {
            items.push(HtmlItems::Unkeyed(unkeyed));
        }

        Ok(HtmlRoot {
            items,
            flat_len,
            keyed_only,
        })
    }
}

impl HtmlRoot {
    pub fn expand(&self) -> TokenStream {
        let expanded: Vec<_> = self.items.iter().map(|i| i.expand()).collect();
        if self.flat_len == 0 {
            quote! {
                ruukh::vdom::VNode::None
            }
        } else if self.flat_len == 1 || self.keyed_only {
            quote! {
                #(#expanded)*
            }
        } else {
            quote! {
                ruukh::vdom::VNode::from(ruukh::vdom::vlist::VList::from(vec![
                    #(#expanded),*
                ]))
            }
        }
    }
}

pub enum HtmlItems {
    Keyed(Vec<HtmlItem>),
    Unkeyed(Vec<HtmlItem>),
}

impl HtmlItems {
    fn expand(&self) -> TokenStream {
        match self {
            HtmlItems::Keyed(ref items) => {
                let expanded: Vec<_> = items.iter().map(HtmlItem::expand).collect();
                let capacity = expanded.len();
                quote! {
                    ruukh::vdom::VNode::from(ruukh::vdom::vlist::VList::from({
                        let mut map = ruukh::reexports::IndexMap::with_capacity_and_hasher(
                            #capacity,
                            ruukh::reexports::FnvBuildHasher::default()
                        );
                        #(map.insert(#expanded);)*
                        map
                    }))
                }
            }
            HtmlItems::Unkeyed(ref items) => {
                let expanded: Vec<_> = items.iter().map(HtmlItem::expand).collect();
                quote! {
                    #(#expanded),*
                }
            }
        }
    }
}

pub enum HtmlItem {
    Element(Box<HtmlElement>),
    ExpressionBlock(RustExpressionBlock),
    Text(Text),
}

impl Parse for HtmlItem {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let lookahead1 = input.lookahead1();
        if lookahead1.peek(Token![<]) {
            Ok(HtmlItem::Element(Box::new(input.parse()?)))
        } else if lookahead1.peek(token::Brace) {
            Ok(HtmlItem::ExpressionBlock(input.parse()?))
        } else if lookahead1.peek(LitStr) {
            Ok(HtmlItem::Text(input.parse()?))
        } else {
            Err(lookahead1.error())
        }
    }
}

impl HtmlItem {
    fn expand(&self) -> TokenStream {
        let expanded = match self {
            HtmlItem::Element(ref element) => {
                let expanded = element.expand();
                quote! {
                    ruukh::vdom::VNode::from(#expanded)
                }
            }
            HtmlItem::ExpressionBlock(ref block) => {
                quote! {
                    ruukh::vdom::VNode::from(#block)
                }
            }
            HtmlItem::Text(ref text) => {
                let string = &text.content;
                quote! {
                    ruukh::vdom::VNode::from(ruukh::vdom::vtext::VText::text(#string))
                }
            }
        };

        if let Some(key) = self.key() {
            let key_expanded = key.expand();
            quote! {
                #key_expanded, #expanded
            }
        } else {
            expanded
        }
    }

    fn key(&self) -> Option<&KeyAttribute> {
        match self {
            HtmlItem::Element(ref el) => el.key(),
            _ => None,
        }
    }
}

pub struct Text {
    pub content: String,
}

impl Parse for Text {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let mut content = Vec::<LitStr>::new();

        while input.peek(LitStr) {
            content.push(input.parse()?);
        }

        Ok(Text {
            content: content
                .into_iter()
                .map(|lit| lit.value())
                .collect::<Vec<_>>()
                .join(""),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn should_parse_html() {
        let _: HtmlRoot = syn::parse_str(
            r#"
            <div>
                <button>"Hello World!"</button>

                <span>"How are you?"</span>
            </div>

            "Nice to meet you."
        "#,
        ).unwrap();
    }

    #[test]
    fn should_parse_html_with_nested_rust_expr() {
        let _: HtmlRoot = syn::parse_str(
            r#"
            <div>
                "My name is "{ "Anonymous" }"."
            </div>
            "#,
        ).unwrap();
    }

    #[test]
    fn should_parse_text() {
        let text: Text =
            syn::parse_str(r#""This is a text. What do you think about it?""#).unwrap();

        assert_eq!(text.content, "This is a text. What do you think about it?");
    }

    #[test]
    fn should_parse_multiline_text() {
        let text: Text = syn::parse_str(
            r#"
                "This is a text. "

                "This is new line."
            "#,
        ).unwrap();

        assert_eq!(text.content, "This is a text. This is new line.");
    }
}
