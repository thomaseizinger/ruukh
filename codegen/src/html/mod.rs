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
//! TEXT -> TEXT_CHAR TEXT | EPS
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
//! TEXT_CHAR -> /[^{}()[]]/
//!
//! DASHED_IDENT -> IDENT-DASHED_IDENT | IDENT
//!
//! N.B. EPS is Epsilon and IDENT & EXPR are Rust constructs.

use self::element::HtmlElement;
use proc_macro2::{Span, TokenStream, TokenTree};
use syn::parse::{Parse, ParseStream, Result as ParseResult};
use syn::token;
use syn::Block as RustExpressionBlock;

mod element;

pub struct HtmlRoot {
    pub items: Vec<HtmlItem>,
}

impl Parse for HtmlRoot {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let mut items = vec![];
        while !input.is_empty() {
            // Encounters an end tag.
            if input.peek(Token![<]) && input.peek2(Token![/]) {
                break;
            }
            items.push(input.parse()?);
        }
        Ok(HtmlRoot { items })
    }
}

impl HtmlRoot {
    pub fn expand(&self) -> TokenStream {
        let expanded: Vec<_> = self.items.iter().map(|i| i.expand()).collect();
        if expanded.is_empty() {
            quote!()
        } else if self.items.len() == 1 {
            quote! {
                #(#expanded)*
            }
        } else {
            quote! {
                ruukh::vdom::VNode::new(ruukh::vdom::vlist::VList::new(vec![
                    #(#expanded),*
                ]))
            }
        }
    }
}

pub enum HtmlItem {
    Element(HtmlElement),
    ExpressionBlock(RustExpressionBlock),
    Text(Text),
}

impl Parse for HtmlItem {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        if input.peek(Token![<]) {
            Ok(HtmlItem::Element(input.parse()?))
        } else if input.peek(token::Brace) {
            Ok(HtmlItem::ExpressionBlock(input.parse()?))
        } else {
            Ok(HtmlItem::Text(input.parse()?))
        }
    }
}

impl HtmlItem {
    fn expand(&self) -> TokenStream {
        match self {
            HtmlItem::Element(ref element) => {
                let expanded = element.expand();
                quote! {
                    ruukh::vdom::VNode::new(#expanded)
                }
            }
            HtmlItem::ExpressionBlock(_) => {
                unimplemented!("Need to implement a mechanism to convert types into VNode");
            }
            HtmlItem::Text(ref text) => {
                let string = &text.content;
                quote! {
                    ruukh::vdom::VNode::new(ruukh::vdom::vtext::VText::text(#string))
                }
            }
        }
    }
}

pub struct Text {
    pub content: String,
}

impl Parse for Text {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let content = input.step(|cursor| {
            let mut content = String::new();
            let mut rest = *cursor;
            let mut prev_span = None;
            while let Some((tt, next)) = rest.token_tree() {
                let cur_span = tt.span();
                if let Some(ref prev_span) = prev_span {
                    add_space_to(&mut content, prev_span, &cur_span);
                }
                prev_span = Some(cur_span);

                match tt {
                    TokenTree::Group(_) => break,
                    TokenTree::Punct(ref punct) if punct.as_char() == '<' => break,
                    TokenTree::Punct(ref punct) => {
                        content.push(punct.as_char());
                    }
                    TokenTree::Literal(ref literal) => {
                        content.push_str(&literal.to_string());
                    }
                    TokenTree::Ident(ref ident) => {
                        content.push_str(&ident.to_string());
                    }
                }
                rest = next;
            }
            Ok((content, rest))
        })?;
        Ok(Text { content })
    }
}

/// Only push one space as HTML does not recognize multiple.
///
/// For <pre> contents when whitespaces are required as-is, use literal string in rust
/// expression instead.
fn add_space_to(string: &mut String, left: &Span, right: &Span) {
    let left = left.end();
    let right = right.start();
    if left.column != right.column || left.line != right.line {
        string.push(' ');
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use syn;

    #[test]
    fn should_parse_html() {
        let _: HtmlRoot = syn::parse_str(
            r#"
            <div>
                <button>Hello World!</button>

                <span>How are you?</span>
            </div>

            Nice to meet you.
        "#,
        ).unwrap();
    }

    #[test]
    fn should_parse_html_with_nested_rust_expr() {
        let _: HtmlRoot = syn::parse_str(
            r#"
            <div>
                My name is { "Anonymous" }.
            </div>
            "#,
        ).unwrap();
    }

    #[test]
    fn should_parse_text() {
        let text: Text =
            syn::parse_str("This is a text.    What do you think about    it?").unwrap();

        assert_eq!(text.content, "This is a text. What do you think about it?");
    }

    #[test]
    fn should_parse_multiline_text() {
        let text: Text = syn::parse_str(
            r#"
            This is a text.

            This is new line.
        "#,
        ).unwrap();

        assert_eq!(text.content, "This is a text. This is new line.");
    }

    #[test]
    fn should_parse_text_with_literal() {
        let text: Text = syn::parse_str(
            r#"
            "This is a literal"

            b"hello"

            12.122

            32
        "#,
        ).unwrap();

        assert_eq!(text.content, r#""This is a literal" b"hello" 12.122 32"#);
    }
}
