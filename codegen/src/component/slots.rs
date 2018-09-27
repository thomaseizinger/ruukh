use syn::{
    custom_keyword, parenthesized,
    parse::{Error, Parse, ParseStream, Result as ParseResult},
    punctuated::Punctuated,
    token, FnArg, Ident, Token,
};

/// Parses the arguments provided to the `#[slots]` attribute.
///
/// Looks like:
/// ```
/// (
///     slot default;
///
///     slot named(arg: i32);
/// )
/// ```
struct SlotsAttributeArgs {
    paren_token: token::Paren,
    arguments: Vec<SlotDeclaration>,
}

impl Parse for SlotsAttributeArgs {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let content;
        let paren_token = parenthesized!(content in input);
        let mut arguments = vec![];

        while !content.is_empty() {
            arguments.push(content.parse()?);
        }

        Ok(SlotsAttributeArgs {
            paren_token,
            arguments,
        })
    }
}

custom_keyword!(slot);

/// Parses a single slot declaration.
///
/// Looks like one of the following kinds:
/// ```
/// slot slot_name;
///
/// slot slot_name(arg: type, ...);
/// ```
struct SlotDeclaration {
    slot: slot,
    ident: Ident,
    paren_token: Option<token::Paren>,
    arguments: Option<Punctuated<FnArg, Token![,]>>,
    semi_token: Token![;],
}

impl Parse for SlotDeclaration {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let slot = input.parse()?;
        let ident: Ident = input.parse()?;

        let (paren_token, arguments) = if input.peek(token::Paren) {
            let content;
            let paren_token = parenthesized!(content in input);
            let arguments = content.parse_terminated(FnArg::parse)?;
            (Some(paren_token), Some(arguments))
        } else {
            (None, None)
        };

        if ident == "default" && paren_token.is_some() {
            return Err(Error::new(
                ident.span(),
                "`default` slot cannot have arguments. Use a different name.",
            ));
        }

        let semi_token = input.parse()?;

        Ok(SlotDeclaration {
            slot,
            ident,
            paren_token,
            arguments,
            semi_token,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn should_parse_slots_argument() {
        let slots_args: SlotsAttributeArgs = syn::parse_str(
            r#" ( 
                slot default;
                
                slot named(arg: i32, arg2: &'static str);
            ) "#,
        ).unwrap();

        assert_eq!(slots_args.arguments.len(), 2);
    }

    #[test]
    fn should_parse_argless_slot_declaration() {
        let slot_desc: SlotDeclaration = syn::parse_str(
            r#" (
                slot default;
            )"#,
        ).unwrap();

        assert_eq!(slot_desc.ident, "default");
        assert!(slot_desc.paren_token.is_none());
        assert!(slot_desc.arguments.is_none());
    }

    #[test]
    fn should_parse_argfull_slot_declaration() {
        let slot_desc: SlotDeclaration = syn::parse_str(
            r#" (
                slot named(arg: i32, arg1: &'static str);
            )"#,
        ).unwrap();

        assert_eq!(slot_desc.ident, "default");
        assert!(slot_desc.paren_token.is_some());
        assert_eq!(slot_desc.arguments.unwrap().len(), 2);
    }

    #[test]
    fn should_not_parse_args_on_default_declaration() {
        let slot_desc = syn::parse_str::<SlotDescription>(
            r#"(
                slot default(arg: i32);
            )"#,
        );

        assert!(slot_desc.is_err());
    }
}
