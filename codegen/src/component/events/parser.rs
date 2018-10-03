use syn::{
    parenthesized,
    parse::{Error, Parse, ParseStream, Result as ParseResult},
    spanned::Spanned,
    Attribute, FnArg, Ident, ReturnType, Token,
};

/// The argument syntax for the `#[events]` attribute.
///
/// i.e. Parses ```ignore,compile_fail
/// (
///     fn event_name(&self, arg: type, ...) -> type;
///     fn event_name(&self, arg: type, ...) -> type;
///     fn event_name(&self, arg: type, ...) -> type;
/// )```
pub struct EventDeclarations {
    pub events: Vec<EventDeclaration>,
}

impl Parse for EventDeclarations {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let content;
        parenthesized!(content in input);

        let mut events = vec![];
        while !content.is_empty() {
            let event: EventDeclaration = content.parse()?;
            events.push(event);
        }
        Ok(EventDeclarations { events })
    }
}

/// The syntax of a single event.
///
/// ```ignore,compile_fail
/// #[optional]
/// fn event_name(&self, arg: type, ...) -> type;
/// ```
pub struct EventDeclaration {
    pub attr: Option<Attribute>,
    pub ident: Ident,
    pub args: Vec<FnArg>,
    pub return_type: ReturnType,
}

impl Parse for EventDeclaration {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let mut attrs = input.call(Attribute::parse_outer)?;
        if attrs.len() > 1 {
            Err(Error::new(
                attrs[0].span(),
                "Multiple attributes found. Only one allowed.",
            ))?;
        }

        input.parse::<Token![fn]>()?;
        let ident = input.parse()?;

        let content;
        parenthesized!(content in input);
        let args = content
            .parse_terminated::<_, Token![,]>(FnArg::parse)?
            .into_iter()
            .collect();

        let return_type = input.parse()?;
        // The `#[component]` macro was pointed to when the last one errored.
        if input.peek(Token![;]) {
            input.parse::<Token![;]>()?;
        } else {
            Err(input.error("expected `;`"))?;
        }

        Ok(EventDeclaration {
            attr: if attrs.is_empty() {
                None
            } else {
                Some(attrs.remove(0))
            },
            ident,
            args,
            return_type,
        })
    }
}
