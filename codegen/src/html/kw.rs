//! Custom keywords used in the parser.
use syn::parse::ParseStream;

custom_keyword!(key);

macro_rules! custom_keywords {
    ($($ident:ident),*) => {
        $(
            custom_keyword!($ident);
        )*
    };
}

custom_keywords![area, base, br, col, embed, hr, img, input, link, meta, param, source, track, wbr];

macro_rules! is_self_closing {
    ($inp:ident is [$($ident:ident),*]) => {
        $(
            $inp.peek2($ident)
        )|| *
    };
}

pub fn is_self_closing(inp: ParseStream) -> bool {
    inp.peek(Token![<])
        && (is_self_closing!(
            inp is
            [area, base, br, col, embed, hr, img, input, link, meta, param, source, track, wbr]
        ))
}
