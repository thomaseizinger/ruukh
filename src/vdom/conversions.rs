//! Conversion from the non-component types to VNode for use in html! expression blocks.
//! Allows the user to use basic types such as string and number types ergonomically within
//! html! expression blocks.

use component::Render;
use std::borrow::Cow;
use vdom::vlist::VList;
use vdom::vtext::VText;
use vdom::VNode;

impl<RCTX: Render> From<String> for VNode<RCTX> {
    fn from(value: String) -> VNode<RCTX> {
        VNode::new(VText::text(value))
    }
}

impl<'a, RCTX: Render> From<&'a String> for VNode<RCTX> {
    fn from(value: &'a String) -> VNode<RCTX> {
        VNode::new(VText::text(value.as_str()))
    }
}

impl<'a, RCTX: Render> From<&'a str> for VNode<RCTX> {
    fn from(value: &'a str) -> VNode<RCTX> {
        VNode::new(VText::text(value))
    }
}

impl<'a, RCTX: Render> From<Cow<'a, str>> for VNode<RCTX> {
    fn from(value: Cow<'a, str>) -> VNode<RCTX> {
        VNode::new(VText::text(value))
    }
}

impl<'a, RCTX: Render> From<&'a Cow<'a, str>> for VNode<RCTX> {
    fn from(value: &'a Cow<'a, str>) -> VNode<RCTX> {
        VNode::new(VText::text(value.as_ref()))
    }
}

macro_rules! impl_with_to_string {
    ($($t:ty),*) => {
        $(
            impl<RCTX: Render> From<$t> for VNode<RCTX> {
                fn from(value: $t) -> VNode<RCTX> {
                    VNode::new(VText::text(value.to_string()))
                }
            }
        )*
    };
}

impl_with_to_string!(
    i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize, f32, f64, bool
);
