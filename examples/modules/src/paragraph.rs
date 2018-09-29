use self::{contents::*, title::*};
use ruukh::prelude::*;

mod contents;
mod title;

#[component]
#[derive(Lifecycle)]
pub struct Paragraph;

impl Render for Paragraph {
    fn render(&self) -> Markup<Self> {
        html! {
            <Title></Title>
            <p>
                <Contents></Contents>
            </p>
        }
    }
}
