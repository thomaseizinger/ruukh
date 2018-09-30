use ruukh::prelude::*;

#[component]
#[derive(Lifecycle)]
pub struct Title;

impl Render for Title {
    fn render(&self) -> Markup<Self> {
        html! {
            <h3>
                "Lorem ipsum dolor"
            </h3>
        }
    }
}
