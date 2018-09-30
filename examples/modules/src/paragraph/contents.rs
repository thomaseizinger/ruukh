use ruukh::prelude::*;

#[component]
#[derive(Lifecycle)]
pub struct Contents;

impl Render for Contents {
    fn render(&self) -> Markup<Self> {
        html! {
            "Lorem ipsum dolor sit amet." 
            "Lorem ipsum dolor sit amet." 
            "Lorem ipsum dolor sit amet." 
            "Lorem ipsum dolor sit amet." 
            "Lorem ipsum dolor sit amet." 
            "Lorem ipsum dolor sit amet." 
        }
    }
}
