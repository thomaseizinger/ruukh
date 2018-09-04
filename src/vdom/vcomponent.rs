//! Component representation in a VDOM.

use component::{EventsPair, Render, Status};
use dom::{DOMInfo, DOMPatch, DOMRemove, DOMReorder};
use std::any::Any;
use std::fmt::{self, Display, Formatter};
use std::marker::PhantomData;
use vdom::{Shared, VNode};
use wasm_bindgen::prelude::JsValue;
use web_api::*;
use MessageSender;

/// The representation of a component in a Virtual DOM.
pub struct VComponent<RCTX: Render>(Box<ComponentManager<RCTX>>);

impl<RCTX: Render> VComponent<RCTX> {
    #[allow(missing_docs)]
    pub fn new<COMP: Render>(
        props: COMP::Props,
        events: <COMP::Events as EventsPair<RCTX>>::Other,
    ) -> VComponent<RCTX>
    where
        COMP::Events: EventsPair<RCTX>,
    {
        VComponent(Box::new(ComponentWrapper::<COMP, RCTX>::new(props, events)))
    }
}

pub(crate) struct ComponentWrapper<COMP: Render, RCTX: Render>
where
    COMP::Events: EventsPair<RCTX>,
{
    component: Option<Shared<COMP>>,
    props: Option<COMP::Props>,
    events: Option<<COMP::Events as EventsPair<RCTX>>::Other>,
    cached_render: Option<VNode<COMP>>,
    _phantom: PhantomData<RCTX>,
}

impl<COMP: Render, RCTX: Render> ComponentWrapper<COMP, RCTX>
where
    COMP::Events: EventsPair<RCTX>,
{
    pub(crate) fn new(
        props: COMP::Props,
        events: <COMP::Events as EventsPair<RCTX>>::Other,
    ) -> ComponentWrapper<COMP, RCTX> {
        ComponentWrapper {
            component: None,
            props: Some(props),
            events: Some(events),
            cached_render: None,
            _phantom: PhantomData,
        }
    }

    fn try_cast(
        other: Box<ComponentManager<RCTX>>,
    ) -> Result<ComponentWrapper<COMP, RCTX>, Box<ComponentManager<RCTX>>> {
        let mut same_type = false;
        {
            let any = other.as_any();
            if any.is::<ComponentWrapper<COMP, RCTX>>() {
                same_type = true;
            }
        }

        if same_type {
            let boxed = other.into_any();
            Ok(*boxed
                .downcast::<ComponentWrapper<COMP, RCTX>>()
                .expect("Impossible! The type cannot be different."))
        } else {
            Err(other)
        }
    }
}

impl<RCTX: Render> DOMPatch<RCTX> for VComponent<RCTX> {
    type Node = Node;

    fn render_walk(
        &mut self,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<RCTX>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        self.0.render_walk(parent, next, render_ctx, rx_sender)
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<RCTX>,
        _: MessageSender,
    ) -> Result<(), JsValue> {
        self.0.patch(old.map(|old| old.0), parent, next, render_ctx)
    }
}

impl<RCTX: Render> DOMReorder for VComponent<RCTX> {
    fn reorder(&self, parent: &Node, next: Option<&Node>) -> Result<(), JsValue> {
        self.0.reorder(parent, next)
    }
}

impl<RCTX: Render> DOMRemove for VComponent<RCTX> {
    type Node = Node;

    fn remove(mut self, parent: &Self::Node) -> Result<(), JsValue> {
        self.0.remove(parent)
    }
}

impl<RCTX: Render> DOMInfo for VComponent<RCTX> {
    fn node(&self) -> Option<&Node> {
        self.0.node()
    }
}

pub(crate) trait ComponentManager<RCTX: Render>: Downcast + Display {
    fn render_walk(
        &mut self,
        parent: &Node,
        next: Option<&Node>,
        render_ctx: Shared<RCTX>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue>;

    fn patch(
        &mut self,
        old: Option<Box<ComponentManager<RCTX>>>,
        parent: &Node,
        next: Option<&Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue>;

    fn reorder(&self, parent: &Node, next: Option<&Node>) -> Result<(), JsValue>;

    fn remove(&mut self, parent: &Node) -> Result<(), JsValue>;

    fn node(&self) -> Option<&Node>;

    fn as_any(&self) -> &Any;
}

impl<COMP: Render, RCTX: Render> ComponentManager<RCTX> for ComponentWrapper<COMP, RCTX>
where
    COMP::Events: EventsPair<RCTX>,
{
    fn render_walk(
        &mut self,
        parent: &Node,
        next: Option<&Node>,
        render_ctx: Shared<RCTX>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        if self.component.is_none() {
            let props = self.props.take().unwrap();
            let events = self.events.take().unwrap();
            let instance = COMP::init(
                props,
                events,
                Shared::new(Status::new(COMP::State::default(), rx_sender.clone())),
                render_ctx,
            );
            instance.created();
            let mut initial_render = instance.render();
            let shared_instance = Shared::new(instance);
            initial_render.patch(
                None,
                parent,
                next,
                shared_instance.clone(),
                rx_sender.clone(),
            )?;
            shared_instance.borrow().mounted();
            self.component = Some(shared_instance);
            self.cached_render = Some(initial_render);
        } else {
            let comp = self.component.as_ref().unwrap();

            let state_changed = comp.borrow_mut().is_state_dirty();
            if state_changed {
                comp.borrow_mut().refresh_state();
            }

            if state_changed || comp.borrow_mut().is_props_dirty() {
                let mut rerender = comp.borrow().render();
                let cached_render = self.cached_render.take();
                rerender.patch(cached_render, parent, next, comp.clone(), rx_sender.clone())?;
                self.cached_render = Some(rerender);
            }
        }
        if let Some(ref mut cached) = self.cached_render {
            cached.render_walk(
                parent,
                next,
                self.component.as_ref().unwrap().clone(),
                rx_sender,
            )?;
        }
        Ok(())
    }

    fn patch(
        &mut self,
        old: Option<Box<ComponentManager<RCTX>>>,
        parent: &Node,
        _: Option<&Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue> {
        if let Some(old) = old {
            match Self::try_cast(old) {
                Ok(same) => {
                    let comp = same.component.unwrap();
                    let props = self.props.take().unwrap();
                    let events = self.events.take().unwrap();

                    // Reuse the older component by passing in the newer props.
                    let old_props = comp.borrow_mut().update(props, events, render_ctx);
                    if let Some(old_props) = old_props {
                        comp.borrow().updated(old_props);
                    }
                    self.component = Some(comp);

                    // Reuse the cached render too to do patches on.
                    self.cached_render = same.cached_render;
                }
                Err(mut not_same) => {
                    // The component is not the same, remove it from the DOM tree.
                    not_same.remove(parent)?;
                }
            }
        }
        Ok(())
    }

    fn reorder(&self, parent: &Node, next: Option<&Node>) -> Result<(), JsValue> {
        if let Some(ref cached_render) = self.cached_render {
            cached_render.reorder(parent, next)?;
        }
        Ok(())
    }

    fn remove(&mut self, parent: &Node) -> Result<(), JsValue> {
        if let Some(cached_render) = self.cached_render.take() {
            cached_render.remove(parent)?;
            let comp = self.component.as_ref().unwrap();
            comp.borrow().destroyed();
        }
        Ok(())
    }

    fn node(&self) -> Option<&Node> {
        self.cached_render.as_ref().and_then(|inner| inner.node())
    }

    fn as_any(&self) -> &Any {
        self
    }
}

impl<RCTX: Render> From<VComponent<RCTX>> for VNode<RCTX> {
    fn from(comp: VComponent<RCTX>) -> VNode<RCTX> {
        VNode::Component(comp)
    }
}

pub(crate) trait Downcast: Any {
    fn into_any(self: Box<Self>) -> Box<Any>;
}

impl<T: Any> Downcast for T {
    fn into_any(self: Box<Self>) -> Box<Any> {
        self
    }
}

impl<RCTX: Render> Display for VComponent<RCTX> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<COMP: Render, RCTX: Render> Display for ComponentWrapper<COMP, RCTX>
where
    COMP::Events: EventsPair<RCTX>,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.cached_render
                .as_ref()
                .expect("Render the component first.")
        )
    }
}

#[cfg(test)]
pub mod wasm_test {
    use component::root_render_ctx;
    use dom::*;
    use prelude::*;
    use wasm_bindgen_test::*;
    use web_api::*;
    use Shared;

    struct Button {
        disabled: bool,
        __status: Shared<Status<()>>,
    }

    struct ButtonProps {
        disabled: bool,
    }

    impl Lifecycle for Button {}

    impl Component for Button {
        type Props = ButtonProps;
        type Events = ();
        type State = ();

        fn init<RCTX: Render>(
            props: Self::Props,
            _: <Self::Events as EventsPair<RCTX>>::Other,
            status: Shared<Status<Self::State>>,
            _: Shared<RCTX>,
        ) -> Self {
            Button {
                disabled: props.disabled,
                __status: status
            }
        }
        fn update<RCTX: Render>(
            &mut self,
            props: Self::Props,
            _: <Self::Events as EventsPair<RCTX>>::Other,
            _: Shared<RCTX>,
        ) -> Option<Self::Props> {
            if self.disabled != props.disabled {
                self.disabled = props.disabled;
                self.__status.borrow_mut().mark_props_dirty();
                Some(ButtonProps {
                    disabled: !self.disabled,
                })
            } else {
                None
            }
        }

        fn refresh_state(&mut self) {
            unreachable!()
        }

        fn is_state_dirty(&mut self) -> bool {
            false
        }

        fn is_props_dirty(&mut self) -> bool {
            self.__status.borrow_mut().is_props_dirty()
        }

        fn set_state<F>(&self, _: F) {
            unreachable!()
        }
    }

    impl Render for Button {
        fn render(&self) -> VNode<Self> {
            VNode::new(VElement::new(
                "button",
                vec![Attribute::new("disabled", self.disabled.to_string())],
                vec![],
                VNode::new(VText::text("Click")),
            ))
        }
    }

    fn container() -> Element {
        html_document.create_element("div").unwrap()
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_component() {
        let mut vcomp = VComponent::new::<Button>(ButtonProps { disabled: false }, ());
        let div = container();
        vcomp
            .render_walk(div.as_ref(), None, root_render_ctx(), ::message_sender())
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<button disabled="false">Click</button>"#
        );
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_component_update() {
        let mut vcomp = VComponent::new::<Button>(ButtonProps { disabled: false }, ());
        let div = container();
        vcomp
            .render_walk(div.as_ref(), None, root_render_ctx(), ::message_sender())
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<button disabled="false">Click</button>"#
        );

        let mut patched = VComponent::new::<Button>(ButtonProps { disabled: true }, ());
        patched
            .patch(
                Some(vcomp),
                div.as_ref(),
                None,
                root_render_ctx(),
                ::message_sender(),
            ).unwrap();
        patched
            .render_walk(div.as_ref(), None, root_render_ctx(), ::message_sender())
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<button disabled="true">Click</button>"#
        );
    }
}
