//! Component representation in a VDOM.

use component::{Render, Status};
use dom::{DOMInfo, DOMPatch, DOMRemove};
use std::any::Any;
use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};
use std::marker::PhantomData;
use std::rc::Rc;
use vdom::{KeyedVNodes, Shared, VNode};
use wasm_bindgen::prelude::JsValue;
use web_api::*;

/// The representation of a component in a Virtual DOM.
pub struct VComponent<RCTX: Render>(Box<ComponentManager<RCTX>>);

impl<RCTX: Render> VComponent<RCTX> {
    #[allow(missing_docs)]
    pub fn new<COMP: Render>(props: COMP::Props, events: COMP::Events) -> VComponent<RCTX> {
        VComponent(Box::new(ComponentWrapper::<COMP, RCTX>::new(props, events)))
    }
}

struct ComponentWrapper<COMP: Render, RCTX: Render> {
    component: Option<Shared<COMP>>,
    props: Option<COMP::Props>,
    events: Option<COMP::Events>,
    cached_render: Option<KeyedVNodes<COMP>>,
    _phantom: PhantomData<RCTX>,
}

impl<COMP: Render, RCTX: Render> ComponentWrapper<COMP, RCTX> {
    fn new(props: COMP::Props, events: COMP::Events) -> ComponentWrapper<COMP, RCTX> {
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
            let any = &other as &Any;
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
        parent: Self::Node,
        next: Option<Self::Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue> {
        self.0.render_walk(parent, next, render_ctx)
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: Self::Node,
        next: Option<Self::Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue> {
        self.0.patch(old.map(|old| old.0), parent, next, render_ctx)
    }
}

impl<RCTX: Render> DOMRemove for VComponent<RCTX> {
    type Node = Node;

    fn remove(mut self, parent: Self::Node) -> Result<(), JsValue> {
        self.0.remove(parent)
    }
}

impl<RCTX: Render> DOMInfo for VComponent<RCTX> {
    fn node(&self) -> Option<Node> {
        self.0.node()
    }
}

trait ComponentManager<RCTX: Render>: Downcast + Display {
    fn render_walk(
        &mut self,
        parent: Node,
        next: Option<Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue>;

    fn patch(
        &mut self,
        old: Option<Box<ComponentManager<RCTX>>>,
        parent: Node,
        next: Option<Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue>;

    fn remove(&mut self, parent: Node) -> Result<(), JsValue>;

    fn node(&self) -> Option<Node>;
}

impl<COMP: Render, RCTX: Render> ComponentManager<RCTX> for ComponentWrapper<COMP, RCTX> {
    fn render_walk(
        &mut self,
        parent: Node,
        next: Option<Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue> {
        if self.component.is_none() {
            let props = self.props.take().unwrap();
            let events = self.events.take().unwrap();
            let instance = COMP::init(
                props,
                events,
                Rc::new(RefCell::new(Status::new(COMP::State::default()))),
                render_ctx,
            );
            instance.created();
            let mut initial_render = instance.render();
            let shared_instance = Rc::new(RefCell::new(instance));
            initial_render.patch(None, parent.clone(), next.clone(), shared_instance.clone())?;
            self.component = Some(shared_instance);
            self.cached_render = Some(initial_render);
        } else {
            let comp = self.component.as_ref().unwrap();

            let state_changed = if comp.borrow_mut().is_state_dirty() {
                comp.borrow_mut().refresh_state()
            } else {
                false
            };

            if state_changed || comp.borrow_mut().is_props_dirty() {
                let mut rerender = comp.borrow().render();
                let cached_render = self.cached_render.take();
                rerender.patch(cached_render, parent.clone(), next.clone(), comp.clone())?;
                self.cached_render = Some(rerender);
            }
        }
        if let Some(ref mut cached) = self.cached_render {
            cached.render_walk(parent, next, self.component.as_ref().unwrap().clone())?;
        }
        Ok(())
    }

    fn patch(
        &mut self,
        old: Option<Box<ComponentManager<RCTX>>>,
        parent: Node,
        _: Option<Node>,
        render_ctx: Shared<RCTX>,
    ) -> Result<(), JsValue> {
        if let Some(old) = old {
            match Self::try_cast(old) {
                Ok(same) => {
                    let comp = same.component.unwrap();
                    let props = self.props.take().unwrap();
                    let events = self.events.take().unwrap();

                    // Reuse the older component by passing in the newer props.
                    if let Some(old_props) = comp.borrow_mut().update(props, events, render_ctx) {
                        comp.borrow().updated(old_props);
                    }
                    self.component = Some(comp);

                    // Reuse the cached render too to do patches on.
                    self.cached_render = same.cached_render;
                }
                Err(mut not_same) => {
                    // The component is not the same, remove it from the DOM tree.
                    not_same.remove(parent.clone())?;
                }
            }
        }
        Ok(())
    }

    fn remove(&mut self, parent: Node) -> Result<(), JsValue> {
        if let Some(cached_render) = self.cached_render.take() {
            cached_render.remove(parent)?;
            let comp = self.component.as_ref().unwrap();
            comp.borrow().destroyed();
        }
        Ok(())
    }

    fn node(&self) -> Option<Node> {
        self.cached_render.as_ref().and_then(|inner| inner.node())
    }
}

impl<RCTX: Render> From<VComponent<RCTX>> for VNode<RCTX> {
    fn from(comp: VComponent<RCTX>) -> VNode<RCTX> {
        VNode::Component(comp)
    }
}

trait Downcast: Any {
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

impl<COMP: Render, RCTX: Render> Display for ComponentWrapper<COMP, RCTX> {
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

    #[derive(Debug)]
    struct Button {
        disabled: bool,
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
            _: Self::Events,
            _: Shared<Status<Self::State>>,
            _: Shared<RCTX>,
        ) -> Self {
            Button {
                disabled: props.disabled,
            }
        }
        fn update<RCTX: Render>(
            &mut self,
            props: Self::Props,
            _: Self::Events,
            _: Shared<RCTX>,
        ) -> Option<Self::Props> {
            if self.disabled != props.disabled {
                self.disabled = props.disabled;
                Some(ButtonProps {
                    disabled: !self.disabled,
                })
            } else {
                None
            }
        }

        fn refresh_state(&mut self) -> bool {
            unreachable!()
        }

        fn is_state_dirty(&mut self) -> bool {
            unreachable!()
        }

        fn is_props_dirty(&mut self) -> bool {
            unreachable!()
        }
    }

    impl Render for Button {
        fn render(&self) -> KeyedVNodes<Self> {
            KeyedVNodes::unkeyed(VElement::new(
                "button",
                vec![Attribute::new("disabled", self.disabled.to_string())],
                vec![],
                KeyedVNodes::unkeyed(VText::text("Click")),
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
            .patch(None, div.clone().into(), None, root_render_ctx())
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
            .patch(None, div.clone().into(), None, root_render_ctx())
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<button disabled="false">Click</button>"#
        );

        let mut patched = VComponent::new::<Button>(ButtonProps { disabled: true }, ());
        patched
            .patch(Some(vcomp), div.clone().into(), None, root_render_ctx())
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<button disabled="true">Click</button>"#
        );
    }
}
