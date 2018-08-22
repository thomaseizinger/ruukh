//! Component representation in a VDOM.

use component::{ComponentStatus, Lifecycle};
use std::any::Any;
use std::fmt::{self, Debug, Display, Formatter};
use {KeyedVNodes, VNode};
if_wasm! {
    use wasm_bindgen::prelude::JsValue;
    use dom::DOMPatch;
    use web_api::*;
}

/// The representation of a component in a Virtual DOM.
pub struct VComponent {
    /// The manager of the components, handles render as well as state management.
    manager: Box<ComponentManager>,
    /// If there are no changes in the state/props of the component, reuse the render
    cached_render: Option<Box<KeyedVNodes>>,
}

impl VComponent {
    #[allow(missing_docs)]
    pub fn new<T: Lifecycle + Debug + 'static>(props: T::Props) -> VComponent {
        VComponent {
            manager: Box::new(ComponentWrapper::<T>::new(props)),
            cached_render: None,
        }
    }
}

impl Debug for VComponent {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "VComponent {{ component: {}, cached_render: {:?} }}",
            self.manager.debug(),
            self.cached_render
        )
    }
}

impl Display for VComponent {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.cached_render
                .as_ref()
                .expect("The component should have been pre-rendered.")
        )
    }
}

#[cfg(target_arch = "wasm32")]
impl DOMPatch for VComponent {
    type Node = Node;

    fn render_walk(&mut self, parent: Self::Node, next: Option<Self::Node>) -> Result<(), JsValue> {
        if !self.manager.is_init() {
            self.manager.init();
            let mut rendered = self.manager.render().unwrap();
            rendered.patch(None, parent, next)?;
            self.cached_render = Some(Box::new(rendered));
            self.manager.mounted();
        } else {
            if self.manager.is_dirty().unwrap() {
                let cached = self.cached_render.take().unwrap();
                self.manager.refresh_state();
                let mut new_render = self.manager.render().unwrap();
                new_render.patch(Some(*cached), parent, next)?;
                self.cached_render = Some(Box::new(new_render));
            } else {
                self.cached_render
                    .as_mut()
                    .unwrap()
                    .render_walk(parent, next)?;
            }
        }
        Ok(())
    }

    fn patch(
        &mut self,
        old: Option<Self>,
        parent: Self::Node,
        next: Option<Self::Node>,
    ) -> Result<(), JsValue> {
        if let Some(old) = old {
            if let Some(old_manager) = self.manager.merge(old.manager) {
                let old_render = old
                    .cached_render
                    .expect("Old components have already rendered once.");
                old_render.remove(parent.clone())?;
                old_manager.destroyed();
            }
        }
        self.render_walk(parent, next)
    }

    fn remove(self, parent: Self::Node) -> Result<(), JsValue> {
        if let Some(cached_render) = self.cached_render {
            cached_render.remove(parent)?;
        }
        Ok(())
    }

    fn node(&self) -> Option<Node> {
        self.cached_render.as_ref().and_then(|cached| cached.node())
    }
}

impl From<VComponent> for VNode {
    fn from(comp: VComponent) -> VNode {
        VNode::Component(comp)
    }
}

/// Trait to handle the lifecycle of the component from initialization to render.Box<ComponentManager>
/// Currently, the interface is not thought out, will define it as needed.
trait ComponentManager: Downcast {
    /// The debug implementation of the component.
    fn debug(&self) -> String;

    /// Whether the component is initialized.
    fn is_init(&self) -> bool;

    /// Initialization of the component for the first time.
    fn init(&mut self);

    /// Try to merge the state of the older component with self
    fn merge(&mut self, other: Box<ComponentManager>) -> Option<Box<ComponentManager>>;

    /// Check whether the component is dirtied
    fn is_dirty(&self) -> Option<bool>;

    /// Mark the component as clean after updation
    fn mark_clean(&mut self);

    /// Refresh the state after it has been found dirty
    fn refresh_state(&mut self);

    /// Propagate the mounted lifecycle hook to the component
    fn mounted(&self);

    /// Propagate the destroyed lifecylce hook to the component
    fn destroyed(&self);

    /// Generate a markup from the component
    fn render(&self) -> Option<KeyedVNodes>;
}

struct ComponentWrapper<T: Lifecycle + 'static> {
    component: Option<T>,
    props: Option<T::Props>,
}

impl<T: Lifecycle + 'static> ComponentWrapper<T> {
    fn new(props: T::Props) -> ComponentWrapper<T> {
        ComponentWrapper {
            component: None,
            props: Some(props),
        }
    }

    fn try_cast(
        other: Box<ComponentManager>,
    ) -> Result<ComponentWrapper<T>, Box<ComponentManager>> {
        let mut same_type = false;
        {
            let any = &other as &Any;
            if any.is::<ComponentWrapper<T>>() {
                same_type = true;
            }
        }

        if same_type {
            let boxed = other.into_any();
            Ok(*boxed
                .downcast::<ComponentWrapper<T>>()
                .expect("Impossible! The type cannot be different."))
        } else {
            Err(other)
        }
    }
}

impl<T: Lifecycle + Debug + 'static> ComponentManager for ComponentWrapper<T> {
    fn debug(&self) -> String {
        format!("{:?}", self.component)
    }

    fn is_init(&self) -> bool {
        self.component.is_some()
    }

    fn init(&mut self) {
        let props = self
            .props
            .take()
            .expect("A component can be initialized only once.");
        let comp = T::init(props, ComponentStatus::new(T::State::default()));
        comp.created();
        self.component = Some(comp);
    }

    fn merge(&mut self, other: Box<ComponentManager>) -> Option<Box<ComponentManager>> {
        match Self::try_cast(other) {
            // The components are same
            Ok(other) => {
                let props = self
                    .props
                    .take()
                    .expect("Older components cannot be merged");

                let old_comp = other
                    .component
                    .expect("Older components must be initialized before they can be merged");

                // Use the state/status from the older component
                let new_comp = T::init(props, old_comp.status());
                // Invoke the lifecycle event of updated.
                new_comp.updated(old_comp.props());
                self.component = Some(new_comp);
                None
            }
            Err(manager) => Some(manager),
        }
    }

    fn is_dirty(&self) -> Option<bool> {
        self.component.as_ref().map(|comp| comp.is_dirty())
    }

    fn mark_clean(&mut self) {
        self.component.as_mut().map(|comp| comp.mark_clean());
    }

    fn refresh_state(&mut self) {
        self.component.as_mut().map(|comp| comp.refresh_state());
    }

    fn mounted(&self) {
        self.component.as_ref().map(|comp| comp.mounted());
    }

    fn destroyed(&self) {
        self.component.as_ref().map(|comp| comp.destroyed());
    }

    fn render(&self) -> Option<KeyedVNodes> {
        self.component.as_ref().map(|comp| comp.render())
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

#[cfg(test)]
#[cfg(target_arch = "wasm32")]
pub mod wasm_test {
    use dom::*;
    use prelude::*;
    use wasm_bindgen_test::*;
    use web_api::*;

    #[derive(Debug)]
    struct Button {
        disabled: bool,
        __status: ComponentStatus<()>,
    }

    struct ButtonProps {
        disabled: bool,
    }

    impl Lifecycle for Button {}

    impl Component for Button {
        type Props = ButtonProps;
        type State = ();

        fn init(props: Self::Props, status: ComponentStatus<Self::State>) -> Self {
            Button {
                disabled: props.disabled,
                __status: status,
            }
        }

        fn props(self) -> Self::Props {
            ButtonProps {
                disabled: self.disabled,
            }
        }

        fn status(&self) -> ComponentStatus<Self::State> {
            self.__status.clone()
        }

        fn refresh_state(&mut self) {}

        fn is_dirty(&self) -> bool {
            false
        }

        fn mark_clean(&mut self) {}
    }

    impl Render for Button {
        fn render(&self) -> KeyedVNodes {
            KeyedVNodes::unkeyed(VElement::new(
                "button",
                vec![Attribute::new("disabled", self.disabled.to_string())],
                KeyedVNodes::unkeyed(VText::text("Click")),
            ))
        }
    }

    fn container() -> Element {
        html_document.create_element("div").unwrap()
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_component() {
        let mut vcomp = VComponent::new::<Button>(ButtonProps { disabled: false });
        let div = container();
        vcomp
            .patch(None, div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<button disabled="false">Click</button>"#
        );
    }

    #[wasm_bindgen_test]
    fn should_patch_container_with_component_update() {
        let mut vcomp = VComponent::new::<Button>(ButtonProps { disabled: false });
        let div = container();
        vcomp
            .patch(None, div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<button disabled="false">Click</button>"#
        );

        let mut patched = VComponent::new::<Button>(ButtonProps { disabled: true });
        patched
            .patch(Some(vcomp), div.clone().into(), None)
            .expect("To patch div");

        assert_eq!(
            div.inner_html(),
            r#"<button disabled="true">Click</button>"#
        );
    }
}
