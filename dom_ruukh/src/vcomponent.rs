use component::{ComponentStatus, Lifecycle};
use std::any::Any;
use std::fmt::{self, Debug, Display, Formatter};
use {KeyedVNodes, VNode};

/// The representation of a component in a Virtual DOM.
pub struct VComponent {
    /// The manager of the components, handles render as well as state management.
    manager: Box<ComponentManager>,
    /// If there are no changes in the state/props of the component, reuse the render
    cached_render: Option<Box<KeyedVNodes>>,
}

impl VComponent {
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

    /// Initialization of the component for the first time.
    fn init(&mut self);

    /// Try to merge the state of the older component with self
    fn merge(&mut self, other: Box<ComponentManager>) -> Option<Box<ComponentManager>>;

    /// Check whether the component is dirtied
    fn is_dirty(&self) -> Option<bool>;

    /// Generate a markup from the component
    fn render(&self) -> KeyedVNodes;
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

    fn render(&self) -> KeyedVNodes {
        self.component
            .as_ref()
            .expect("Initialize the component before rendering.")
            .render()
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
