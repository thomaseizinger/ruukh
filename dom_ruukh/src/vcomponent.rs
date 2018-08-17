use component::{ComponentStatus, Lifecycle};
use std::any::Any;
use std::fmt::{self, Debug, Formatter};
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

    /// Try to merge the state of the older component with self
    fn merge(&mut self, other: Box<ComponentManager>) -> Option<Box<ComponentManager>>;

    /// Generate a markup from the component
    fn render(&self) -> KeyedVNodes;
}

type TryCast<T> = fn(Box<ComponentManager>) -> Result<T, Box<ComponentManager>>;

struct ComponentWrapper<T: Lifecycle + 'static> {
    component: T,
    try_cast: TryCast<T>,
}

impl<T: Lifecycle + 'static> ComponentWrapper<T> {
    fn new(props: T::Props) -> ComponentWrapper<T> {
        ComponentWrapper {
            component: T::init(props, ComponentStatus::new(T::State::default())),
            try_cast: |other| {
                let mut same_type = false;
                {
                    let any = &other as &Any;
                    if any.is::<T>() {
                        same_type = true;
                    }
                }

                if same_type {
                    let boxed = other.into_any();
                    Ok(*boxed
                        .downcast::<T>()
                        .expect("Impossible! The type cannot be different."))
                } else {
                    Err(other)
                }
            },
        }
    }
}

impl<T: Lifecycle + Debug + 'static> ComponentManager for ComponentWrapper<T> {
    fn debug(&self) -> String {
        format!("{:?}", self.component)
    }

    fn merge(&mut self, other: Box<ComponentManager>) -> Option<Box<ComponentManager>> {
        match (self.try_cast)(other) {
            // The components are same
            Ok(other) => {
                // Use the state/status from the older component
                self.component.reuse_status(other.status());
                // Update the state values to the newer updated one
                self.component.refresh_state();

                // Invoke the lifecycle event of updated.
                self.component.updated(other.props());
                None
            }
            Err(manager) => Some(manager),
        }
    }

    fn render(&self) -> KeyedVNodes {
        self.component.render()
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
