use std::cell::RefCell;
use std::rc::Rc;
use KeyedVNodes;

/// Trait to define a Component. You do not need to implement this trait.
/// Use the auto derive provided as `#[derive(Component)]`.
///
/// A concrete implementation of a component must have two forms of state.
/// One, the read only state which is accessed by the user. Another, a
/// meta-state which stores the mutable state as well as other helpful values
/// to notify for component re-render.
///
/// Note: The user should not worry about the implementation detail of the
/// actual component. Use the auto derive on the component.
pub trait Component: Render {
    /// The prop type of a Component.
    type Props;
    /// The state type of a Component.
    type State: Default;

    /// Initializes a component with props as well as a meta-state called status
    fn init(props: Self::Props, status: ComponentStatus<Self::State>) -> Self;

    /// Updated the component with newer props and return older props (if changed). Also, set
    /// the component as dirty (if props changed), so that the component is re-rendered.
    fn update(&mut self, props: Self::Props) -> Option<Self::Props>;

    /// Update the read only state from the mutated status and return true if it has been updated.
    fn refresh_state(&mut self) -> bool;

    /// To find whether the component status has been altered
    fn is_dirty(&self) -> bool;

    /// Mark the component as clean after updation
    fn mark_clean(&mut self);
}

// Stores the metadata related to the state along with the state
struct Status<T> {
    state: T,
    dirty: bool,
}

/// Stores the state as well as the metadata to the state
#[derive(Clone)]
pub struct ComponentStatus<T>(Rc<RefCell<Status<T>>>);

impl<T> ComponentStatus<T> {
    #[allow(missing_docs)]
    pub fn new(state: T) -> ComponentStatus<T> {
        ComponentStatus(Rc::new(RefCell::new(Status {
            state,
            dirty: false,
        })))
    }
}

/// The lifecycle of a stateful component. Implement only the appropriate
/// hooks as needed.
///
/// For stateless component, mark the component with `#[stateless]`.
pub trait Lifecycle: Component {
    /// Invoked when the component is first created
    fn created(&self) {}

    /// Invoked when the component props are updated
    #[allow(unused_variables)]
    fn updated(&self, old_props: Self::Props) {}

    /// Invoked when the component is mounted onto the DOM tree
    fn mounted(&self) {}

    /// Invoked when the component is removed from the DOM tree
    fn destroyed(&self) {}
}

/// Trait to render a view for the component.
pub trait Render {
    #[allow(missing_docs)]
    fn render(&self) -> KeyedVNodes;
}
