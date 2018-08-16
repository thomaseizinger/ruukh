use std::mem;

trait Renderer {
    fn render(&self) -> Markup;
}

trait Component
where
    Self: Sized,
{
    type Props;
    type State;

    fn init(props: Self::Props, state: Rc<RefCell<ComponentStatus<Self::State>>>) -> Self;

    fn update_props(&mut self, props: Self::Props) -> Self::Props;

    fn update_state(&mut self) -> Self::State;

    fn is_dirty(&mut self) -> bool;
}

struct ComponentContainer<T: Component> {
    component: T,
}

impl<T: Component> ComponentContainer<T> {
    fn new(props: ButtonProps, scheduler: Scheduler) -> ComponentContainer
    where
        T::State: Default,
    {
        let state = Rc::new(RefCell::new(ComponentStatus {
            state: T::State::default(),
            dirty: false,
            tx: scheduler,
        }));
        let component = T::init(props, state);
        ComponentContainer { component }
    }

    fn reuse_state(&mut self, props: T::Props) -> T::Props {
        self.component.update_props(self.props)
    }

    fn update_state(&mut self) -> T::State {
        self.component.update_state()
    }

    fn is_dirty(&self) -> bool {
        self.component.__is_dirty()
    }
}

struct ButtonProps {
    count: i32,
}

struct ComponentStatus<T> {
    state: T,
    dirty: bool,
    tx: Scheduler,
}

type State<T> = Rc<RefCell<ComponentStatus<T>>>;

struct ButtonState {
    toggle: bool,
}

struct Button {
    count: i32,
    toggle: bool,
    __state: State<ButtonState>,
}

impl Component for Button {
    type Props = ButtonProps;
    type State = ButtonState;

    fn init(props: ButtonProps, state: State<ButtonState>) -> Button {
        let __state = state.borrow();
        let toggle = __state.toggle.clone();
        Button {
            count: props.count,
            toggle,
            state,
        }
    }

    fn update_props(&mut self, mut props: ButtonProps) -> ButtonProps {
        mem::swap(&mut self.count, &mut props.count);
        // Return older props
        props
    }

    fn update_state(&mut self) -> ButtonState {
        let state = self.__state.borrow();
        let mut toggle = state.toggle.clone();
        mem::swap(&mut self.toggle, &mut toggle);
        ButtonState { toggle }
    }

    fn is_dirty(&self) -> bool {
        let state = self.__state.borrow();
        state.__is_dirty
    }

    fn set<F>(f: F)
    where
        F: FnMut(T::State),
    {
        let mut __state = self.__state.borrow_mut();
        f(&mut __state.state);
        __state.__dirty = true;
        __state.__tx.send_req();
    }
}

impl Button {
    fn on_input(&self, event: InputEvent) {
        self.set(|state| {
            state.toggle = true;
        });
    }
}

impl Renderer for Button {
    fn render(&self) -> Markup {
        h!(
            tag: "div",
            child: h!([
                h!(
                    tag: "h1",
                    child: "Hello! How are you?"
                ),
                h!(
                    tag: "p",
                    child: "Everything started here! Nothing could be stopped."
                ),
                h!(
                    tag: "button",
                    class: "btn btn-primary",
                    @input: |event| {
                        self.on_input(event);
                    },
                    child: if self.toggle { "Clicked" } else { "Unclicked" }
                )
            ])
        )
    }
}

#[derive(Component)]
struct Button {
    count: i32,
    #[state]
    toggle: bool,
}

impl Button {
    fn on_input(&self, event: InputEvent) {
        self.set(|state| {
            state.toggle = true;
        });
    }
}

impl Renderer for Button {
    fn render(&self) -> Markup {
        html! {
            <div>
                <h1>Hello! How are you?</h1>
                <p>Everything started here! Nothing could be stopped.</p>
                <button class="btn btn-primary" @input=invoke!(self.on_input)>
                    { if self.toggle { "Clicked" } else { "Unclicked" } }
                </button>
            </div>
        }
    }
}

bundle! {
    <template>
        <div>
            <h1>Hello! How are you?</h1>
            <p>Everything started here! Nothing could be stopped.</p>
            <button class="btn btn-primary" @input=invoke!(self.on_input)>
                { if self.toggle { "Clicked" } else { "Unclicked" } }
            </button>
        </div>
    </template>

    <script>
        #[derive(Component)]
        struct Button {
            count: i32,
            #[state]
            toggle: bool
        }

        impl Button {
            fn on_input(&self, event: InputEvent) {
                self.set(|state| {
                    state.toggle = true;
                });
            }
        }
    </script>

    <style lang="scss">
        .btn {
            padding: 1rem 2rem;
            background-color: white;
            border-radius: 4px;

            .btn-primary {
                background-color: blue;
            }
        }
    </style>
}

enum Size {
    Sm,
    Md,
    Lg,
}

enum Variant {
    Primary,
    Secondary,
    Light,
    Dark,
    Warning,
    Danger,
    Other(String),
}

#[derive(Component)]
#[stateless]
struct BButton {
    #[prop(default = Size::Md)]
    size: Size,

    #[prop(default = Variant::Primary)]
    variant: Variant,

    disabled: Option<bool>,

    on_click: Event<Fn()>,

    child: Slot,

    side_val: Slot
}

impl Renderer for BButton {
    fn render(&self) -> Markup {
        html! {
            <button
                class=format!("btn btn-{} btn-{}", self.size, self.variant)
                disabled=self.disabled
                @click=|_| { self.on_click() }
            >
                <slot value=self.child></slot>
                <slot value=self.side_val></slot>
            </button>

            // special
            <style>
                
            </style>
        }
    }
}

render!(
    <b-button 
        size=Size::Lg 
        variant=Variant::Warning
    >
        Hello
        <slot name="side_val">
            World!
        </slot>
    </b-button>
)
