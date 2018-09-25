use ruukh::{component::Status, prelude::*};

#[test]
fn should_impl_lifecycle() {
    impl Component for Button {
        type Props = ();
        type Events = ();
        type State = ();

        fn init(_: Self::Props, _: Self::Events, _: Status<Self::State>) -> Self {
            unimplemented!()
        }

        fn update(&mut self, _: Self::Props, _: Self::Events) -> Option<Self::Props> {
            unimplemented!()
        }

        fn refresh_state(&mut self) {
            unimplemented!()
        }

        fn take_state_dirty(&self) -> bool {
            unimplemented!()
        }

        fn take_props_dirty(&self) -> bool {
            unimplemented!()
        }

        fn set_state<F>(&self, _: F) {
            unimplemented!()
        }
    }

    #[derive(Lifecycle)]
    struct Button;

    Button.created();
}
