extern crate ruukh;
extern crate ruukh_codegen;

use ruukh::component::{EventsPair, Status};
use ruukh::prelude::*;
use ruukh::Shared;

#[test]
fn should_impl_lifecycle() {
    impl Component for Button {
        type Props = ();
        type Events = ();
        type State = ();

        fn init<RCTX: Render>(
            _: Self::Props,
            _: <Self::Events as EventsPair<RCTX>>::Other,
            _: Shared<Status<Self::State>>,
            _: Shared<RCTX>,
        ) -> Self
        where
            Self::Events: EventsPair<RCTX>,
        {
            unimplemented!()
        }

        fn update<RCTX: Render>(
            &mut self,
            _: Self::Props,
            _: <Self::Events as EventsPair<RCTX>>::Other,
            _: Shared<RCTX>,
        ) -> Option<Self::Props>
        where
            Self::Events: EventsPair<RCTX>,
        {
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
