//! Slot representation in a VDOM.
use crate::{component::Render, dom::DOMPatch, vdom::VNode, web_api::*, MessageSender, Shared};
use std::{cell::RefCell, collections::HashMap, marker::PhantomData, rc::Rc};
use wasm_bindgen::prelude::JsValue;

/// Index of the slot in `SlotCache`.
pub type SlotIndex = usize;

/// Slot representation in a VNode.
pub struct VSlot<RCTX: Render> {
    index: SlotIndex,
    slot_link: Box<SlotLink>,
    _phantom: PhantomData<RCTX>,
}

impl<RCTX: Render> VSlot<RCTX> {
    /// Resolve the slot and create a new VSlot.
    pub fn resolve_slot<ARGS: 'static>(
        slot_resolve: Shared<SlotUse<ARGS>>,
        args: ARGS,
    ) -> VSlot<RCTX> {
        let index = slot_resolve.borrow().slot_use(args);
        VSlot::new(index, Box::new(slot_resolve))
    }

    /// Create a new VSlot.
    fn new(index: SlotIndex, slot_link: Box<SlotLink>) -> VSlot<RCTX> {
        VSlot {
            index,
            slot_link,
            _phantom: PhantomData,
        }
    }
}

impl<RCTX: Render> DOMPatch for VSlot<RCTX> {
    type RenderContext = RCTX;
    type Node = Node;

    fn render_walk(
        &mut self,
        _: &Self::Node,
        _: Option<&Self::Node>,
        _: Shared<Self::RenderContext>,
        _: MessageSender,
    ) -> Result<(), JsValue> {
        unreachable!("Slot does not contain a component here to walk upon.");
    }

    fn patch(
        &mut self,
        _: Option<&mut Self>,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        _: Shared<Self::RenderContext>,
        _: MessageSender,
    ) -> Result<(), JsValue> {
        self.slot_link
            .set_parent_and_next_of(self.index, parent.clone(), next.cloned());
        Ok(())
    }

    fn reorder(&self, parent: &Self::Node, next: Option<&Self::Node>) -> Result<(), JsValue> {
        self.slot_link
            .set_parent_and_next_of(self.index, parent.clone(), next.cloned());
        Ok(())
    }

    fn remove(&self, _: &Self::Node) -> Result<(), JsValue> {
        unreachable!("Slot does not have anything to remove.");
    }

    fn node(&self) -> Option<&Node> {
        unreachable!("Slot has no node attached to it.");
    }
}

/// A registry of the resolved vnodes and the slots of a component.
pub struct SlotRegistry<COMP: Render, RCTX: Render> {
    cache: SlotCache<RCTX>,
    slots: Option<COMP::Slots>,
}

impl<COMP: Render, RCTX: Render> SlotRegistry<COMP, RCTX> {
    /// Create a new SlotRegistry with component Slots and a cache.
    pub fn new(slots: COMP::Slots, cache: SlotCache<RCTX>) -> SlotRegistry<COMP, RCTX> {
        SlotRegistry {
            cache,
            slots: Some(slots),
        }
    }

    pub(crate) fn take_slots(&mut self) -> COMP::Slots {
        self.slots.take().unwrap()
    }
}

impl<COMP: Render<Slots = ()>, RCTX: Render> Default for SlotRegistry<COMP, RCTX> {
    fn default() -> Self {
        SlotRegistry::new((), SlotCache::new())
    }
}

impl<COMP: Render, RCTX: Render> DOMPatch for SlotRegistry<COMP, RCTX> {
    type RenderContext = RCTX;
    type Node = ();

    fn render_walk(
        &mut self,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<Self::RenderContext>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        self.cache.render_walk(parent, next, render_ctx, rx_sender)
    }

    fn patch(
        &mut self,
        old: Option<&mut Self>,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<Self::RenderContext>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        self.cache.patch(
            old.map(|o| &mut o.cache),
            parent,
            next,
            render_ctx,
            rx_sender,
        )
    }

    fn reorder(&self, parent: &Self::Node, next: Option<&Self::Node>) -> Result<(), JsValue> {
        self.cache.reorder(parent, next)
    }

    fn remove(&self, parent: &Self::Node) -> Result<(), JsValue> {
        self.cache.remove(parent)
    }

    fn node(&self) -> Option<&Node> {
        self.cache.node()
    }
}

/// A cache to hold resolved slots of a component.
pub struct SlotCache<RCTX: Render>(HashMap<&'static str, ResolvedSlots<RCTX>>);

impl<RCTX: Render> SlotCache<RCTX> {
    /// Create a new slot.
    pub fn new() -> SlotCache<RCTX> {
        Default::default()
    }

    /// Get a cache for a slot.
    pub fn cache_for_slot(&mut self, slot_name: &'static str) -> ResolvedSlots<RCTX> {
        if let Some(slot) = self.0.get(slot_name) {
            slot.clone()
        } else {
            let slot = ResolvedSlots::empty();
            self.0.insert(slot_name, slot.clone());
            slot
        }
    }
}

impl<RCTX: Render> Default for SlotCache<RCTX> {
    fn default() -> Self {
        SlotCache(HashMap::new())
    }
}

impl<RCTX: Render> DOMPatch for SlotCache<RCTX> {
    type RenderContext = RCTX;
    type Node = ();

    fn render_walk(
        &mut self,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<Self::RenderContext>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        for (_, resolved_slots) in self.0.iter_mut() {
            for resolved_slot in resolved_slots.0.borrow_mut().iter_mut() {
                resolved_slot.render_walk(parent, next, render_ctx.clone(), rx_sender.clone())?;
            }
        }
        Ok(())
    }

    fn patch(
        &mut self,
        mut old: Option<&mut Self>,
        parent: &Self::Node,
        next: Option<&Self::Node>,
        render_ctx: Shared<Self::RenderContext>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        for (slot_name, resolved_slots) in self.0.iter_mut() {
            if let Some(ref mut old) = old {
                if let Some(old_resolved_slots) = old.0.get_mut(slot_name) {
                    for (index, resolved_slot) in
                        resolved_slots.0.borrow_mut().iter_mut().enumerate()
                    {
                        resolved_slot.patch(
                            old_resolved_slots.0.borrow_mut().get_mut(index),
                            parent,
                            next,
                            render_ctx.clone(),
                            rx_sender.clone(),
                        )?;
                    }

                    for old_resolved_slot in old_resolved_slots
                        .0
                        .borrow_mut()
                        .iter()
                        .skip(resolved_slots.0.borrow().len())
                    {
                        old_resolved_slot.remove(parent)?;
                    }

                    continue;
                }
            }

            for resolved_slot in resolved_slots.0.borrow_mut().iter_mut() {
                resolved_slot.patch(None, parent, next, render_ctx.clone(), rx_sender.clone())?;
            }
        }
        Ok(())
    }

    fn reorder(&self, parent: &Self::Node, next: Option<&Self::Node>) -> Result<(), JsValue> {
        for (_, resolved_slots) in self.0.iter() {
            for resolved_slot in resolved_slots.0.borrow_mut().iter() {
                resolved_slot.reorder(parent, next)?;
            }
        }
        Ok(())
    }

    fn remove(&self, parent: &Self::Node) -> Result<(), JsValue> {
        for (_, resolved_slots) in self.0.iter() {
            for resolved_slot in resolved_slots.0.borrow_mut().iter() {
                resolved_slot.remove(parent)?;
            }
        }
        Ok(())
    }

    fn node(&self) -> Option<&Node> {
        unimplemented!("This feature has no purpose here.");
    }
}

/// A wrapper around a list of Resolved Slots.
pub struct ResolvedSlots<RCTX: Render>(Shared<Vec<ResolvedSlot<RCTX>>>);

impl<RCTX: Render> ResolvedSlots<RCTX> {
    fn empty() -> Self {
        ResolvedSlots(Rc::new(RefCell::new(vec![])))
    }
}

impl<RCTX: Render> Clone for ResolvedSlots<RCTX> {
    fn clone(&self) -> Self {
        ResolvedSlots(self.0.clone())
    }
}

/// A representation of a resolved Slot VNodes.
struct ResolvedSlot<RCTX: Render> {
    parent: Option<Node>,
    next: Option<Node>,
    vnode: VNode<RCTX>,
}

impl<RCTX: Render> DOMPatch for ResolvedSlot<RCTX> {
    type RenderContext = RCTX;
    type Node = ();

    fn render_walk(
        &mut self,
        _: &Self::Node,
        _: Option<&Self::Node>,
        render_ctx: Shared<Self::RenderContext>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        self.vnode.render_walk(
            self.parent.as_ref().unwrap(),
            self.next.as_ref(),
            render_ctx,
            rx_sender,
        )
    }

    fn patch(
        &mut self,
        old: Option<&mut Self>,
        _: &Self::Node,
        _: Option<&Self::Node>,
        render_ctx: Shared<Self::RenderContext>,
        rx_sender: MessageSender,
    ) -> Result<(), JsValue> {
        self.vnode.patch(
            old.map(|o| &mut o.vnode),
            self.parent.as_ref().unwrap(),
            self.next.as_ref(),
            render_ctx,
            rx_sender,
        )
    }

    fn reorder(&self, _: &Self::Node, _: Option<&Self::Node>) -> Result<(), JsValue> {
        self.vnode
            .reorder(self.parent.as_ref().unwrap(), self.next.as_ref())
    }

    fn remove(&self, _: &Self::Node) -> Result<(), JsValue> {
        self.vnode.remove(self.parent.as_ref().unwrap())
    }

    fn node(&self) -> Option<&Node> {
        unimplemented!("This feature has no purpose here.");
    }
}

/// A description of a Slot, what it renders when it is used.
pub struct SlotRenderer<RCTX: Render, ARGS> {
    slot_fn: Box<Fn(ARGS) -> VNode<RCTX>>,
    cache: ResolvedSlots<RCTX>,
}

impl<RCTX: Render, ARGS> SlotRenderer<RCTX, ARGS> {
    /// Create a new SlotRenderer with a slot vnode generator fn and a cache.
    pub fn new(slot_fn: Box<Fn(ARGS) -> VNode<RCTX>>, cache: ResolvedSlots<RCTX>) -> Self {
        SlotRenderer { slot_fn, cache }
    }
}

/// Trait to invoke slot usage.
pub trait SlotUse<ARGS>: SlotLink {
    /// Use the slot with the given arguments.
    fn slot_use(&self, args: ARGS) -> SlotIndex;
}

impl<RCTX: Render, ARGS> SlotUse<ARGS> for SlotRenderer<RCTX, ARGS> {
    fn slot_use(&self, args: ARGS) -> SlotIndex {
        let rendered = (self.slot_fn)(args);
        self.cache.0.borrow_mut().push(ResolvedSlot {
            parent: None,
            next: None,
            vnode: rendered,
        });
        self.cache.0.borrow().len()
    }
}

/// A link to manipulate [`ResolvedSlot`](struct.ResolvedSlot.html).
pub trait SlotLink {
    /// Set the parent and next node for resolved slot at index.
    fn set_parent_and_next_of(&self, slot_index: SlotIndex, parent: Node, next: Option<Node>);
}

impl<RCTX: Render, ARGS> SlotLink for SlotRenderer<RCTX, ARGS> {
    fn set_parent_and_next_of(&self, slot_index: SlotIndex, parent: Node, next: Option<Node>) {
        let mut cache = self.cache.0.borrow_mut();
        if let Some(resolved_slot) = cache.get_mut(slot_index) {
            resolved_slot.parent = Some(parent);
            resolved_slot.next = next;
        }
    }
}

impl<ARGS> SlotLink for Shared<SlotUse<ARGS>> {
    fn set_parent_and_next_of(&self, slot_index: SlotIndex, parent: Node, next: Option<Node>) {
        self.borrow()
            .set_parent_and_next_of(slot_index, parent, next);
    }
}
