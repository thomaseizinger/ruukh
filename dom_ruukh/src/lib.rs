//! The Virtual DOM library which backs the `ruukh` frontend framework.

use velement::VElement;
use vlist::VList;
use vtext::VText;

mod velement;
mod vlist;
mod vtext;

/// A keyed virtual node in a virtual DOM tree.
#[derive(Debug)]
pub struct KeyedVNodes {
    key: Option<Key>,
    node: VNode
}

/// A virtual node in a virtual DOM tree.
#[derive(Debug)]
pub enum VNode {
    Text(VText),
    Element(VElement),
    List(VList),
}

/// Keys to identify the VNode in Virtual DOM.
/// Only the basic types are supported.
#[derive(Debug)]
pub enum Key {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    String(String),
}
