//! The Virtual DOM library which backs the `ruukh` frontend framework.

use velement::VElement;
use vlist::VList;
use vtext::VText;

mod velement;
mod vlist;
mod vtext;

/// A virtual node in a virtual DOM tree.
#[derive(Debug)]
pub enum VNode {
    Text(VText),
    Element(VElement),
    List(VList),
}
