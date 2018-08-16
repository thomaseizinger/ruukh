//! The Virtual DOM library which backs the `ruukh` frontend framework.

use vtext::VText;

mod vtext;

/// A virtual node in a virtual DOM tree.
#[derive(Debug)]
pub enum VNode {
    Text(VText)
}
