use std::fmt::{self, Display, Formatter};
use {KeyedVNodes, VNode};

/// The representation of a list of vnodes in the vtree.
#[derive(Debug)]
pub struct VList(pub Vec<KeyedVNodes>);

impl From<VList> for VNode {
    fn from(list: VList) -> VNode {
        VNode::List(list)
    }
}

impl Display for VList {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for vnode in self.0.iter() {
            write!(f, "{}", vnode)?;
        }
        Ok(())
    }
}
