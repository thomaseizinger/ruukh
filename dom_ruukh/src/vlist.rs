use {KeyedVNodes, VNode};

/// The representation of a list of vnodes in the vtree.
#[derive(Debug)]
pub struct VList(pub Vec<KeyedVNodes>);

impl From<VList> for VNode {
    fn from(list: VList) -> VNode {
        VNode::List(list)
    }
}
