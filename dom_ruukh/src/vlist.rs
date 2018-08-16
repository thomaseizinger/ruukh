use KeyedVNodes;

/// The representation of a list of vnodes in the vtree.
#[derive(Debug)]
pub struct VList(pub Vec<KeyedVNodes>);
