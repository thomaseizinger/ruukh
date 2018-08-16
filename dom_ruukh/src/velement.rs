use KeyedVNodes;

/// The representation of an element in virtual DOM.
#[derive(Debug)]
pub struct VElement {
    pub tag: String,
    pub attributes: Vec<Attribute>,
    pub child: Option<Box<KeyedVNodes>>,
}

#[derive(Debug)]
pub struct Attribute {
    pub key: String,
    pub value: String,
}
