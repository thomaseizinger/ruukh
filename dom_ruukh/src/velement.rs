use KeyedVNodes;

/// The representation of an element in virtual DOM.
#[derive(Debug)]
pub struct VElement {
    /// The tag of the element. Eg: h, p, div, ...
    pub tag: String,
    /// The attributes of the given element
    pub attributes: Vec<Attribute>,
    /// The child node of the given element
    pub child: Option<Box<KeyedVNodes>>,
}

/// The key, value pair of the attributes on an element.
#[derive(Debug)]
pub struct Attribute {
    /// The key of the attribute
    pub key: String,
    /// The value pair of the attribute key
    pub value: String,
}
