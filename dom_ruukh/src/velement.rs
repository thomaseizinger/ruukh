use VNode;

/// The representation of an element in virtual DOM.
#[derive(Debug)]
pub struct VElement {
    tag: String,
    attributes: Vec<Attribute>,
    child: Option<Box<VNode>>
}

#[derive(Debug)]
pub struct Attribute {
    key: String,
    value: String
}
