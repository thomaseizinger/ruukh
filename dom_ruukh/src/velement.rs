use std::fmt::{self, Display, Formatter};
use {KeyedVNodes, VNode};

/// The representation of an element in virtual DOM.
#[derive(Debug)]
pub struct VElement {
    /// The tag of the element. Eg: h, p, div, ...
    pub tag: String,
    /// The attributes of the given element
    pub attributes: Attributes,
    /// The child node of the given element
    pub child: Option<Box<KeyedVNodes>>,
}

#[derive(Debug)]
pub struct Attributes(pub Vec<Attribute>);

/// The key, value pair of the attributes on an element.
#[derive(Debug)]
pub struct Attribute {
    /// The key of the attribute
    pub key: String,
    /// The value pair of the attribute key
    pub value: String,
}

impl From<VElement> for VNode {
    fn from(el: VElement) -> VNode {
        VNode::Element(el)
    }
}

const VOID_TAGS: [&'static str; 14] = [
    "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source",
    "track", "wbr",
];

impl Display for VElement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if VOID_TAGS.contains(&self.tag.as_str()) {
            write!(f, "<{}{}>", self.tag, self.attributes)?;
            if self.child.is_some() {
                panic!(
                    "Element with a void tag `{}` cannot have a child.",
                    self.tag
                );
            } else {
                Ok(())
            }
        } else {
            if let Some(ref child) = self.child {
                write!(
                    f,
                    "<{tag}{attributes}>{child}</{tag}>",
                    tag = self.tag,
                    attributes = self.attributes,
                    child = child
                )
            } else {
                write!(
                    f,
                    "<{tag}{attributes}></{tag}>",
                    tag = self.tag,
                    attributes = self.attributes
                )
            }
        }
    }
}

impl Display for Attributes {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for attr in self.0.iter() {
            write!(f, " {}", attr)?;
        }
        Ok(())
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}=\"{}\"", self.key, self.value)
    }
}
