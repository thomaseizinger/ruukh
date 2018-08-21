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

#[cfg(test)]
mod test {
    use super::{Attribute, Attributes, VElement};
    use vtext::VText;
    use KeyedVNodes;

    #[test]
    fn should_display_an_attribute() {
        let attribute = Attribute {
            key: "class".to_string(),
            value: "font-large bg-white".to_string(),
        };
        assert_eq!(format!("{}", attribute), "class=\"font-large bg-white\"");
    }

    #[test]
    fn should_display_a_div() {
        let div = VElement {
            tag: "div".to_string(),
            attributes: Attributes(vec![]),
            child: None,
        };
        assert_eq!(format!("{}", div), "<div></div>");
    }

    #[test]
    fn should_display_a_button_with_text() {
        let button = VElement {
            tag: "button".to_string(),
            attributes: Attributes(vec![]),
            child: Some(Box::new(KeyedVNodes {
                key: None,
                vnode: VText {
                    content: "Click".to_string(),
                    is_comment: false,
                }.into(),
            })),
        };
        assert_eq!(format!("{}", button), "<button>Click</button>");
    }

    #[test]
    fn should_display_an_attributed_p() {
        let p = VElement {
            tag: "p".to_string(),
            attributes: Attributes(vec![
                Attribute {
                    key: "class".to_string(),
                    value: "mt-3".to_string(),
                },
                Attribute {
                    key: "style".to_string(),
                    value: "background-color: grey;".to_string(),
                },
            ]),
            child: None,
        };
        assert_eq!(
            format!("{}", p),
            "<p class=\"mt-3\" style=\"background-color: grey;\"></p>"
        );
    }
}
