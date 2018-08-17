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

#[cfg(test)]
mod test {
    use super::VList;
    use velement::{Attributes, VElement};
    use vtext::VText;
    use KeyedVNodes;

    #[test]
    fn should_display_a_list_of_vnodes() {
        let list = VList(vec![
            KeyedVNodes {
                key: None,
                node: VText {
                    content: "First of the node".to_string(),
                    is_comment: false,
                }.into(),
            },
            KeyedVNodes {
                key: None,
                node: VElement {
                    tag: "input".to_string(),
                    attributes: Attributes(vec![]),
                    child: None,
                }.into(),
            },
        ]);
        assert_eq!(format!("{}", list), "First of the node<input>");
    }
}
