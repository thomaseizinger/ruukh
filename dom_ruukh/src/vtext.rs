use std::fmt::{self, Display, Formatter};
use VNode;

/// The representation of string in virtual dom tree.
#[derive(Debug)]
pub struct VText {
    /// The content of a text string
    pub content: String,
    /// Whether the content is a comment
    pub is_comment: bool,
}

impl From<VText> for VNode {
    fn from(text: VText) -> VNode {
        VNode::Text(text)
    }
}

impl Display for VText {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.is_comment {
            write!(f, "<!-- {} -->", self.content)
        } else {
            write!(f, "{}", self.content)
        }
    }
}

#[cfg(test)]
mod test {
    use super::VText;

    #[test]
    fn should_display_text() {
        let text = VText {
            content: "This is a very fine day!".to_string(),
            is_comment: false,
        };
        assert_eq!(format!("{}", text), "This is a very fine day!");
    }

    #[test]
    fn should_display_comment() {
        let comment = VText {
            content: "Something to remind the hacky users.".to_string(),
            is_comment: true,
        };
        assert_eq!(
            format!("{}", comment),
            "<!-- Something to remind the hacky users. -->"
        );
    }
}
