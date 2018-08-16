/// The representation of string in virtual dom tree.
#[derive(Debug)]
pub struct VText {
    pub content: String,
    pub is_comment: bool
}
