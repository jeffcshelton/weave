use super::{Block, Scope};

pub struct Function {
  blocks: Vec<Block>,
  scope: Scope,
}
