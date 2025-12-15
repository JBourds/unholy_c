use super::*;
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Negate,
    Complement,
    Not,
}

impl From<ast::UnaryOp> for UnaryOp {
    fn from(node: ast::UnaryOp) -> Self {
        match node {
            ast::UnaryOp::Complement => Self::Complement,
            ast::UnaryOp::Negate => Self::Negate,
            ast::UnaryOp::Not => Self::Not,
            _ => unreachable!(),
        }
    }
}
