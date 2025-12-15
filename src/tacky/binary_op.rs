use super::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitAnd,
    BitOr,
    Xor,
    LShift,
    RShift,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Assign,
    AddAssign,
    SubAssign,
    MultAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    LShiftAssign,
    RShiftAssign,
}

impl From<ast::BinaryOp> for BinaryOp {
    fn from(node: ast::BinaryOp) -> Self {
        match node {
            ast::BinaryOp::Add => Self::Add,
            ast::BinaryOp::Subtract => Self::Subtract,
            ast::BinaryOp::Multiply => Self::Multiply,
            ast::BinaryOp::Divide => Self::Divide,
            ast::BinaryOp::Remainder => Self::Remainder,
            ast::BinaryOp::BitAnd => Self::BitAnd,
            ast::BinaryOp::BitOr => Self::BitOr,
            ast::BinaryOp::Xor => Self::Xor,
            ast::BinaryOp::LShift => Self::LShift,
            ast::BinaryOp::RShift => Self::RShift,
            ast::BinaryOp::And => unreachable!("Logical operands are not Binary operands"),
            ast::BinaryOp::Or => unreachable!("Logical operands are not Binary operands"),
            ast::BinaryOp::Equal => Self::Equal,
            ast::BinaryOp::NotEqual => Self::NotEqual,
            ast::BinaryOp::LessThan => Self::LessThan,
            ast::BinaryOp::LessOrEqual => Self::LessOrEqual,
            ast::BinaryOp::GreaterThan => Self::GreaterThan,
            ast::BinaryOp::GreaterOrEqual => Self::GreaterOrEqual,
            ast::BinaryOp::Assign => Self::Assign,
            ast::BinaryOp::AddAssign => Self::AddAssign,
            ast::BinaryOp::SubAssign => Self::SubAssign,
            ast::BinaryOp::MultAssign => Self::MultAssign,
            ast::BinaryOp::DivAssign => Self::DivAssign,
            ast::BinaryOp::ModAssign => Self::ModAssign,
            ast::BinaryOp::AndAssign => Self::AndAssign,
            ast::BinaryOp::OrAssign => Self::OrAssign,
            ast::BinaryOp::XorAssign => Self::XorAssign,
            ast::BinaryOp::LShiftAssign => Self::LShiftAssign,
            ast::BinaryOp::RShiftAssign => Self::RShiftAssign,
            ast::BinaryOp::Ternary => {
                unreachable!("Ternary expressions are not true Binary operands")
            }
        }
    }
}
