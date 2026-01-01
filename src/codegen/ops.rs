use crate::tacky;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Negate,
    Complement,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    DivDouble,
    BitAnd,
    BitOr,
    Xor,
    LShift,
    Sar,
    Shr,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Negate => write!(f, "neg"),
            Self::Complement => write!(f, "not"),
        }
    }
}

impl From<tacky::UnaryOp> for UnaryOp {
    fn from(node: tacky::UnaryOp) -> Self {
        match node {
            tacky::UnaryOp::Negate => Self::Negate,
            tacky::UnaryOp::Complement => Self::Complement,
            _ => unreachable!(),
        }
    }
}

impl BinaryOp {
    pub(super) fn from_op_and_sign(op: tacky::BinaryOp, signed: bool) -> Self {
        match (op, signed) {
            (tacky::BinaryOp::Add, _) => Self::Add,
            (tacky::BinaryOp::Subtract, _) => Self::Subtract,
            (tacky::BinaryOp::Multiply, _) => Self::Multiply,
            (tacky::BinaryOp::BitAnd, _) => Self::BitAnd,
            (tacky::BinaryOp::BitOr, _) => Self::BitOr,
            (tacky::BinaryOp::Xor, _) => Self::Xor,
            (tacky::BinaryOp::LShift, _) => Self::LShift,
            (tacky::BinaryOp::RShift, true) => Self::Sar,
            (tacky::BinaryOp::RShift, false) => Self::Shr,
            _ => unreachable!("No instruction conversion in binary op."),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "add"),
            Self::Subtract => write!(f, "sub"),
            Self::Multiply => write!(f, "imul"),
            Self::DivDouble => write!(f, "div"),
            Self::BitAnd => write!(f, "and"),
            Self::BitOr => write!(f, "or"),
            Self::Xor => write!(f, "xor"),
            Self::LShift => write!(f, "sal"),
            Self::Sar => write!(f, "sar"),
            Self::Shr => write!(f, "shr"),
        }
    }
}
