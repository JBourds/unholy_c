use crate::tacky;
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
    A,
    AE,
    B,
    BE,
    P,
}

impl CondCode {
    pub(super) fn from_uses_cf_zf_op(value: tacky::BinaryOp, uses_cf_zf: bool) -> Self {
        match value {
            tacky::BinaryOp::Equal => Self::E,
            tacky::BinaryOp::NotEqual => Self::NE,
            tacky::BinaryOp::LessThan => {
                if uses_cf_zf {
                    Self::B
                } else {
                    Self::L
                }
            }
            tacky::BinaryOp::LessOrEqual => {
                if uses_cf_zf {
                    Self::BE
                } else {
                    Self::LE
                }
            }
            tacky::BinaryOp::GreaterThan => {
                if uses_cf_zf {
                    Self::A
                } else {
                    Self::G
                }
            }
            tacky::BinaryOp::GreaterOrEqual => {
                if uses_cf_zf {
                    Self::AE
                } else {
                    Self::GE
                }
            }
            _ => unreachable!("Only relational operands can convert to CondCode"),
        }
    }
}

impl fmt::Display for CondCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::E => write!(f, "e"),
            Self::NE => write!(f, "ne"),
            Self::G => write!(f, "g"),
            Self::GE => write!(f, "ge"),
            Self::L => write!(f, "l"),
            Self::LE => write!(f, "le"),
            Self::A => write!(f, "a"),
            Self::AE => write!(f, "ae"),
            Self::B => write!(f, "b"),
            Self::BE => write!(f, "be"),
            Self::P => write!(f, "p"),
        }
    }
}
