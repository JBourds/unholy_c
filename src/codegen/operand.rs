use super::{AssemblyType, Reg, StaticConstant};
use crate::{ast, tacky};
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Imm(ast::Constant),
    Reg(Reg),
    Pseudo {
        name: Rc<String>,
        size: usize,
        r#type: AssemblyType,
    },
    PseudoMem {
        name: Rc<String>,
        offset: usize,
    },
    Memory {
        reg: Reg,
        offset: isize,
        size: usize,
        r#type: AssemblyType,
    },
    Data {
        name: Rc<String>,
        size: usize,
        r#type: AssemblyType,
        is_const: bool,
    },
    Indexed {
        base: Reg,
        index: Reg,
        scale: usize,
    },
}

pub(super) fn make_zero(size_bytes: usize, signed: bool) -> Operand {
    match (size_bytes, signed) {
        (1, true) => Operand::Imm(ast::Constant::I8(0)),
        (2, true) => Operand::Imm(ast::Constant::I16(0)),
        (4, true) => Operand::Imm(ast::Constant::I32(0)),
        (8, true) => Operand::Imm(ast::Constant::I64(0)),
        (1, false) => Operand::Imm(ast::Constant::U8(0)),
        (2, false) => Operand::Imm(ast::Constant::U16(0)),
        (4, false) => Operand::Imm(ast::Constant::U32(0)),
        (8, false) => Operand::Imm(ast::Constant::U64(0)),
        _ => unreachable!(
            "Unable to create a {} constant operand with {size_bytes} size",
            if signed { "signed" } else { "unsigned" }
        ),
    }
}

impl Operand {
    pub(super) fn size(&self) -> usize {
        match self {
            Self::Imm(c) => c.get_type().size_of(),
            Self::Reg(r) => r.size(),
            Self::Pseudo { size, .. } => *size,
            Self::Memory { r#type, .. } => r#type.size_bytes(),
            Self::Data { size, .. } => *size,
            Operand::Indexed { .. } => core::mem::size_of::<usize>(),
            Operand::PseudoMem { .. } => unimplemented!(),
        }
    }

    pub(super) fn with_offset(self, add: usize) -> Self {
        match self {
            Operand::PseudoMem { name, offset } => Operand::PseudoMem {
                name,
                offset: offset + add,
            },
            Operand::Memory {
                reg,
                offset,
                size,
                r#type,
            } => Operand::Memory {
                reg,
                offset: offset + add as isize,
                size,
                r#type,
            },
            _ => unreachable!("{self:#?}"),
        }
    }

    pub(super) fn is_reg(&self) -> bool {
        matches!(self, Self::Reg(_))
    }

    pub(super) fn is_imm(&self) -> bool {
        matches!(self, Self::Imm(_))
    }

    pub(super) fn is_mem(&self) -> bool {
        !(self.is_reg() || self.is_imm())
    }

    pub(super) fn from_tacky(
        val: tacky::Val,
        symbols: &tacky::SymbolTable,
        float_constants: &mut HashSet<StaticConstant>,
    ) -> Self {
        match val {
            tacky::Val::Constant(ast::Constant::F32(v)) => {
                let static_const = StaticConstant::from(v);
                if !float_constants.contains(&static_const) {
                    float_constants.insert(static_const.clone());
                }
                let val_type = AssemblyType::from_tacky(&val, symbols);
                Operand::Data {
                    name: static_const.id,
                    size: val_type.size_bytes(),
                    r#type: val_type,
                    is_const: true,
                }
            }
            tacky::Val::Constant(ast::Constant::F64(v)) => {
                let static_const = StaticConstant::from(v);
                if !float_constants.contains(&static_const) {
                    float_constants.insert(static_const.clone());
                }
                let val_type = AssemblyType::from_tacky(&val, symbols);
                Operand::Data {
                    name: static_const.id,
                    size: val_type.size_bytes(),
                    r#type: val_type,
                    is_const: true,
                }
            }
            tacky::Val::Constant(i) => Self::Imm(i),
            tacky::Val::Var(ref name) => {
                let val_type = AssemblyType::from_tacky(&val, symbols);
                match val_type {
                    AssemblyType::ByteArray { .. } => Self::PseudoMem {
                        name: Rc::clone(name),
                        offset: 0,
                    },
                    _ => Self::Pseudo {
                        name: Rc::clone(name),
                        size: val_type.size_bytes(),
                        r#type: val_type,
                    },
                }
            }
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Imm(v) => write!(f, "{v}"),
            Self::Reg(r) => write!(f, "{r}"),
            Self::Memory { reg, offset, .. } => {
                write!(f, "[{reg}{offset:+}]")
            }
            Self::Data { name, is_const, .. } => {
                if *is_const {
                    write!(f, "\".L_{name}\"[rip]")
                } else {
                    write!(f, "\"{name}\"[rip]")
                }
            }
            Operand::Indexed { base, index, scale } => write!(f, "[{base} + {index} * {scale}]"),
            Self::Pseudo { .. } => {
                unreachable!("Cannot create asm representation for a pseudoregister.")
            }
            Operand::PseudoMem { .. } => {
                unreachable!("Cannot create asm representation for pseudo memory.")
            }
        }
    }
}
