use super::{MAX_AGGREGATE_ALIGNMENT, Operand, Reg, RegSection};
use crate::{ast, tacky};

#[derive(Clone, Debug, PartialEq)]
pub enum AssemblyType {
    Byte,
    Word,
    Longword,
    Quadword,
    Pointer,
    Float,
    Double,
    ByteArray { size: usize, alignment: usize },
}

impl AssemblyType {
    pub fn uses_xmm_regs(&self) -> bool {
        matches!(self, Self::Float | Self::Double)
    }

    pub(super) fn from_tacky(val: &tacky::Val, symbols: &tacky::SymbolTable) -> Self {
        Self::from_ast_type(val.get_type(symbols))
    }

    pub(super) fn from_ast_type(r#type: ast::Type) -> Self {
        match r#type {
            ast::Type {
                base: ast::BaseType::Float(_),
                ..
            } => Self::Float,
            ast::Type {
                base: ast::BaseType::Double(_),
                ..
            } => Self::Double,
            ast::Type {
                base: ast::BaseType::Int { nbytes, .. },
                ..
            } => match nbytes {
                1 => Self::Byte,
                2 => Self::Word,
                4 => Self::Longword,
                8 => Self::Quadword,
                _ => unreachable!(),
            },
            ast::Type {
                base: ast::BaseType::Ptr { .. },
                ..
            } => Self::Quadword,
            ast::Type {
                base: ast::BaseType::Array { element, size },
                ..
            } => Self::ByteArray {
                size: element.size_of() * size,
                alignment: std::cmp::min(element.size_of(), MAX_AGGREGATE_ALIGNMENT),
            },
            _ => unimplemented!(),
        }
    }

    pub fn size_bytes(&self) -> usize {
        match self {
            AssemblyType::Byte => core::mem::size_of::<u8>(),
            AssemblyType::Word => core::mem::size_of::<u16>(),
            AssemblyType::Longword => core::mem::size_of::<u32>(),
            AssemblyType::Quadword => core::mem::size_of::<u64>(),
            AssemblyType::Pointer => core::mem::size_of::<usize>(),
            AssemblyType::Float => core::mem::size_of::<f32>(),
            AssemblyType::Double => core::mem::size_of::<f64>(),
            AssemblyType::ByteArray { size, .. } => *size,
        }
    }

    pub(super) fn alignment(&self) -> usize {
        match self {
            AssemblyType::ByteArray { alignment, .. } => *alignment,
            _ => self.size_bytes(),
        }
    }
}

impl From<&Operand> for AssemblyType {
    fn from(value: &Operand) -> Self {
        match value {
            Operand::Imm(constant) => match constant {
                ast::Constant::I8(_) | ast::Constant::U8(_) => Self::Byte,
                ast::Constant::I16(_) | ast::Constant::U16(_) => Self::Word,
                ast::Constant::I32(_) | ast::Constant::U32(_) => Self::Longword,
                ast::Constant::I64(_) | ast::Constant::U64(_) => Self::Quadword,
                ast::Constant::F32(_) => Self::Float,
                ast::Constant::F64(_) => Self::Double,
            },
            Operand::Reg(reg) => match reg {
                Reg::X86 { section, .. } | Reg::X64 { section, .. } => match section {
                    RegSection::LowByte | RegSection::HighByte => Self::Byte,
                    RegSection::Word => Self::Word,
                    RegSection::Dword => Self::Longword,
                    RegSection::Qword => Self::Quadword,
                },
                Reg::Xmm { section, .. } => match section {
                    RegSection::Dword => Self::Float,
                    RegSection::Qword => Self::Double,
                    _ => unreachable!(),
                },
            },
            Operand::Pseudo { r#type, .. } => r#type.clone(),
            Operand::Memory { r#type, .. } => r#type.clone(),
            Operand::Data { r#type, .. } => r#type.clone(),
            Operand::Indexed { .. } => Self::Quadword,
            Operand::PseudoMem { .. } => todo!(),
        }
    }
}
