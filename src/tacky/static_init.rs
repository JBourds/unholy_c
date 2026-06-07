use std::rc::Rc;

use crate::ast;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum FpNumber {
    F32(u32),
    F64(u64),
}

impl FpNumber {
    pub fn asm_block(&self) -> String {
        match self {
            FpNumber::F32(val) => {
                format!("\t.long {}\n\n", val)
            }
            FpNumber::F64(val) => {
                format!("\t.quad {}\n\n", val)
            }
        }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum StaticInit {
    Float(FpNumber),
    String {
        data: Rc<[u8]>,
        null_terminated: bool,
    },
    Pointer(Rc<String>),
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    Zero(usize),
}

impl StaticInit {
    pub fn asm_block(&self) -> String {
        match self {
            StaticInit::Float(fp) => fp.asm_block(),
            StaticInit::String {
                data,
                null_terminated,
            } => {
                let octal_escaped = data.iter().fold(String::new(), |mut s, c| {
                    s.push_str(format!("\\{c:0}").as_str());
                    s
                });
                if *null_terminated {
                    format!("\t.asciz \"{octal_escaped}\"\n\n")
                } else {
                    format!("\t.ascii \"{octal_escaped}\"\n\n")
                }
            }
            StaticInit::Pointer(name) => {
                format!("\t.quad {name}\n\n")
            }
            StaticInit::I8(v) => {
                if *v == 0 {
                    format!("\t.zero {}\n\n", core::mem::size_of::<i8>())
                } else {
                    format!("\t.byte {v}\n\n")
                }
            }
            StaticInit::U8(v) => {
                if *v == 0 {
                    format!("\t.zero {}\n\n", core::mem::size_of::<u8>())
                } else {
                    format!("\t.byte {v}\n\n")
                }
            }
            StaticInit::I16(v) => {
                if *v == 0 {
                    format!("\t.zero {}\n\n", core::mem::size_of::<i16>())
                } else {
                    format!("\t.short {v}\n\n")
                }
            }
            StaticInit::U16(v) => {
                if *v == 0 {
                    format!("\t.zero {}\n\n", core::mem::size_of::<u16>())
                } else {
                    format!("\t.short {v}\n\n")
                }
            }
            StaticInit::I32(v) => {
                if *v == 0 {
                    format!("\t.zero {}\n\n", core::mem::size_of::<i32>())
                } else {
                    format!("\t.long {v}\n\n")
                }
            }
            StaticInit::U32(v) => {
                if *v == 0 {
                    format!("\t.zero {}\n\n", core::mem::size_of::<u32>())
                } else {
                    format!("\t.long {v}\n\n")
                }
            }
            StaticInit::I64(v) => {
                if *v == 0 {
                    format!("\t.zero {}\n\n", core::mem::size_of::<i64>())
                } else {
                    format!("\t.quad {v}\n\n")
                }
            }
            StaticInit::U64(v) => {
                if *v == 0 {
                    format!("\t.zero {}\n\n", core::mem::size_of::<u64>())
                } else {
                    format!("\t.quad {v}\n\n")
                }
            }
            StaticInit::Zero(num) => {
                format!("\t.zero {num}\n\n")
            }
        }
    }
}

impl From<ast::Constant> for StaticInit {
    fn from(value: ast::Constant) -> Self {
        match value {
            ast::Constant::ICHAR(v) => Self::I8(v as i8),
            ast::Constant::UCHAR(v) => Self::U8(v as u8),
            ast::Constant::I8(v) => Self::I8(v),
            ast::Constant::I16(v) => Self::I16(v),
            ast::Constant::I32(v) => Self::I32(v),
            ast::Constant::I64(v) => Self::I64(v),
            ast::Constant::U8(v) => Self::U8(v),
            ast::Constant::U16(v) => Self::U16(v),
            ast::Constant::U32(v) => Self::U32(v),
            ast::Constant::U64(v) => Self::U64(v),
            ast::Constant::F32(v) => {
                Self::Float(FpNumber::F32(u32::from_ne_bytes(v.to_ne_bytes())))
            }
            ast::Constant::F64(v) => {
                Self::Float(FpNumber::F64(u64::from_ne_bytes(v.to_ne_bytes())))
            }
        }
    }
}
