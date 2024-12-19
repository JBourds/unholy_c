use crate::tacky;
use anyhow::{Context, Result};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub function: Function,
}

impl TryFrom<&tacky::Program> for Program {
    type Error = anyhow::Error;
    fn try_from(node: &tacky::Program) -> Result<Self> {
        let function = Function::try_from(&node.function)
            .context("Failed to compile intermediate representation into assembly nodes.")?;
        Ok(Program { function })
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Rc<String>,
    pub instructions: Vec<Instruction<Final>>,
}

impl TryFrom<&tacky::Function> for Function {
    type Error = anyhow::Error;
    fn try_from(node: &tacky::Function) -> Result<Self> {
        let mut instructions: Vec<Instruction<Initial>> = node
            .instructions
            .iter()
            .map(Vec::<Instruction<Initial>>::try_from)
            .try_fold(
                vec![Instruction::<Initial>::new(InstructionType::AllocStack(0))],
                |mut unrolled, result| {
                    unrolled.extend(result.context(
                        "Failed to parse assembly function from intermediate representation.",
                    )?);
                    Ok::<Vec<Instruction<Initial>>, Self::Error>(unrolled)
                },
            )
            .context("Failed to generate function definition from statements.")?;

        // Get stack offsets for each pseudoregister as we fix them up
        let mut stack_offsets = HashMap::new();
        let mut stack_bound = 0;
        let mut fixed_instructions: Vec<Instruction<Offset>> = instructions
            .drain(..)
            .map(|instr| Instruction::<Offset>::new(instr, &mut stack_offsets, &mut stack_bound))
            .collect();

        // Setup stack prologue
        fixed_instructions[0].op = InstructionType::AllocStack(stack_bound);

        let final_instructions: Vec<Instruction<Final>> = fixed_instructions
            .drain(..)
            .map(Vec::<Instruction<Final>>::from)
            .fold(Vec::new(), |mut v, instr| {
                v.extend(instr);
                v
            });

        Ok(Function {
            name: Rc::clone(&node.name),
            instructions: final_instructions,
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Negate,
    Complement,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Negate => write!(f, "neg"),
            Self::Complement => write!(f, "not"),
        }
    }
}

impl From<&tacky::UnaryOp> for UnaryOp {
    fn from(node: &tacky::UnaryOp) -> Self {
        match node {
            tacky::UnaryOp::Negate => Self::Negate,
            tacky::UnaryOp::Complement => Self::Complement,
        }
    }
}

#[derive(Debug, PartialEq)]
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
}

impl From<&tacky::BinaryOp> for BinaryOp {
    fn from(node: &tacky::BinaryOp) -> Self {
        match node {
            tacky::BinaryOp::Add => Self::Add,
            tacky::BinaryOp::Subtract => Self::Subtract,
            tacky::BinaryOp::Multiply => Self::Multiply,
            tacky::BinaryOp::Divide => Self::Divide,
            tacky::BinaryOp::Remainder => Self::Remainder,
            tacky::BinaryOp::BitAnd => Self::BitAnd,
            tacky::BinaryOp::BitOr => Self::BitOr,
            tacky::BinaryOp::Xor => Self::Xor,
            tacky::BinaryOp::LShift => Self::LShift,
            tacky::BinaryOp::RShift => Self::RShift,
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "add"),
            Self::Subtract => write!(f, "sub"),
            Self::Multiply => write!(f, "imul"),
            Self::BitAnd => write!(f, "and"),
            Self::BitOr => write!(f, "or"),
            Self::Xor => write!(f, "xor"),
            Self::LShift => write!(f, "shl"),
            Self::RShift => write!(f, "shr"),
            Self::Divide => unreachable!("Division has no direct binary instruction."),
            Self::Remainder => unreachable!("Remainder has no direct binary instruction"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RegSection {
    LowByte,
    HighByte,
    Word,
    Dword,
    Qword,
}

impl RegSection {
    pub fn size(&self) -> usize {
        match self {
            RegSection::LowByte => 1,
            RegSection::HighByte => 1,
            RegSection::Word => 2,
            RegSection::Dword => 4,
            RegSection::Qword => 8,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum X86Reg {
    // Generic registers
    Ax,
    Bx,
    Cx,
    Dx,
    // Base and stack pointer
    Bp,
    Sp,
    // Source and destination index
    Si,
    Di,
}
impl From<&X86Reg> for &str {
    fn from(value: &X86Reg) -> Self {
        match value {
            X86Reg::Ax => "ax",
            X86Reg::Bx => "bx",
            X86Reg::Cx => "cx",
            X86Reg::Dx => "dx",
            X86Reg::Bp => "bp",
            X86Reg::Sp => "sp",
            X86Reg::Si => "si",
            X86Reg::Di => "di",
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum X64Reg {
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}
impl From<&X64Reg> for usize {
    fn from(value: &X64Reg) -> Self {
        match value {
            X64Reg::R8 => 8,
            X64Reg::R9 => 9,
            X64Reg::R10 => 11,
            X64Reg::R11 => 11,
            X64Reg::R12 => 12,
            X64Reg::R13 => 13,
            X64Reg::R14 => 14,
            X64Reg::R15 => 15,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Reg {
    X86 { reg: X86Reg, section: RegSection },
    X64 { reg: X64Reg, section: RegSection },
}

impl Reg {
    pub fn size(&self) -> usize {
        match self {
            Self::X86 { reg: _, section } => section.size(),
            Self::X64 { reg: _, section } => section.size(),
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::X86 { reg, section } => {
                let prefix = match section {
                    RegSection::LowByte => "h",
                    RegSection::HighByte => "h",
                    RegSection::Word => "",
                    RegSection::Dword => "e",
                    RegSection::Qword => "r",
                };
                write!(f, "{}{}", prefix, <&str>::from(reg))
            }
            Self::X64 { reg, section } => {
                let suffix = match section {
                    RegSection::LowByte => "b",
                    RegSection::HighByte => "h",
                    RegSection::Word => "w",
                    RegSection::Dword => "d",
                    RegSection::Qword => "",
                };
                write!(f, "r{}{}", usize::from(reg), suffix)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
struct Initial;

#[derive(Debug, PartialEq)]
struct Offset;

#[derive(Debug, PartialEq)]
pub struct Final;

#[derive(Debug, PartialEq)]
pub enum InstructionType {
    Mov {
        src: Operand,
        dst: Operand,
    },
    Unary {
        op: UnaryOp,
        dst: Operand,
    },
    Binary {
        op: BinaryOp,
        src1: Operand,
        src2: Operand,
    },
    Idiv(Operand),
    Cdq,
    AllocStack(usize),
    Ret,
}

#[derive(Debug, PartialEq)]
pub struct Instruction<T> {
    pub op: InstructionType,
    phantom: PhantomData<T>,
}

impl Instruction<Initial> {
    fn new(op: InstructionType) -> Self {
        Self {
            op,
            phantom: PhantomData::<Initial>,
        }
    }
}

impl Instruction<Offset> {
    fn new(
        instruction: Instruction<Initial>,
        stack_offsets: &mut HashMap<Rc<String>, usize>,
        stack_bound: &mut usize,
    ) -> Self {
        let mut convert_operand_offset = |op| {
            if let Operand::Pseudo { ref name, size } = op {
                if let Entry::Vacant(e) = stack_offsets.entry(Rc::clone(name)) {
                    *stack_bound += size;
                    e.insert(*stack_bound);
                }
                // SAFETY: We just checked this condition
                unsafe {
                    Operand::StackOffset {
                        offset: *stack_offsets.get(name).unwrap_unchecked(),
                        size,
                    }
                }
            } else {
                op
            }
        };

        Self {
            op: match instruction.op {
                InstructionType::Mov { src, dst } => InstructionType::Mov {
                    src: convert_operand_offset(src),
                    dst: convert_operand_offset(dst),
                },
                InstructionType::Unary { op, dst } => InstructionType::Unary {
                    op,
                    dst: convert_operand_offset(dst),
                },
                InstructionType::Binary { op, src1, src2 } => InstructionType::Binary {
                    op,
                    src1: convert_operand_offset(src1),
                    src2: convert_operand_offset(src2),
                },
                instr => instr,
            },
            phantom: PhantomData::<Offset>,
        }
    }
}

// Final stage of rewriting to follow semantics of assembly instructions
impl From<Instruction<Offset>> for Vec<Instruction<Final>> {
    fn from(instr: Instruction<Offset>) -> Vec<Instruction<Final>> {
        let new_instr = |op| Instruction::<Final> {
            op,
            phantom: PhantomData::<Final>,
        };
        match instr.op {
            InstructionType::Mov {
                src:
                    Operand::StackOffset {
                        offset: src_offset,
                        size: src_size,
                    },
                dst:
                    Operand::StackOffset {
                        offset: dst_offset,
                        size: dst_size,
                    },
            } => {
                vec![
                    new_instr(InstructionType::Mov {
                        src: Operand::StackOffset {
                            offset: src_offset,
                            size: src_size,
                        },
                        dst: Operand::Reg(Reg::X64 {
                            reg: X64Reg::R10,
                            section: RegSection::Qword,
                        }),
                    }),
                    new_instr(InstructionType::Mov {
                        src: Operand::Reg(Reg::X64 {
                            reg: X64Reg::R10,
                            section: RegSection::Qword,
                        }),
                        dst: Operand::StackOffset {
                            offset: dst_offset,
                            size: dst_size,
                        },
                    }),
                ]
            }
            InstructionType::Binary {
                op,
                src1:
                    Operand::StackOffset {
                        offset: src1_offset,
                        size: src1_size,
                    },
                src2:
                    Operand::StackOffset {
                        offset: src2_offset,
                        size: src2_size,
                    },
            } => {
                vec![
                    new_instr(InstructionType::Mov {
                        src: Operand::StackOffset {
                            offset: src1_offset,
                            size: src1_size,
                        },
                        dst: Operand::Reg(Reg::X64 {
                            reg: X64Reg::R10,
                            section: RegSection::Qword,
                        }),
                    }),
                    new_instr(InstructionType::Binary {
                        op,
                        src1: Operand::Reg(Reg::X64 {
                            reg: X64Reg::R10,
                            section: RegSection::Qword,
                        }),
                        src2: Operand::StackOffset {
                            offset: src2_offset,
                            size: src2_size,
                        },
                    }),
                ]
            }
            InstructionType::Idiv(Operand::Imm(v)) => vec![
                new_instr(InstructionType::Mov {
                    src: Operand::Imm(v),
                    dst: Operand::Reg(Reg::X64 {
                        reg: X64Reg::R10,
                        section: RegSection::Qword,
                    }),
                }),
                new_instr(InstructionType::Idiv(Operand::Reg(Reg::X64 {
                    reg: X64Reg::R10,
                    section: RegSection::Qword,
                }))),
            ],

            instr => vec![new_instr(instr)],
        }
    }
}

impl From<&tacky::Instruction> for Vec<Instruction<Initial>> {
    fn from(instruction: &tacky::Instruction) -> Vec<Instruction<Initial>> {
        let new_instr = |op| Instruction::<Initial> {
            op,
            phantom: PhantomData::<Initial>,
        };
        match instruction {
            tacky::Instruction::Return(None) => {
                vec![new_instr(InstructionType::Ret)]
            }
            tacky::Instruction::Return(Some(ref val)) => vec![
                new_instr(InstructionType::Mov {
                    src: Operand::from(val),
                    dst: Operand::Reg(Reg::X86 {
                        reg: X86Reg::Ax,
                        section: RegSection::Dword,
                    }),
                }),
                new_instr(InstructionType::Ret),
            ],
            tacky::Instruction::Unary { op, src, dst } => vec![
                new_instr(InstructionType::Mov {
                    src: src.into(),
                    dst: dst.into(),
                }),
                new_instr(InstructionType::Unary {
                    op: op.into(),
                    dst: dst.into(),
                }),
            ],
            tacky::Instruction::Binary {
                op,
                src1,
                src2,
                dst,
            } => match op {
                tacky::BinaryOp::Add | tacky::BinaryOp::Subtract => {
                    vec![
                        new_instr(InstructionType::Mov {
                            src: src1.into(),
                            dst: dst.into(),
                        }),
                        new_instr(InstructionType::Binary {
                            op: op.into(),
                            src1: src2.into(),
                            src2: dst.into(),
                        }),
                    ]
                }
                tacky::BinaryOp::Multiply => {
                    vec![
                        new_instr(InstructionType::Mov {
                            src: src2.into(),
                            dst: Operand::Reg(Reg::X64 {
                                reg: X64Reg::R11,
                                section: RegSection::Qword,
                            }),
                        }),
                        new_instr(InstructionType::Binary {
                            op: BinaryOp::Multiply,
                            src1: src1.into(),
                            src2: Operand::Reg(Reg::X64 {
                                reg: X64Reg::R11,
                                section: RegSection::Qword,
                            }),
                        }),
                        new_instr(InstructionType::Mov {
                            src: Operand::Reg(Reg::X64 {
                                reg: X64Reg::R11,
                                section: RegSection::Qword,
                            }),
                            dst: dst.into(),
                        }),
                    ]
                }
                tacky::BinaryOp::Divide => vec![
                    new_instr(InstructionType::Mov {
                        src: src1.into(),
                        dst: Operand::Reg(Reg::X86 {
                            reg: X86Reg::Ax,
                            section: RegSection::Dword,
                        }),
                    }),
                    new_instr(InstructionType::Cdq),
                    new_instr(InstructionType::Idiv(src2.into())),
                    new_instr(InstructionType::Mov {
                        src: Operand::Reg(Reg::X86 {
                            reg: X86Reg::Ax,
                            section: RegSection::Dword,
                        }),
                        dst: dst.into(),
                    }),
                ],
                tacky::BinaryOp::Remainder => vec![
                    new_instr(InstructionType::Mov {
                        src: src1.into(),
                        dst: Operand::Reg(Reg::X86 {
                            reg: X86Reg::Ax,
                            section: RegSection::Dword,
                        }),
                    }),
                    new_instr(InstructionType::Cdq),
                    new_instr(InstructionType::Idiv(src2.into())),
                    new_instr(InstructionType::Mov {
                        src: Operand::Reg(Reg::X86 {
                            reg: X86Reg::Dx,
                            section: RegSection::Dword,
                        }),
                        dst: dst.into(),
                    }),
                ],
                _ => todo!(),
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Imm(i32),
    Reg(Reg),
    Pseudo { name: Rc<String>, size: usize },
    StackOffset { offset: usize, size: usize },
}

// TODO: Unhardcode immediate size
impl Operand {
    pub fn size(&self) -> usize {
        match self {
            Self::Imm(_) => 4,
            Self::Reg(r) => r.size(),
            Self::Pseudo { size, .. } => *size,
            Self::StackOffset { size, .. } => *size,
        }
    }
}

// TODO: Unhardcode size of 4
impl From<&tacky::Val> for Operand {
    fn from(val: &tacky::Val) -> Self {
        match val {
            tacky::Val::Constant(i) => Self::Imm(*i),
            tacky::Val::Var(r) => Self::Pseudo {
                name: Rc::clone(r),
                size: 4,
            },
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Imm(v) => write!(f, "{v}"),
            Self::Reg(r) => write!(f, "{r}"),
            Self::StackOffset { offset, .. } => write!(f, "[rbp-{offset}]"),
            Self::Pseudo { .. } => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tacky_ret_to_asm() {
        let tacky = tacky::Instruction::Return(Some(tacky::Val::Constant(2)));
        let expected: Vec<Instruction<Initial>> = vec![
            InstructionType::Mov {
                src: Operand::Imm(2),
                dst: Operand::Reg(Reg::X86 {
                    reg: X86Reg::Ax,
                    section: RegSection::Dword,
                }),
            },
            InstructionType::Ret,
        ]
        .into_iter()
        .map(Instruction::<Initial>::new)
        .collect();
        let actual = Vec::<Instruction<Initial>>::from(&tacky);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tacky_unary_to_asm() {
        let pseudo = Rc::new("pseudoreg".to_string());
        let tacky = tacky::Instruction::Unary {
            op: tacky::UnaryOp::Complement,
            src: tacky::Val::Constant(2),
            dst: tacky::Val::Var(Rc::clone(&pseudo)),
        };
        let expected: Vec<Instruction<Initial>> = vec![
            InstructionType::Mov {
                src: Operand::Imm(2),
                dst: Operand::Pseudo {
                    name: Rc::clone(&pseudo),
                    size: 4,
                },
            },
            InstructionType::Unary {
                op: UnaryOp::Complement,
                dst: Operand::Pseudo {
                    name: Rc::clone(&pseudo),
                    size: 4,
                },
            },
        ]
        .into_iter()
        .map(Instruction::<Initial>::new)
        .collect();
        let actual = Vec::<Instruction<Initial>>::from(&tacky);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_stack_pass() {
        let pseudo = Rc::new("pseudoreg".to_string());
        let tacky = tacky::Instruction::Unary {
            op: tacky::UnaryOp::Complement,
            src: tacky::Val::Constant(2),
            dst: tacky::Val::Var(Rc::clone(&pseudo)),
        };
        let expected: Vec<Instruction<Initial>> = vec![
            InstructionType::Mov {
                src: Operand::Imm(2),
                dst: Operand::Pseudo {
                    name: Rc::clone(&pseudo),
                    size: 4,
                },
            },
            InstructionType::Unary {
                op: UnaryOp::Complement,
                dst: Operand::Pseudo {
                    name: Rc::clone(&pseudo),
                    size: 4,
                },
            },
        ]
        .into_iter()
        .map(Instruction::<Initial>::new)
        .collect();
        let actual = Vec::<Instruction<Initial>>::from(&tacky);

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_instruction_fixup() {
        let pseudo = Rc::new("pseudoreg".to_string());
        let tacky = tacky::Instruction::Unary {
            op: tacky::UnaryOp::Complement,
            src: tacky::Val::Constant(2),
            dst: tacky::Val::Var(Rc::clone(&pseudo)),
        };
        let expected: Vec<Instruction<Initial>> = vec![
            InstructionType::Mov {
                src: Operand::Imm(2),
                dst: Operand::Pseudo {
                    name: Rc::clone(&pseudo),
                    size: 4,
                },
            },
            InstructionType::Unary {
                op: UnaryOp::Complement,
                dst: Operand::Pseudo {
                    name: Rc::clone(&pseudo),
                    size: 4,
                },
            },
        ]
        .into_iter()
        .map(Instruction::<Initial>::new)
        .collect();
        let actual = Vec::<Instruction<Initial>>::from(&tacky);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_binary_expressions() {
        let tacky_fn = tacky::Function {
            name: Rc::new("test_fn".to_string()),
            instructions: vec![
                tacky::Instruction::Binary {
                    op: tacky::BinaryOp::Multiply,
                    src1: tacky::Val::Constant(1),
                    src2: tacky::Val::Constant(2),
                    dst: tacky::Val::Var(Rc::new("tmp.0".to_string())),
                },
                tacky::Instruction::Binary {
                    op: tacky::BinaryOp::Add,
                    src1: tacky::Val::Constant(4),
                    src2: tacky::Val::Constant(5),
                    dst: tacky::Val::Var(Rc::new("tmp.1".to_string())),
                },
                tacky::Instruction::Binary {
                    op: tacky::BinaryOp::Multiply,
                    src1: tacky::Val::Constant(3),
                    src2: tacky::Val::Var(Rc::new("tmp.1".to_string())),
                    dst: tacky::Val::Var(Rc::new("tmp.2".to_string())),
                },
                tacky::Instruction::Binary {
                    op: tacky::BinaryOp::Subtract,
                    src1: tacky::Val::Var(Rc::new("tmp.0".to_string())),
                    src2: tacky::Val::Var(Rc::new("tmp.2".to_string())),
                    dst: tacky::Val::Var(Rc::new("tmp.3".to_string())),
                },
            ],
        };
        // Because the resulting AST is huge just check that it parses
        let _ = Function::try_from(&tacky_fn).unwrap();
    }
}
