use crate::tacky;
use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;
use std::rc::Rc;

const SYSTEM_V_REGS: [Operand; 6] = [
    Operand::Reg(Reg::X86 {
        reg: X86Reg::Di,
        section: RegSection::Dword,
    }),
    Operand::Reg(Reg::X86 {
        reg: X86Reg::Si,
        section: RegSection::Dword,
    }),
    Operand::Reg(Reg::X86 {
        reg: X86Reg::Dx,
        section: RegSection::Dword,
    }),
    Operand::Reg(Reg::X86 {
        reg: X86Reg::Cx,
        section: RegSection::Dword,
    }),
    Operand::Reg(Reg::X64 {
        reg: X64Reg::R8,
        section: RegSection::Dword,
    }),
    Operand::Reg(Reg::X64 {
        reg: X64Reg::R9,
        section: RegSection::Dword,
    }),
];

#[derive(Debug, PartialEq)]
pub struct Program {
    pub functions: Vec<Function>,
}

impl From<tacky::Program> for Program {
    fn from(node: tacky::Program) -> Self {
        let valid_functions = node.functions.into_iter().map(Function::from).collect();
        Program {
            functions: valid_functions,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Rc<String>,
    pub instructions: Vec<Instruction<Final>>,
}

impl From<tacky::Function> for Function {
    fn from(node: tacky::Function) -> Self {
        let tacky::Function {
            name,
            mut params,
            instructions: fun_instructions,
        } = node;
        // Create the System V register/stack mappings here
        let (reg_args, stack_args) = {
            let to_drain = std::cmp::min(SYSTEM_V_REGS.len(), params.len());
            let reg_args = params
                .drain(..to_drain)
                .collect::<Vec<Option<Rc<String>>>>();
            let stack_args: Vec<Option<Rc<String>>> = std::mem::take(&mut params);
            (reg_args, stack_args)
        };
        let mut instructions = vec![
            Instruction::<Initial>::new(InstructionType::Push(Operand::Reg(Reg::X86 {
                reg: X86Reg::Bp,
                section: RegSection::Qword,
            }))),
            Instruction::<Initial>::new(InstructionType::Mov {
                src: Operand::Reg(Reg::X86 {
                    reg: X86Reg::Sp,
                    section: RegSection::Qword,
                }),
                dst: Operand::Reg(Reg::X86 {
                    reg: X86Reg::Bp,
                    section: RegSection::Qword,
                }),
            }),
            Instruction::<Initial>::new(InstructionType::AllocStack(0)),
        ];

        let mut mappings = HashMap::new();
        // We always start with a stack bound of 8 for RBP
        // Include register args here since we move into them
        for (src_reg, dst_arg) in
            std::iter::zip(SYSTEM_V_REGS.into_iter(), reg_args.into_iter().flatten())
        {
            instructions.push(Instruction::<Initial>::new(InstructionType::Mov {
                src: src_reg,
                dst: Operand::Pseudo {
                    name: dst_arg,
                    size: 4,
                },
            }));
        }

        // Hardcoded 8 here due to pushing RBP
        let mut stack_bound = 8;
        for arg in stack_args.into_iter().flatten() {
            stack_bound += 8;
            instructions.push(Instruction::<Initial>::new(InstructionType::Mov {
                src: Operand::StackOffset {
                    offset: stack_bound,
                    size: 4,
                },
                dst: Operand::Pseudo { name: arg, size: 4 },
            }));
        }

        for instr in fun_instructions.into_iter() {
            instructions.extend(Vec::<Instruction<Initial>>::from(instr));
        }

        // Get stack offsets for each pseudoregister as we fix them up
        // Start moving down for arguments & temp vars used here
        let mut stack_bound = 0;
        let mut requires_fixup = vec![];
        let mut fixed_instructions: Vec<Instruction<Offset>> = instructions
            .drain(..)
            .enumerate()
            .map(|(i, instr)| {
                let (instr, needs_fixing) =
                    Instruction::<Offset>::new(instr, &mut mappings, &mut stack_bound);
                if needs_fixing {
                    requires_fixup.push(i);
                }
                instr
            })
            .collect();

        // Setup stack prologue
        // Sixteen byte alignment is required
        stack_bound += match stack_bound % 16 {
            0 => 0,
            remainder => 16 - remainder,
        };
        fixed_instructions[2].op = InstructionType::AllocStack(stack_bound as usize);

        for i in requires_fixup {
            let instr = match fixed_instructions[i].op.clone() {
                InstructionType::Mov { src, dst } => {
                    let src = match src {
                        // Don't fixup stack args (+ offset)
                        Operand::StackOffset { offset, size } if offset < 0 => Operand::StackOffset { offset: offset - stack_bound, size },
                        op @ Operand::StackOffset { .. } => op,
                        _ => unreachable!("This applies to args passed on the stack only")
                    };
                    InstructionType::Mov { src , dst }
                },
                _ => unreachable!("We should only be fixing up mov instructions which are used to allocate this current functions args"),
            };
            fixed_instructions[i].op = instr;
        }

        let final_instructions: Vec<Instruction<Final>> = fixed_instructions
            .drain(..)
            .map(Vec::<Instruction<Final>>::from)
            .fold(Vec::new(), |mut v, instr| {
                v.extend(instr);
                v
            });

        Function {
            name,
            instructions: final_instructions,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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

impl From<tacky::UnaryOp> for UnaryOp {
    fn from(node: tacky::UnaryOp) -> Self {
        match node {
            tacky::UnaryOp::Negate => Self::Negate,
            tacky::UnaryOp::Complement => Self::Complement,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    BitAnd,
    BitOr,
    Xor,
    LShift,
    RShift,
}

impl From<tacky::BinaryOp> for BinaryOp {
    fn from(node: tacky::BinaryOp) -> Self {
        match node {
            tacky::BinaryOp::Add => Self::Add,
            tacky::BinaryOp::Subtract => Self::Subtract,
            tacky::BinaryOp::Multiply => Self::Multiply,
            tacky::BinaryOp::BitAnd => Self::BitAnd,
            tacky::BinaryOp::BitOr => Self::BitOr,
            tacky::BinaryOp::Xor => Self::Xor,
            tacky::BinaryOp::LShift => Self::LShift,
            tacky::BinaryOp::RShift => Self::RShift,
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
            Self::BitAnd => write!(f, "and"),
            Self::BitOr => write!(f, "or"),
            Self::Xor => write!(f, "xor"),
            Self::LShift => write!(f, "sal"),
            Self::RShift => write!(f, "sar"),
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
            X64Reg::R10 => 10,
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

    pub fn as_section(self, section: RegSection) -> Self {
        match self {
            Self::X86 { reg, .. } => Self::X86 { reg, section },
            Self::X64 { reg, .. } => Self::X64 { reg, section },
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::X86 { reg, section } => {
                let prefix = match section {
                    RegSection::Word => "",
                    RegSection::Dword => "e",
                    RegSection::Qword => "r",
                    _ => "",
                };
                let suffix = match section {
                    RegSection::LowByte => "l",
                    RegSection::HighByte => "h",
                    _ => "",
                };
                let reg_str = match reg {
                    reg @ X86Reg::Ax | reg @ X86Reg::Bx | reg @ X86Reg::Cx | reg @ X86Reg::Dx
                        if !suffix.is_empty() =>
                    {
                        &<&str>::from(reg)[..1]
                    }
                    reg => <&str>::from(reg),
                };

                write!(f, "{prefix}{reg_str}{suffix}")
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

#[derive(Clone, Debug, PartialEq)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
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
        }
    }
}

impl From<tacky::BinaryOp> for CondCode {
    fn from(value: tacky::BinaryOp) -> Self {
        match value {
            tacky::BinaryOp::Equal => Self::E,
            tacky::BinaryOp::NotEqual => Self::NE,
            tacky::BinaryOp::LessThan => Self::L,
            tacky::BinaryOp::LessOrEqual => Self::LE,
            tacky::BinaryOp::GreaterThan => Self::G,
            tacky::BinaryOp::GreaterOrEqual => Self::GE,
            _ => unreachable!("Only relational operands can convert to CondCode"),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Initial;

#[derive(Debug, PartialEq)]
struct Offset;

#[derive(Debug, PartialEq)]
pub struct Final;

#[derive(Clone, Debug, PartialEq)]
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
    Cmp {
        src: Operand,
        dst: Operand,
    },
    Idiv(Operand),
    Cdq,
    Jmp(Rc<String>),
    JmpCC {
        cond_code: CondCode,
        identifier: Rc<String>,
    },
    SetCC {
        cond_code: CondCode,
        dst: Operand,
    },
    Label(Rc<String>),
    AllocStack(usize),
    DeAllocStack(usize),
    Push(Operand),
    // Invariant: Can only pop to a register or stack offset
    Pop(Operand),
    Call(Rc<String>),
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
        mappings: &mut HashMap<Rc<String>, Operand>,
        stack_bound: &mut isize,
    ) -> (Self, bool) {
        let mut needs_fixing = false;
        let mut convert_operand_offset = |op| match op {
            Operand::Pseudo { ref name, size } => mappings
                .entry(Rc::clone(name))
                .or_insert_with(|| {
                    *stack_bound += size as isize;
                    Operand::StackOffset {
                        offset: -*stack_bound,
                        size,
                    }
                })
                .clone(),
            op @ Operand::StackOffset { .. } => {
                needs_fixing = true;
                op
            }
            _ => op,
        };

        (
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
                    InstructionType::Idiv(op) => InstructionType::Idiv(convert_operand_offset(op)),
                    InstructionType::Cmp { src, dst } => InstructionType::Cmp {
                        src: convert_operand_offset(src),
                        dst: convert_operand_offset(dst),
                    },
                    InstructionType::SetCC { cond_code, dst } => InstructionType::SetCC {
                        cond_code,
                        dst: convert_operand_offset(dst),
                    },
                    InstructionType::Push(op) => InstructionType::Push(convert_operand_offset(op)),
                    instr => instr,
                },
                phantom: PhantomData::<Offset>,
            },
            needs_fixing,
        )
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
                            section: RegSection::Dword,
                        }),
                    }),
                    new_instr(InstructionType::Mov {
                        src: Operand::Reg(Reg::X64 {
                            reg: X64Reg::R10,
                            section: RegSection::Dword,
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
                            section: RegSection::Dword,
                        }),
                    }),
                    new_instr(InstructionType::Binary {
                        op,
                        src1: Operand::Reg(Reg::X64 {
                            reg: X64Reg::R10,
                            section: RegSection::Dword,
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
                        section: RegSection::Dword,
                    }),
                }),
                new_instr(InstructionType::Idiv(Operand::Reg(Reg::X64 {
                    reg: X64Reg::R10,
                    section: RegSection::Dword,
                }))),
            ],
            InstructionType::Cmp {
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
                            section: RegSection::Dword,
                        }),
                    }),
                    new_instr(InstructionType::Cmp {
                        src: Operand::Reg(Reg::X64 {
                            reg: X64Reg::R10,
                            section: RegSection::Dword,
                        }),
                        dst: Operand::StackOffset {
                            offset: dst_offset,
                            size: dst_size,
                        },
                    }),
                ]
            }
            InstructionType::Cmp {
                src,
                dst: Operand::Imm(i),
            } => {
                vec![
                    new_instr(InstructionType::Mov {
                        src: Operand::Imm(i),
                        dst: Operand::Reg(Reg::X64 {
                            reg: X64Reg::R11,
                            section: RegSection::Dword,
                        }),
                    }),
                    new_instr(InstructionType::Cmp {
                        src,
                        dst: Operand::Reg(Reg::X64 {
                            reg: X64Reg::R11,
                            section: RegSection::Dword,
                        }),
                    }),
                ]
            }

            instr => vec![new_instr(instr)],
        }
    }
}

impl From<tacky::Instruction> for Vec<Instruction<Initial>> {
    fn from(instruction: tacky::Instruction) -> Vec<Instruction<Initial>> {
        let new_instr = |op| Instruction::<Initial> {
            op,
            phantom: PhantomData::<Initial>,
        };
        match instruction {
            tacky::Instruction::Return(None) => {
                vec![new_instr(InstructionType::Ret)]
            }
            tacky::Instruction::Return(Some(val)) => vec![
                new_instr(InstructionType::Mov {
                    src: Operand::from(val),
                    dst: Operand::Reg(Reg::X86 {
                        reg: X86Reg::Ax,
                        section: RegSection::Dword,
                    }),
                }),
                new_instr(InstructionType::Ret),
            ],
            tacky::Instruction::Unary { op, src, dst } => match op {
                tacky::UnaryOp::Not => vec![
                    new_instr(InstructionType::Cmp {
                        src: Operand::Imm(0),
                        dst: src.into(),
                    }),
                    new_instr(InstructionType::Mov {
                        src: Operand::Imm(0),
                        dst: dst.clone().into(),
                    }),
                    new_instr(InstructionType::SetCC {
                        cond_code: CondCode::E,
                        dst: {
                            // FIXME: Since SetCC takes a byte value we must manually
                            // fixup the stack location size
                            let dst: Operand = dst.into();
                            match dst {
                                Operand::Pseudo { name, .. } => Operand::Pseudo { name, size: 1 },
                                _ => dst,
                            }
                        },
                    }),
                ],
                _ => vec![
                    new_instr(InstructionType::Mov {
                        src: src.into(),
                        dst: dst.clone().into(),
                    }),
                    new_instr(InstructionType::Unary {
                        op: op.into(),
                        dst: dst.into(),
                    }),
                ],
            },
            tacky::Instruction::Binary {
                op,
                src1,
                src2,
                dst,
            } => match op {
                tacky::BinaryOp::Add
                | tacky::BinaryOp::Subtract
                | tacky::BinaryOp::BitAnd
                | tacky::BinaryOp::BitOr
                | tacky::BinaryOp::Xor => {
                    vec![
                        new_instr(InstructionType::Mov {
                            src: src1.into(),
                            dst: dst.clone().into(),
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
                                section: RegSection::Dword,
                            }),
                        }),
                        new_instr(InstructionType::Binary {
                            op: BinaryOp::Multiply,
                            src1: src1.into(),
                            src2: Operand::Reg(Reg::X64 {
                                reg: X64Reg::R11,
                                section: RegSection::Dword,
                            }),
                        }),
                        new_instr(InstructionType::Mov {
                            src: Operand::Reg(Reg::X64 {
                                reg: X64Reg::R11,
                                section: RegSection::Dword,
                            }),
                            dst: dst.into(),
                        }),
                    ]
                }
                tacky::BinaryOp::Divide => {
                    let eax = Operand::Reg(Reg::X86 {
                        reg: X86Reg::Ax,
                        section: RegSection::Dword,
                    });
                    vec![
                        new_instr(InstructionType::Mov {
                            src: src1.into(),
                            dst: eax.clone(),
                        }),
                        new_instr(InstructionType::Cdq),
                        new_instr(InstructionType::Idiv(src2.into())),
                        new_instr(InstructionType::Mov {
                            src: eax,
                            dst: dst.into(),
                        }),
                    ]
                }
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
                op @ tacky::BinaryOp::LShift | op @ tacky::BinaryOp::RShift => {
                    let mut v = vec![];
                    let src = match src2 {
                        tacky::Val::Constant(v) => Operand::Imm(v),
                        _ => {
                            let cl_reg = Operand::Reg(Reg::X86 {
                                reg: X86Reg::Cx,
                                section: RegSection::LowByte,
                            });
                            v.push(new_instr(InstructionType::Mov {
                                src: src2.into(),
                                dst: cl_reg.clone(),
                            }));
                            cl_reg
                        }
                    };
                    v.push(new_instr(InstructionType::Mov {
                        src: src1.into(),
                        dst: dst.clone().into(),
                    }));
                    v.push(new_instr(InstructionType::Binary {
                        op: op.into(),
                        src1: src,
                        src2: dst.into(),
                    }));
                    v
                }
                tacky::BinaryOp::Equal
                | tacky::BinaryOp::NotEqual
                | tacky::BinaryOp::LessThan
                | tacky::BinaryOp::LessOrEqual
                | tacky::BinaryOp::GreaterThan
                | tacky::BinaryOp::GreaterOrEqual => vec![
                    new_instr(InstructionType::Cmp {
                        src: src2.into(),
                        dst: src1.into(),
                    }),
                    new_instr(InstructionType::Mov {
                        src: Operand::Imm(0),
                        dst: dst.clone().into(),
                    }),
                    new_instr(InstructionType::SetCC {
                        cond_code: op.into(),
                        dst: {
                            // FIXME: Since SetCC takes a byte value we must manually
                            // fixup the stack location size
                            let dst: Operand = dst.into();
                            match dst {
                                Operand::Pseudo { name, .. } => Operand::Pseudo { name, size: 1 },
                                _ => dst,
                            }
                        },
                    }),
                ],
                _ => unimplemented!(),
            },
            tacky::Instruction::JumpIfZero { condition, target } => vec![
                new_instr(InstructionType::Cmp {
                    src: Operand::Imm(0),
                    dst: condition.into(),
                }),
                new_instr(InstructionType::JmpCC {
                    cond_code: CondCode::E,
                    identifier: target,
                }),
            ],
            tacky::Instruction::JumpIfNotZero { condition, target } => vec![
                new_instr(InstructionType::Cmp {
                    src: Operand::Imm(0),
                    dst: condition.into(),
                }),
                new_instr(InstructionType::JmpCC {
                    cond_code: CondCode::NE,
                    identifier: target,
                }),
            ],
            tacky::Instruction::Jump(label) => {
                vec![new_instr(InstructionType::Jmp(label))]
            }
            tacky::Instruction::Copy { src, dst } => vec![new_instr(InstructionType::Mov {
                src: src.into(),
                dst: dst.into(),
            })],
            tacky::Instruction::Label(label) => {
                vec![new_instr(InstructionType::Label(label))]
            }
            tacky::Instruction::FunCall {
                name,
                mut args,
                dst,
            } => {
                let (reg_args, stack_args) = {
                    let to_drain = std::cmp::min(SYSTEM_V_REGS.len(), args.len());
                    let reg_args = args.drain(..to_drain).collect::<Vec<tacky::Val>>();
                    let stack_args = args.drain(..);
                    (reg_args, stack_args)
                };

                let num_stack_args = stack_args.len();
                let stack_padding = if num_stack_args % 2 == 1 { 8 } else { 0 };
                let mut v = vec![];

                if stack_padding != 0 {
                    v.push(new_instr(InstructionType::AllocStack(stack_padding)));
                }
                for (dst_reg, src_arg) in std::iter::zip(SYSTEM_V_REGS.iter(), reg_args.into_iter())
                {
                    v.push(new_instr(InstructionType::Mov {
                        src: src_arg.into(),
                        dst: dst_reg.clone(),
                    }));
                }

                for arg in stack_args.into_iter().rev() {
                    match arg.into() {
                        Operand::Imm(i) => {
                            v.push(new_instr(InstructionType::Push(Operand::Imm(i))))
                        }
                        // NOTE: If we go to push a non-64 bit register here,
                        // it will need to be rewritten in emission as pushing
                        // the full 64-bit register
                        Operand::Reg(r) => {
                            v.push(new_instr(InstructionType::Push(Operand::Reg(r))))
                        }
                        Operand::Pseudo { name, size } => {
                            v.extend([
                                new_instr(InstructionType::Mov {
                                    src: Operand::Pseudo { name, size },
                                    dst: Operand::Reg(Reg::X86 {
                                        reg: X86Reg::Ax,
                                        section: RegSection::Dword,
                                    }),
                                }),
                                new_instr(InstructionType::Push(Operand::Reg(Reg::X86 {
                                    reg: X86Reg::Ax,
                                    section: RegSection::Dword,
                                }))),
                            ]);
                        }
                        Operand::StackOffset { offset, size } => {
                            v.extend([
                                new_instr(InstructionType::Mov {
                                    src: Operand::StackOffset { offset, size },
                                    dst: Operand::Reg(Reg::X86 {
                                        reg: X86Reg::Ax,
                                        section: RegSection::Dword,
                                    }),
                                }),
                                new_instr(InstructionType::Push(Operand::Reg(Reg::X86 {
                                    reg: X86Reg::Ax,
                                    section: RegSection::Dword,
                                }))),
                            ]);
                        }
                    }
                }
                v.push(new_instr(InstructionType::Call(name)));

                let bytes_to_remove = 8 * num_stack_args + stack_padding;
                if bytes_to_remove != 0 {
                    v.push(new_instr(InstructionType::DeAllocStack(bytes_to_remove)));
                }
                v.push(new_instr(InstructionType::Mov {
                    src: Operand::Reg(Reg::X86 {
                        reg: X86Reg::Ax,
                        section: RegSection::Dword,
                    }),
                    dst: dst.into(),
                }));
                v
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Imm(i32),
    Reg(Reg),
    Pseudo { name: Rc<String>, size: usize },
    StackOffset { offset: isize, size: usize },
}

// TODO: Unhardcode immediate size- gets pushed as 8 bytes
impl Operand {
    pub fn size(&self) -> usize {
        match self {
            Self::Imm(_) => 8,
            Self::Reg(r) => r.size(),
            Self::Pseudo { size, .. } => *size,
            Self::StackOffset { size, .. } => *size,
        }
    }
}

// FIXME: Unhardcode size of 4
impl From<tacky::Val> for Operand {
    fn from(val: tacky::Val) -> Self {
        match val {
            tacky::Val::Constant(i) => Self::Imm(i),
            tacky::Val::Var(r) => Self::Pseudo { name: r, size: 4 },
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Imm(v) => write!(f, "{v}"),
            Self::Reg(r) => write!(f, "{r}"),
            Self::StackOffset { offset, .. } => {
                write!(f, "[rbp{offset:+}]")
            }
            Self::Pseudo { .. } => {
                unreachable!("Cannot create asm representation for a pseudioregister.")
            }
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
        let actual = Vec::<Instruction<Initial>>::from(tacky);
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
        let actual = Vec::<Instruction<Initial>>::from(tacky);
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
        let actual = Vec::<Instruction<Initial>>::from(tacky);

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
        let actual = Vec::<Instruction<Initial>>::from(tacky);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_binary_expressions() {
        let tacky_fn = tacky::Function {
            name: Rc::new("test_fn".to_string()),
            params: vec![],
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
        let _ = Function::from(tacky_fn);
    }
}
