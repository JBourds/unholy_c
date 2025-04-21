use anyhow::{anyhow, bail, Result};

use crate::{ast, sema, tacky};
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
pub enum TopLevel {
    Fun(Function),
    Static(StaticVariable),
}

#[derive(Debug, PartialEq)]
pub struct StaticVariable {
    pub identifier: Rc<String>,
    pub global: bool,
    pub init: Option<Rc<[u8]>>,
}

impl From<tacky::StaticVariable> for StaticVariable {
    fn from(value: tacky::StaticVariable) -> Self {
        let tacky::StaticVariable {
            identifier,
            external_linkage: global,
            init,
        } = value;
        Self {
            identifier,
            global,
            init,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub top_level: Vec<TopLevel>,
}

impl From<tacky::Program> for Program {
    fn from(prog: tacky::Program) -> Self {
        let mut top_level = vec![];
        for item in prog.top_level.into_iter() {
            match item {
                tacky::TopLevel::Fun(f) => {
                    top_level.push(TopLevel::Fun(Function::from_with_storage(f, &prog.symbols)))
                }
                tacky::TopLevel::Static(s) => top_level.push(TopLevel::Static(s.into())),
            }
        }
        Program { top_level }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Rc<String>,
    pub global: bool,
    pub instructions: Vec<Instruction<Final>>,
}

impl Function {
    fn from_with_storage(node: tacky::Function, symbols: &tacky::SymbolTable) -> Self {
        let tacky::Function {
            name,
            mut params,
            external_linkage: global,
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
            Instruction::<Initial>::new(InstructionType::allocate_stack(0)),
        ];

        let mut mappings = HashMap::new();
        // We always start with a stack bound of 8 for RBP
        // Include register args here since we move into them
        for (src_reg, dst_arg) in
            std::iter::zip(SYSTEM_V_REGS.into_iter(), reg_args.into_iter().flatten())
        {
            let param_symbol = symbols
                .get(&dst_arg)
                .expect("function param should already be in symbol table");
            instructions.push(Instruction::<Initial>::new(InstructionType::Mov {
                src: src_reg,
                dst: Operand::Pseudo {
                    name: dst_arg,
                    size: param_symbol.r#type.size_of(),
                },
            }));
        }

        // Hardcoded 8 here due to pushing RBP
        let mut stack_bound = 8;
        for arg in stack_args.into_iter().flatten() {
            stack_bound += 8;
            let arg_symbol = symbols
                .get(&arg)
                .expect("function param should already be in symbol table");
            instructions.push(Instruction::<Initial>::new(InstructionType::Mov {
                src: Operand::StackOffset {
                    offset: stack_bound,
                    size: arg_symbol.r#type.size_of(),
                },
                dst: Operand::Pseudo {
                    name: arg,
                    size: arg_symbol.r#type.size_of(),
                },
            }));
        }

        for instr in fun_instructions.into_iter() {
            instructions.extend(Instruction::<Initial>::from_tacky(instr, symbols));
        }

        // Get stack offsets for each pseudoregister as we fix them up
        // Start moving down for arguments & temp vars used here
        let mut stack_bound = 0;
        let mut requires_fixup = vec![];
        let mut fixed_instructions: Vec<Instruction<WithStorage>> = instructions
            .drain(..)
            .enumerate()
            .map(|(i, instr)| {
                let (instr, needs_fixing) = Instruction::<WithStorage>::new(
                    instr,
                    symbols,
                    &mut mappings,
                    &mut stack_bound,
                );
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
        fixed_instructions[2].op = InstructionType::allocate_stack(stack_bound);
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
            global,
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

    pub fn from_size(size: usize) -> Result<Self> {
        match size {
            _ if Self::LowByte.size() == size => Ok(Self::LowByte),
            _ if Self::HighByte.size() == size => Ok(Self::HighByte),
            _ if Self::Word.size() == size => Ok(Self::Word),
            _ if Self::Dword.size() == size => Ok(Self::Dword),
            _ if Self::Qword.size() == size => Ok(Self::Qword),
            _ => bail!("Could not convert size {size} to register"),
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
enum Initial {}
#[derive(Debug, PartialEq)]
enum WithStorage {}
#[derive(Debug, PartialEq)]
pub enum Final {}

#[derive(Clone, Debug, PartialEq)]
pub enum InstructionType {
    Mov {
        src: Operand,
        dst: Operand,
    },
    Movsx {
        src: Operand,
        dst: Operand,
    },
    Unary {
        op: UnaryOp,
        dst: Operand,
    },
    Binary {
        op: BinaryOp,
        src: Operand,
        dst: Operand,
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
    Push(Operand),
    // Invariant: Can only pop to a register or stack offset
    Pop(Operand),
    Call(Rc<String>),
    Ret,
}

impl InstructionType {
    pub fn allocate_stack(bytes: isize) -> Self {
        Self::Binary {
            op: BinaryOp::Subtract,
            src: Operand::Imm(ast::Constant::Long(bytes.try_into().expect("i64 == isize"))),
            dst: Operand::Reg(Reg::X86 {
                reg: X86Reg::Sp,
                section: RegSection::Qword,
            }),
        }
    }

    pub fn deallocate_stack(bytes: isize) -> Self {
        Self::Binary {
            op: BinaryOp::Add,
            src: Operand::Imm(ast::Constant::Long(bytes.try_into().expect("i64 == isize"))),
            dst: Operand::Reg(Reg::X86 {
                reg: X86Reg::Sp,
                section: RegSection::Qword,
            }),
        }
    }
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

impl Instruction<WithStorage> {
    fn new(
        instruction: Instruction<Initial>,
        symbols: &tacky::SymbolTable,
        mappings: &mut HashMap<Rc<String>, Operand>,
        stack_bound: &mut isize,
    ) -> (Self, bool) {
        let mut needs_fixing = false;
        let mut convert_operand_offset = |op| match op {
            Operand::Pseudo { ref name, size } => {
                // TODO: Add calculations for type size in earlier passes and
                // remove hardcoded "4" here
                match symbols.get(name) {
                    // 1. Check for static storage
                    Some(entry)
                        if matches!(entry.attribute, sema::tc::Attribute::Static { .. }) =>
                    {
                        Operand::Data {
                            name: Rc::clone(name),
                            size: 4,
                        }
                    }
                    // 2. If it is not static, put it on the stack
                    _ => mappings
                        .entry(Rc::clone(name))
                        .or_insert_with(|| {
                            *stack_bound += size as isize;
                            Operand::StackOffset {
                                offset: -*stack_bound,
                                size,
                            }
                        })
                        .clone(),
                }
            }
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
                    InstructionType::Binary { op, src, dst } => InstructionType::Binary {
                        op,
                        src: convert_operand_offset(src),
                        dst: convert_operand_offset(dst),
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
                phantom: PhantomData::<WithStorage>,
            },
            needs_fixing,
        )
    }
}

// Final stage of rewriting to follow semantics of assembly instructions
impl From<Instruction<WithStorage>> for Vec<Instruction<Final>> {
    fn from(instr: Instruction<WithStorage>) -> Vec<Instruction<Final>> {
        let new_instr = |op| Instruction::<Final> {
            op,
            phantom: PhantomData::<Final>,
        };

        let r10 = Operand::Reg(Reg::X64 {
            reg: X64Reg::R10,
            section: RegSection::Dword,
        });
        let r11 = Operand::Reg(Reg::X64 {
            reg: X64Reg::R11,
            section: RegSection::Dword,
        });
        match instr.op {
            InstructionType::Mov {
                src: src @ Operand::StackOffset { .. } | src @ Operand::Data { .. },
                dst: dst @ Operand::StackOffset { .. } | dst @ Operand::Data { .. },
            } => {
                vec![
                    new_instr(InstructionType::Mov {
                        src,
                        dst: r10.clone(),
                    }),
                    new_instr(InstructionType::Mov {
                        src: r10.clone(),
                        dst,
                    }),
                ]
            }
            InstructionType::Binary {
                op,
                src: src @ Operand::StackOffset { .. } | src @ Operand::Data { .. },
                dst: dst @ Operand::StackOffset { .. } | dst @ Operand::Data { .. },
            } => {
                vec![
                    new_instr(InstructionType::Mov {
                        src,
                        dst: r10.clone(),
                    }),
                    new_instr(InstructionType::Binary { op, src: r10, dst }),
                ]
            }
            InstructionType::Idiv(src @ Operand::Imm(_)) => vec![
                new_instr(InstructionType::Mov {
                    src,
                    dst: r10.clone(),
                }),
                new_instr(InstructionType::Idiv(r10)),
            ],
            InstructionType::Cmp {
                src: src @ Operand::StackOffset { .. } | src @ Operand::Data { .. },
                dst: dst @ Operand::StackOffset { .. } | dst @ Operand::Data { .. },
            } => {
                vec![
                    new_instr(InstructionType::Mov {
                        src,
                        dst: r11.clone(),
                    }),
                    new_instr(InstructionType::Cmp { src: r11, dst }),
                ]
            }
            InstructionType::Cmp {
                src,
                dst: imm @ Operand::Imm(_),
            } => {
                vec![
                    new_instr(InstructionType::Mov {
                        src: imm,
                        dst: r10.clone(),
                    }),
                    new_instr(InstructionType::Cmp { src, dst: r10 }),
                ]
            }

            instr => vec![new_instr(instr)],
        }
    }
}

impl Instruction<Initial> {
    fn from_tacky(instruction: tacky::Instruction, symbols: &tacky::SymbolTable) -> Vec<Self> {
        let new_instr = |op| Instruction::<Initial> {
            op,
            phantom: PhantomData::<Initial>,
        };
        let eax = Operand::Reg(Reg::X86 {
            reg: X86Reg::Ax,
            section: RegSection::Dword,
        });
        let r11 = Operand::Reg(Reg::X64 {
            reg: X64Reg::R11,
            section: RegSection::Dword,
        });
        match instruction {
            tacky::Instruction::Return(None) => {
                vec![new_instr(InstructionType::Ret)]
            }
            tacky::Instruction::SignExtend { src, dst } => todo!(),
            tacky::Instruction::Truncate { src, dst } => todo!(),
            tacky::Instruction::Return(Some(val)) => {
                let src = Operand::from_tacky(val, symbols);
                vec![
                    new_instr(InstructionType::Mov {
                        src: src.clone(),
                        dst: Operand::Reg(Reg::X86 {
                            reg: X86Reg::Ax,
                            section: RegSection::from_size(src.size())
                                .expect("NOT IMPLEMENTED YET :("),
                        }),
                    }),
                    new_instr(InstructionType::Ret),
                ]
            }
            tacky::Instruction::Unary { op, src, dst } => match op {
                tacky::UnaryOp::Not => vec![
                    new_instr(InstructionType::Cmp {
                        src: Operand::Imm(ast::Constant::Int(0)),
                        dst: Operand::from_tacky(src, symbols),
                    }),
                    new_instr(InstructionType::Mov {
                        src: Operand::Imm(ast::Constant::Int(0)),
                        dst: Operand::from_tacky(dst.clone(), symbols),
                    }),
                    new_instr(InstructionType::SetCC {
                        cond_code: CondCode::E,
                        dst: {
                            // FIXME: Since SetCC takes a byte value we must manually
                            // fixup the stack location size
                            let dst: Operand = Operand::from_tacky(dst, symbols);
                            match dst {
                                Operand::Pseudo { name, .. } => Operand::Pseudo { name, size: 1 },
                                _ => dst,
                            }
                        },
                    }),
                ],
                _ => vec![
                    new_instr(InstructionType::Mov {
                        src: Operand::from_tacky(src, symbols),
                        dst: Operand::from_tacky(dst.clone(), symbols),
                    }),
                    new_instr(InstructionType::Unary {
                        op: op.into(),
                        dst: Operand::from_tacky(dst, symbols),
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
                            src: Operand::from_tacky(src1, symbols),
                            dst: Operand::from_tacky(dst.clone(), symbols),
                        }),
                        new_instr(InstructionType::Binary {
                            op: op.into(),
                            src: Operand::from_tacky(src2, symbols),
                            dst: Operand::from_tacky(dst, symbols),
                        }),
                    ]
                }
                tacky::BinaryOp::Multiply => {
                    vec![
                        new_instr(InstructionType::Mov {
                            src: Operand::from_tacky(src2, symbols),
                            dst: r11.clone(),
                        }),
                        new_instr(InstructionType::Binary {
                            op: BinaryOp::Multiply,
                            src: Operand::from_tacky(src1, symbols),
                            dst: r11.clone(),
                        }),
                        new_instr(InstructionType::Mov {
                            src: r11,
                            dst: Operand::from_tacky(dst, symbols),
                        }),
                    ]
                }
                tacky::BinaryOp::Divide => {
                    vec![
                        new_instr(InstructionType::Mov {
                            src: Operand::from_tacky(src1, symbols),
                            dst: eax.clone(),
                        }),
                        new_instr(InstructionType::Cdq),
                        new_instr(InstructionType::Idiv(Operand::from_tacky(src2, symbols))),
                        new_instr(InstructionType::Mov {
                            src: eax,
                            dst: Operand::from_tacky(dst, symbols),
                        }),
                    ]
                }
                tacky::BinaryOp::Remainder => vec![
                    new_instr(InstructionType::Mov {
                        src: Operand::from_tacky(src1, symbols),
                        dst: eax,
                    }),
                    new_instr(InstructionType::Cdq),
                    new_instr(InstructionType::Idiv(Operand::from_tacky(src2, symbols))),
                    new_instr(InstructionType::Mov {
                        src: Operand::Reg(Reg::X86 {
                            reg: X86Reg::Dx,
                            section: RegSection::Dword,
                        }),
                        dst: Operand::from_tacky(dst, symbols),
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
                                src: Operand::from_tacky(src2, symbols),
                                dst: cl_reg.clone(),
                            }));
                            cl_reg
                        }
                    };
                    v.push(new_instr(InstructionType::Mov {
                        src: Operand::from_tacky(src1, symbols),
                        dst: Operand::from_tacky(dst.clone(), symbols),
                    }));
                    v.push(new_instr(InstructionType::Binary {
                        op: op.into(),
                        src,
                        dst: Operand::from_tacky(dst, symbols),
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
                        src: Operand::from_tacky(src2, symbols),
                        dst: Operand::from_tacky(src1, symbols),
                    }),
                    new_instr(InstructionType::Mov {
                        src: Operand::Imm(ast::Constant::Int(0)),
                        dst: Operand::from_tacky(dst.clone(), symbols),
                    }),
                    new_instr(InstructionType::SetCC {
                        cond_code: op.into(),
                        dst: {
                            // FIXME: Since SetCC takes a byte value we must manually
                            // fixup the stack location size
                            let dst: Operand = Operand::from_tacky(dst, symbols);
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
                    src: Operand::Imm(ast::Constant::Int(0)),
                    dst: Operand::from_tacky(condition, symbols),
                }),
                new_instr(InstructionType::JmpCC {
                    cond_code: CondCode::E,
                    identifier: target,
                }),
            ],
            tacky::Instruction::JumpIfNotZero { condition, target } => vec![
                new_instr(InstructionType::Cmp {
                    src: Operand::Imm(ast::Constant::Int(0)),
                    dst: Operand::from_tacky(condition, symbols),
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
                src: Operand::from_tacky(src, symbols),
                dst: Operand::from_tacky(dst, symbols),
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
                    v.push(new_instr(InstructionType::allocate_stack(stack_padding)));
                }
                for (dst_reg, src_arg) in std::iter::zip(SYSTEM_V_REGS.iter(), reg_args.into_iter())
                {
                    v.push(new_instr(InstructionType::Mov {
                        src: Operand::from_tacky(src_arg, symbols),
                        dst: dst_reg.clone(),
                    }));
                }

                for arg in stack_args.into_iter().rev() {
                    match Operand::from_tacky(arg, symbols) {
                        Operand::Imm(i) => {
                            v.push(new_instr(InstructionType::Push(Operand::Imm(i))))
                        }
                        // NOTE: If we go to push a non-64 bit register here,
                        // it will need to be rewritten in emission as pushing
                        // the full 64-bit register
                        Operand::Reg(r) => {
                            v.push(new_instr(InstructionType::Push(Operand::Reg(r))))
                        }
                        src @ Operand::StackOffset { .. }
                        | src @ Operand::Pseudo { .. }
                        | src @ Operand::Data { .. } => {
                            v.extend([
                                new_instr(InstructionType::Mov {
                                    src,
                                    dst: eax.clone(),
                                }),
                                new_instr(InstructionType::Push(eax.clone())),
                            ]);
                        }
                    }
                }
                v.push(new_instr(InstructionType::Call(name)));

                let bytes_to_remove = 8 * num_stack_args + stack_padding as usize;
                if bytes_to_remove != 0 {
                    v.push(new_instr(InstructionType::deallocate_stack(
                        bytes_to_remove as isize,
                    )));
                }
                v.push(new_instr(InstructionType::Mov {
                    src: eax,
                    dst: Operand::from_tacky(dst, symbols),
                }));
                v
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Imm(ast::Constant),
    Reg(Reg),
    Pseudo { name: Rc<String>, size: usize },
    StackOffset { offset: isize, size: usize },
    Data { name: Rc<String>, size: usize },
}

// TODO: Unhardcode immediate size- gets pushed as 8 bytes
impl Operand {
    pub fn size(&self) -> usize {
        match self {
            Self::Imm(_) => 8,
            Self::Reg(r) => r.size(),
            Self::Pseudo { size, .. } => *size,
            Self::StackOffset { size, .. } => *size,
            Self::Data { size, .. } => *size,
        }
    }

    fn from_tacky(val: tacky::Val, symbols: &tacky::SymbolTable) -> Self {
        match val {
            tacky::Val::Constant(i) => Self::Imm(i),
            tacky::Val::Var(r) => {
                let symbol = symbols
                    .get(&r)
                    .expect("every tacky val is already in the symbol table");
                Self::Pseudo {
                    name: r,
                    size: symbol.r#type.size_of(),
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
            Self::StackOffset { offset, .. } => {
                write!(f, "[rbp{offset:+}]")
            }
            Self::Data { name, .. } => {
                write!(f, "{name}[rip]")
            }
            Self::Pseudo { .. } => {
                unreachable!("Cannot create asm representation for a pseudioregister.")
            }
        }
    }
}
