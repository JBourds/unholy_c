use anyhow::{Result, bail};

use crate::{ast, sema, tacky};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::marker::PhantomData;
use std::rc::Rc;

const SYSTEM_V_GP_REGS: [Reg; 6] = [
    Reg::X86 {
        reg: X86Reg::Di,
        section: RegSection::Dword,
    },
    Reg::X86 {
        reg: X86Reg::Si,
        section: RegSection::Dword,
    },
    Reg::X86 {
        reg: X86Reg::Dx,
        section: RegSection::Dword,
    },
    Reg::X86 {
        reg: X86Reg::Cx,
        section: RegSection::Dword,
    },
    Reg::X64 {
        reg: X64Reg::R8,
        section: RegSection::Dword,
    },
    Reg::X64 {
        reg: X64Reg::R9,
        section: RegSection::Dword,
    },
];

const SYSTEM_V_FP_REGS: [Reg; 8] = [
    Reg::XMM {
        reg: XMMReg::XMM0,
        section: RegSection::Dword,
    },
    Reg::XMM {
        reg: XMMReg::XMM1,
        section: RegSection::Dword,
    },
    Reg::XMM {
        reg: XMMReg::XMM2,
        section: RegSection::Dword,
    },
    Reg::XMM {
        reg: XMMReg::XMM3,
        section: RegSection::Dword,
    },
    Reg::XMM {
        reg: XMMReg::XMM4,
        section: RegSection::Dword,
    },
    Reg::XMM {
        reg: XMMReg::XMM5,
        section: RegSection::Dword,
    },
    Reg::XMM {
        reg: XMMReg::XMM6,
        section: RegSection::Dword,
    },
    Reg::XMM {
        reg: XMMReg::XMM7,
        section: RegSection::Dword,
    },
];

#[derive(Debug, PartialEq)]
pub enum TopLevel {
    Fun(Function),
    StaticVariable(StaticVariable),
    StaticConstant(StaticConstant),
}

// This is cursed.
// Checking for uniqueness of floats within a set is annoying since they don't
// implement hashing. This is a hacky solution which kills several birds (and
// code reviewers) with one stone by using the formatted float string as:
//  1. A unique name + hash key
//  2. A representation for the code emission pass
//
// RC so we feel less bad about cloning this bad puppy
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StaticConstant {
    id: Rc<String>,
    alignment: usize,
}

impl StaticConstant {
    fn new(id: Rc<String>, alignment: usize) -> Self {
        Self { id, alignment }
    }
}

impl From<f32> for StaticConstant {
    fn from(value: f32) -> Self {
        Self::new(
            Rc::new(format!("{value:.32}")),
            core::mem::align_of::<f32>(),
        )
    }
}

impl From<f64> for StaticConstant {
    fn from(value: f64) -> Self {
        Self::new(
            Rc::new(format!("{value:.64}")),
            core::mem::align_of::<f64>(),
        )
    }
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

#[derive(Debug)]
pub struct Program {
    pub top_level: Vec<TopLevel>,
    pub symbols: tacky::SymbolTable,
}

impl From<tacky::Program> for Program {
    fn from(prog: tacky::Program) -> Self {
        let mut top_level = vec![];
        let mut constants = HashSet::new();
        for item in prog.top_level.into_iter() {
            match item {
                tacky::TopLevel::Fun(f) => {
                    let fun = Function::from_with_storage(f, &prog.symbols, &mut constants);
                    top_level.push(TopLevel::Fun(fun));
                }
                tacky::TopLevel::Static(s) => top_level.push(TopLevel::StaticVariable(s.into())),
            }
        }
        top_level.extend(
            constants
                .into_iter()
                .map(TopLevel::StaticConstant)
                .collect::<Vec<_>>(),
        );
        Program {
            top_level,
            symbols: prog.symbols,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Rc<String>,
    pub global: bool,
    pub instructions: Vec<Instruction<Final>>,
}

impl Function {
    fn from_with_storage(
        node: tacky::Function,
        symbols: &tacky::SymbolTable,
        constants: &mut HashSet<StaticConstant>,
    ) -> Self {
        let tacky::Function {
            name,
            mut params,
            external_linkage: global,
            instructions: fun_instructions,
        } = node;
        // Create the System V register/stack mappings here
        let (reg_args, stack_args) = {
            let to_drain = std::cmp::min(SYSTEM_V_GP_REGS.len(), params.len());
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
            std::iter::zip(SYSTEM_V_GP_REGS.into_iter(), reg_args.into_iter().flatten())
        {
            let param_symbol = symbols
                .get(&dst_arg)
                .expect("function param should already be in symbol table");
            instructions.push(Instruction::<Initial>::new(InstructionType::Mov {
                src: Operand::Reg(src_reg.as_section(
                    RegSection::from_size(param_symbol.r#type.size_of()).expect("FIXME"),
                )),
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
            instructions.extend(Instruction::<Initial>::from_tacky(
                instr, symbols, constants,
            ));
        }

        // Get stack offsets for each pseudoregister as we fix them up
        // Start moving down for arguments & temp vars used here
        let mut stack_bound = 0;
        let mut fixed_instructions: Vec<Instruction<WithStorage>> = instructions
            .drain(..)
            .map(|instr| {
                Instruction::<WithStorage>::new(instr, symbols, &mut mappings, &mut stack_bound)
            })
            .collect();

        // Setup stack prologue
        // Sixteen byte alignment is required
        stack_bound += match stack_bound % 16 {
            0 => 0,
            remainder => 16 - remainder,
        };
        fixed_instructions[2].op = InstructionType::allocate_stack(stack_bound);

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
    Sar,
    Shr,
}

impl BinaryOp {
    fn from_op_and_sign(op: tacky::BinaryOp, signed: bool) -> Self {
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
            Self::BitAnd => write!(f, "and"),
            Self::BitOr => write!(f, "or"),
            Self::Xor => write!(f, "xor"),
            Self::LShift => write!(f, "sal"),
            Self::Sar => write!(f, "sar"),
            Self::Shr => write!(f, "shr"),
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
pub enum XMMReg {
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15,
}

impl From<&XMMReg> for usize {
    fn from(value: &XMMReg) -> Self {
        match value {
            XMMReg::XMM0 => 0,
            XMMReg::XMM1 => 1,
            XMMReg::XMM2 => 2,
            XMMReg::XMM3 => 3,
            XMMReg::XMM4 => 4,
            XMMReg::XMM5 => 5,
            XMMReg::XMM6 => 6,
            XMMReg::XMM7 => 7,
            XMMReg::XMM8 => 8,
            XMMReg::XMM9 => 9,
            XMMReg::XMM10 => 10,
            XMMReg::XMM11 => 11,
            XMMReg::XMM12 => 12,
            XMMReg::XMM13 => 13,
            XMMReg::XMM14 => 14,
            XMMReg::XMM15 => 15,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Reg {
    X86 { reg: X86Reg, section: RegSection },
    X64 { reg: X64Reg, section: RegSection },
    XMM { reg: XMMReg, section: RegSection },
}

impl Reg {
    pub fn size(&self) -> usize {
        match self {
            Self::X86 { reg: _, section } => section.size(),
            Self::X64 { reg: _, section } => section.size(),
            Self::XMM { reg: _, section } => section.size(),
        }
    }

    pub fn as_section(self, section: RegSection) -> Self {
        match self {
            Self::X86 { reg, .. } => Self::X86 { reg, section },
            Self::X64 { reg, .. } => Self::X64 { reg, section },
            Self::XMM { reg, .. } => Self::XMM { reg, section },
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
            Self::XMM { reg, section } => {
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
    A,
    AE,
    B,
    BE,
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
        }
    }
}

impl CondCode {
    fn from_uses_cf_zf_op(value: tacky::BinaryOp, uses_cf_zf: bool) -> Self {
        match value {
            tacky::BinaryOp::Equal => Self::E,
            tacky::BinaryOp::NotEqual => Self::NE,
            tacky::BinaryOp::LessThan => {
                if uses_cf_zf {
                    Self::L
                } else {
                    Self::B
                }
            }
            tacky::BinaryOp::LessOrEqual => {
                if uses_cf_zf {
                    Self::LE
                } else {
                    Self::BE
                }
            }
            tacky::BinaryOp::GreaterThan => {
                if uses_cf_zf {
                    Self::G
                } else {
                    Self::A
                }
            }
            tacky::BinaryOp::GreaterOrEqual => {
                if uses_cf_zf {
                    Self::GE
                } else {
                    Self::AE
                }
            }
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
    MovZeroExtend {
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
    Div(Operand),
    Cdq(RegSection),
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
    Cvttsd2si {
        src: Operand,
        dst: Operand,
    },
    Cvtsi2sd {
        src: Operand,
        dst: Operand,
    },
    DivDouble {
        src: Operand,
        dst: Operand,
    },
}

impl InstructionType {
    pub fn allocate_stack(bytes: usize) -> Self {
        Self::Binary {
            op: BinaryOp::Subtract,
            src: Operand::Imm(ast::Constant::I64(bytes.try_into().expect("i64 == isize"))),
            dst: Operand::Reg(Reg::X86 {
                reg: X86Reg::Sp,
                section: RegSection::Qword,
            }),
        }
    }

    pub fn deallocate_stack(bytes: usize) -> Self {
        Self::Binary {
            op: BinaryOp::Add,
            src: Operand::Imm(ast::Constant::I64(bytes.try_into().expect("i64 == isize"))),
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
        stack_bound: &mut usize,
    ) -> Self {
        let align_up = |addr: usize, align: usize| -> usize {
            let remainder = addr % align;
            if remainder == 0 {
                addr // addr already aligned
            } else {
                addr - remainder + align
            }
        };
        let mut convert_operand_offset = |op| match op {
            Operand::Pseudo { ref name, size } => {
                match symbols.get(name) {
                    // 1. Check for static storage
                    Some(entry)
                        if matches!(entry.attribute, sema::tc::Attribute::Static { .. }) =>
                    {
                        Operand::Data {
                            name: Rc::clone(name),
                            size: entry.r#type.size_of(),
                        }
                    }
                    // 2. If it is not static, put it on the stack
                    _ => mappings
                        .entry(Rc::clone(name))
                        .or_insert_with(|| {
                            *stack_bound = align_up(*stack_bound, size);
                            *stack_bound += size;
                            Operand::StackOffset {
                                offset: -(*stack_bound as isize),
                                size,
                            }
                        })
                        .clone(),
                }
            }
            _ => op,
        };

        Self {
            op: match instruction.op {
                InstructionType::Mov { src, dst } => InstructionType::Mov {
                    src: convert_operand_offset(src),
                    dst: convert_operand_offset(dst),
                },
                InstructionType::Movsx { src, dst } => InstructionType::Movsx {
                    src: convert_operand_offset(src),
                    dst: convert_operand_offset(dst),
                },
                InstructionType::MovZeroExtend { src, dst } => InstructionType::MovZeroExtend {
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
                InstructionType::Div(op) => InstructionType::Div(convert_operand_offset(op)),
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
        }
    }
}

impl Instruction<WithStorage> {
    fn from_op(op: InstructionType) -> Self {
        Self {
            op,
            phantom: PhantomData::<WithStorage>,
        }
    }
    fn fixup_stack_vars(self) -> Vec<Self> {
        match self.op {
            InstructionType::Mov {
                src: src @ Operand::StackOffset { .. } | src @ Operand::Data { .. },
                dst: dst @ Operand::StackOffset { .. } | dst @ Operand::Data { .. },
            } => {
                let r10 = Operand::Reg(Reg::X64 {
                    reg: X64Reg::R10,
                    section: RegSection::from_size(dst.size()).expect("FIXME"),
                });
                vec![
                    Self::from_op(InstructionType::Mov {
                        src,
                        dst: r10.clone(),
                    }),
                    Self::from_op(InstructionType::Mov { src: r10, dst }),
                ]
            }
            InstructionType::Movsx { src, dst } => {
                let (src, src_instrs) = match src {
                    src @ Operand::Imm(..) => {
                        let r10 = Operand::Reg(Reg::X64 {
                            reg: X64Reg::R10,
                            section: RegSection::from_size(src.size()).expect("FIXME"),
                        });

                        let instrs = vec![Self::from_op(InstructionType::Mov {
                            src,
                            dst: r10.clone(),
                        })];
                        (r10, instrs)
                    }
                    src => (src, vec![]),
                };

                let (dst, dst_instrs) = match dst {
                    dst @ Operand::StackOffset { .. } | dst @ Operand::Data { .. } => {
                        let r11 = Operand::Reg(Reg::X64 {
                            reg: X64Reg::R11,
                            section: RegSection::from_size(dst.size()).expect("FIXME"),
                        });

                        let instrs = vec![Self::from_op(InstructionType::Mov {
                            src: r11.clone(),
                            dst,
                        })];
                        (r11, instrs)
                    }
                    dst => (dst, vec![]),
                };

                let mut instrs = vec![];
                instrs.extend(src_instrs);

                instrs.push(Self::from_op(InstructionType::Movsx { src, dst }));

                instrs.extend(dst_instrs);

                instrs
            }
            InstructionType::MovZeroExtend {
                src,
                dst: reg @ Operand::Reg(_),
            } => {
                vec![Self::from_op(InstructionType::Mov { src, dst: reg })]
            }
            InstructionType::MovZeroExtend {
                src,
                dst: dst @ Operand::StackOffset { .. },
            } => {
                vec![
                    Self::from_op(InstructionType::Mov {
                        src,
                        dst: Operand::Reg(Reg::X64 {
                            reg: X64Reg::R11,
                            section: RegSection::Dword,
                        }),
                    }),
                    Self::from_op(InstructionType::Mov {
                        src: Operand::Reg(Reg::X64 {
                            reg: X64Reg::R11,
                            section: RegSection::Qword,
                        }),
                        dst,
                    }),
                ]
            }
            InstructionType::Binary {
                op,
                src: src @ Operand::StackOffset { .. } | src @ Operand::Data { .. },
                dst: dst @ Operand::StackOffset { .. } | dst @ Operand::Data { .. },
            } => {
                let r10 = Operand::Reg(Reg::X64 {
                    reg: X64Reg::R10,
                    section: RegSection::from_size(dst.size()).expect("FIXME"),
                });
                vec![
                    Self::from_op(InstructionType::Mov {
                        src,
                        dst: r10.clone(),
                    }),
                    Self::from_op(InstructionType::Binary { op, src: r10, dst }),
                ]
            }
            InstructionType::Idiv(src @ Operand::Imm(_)) => {
                let r10 = Operand::Reg(Reg::X64 {
                    reg: X64Reg::R10,
                    section: RegSection::from_size(src.size()).expect("FIXME"),
                });
                vec![
                    Self::from_op(InstructionType::Mov {
                        src,
                        dst: r10.clone(),
                    }),
                    Self::from_op(InstructionType::Idiv(r10)),
                ]
            }
            InstructionType::Div(src @ Operand::Imm(_)) => {
                let r10 = Operand::Reg(Reg::X64 {
                    reg: X64Reg::R10,
                    section: RegSection::from_size(src.size()).expect("FIXME"),
                });
                vec![
                    Self::from_op(InstructionType::Mov {
                        src,
                        dst: r10.clone(),
                    }),
                    Self::from_op(InstructionType::Div(r10)),
                ]
            }
            InstructionType::Cmp {
                src: src @ Operand::StackOffset { .. } | src @ Operand::Data { .. },
                dst: dst @ Operand::StackOffset { .. } | dst @ Operand::Data { .. },
            } => {
                let r11 = Operand::Reg(Reg::X64 {
                    reg: X64Reg::R11,
                    section: RegSection::from_size(dst.size()).expect("FIXME"),
                });
                vec![
                    Self::from_op(InstructionType::Mov {
                        src,
                        dst: r11.clone(),
                    }),
                    Self::from_op(InstructionType::Cmp { src: r11, dst }),
                ]
            }
            InstructionType::Cmp {
                src,
                dst: imm @ Operand::Imm(_),
            } => {
                let r11 = Operand::Reg(Reg::X64 {
                    reg: X64Reg::R11,
                    section: RegSection::from_size(imm.size()).expect("FIXME"),
                });
                vec![
                    Self::from_op(InstructionType::Mov {
                        src: imm,
                        dst: r11.clone(),
                    }),
                    Self::from_op(InstructionType::Cmp { src, dst: r11 }),
                ]
            }

            instr => vec![Self::from_op(instr)],
        }
    }

    fn fixup_immediates(self) -> Vec<Self> {
        match self.op {
            InstructionType::Binary {
                op:
                    op @ BinaryOp::Add
                    | op @ BinaryOp::Subtract
                    | op @ BinaryOp::Multiply
                    | op @ BinaryOp::BitAnd
                    | op @ BinaryOp::BitOr
                    | op @ BinaryOp::Xor,
                src: src @ Operand::Imm(..),
                dst,
            } if src.size() > 4 => {
                assert!(
                    !matches!(dst, Operand::Imm(..)),
                    "The destination of an immediate in addition should have already been resolved"
                );
                let r10 = Operand::Reg(Reg::X64 {
                    reg: X64Reg::R10,
                    section: RegSection::from_size(src.size()).expect("FIXME"),
                });
                vec![
                    Self::from_op(InstructionType::Mov {
                        src,
                        dst: r10.clone(),
                    }),
                    Self::from_op(InstructionType::Binary { op, src: r10, dst }),
                ]
            }
            InstructionType::Cmp {
                src: src @ Operand::Imm(..),
                dst,
            } if src.size() > 4 => {
                assert!(
                    !matches!(dst, Operand::Imm(..)),
                    "The destination of an immediate in addition should have already been resolved"
                );
                let r10 = Operand::Reg(Reg::X64 {
                    reg: X64Reg::R10,
                    section: RegSection::from_size(src.size()).expect("FIXME"),
                });
                vec![
                    Self::from_op(InstructionType::Mov {
                        src,
                        dst: r10.clone(),
                    }),
                    Self::from_op(InstructionType::Cmp { src: r10, dst }),
                ]
            }
            // Anything outside of the range of i32 needs a mov first
            InstructionType::Push(Operand::Imm(constant)) if !constant.fits_in::<i32>() => {
                let imm = Operand::Imm(constant);
                let r10 = Operand::Reg(Reg::X64 {
                    reg: X64Reg::R10,
                    section: RegSection::from_size(imm.size()).expect("FIXME"),
                });
                vec![
                    Self::from_op(InstructionType::Mov {
                        src: imm,
                        dst: r10.clone(),
                    }),
                    Self::from_op(InstructionType::Push(r10)),
                ]
            }
            InstructionType::Mov {
                src: imm @ Operand::Imm(..),
                dst: dst @ Operand::StackOffset { .. } | dst @ Operand::Data { .. },
            } if imm.size() > 4 => {
                let r10 = Operand::Reg(Reg::X64 {
                    reg: X64Reg::R10,
                    section: RegSection::from_size(dst.size()).expect("FIXME"),
                });
                vec![
                    Self::from_op(InstructionType::Mov {
                        src: imm,
                        dst: r10.clone(),
                    }),
                    Self::from_op(InstructionType::Mov { src: r10, dst }),
                ]
            }
            InstructionType::Mov {
                src: imm @ Operand::Imm(..),
                dst,
            } if dst.size() < imm.size() => {
                let Operand::Imm(c) = imm else { unreachable!() };
                // FIXME: Constant should probably provide some way to truncate here
                let imm = match c {
                    ast::Constant::I64(i) => Operand::Imm(ast::Constant::I32(i as i32)),
                    _ => unreachable!(),
                };
                vec![Self::from_op(InstructionType::Mov { src: imm, dst })]
            }
            op => vec![Self::from_op(op)],
        }
    }
}

// Final stage of rewriting to follow semantics of assembly instructions
impl From<Instruction<WithStorage>> for Vec<Instruction<Final>> {
    fn from(instr: Instruction<WithStorage>) -> Vec<Instruction<Final>> {
        instr
            .fixup_stack_vars()
            .into_iter()
            .flat_map(Instruction::<WithStorage>::fixup_immediates)
            .map(|instr| Instruction::<Final> {
                op: instr.op,
                phantom: PhantomData::<Final>,
            })
            .collect::<Vec<Instruction<Final>>>()
    }
}

impl Instruction<Initial> {
    fn from_tacky(
        instruction: tacky::Instruction,
        symbols: &tacky::SymbolTable,
        float_constants: &mut HashSet<StaticConstant>,
    ) -> Vec<Self> {
        let new_instr = |op| Instruction::<Initial> {
            op,
            phantom: PhantomData::<Initial>,
        };

        match instruction {
            tacky::Instruction::Return(None) => {
                vec![new_instr(InstructionType::Ret)]
            }
            tacky::Instruction::SignExtend { src, dst } => {
                vec![new_instr(InstructionType::Movsx {
                    src: Operand::from_tacky(src, symbols, float_constants),
                    dst: Operand::from_tacky(dst, symbols, float_constants),
                })]
            }
            tacky::Instruction::Truncate { src, dst } => {
                // dst should have a smaller type here
                vec![new_instr(InstructionType::Mov {
                    src: Operand::from_tacky(src, symbols, float_constants),
                    dst: Operand::from_tacky(dst, symbols, float_constants),
                })]
            }
            tacky::Instruction::Return(Some(val)) => {
                let src = Operand::from_tacky(val, symbols, float_constants);
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
            tacky::Instruction::Unary { op, src, dst } => {
                if is_float(&src, symbols) && matches!(op, tacky::UnaryOp::Not) {
                    // Super special 16-byte alignemnt needed here for SSE
                    let s = Rc::new("-0.0".to_string());
                    float_constants.insert(StaticConstant::new(Rc::clone(&s), 16));
                    let dst = Operand::from_tacky(dst, symbols, float_constants);
                    return vec![
                        new_instr(InstructionType::Mov {
                            src: Operand::from_tacky(src, symbols, float_constants),
                            dst: dst.clone(),
                        }),
                        new_instr(InstructionType::Binary {
                            op: BinaryOp::Xor,
                            src: Operand::Data { name: s, size: 16 },
                            dst,
                        }),
                    ];
                }

                match op {
                    tacky::UnaryOp::Not => vec![
                        new_instr(InstructionType::Cmp {
                            src: Operand::Imm(ast::Constant::I32(0)),
                            dst: Operand::from_tacky(src, symbols, float_constants),
                        }),
                        new_instr(InstructionType::Mov {
                            src: Operand::Imm(ast::Constant::I32(0)),
                            dst: Operand::from_tacky(dst.clone(), symbols, float_constants),
                        }),
                        new_instr(InstructionType::SetCC {
                            cond_code: CondCode::E,
                            dst: {
                                // FIXME: Since SetCC takes a byte value we must manually
                                // fixup the stack location size
                                // FIXME: This maybe should also edit the symbol table
                                let dst: Operand =
                                    Operand::from_tacky(dst, symbols, float_constants);
                                match dst {
                                    Operand::Pseudo { name, .. } => {
                                        Operand::Pseudo { name, size: 1 }
                                    }
                                    _ => dst,
                                }
                            },
                        }),
                    ],
                    _ => vec![
                        new_instr(InstructionType::Mov {
                            src: Operand::from_tacky(src, symbols, float_constants),
                            dst: Operand::from_tacky(dst.clone(), symbols, float_constants),
                        }),
                        new_instr(InstructionType::Unary {
                            op: op.into(),
                            dst: Operand::from_tacky(dst, symbols, float_constants),
                        }),
                    ],
                }
            }
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
                    let signed_op = is_signed(&src1, symbols);
                    vec![
                        new_instr(InstructionType::Mov {
                            src: Operand::from_tacky(src1, symbols, float_constants),
                            dst: Operand::from_tacky(dst.clone(), symbols, float_constants),
                        }),
                        new_instr(InstructionType::Binary {
                            op: BinaryOp::from_op_and_sign(op, signed_op),
                            src: Operand::from_tacky(src2, symbols, float_constants),
                            dst: Operand::from_tacky(dst, symbols, float_constants),
                        }),
                    ]
                }
                tacky::BinaryOp::Multiply => {
                    let src1 = Operand::from_tacky(src1, symbols, float_constants);
                    let r11 = Operand::Reg(Reg::X64 {
                        reg: X64Reg::R11,
                        section: RegSection::from_size(src1.size()).expect("FIXME"),
                    });
                    vec![
                        new_instr(InstructionType::Mov {
                            src: Operand::from_tacky(src2, symbols, float_constants),
                            dst: r11.clone(),
                        }),
                        new_instr(InstructionType::Binary {
                            op: BinaryOp::Multiply,
                            src: src1,
                            dst: r11.clone(),
                        }),
                        new_instr(InstructionType::Mov {
                            src: r11,
                            dst: Operand::from_tacky(dst, symbols, float_constants),
                        }),
                    ]
                }
                tacky::BinaryOp::Divide => {
                    // Check for double division
                    assert!(
                        is_float(&src1, symbols) == is_float(&src2, symbols),
                        "Either both operators should be floats or neither should be floats."
                    );
                    if is_float(&src1, symbols) {
                        let src1 = Operand::from_tacky(src1, symbols, float_constants);
                        let src2 = Operand::from_tacky(src2, symbols, float_constants);
                        let dst = Operand::from_tacky(dst, symbols, float_constants);
                        return vec![
                            new_instr(InstructionType::Mov {
                                src: src1,
                                dst: dst.clone(),
                            }),
                            new_instr(InstructionType::DivDouble { src: src2, dst }),
                        ];
                    }

                    // No doubles, process integer division
                    let signed_div = is_signed(&src1, symbols);
                    let src1 = Operand::from_tacky(src1, symbols, float_constants);
                    let op_size = src1.size();
                    let section = RegSection::from_size(op_size).expect("NOT IMPLEMENTED YET :(");
                    let ax = Operand::Reg(Reg::X86 {
                        reg: X86Reg::Ax,
                        section,
                    });
                    let dx = Operand::Reg(Reg::X86 {
                        reg: X86Reg::Dx,
                        section: RegSection::from_size(src1.size())
                            .expect("NOT IMPLEMENTED YET :("),
                    });
                    if signed_div {
                        vec![
                            new_instr(InstructionType::Mov {
                                src: src1,
                                dst: ax.clone(),
                            }),
                            new_instr(InstructionType::Cdq(section)),
                            new_instr(InstructionType::Idiv(Operand::from_tacky(
                                src2,
                                symbols,
                                float_constants,
                            ))),
                            new_instr(InstructionType::Mov {
                                src: ax,
                                dst: Operand::from_tacky(dst, symbols, float_constants),
                            }),
                        ]
                    } else {
                        vec![
                            new_instr(InstructionType::Mov {
                                src: src1,
                                dst: ax.clone(),
                            }),
                            new_instr(InstructionType::Mov {
                                src: make_zero(op_size, signed_div),
                                dst: dx.clone(),
                            }),
                            new_instr(InstructionType::Div(Operand::from_tacky(
                                src2,
                                symbols,
                                float_constants,
                            ))),
                            new_instr(InstructionType::Mov {
                                src: ax,
                                dst: Operand::from_tacky(dst, symbols, float_constants),
                            }),
                        ]
                    }
                }
                tacky::BinaryOp::Remainder => {
                    let signed_rem = is_signed(&src1, symbols);
                    let src1 = Operand::from_tacky(src1, symbols, float_constants);
                    let op_size = src1.size();
                    let section = RegSection::from_size(op_size).expect("NOT IMPLEMENTED YET :(");
                    let ax = Operand::Reg(Reg::X86 {
                        reg: X86Reg::Ax,
                        section,
                    });
                    let dx = Operand::Reg(Reg::X86 {
                        reg: X86Reg::Dx,
                        section: RegSection::from_size(src1.size())
                            .expect("NOT IMPLEMENTED YET :("),
                    });

                    if signed_rem {
                        vec![
                            new_instr(InstructionType::Mov { src: src1, dst: ax }),
                            new_instr(InstructionType::Cdq(section)),
                            new_instr(InstructionType::Idiv(Operand::from_tacky(
                                src2,
                                symbols,
                                float_constants,
                            ))),
                            new_instr(InstructionType::Mov {
                                src: dx,
                                dst: Operand::from_tacky(dst, symbols, float_constants),
                            }),
                        ]
                    } else {
                        vec![
                            new_instr(InstructionType::Mov { src: src1, dst: ax }),
                            new_instr(InstructionType::Mov {
                                src: make_zero(op_size, signed_rem),
                                dst: dx.clone(),
                            }),
                            new_instr(InstructionType::Div(Operand::from_tacky(
                                src2,
                                symbols,
                                float_constants,
                            ))),
                            new_instr(InstructionType::Mov {
                                src: dx,
                                dst: Operand::from_tacky(dst, symbols, float_constants),
                            }),
                        ]
                    }
                }
                op @ tacky::BinaryOp::LShift | op @ tacky::BinaryOp::RShift => {
                    let signed_op = is_signed(&src1, symbols);
                    let mut v = vec![];
                    let src = match src2 {
                        tacky::Val::Constant(v) => Operand::Imm(v),
                        _ => {
                            let cl_reg = Operand::Reg(Reg::X86 {
                                reg: X86Reg::Cx,
                                section: RegSection::LowByte,
                            });
                            v.push(new_instr(InstructionType::Mov {
                                src: Operand::from_tacky(src2, symbols, float_constants),
                                dst: cl_reg.clone(),
                            }));
                            cl_reg
                        }
                    };
                    v.push(new_instr(InstructionType::Mov {
                        src: Operand::from_tacky(src1, symbols, float_constants),
                        dst: Operand::from_tacky(dst.clone(), symbols, float_constants),
                    }));
                    v.push(new_instr(InstructionType::Binary {
                        op: BinaryOp::from_op_and_sign(op, signed_op),
                        src,
                        dst: Operand::from_tacky(dst, symbols, float_constants),
                    }));
                    v
                }
                tacky::BinaryOp::Equal
                | tacky::BinaryOp::NotEqual
                | tacky::BinaryOp::LessThan
                | tacky::BinaryOp::LessOrEqual
                | tacky::BinaryOp::GreaterThan
                | tacky::BinaryOp::GreaterOrEqual => {
                    // Unsigned integers and doubles both set the CF and ZF
                    // when doing comparisons
                    assert!(
                        is_float(&src1, symbols) == is_float(&src2, symbols),
                        "Either both operators should be floats or neither should be floats."
                    );
                    let use_cf_zf_cmp = !is_signed(&src1, symbols) || is_float(&src2, symbols);
                    vec![
                        new_instr(InstructionType::Cmp {
                            src: Operand::from_tacky(src2, symbols, float_constants),
                            dst: Operand::from_tacky(src1, symbols, float_constants),
                        }),
                        new_instr(InstructionType::Mov {
                            src: Operand::Imm(ast::Constant::I32(0)),
                            dst: Operand::from_tacky(dst.clone(), symbols, float_constants),
                        }),
                        new_instr(InstructionType::SetCC {
                            cond_code: CondCode::from_uses_cf_zf_op(op, use_cf_zf_cmp),
                            dst: {
                                // FIXME: Since SetCC takes a byte value we must manually
                                // fixup the stack location size
                                let dst = Operand::from_tacky(dst, symbols, float_constants);
                                match dst {
                                    Operand::Pseudo { name, .. } => {
                                        Operand::Pseudo { name, size: 1 }
                                    }
                                    _ => dst,
                                }
                            },
                        }),
                    ]
                }
                _ => unimplemented!(),
            },
            tacky::Instruction::JumpIfZero { condition, target } => vec![
                new_instr(InstructionType::Cmp {
                    src: Operand::Imm(ast::Constant::I32(0)),
                    dst: Operand::from_tacky(condition, symbols, float_constants),
                }),
                new_instr(InstructionType::JmpCC {
                    cond_code: CondCode::E,
                    identifier: target,
                }),
            ],
            tacky::Instruction::JumpIfNotZero { condition, target } => vec![
                new_instr(InstructionType::Cmp {
                    src: Operand::Imm(ast::Constant::I32(0)),
                    dst: Operand::from_tacky(condition, symbols, float_constants),
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
                src: Operand::from_tacky(src, symbols, float_constants),
                dst: Operand::from_tacky(dst, symbols, float_constants),
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
                    let to_drain = std::cmp::min(SYSTEM_V_GP_REGS.len(), args.len());
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
                for (dst_reg, src_arg) in
                    std::iter::zip(SYSTEM_V_GP_REGS.iter(), reg_args.into_iter())
                {
                    let src_arg = Operand::from_tacky(src_arg, symbols, float_constants);
                    let size = src_arg.size();
                    v.push(new_instr(InstructionType::Mov {
                        src: src_arg,
                        dst: Operand::Reg(
                            (*dst_reg).as_section(RegSection::from_size(size).expect("FIXME")),
                        ),
                    }));
                }

                for arg in stack_args.into_iter().rev() {
                    match Operand::from_tacky(arg, symbols, float_constants) {
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
                            let size = src.size();
                            // FIXME: This should really stop being hardcoded
                            if size == 8 {
                                v.push(new_instr(InstructionType::Push(src)));
                            } else {
                                let ax = Operand::Reg(Reg::X86 {
                                    reg: X86Reg::Ax,
                                    section: RegSection::from_size(src.size())
                                        .expect("NOT IMPLEMENTED YET :("),
                                });

                                v.extend([
                                    new_instr(InstructionType::Mov {
                                        src,
                                        dst: ax.clone(),
                                    }),
                                    new_instr(InstructionType::Push(ax)),
                                ]);
                            }
                        }
                    }
                }
                v.push(new_instr(InstructionType::Call(name)));

                let bytes_to_remove = 8 * num_stack_args + stack_padding;
                if bytes_to_remove != 0 {
                    v.push(new_instr(InstructionType::deallocate_stack(
                        bytes_to_remove,
                    )));
                }
                let dst = Operand::from_tacky(dst, symbols, float_constants);
                let ax = Operand::Reg(Reg::X86 {
                    reg: X86Reg::Ax,
                    section: RegSection::from_size(dst.size()).expect("NOT IMPLEMENTED YET :("),
                });
                v.push(new_instr(InstructionType::Mov { src: ax, dst }));
                v
            }
            tacky::Instruction::ZeroExtend { src, dst } => {
                vec![new_instr(InstructionType::MovZeroExtend {
                    src: Operand::from_tacky(src, symbols, float_constants),
                    dst: Operand::from_tacky(dst, symbols, float_constants),
                })]
            }
            tacky::Instruction::DoubleToInt { .. } => todo!(),
            tacky::Instruction::IntToDouble { .. } => todo!(),
            tacky::Instruction::DoubleToUInt { .. } => todo!(),
            tacky::Instruction::UIntToDouble { .. } => todo!(),
        }
    }
}

fn is_float(val: &tacky::Val, symbols: &tacky::SymbolTable) -> bool {
    matches!(
        val.get_type(symbols),
        ast::Type {
            base: ast::BaseType::Float(_) | ast::BaseType::Double(_),
            ptr: None,
            ..
        }
    )
}

fn is_signed(val: &tacky::Val, symbols: &tacky::SymbolTable) -> bool {
    match val.get_type(symbols) {
        ast::Type {
            base: ast::BaseType::Int { signed, .. },
            ptr: None,
            ..
        } => signed.is_none_or(|signed| signed),
        _ => true,
    }
}

fn make_zero(size_bytes: usize, signed: bool) -> Operand {
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

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Imm(ast::Constant),
    Reg(Reg),
    Pseudo { name: Rc<String>, size: usize },
    StackOffset { offset: isize, size: usize },
    Data { name: Rc<String>, size: usize },
}

impl Operand {
    pub fn size(&self) -> usize {
        match self {
            Self::Imm(c) => c.get_type().size_of(),
            Self::Reg(r) => r.size(),
            Self::Pseudo { size, .. } => *size,
            Self::StackOffset { size, .. } => *size,
            Self::Data { size, .. } => *size,
        }
    }

    fn from_tacky(
        val: tacky::Val,
        symbols: &tacky::SymbolTable,
        float_constants: &mut HashSet<StaticConstant>,
    ) -> Self {
        match val {
            tacky::Val::Constant(c @ ast::Constant::F32(v)) => {
                let static_const = StaticConstant::from(v);
                if !float_constants.contains(&static_const) {
                    float_constants.insert(static_const.clone());
                }
                Operand::Data {
                    name: static_const.id,
                    size: c.size_bytes(),
                }
            }
            tacky::Val::Constant(c @ ast::Constant::F64(v)) => {
                let static_const = StaticConstant::from(v);
                if !float_constants.contains(&static_const) {
                    float_constants.insert(static_const.clone());
                }
                Operand::Data {
                    name: static_const.id,
                    size: c.size_bytes(),
                }
            }
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
