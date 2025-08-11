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
    Reg::Xmm {
        reg: XmmReg::XMM0,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM1,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM2,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM3,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM4,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM5,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM6,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM7,
        section: RegSection::Dword,
    },
];

#[derive(Clone, Debug, PartialEq)]
pub enum AssemblyType {
    Byte,
    Word,
    Longword,
    Quadword,
    Pointer,
    Float,
    Double,
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
        }
    }
}

impl AssemblyType {
    pub fn uses_xmm_regs(&self) -> bool {
        matches!(self, Self::Float | Self::Double)
    }

    fn from_tacky(val: &tacky::Val, symbols: &tacky::SymbolTable) -> Self {
        Self::from_ast_type(val.get_type(symbols))
    }

    fn from_ast_type(r#type: ast::Type) -> Self {
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
            _ => unimplemented!(),
        }
    }

    fn size_bytes(&self) -> usize {
        match self {
            AssemblyType::Byte => core::mem::size_of::<u8>(),
            AssemblyType::Word => core::mem::size_of::<u16>(),
            AssemblyType::Longword => core::mem::size_of::<u32>(),
            AssemblyType::Quadword => core::mem::size_of::<u64>(),
            AssemblyType::Pointer => core::mem::size_of::<usize>(),
            AssemblyType::Float => core::mem::size_of::<f32>(),
            AssemblyType::Double => core::mem::size_of::<f64>(),
        }
    }
}

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
    pub id: Rc<String>,
    pub val: FpNumber,
    pub alignment: usize,
}

impl StaticConstant {
    const LONG_MAX_VAL: f64 = i64::MAX as f64;

    fn new(id: Rc<String>, val: FpNumber, alignment: usize) -> Self {
        Self { id, val, alignment }
    }

    fn with_alignment(self, alignment: usize) -> Self {
        Self { alignment, ..self }
    }

    fn id(&self) -> Rc<String> {
        Rc::clone(&self.id)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum FpNumber {
    F32(u32),
    F64(u64),
}

impl From<f32> for StaticConstant {
    fn from(value: f32) -> Self {
        let val = u32::from_ne_bytes(value.to_ne_bytes());
        Self::new(
            Rc::new(ryu::Buffer::new().format(value).to_string()),
            FpNumber::F32(val),
            core::mem::align_of::<f32>(),
        )
    }
}

impl From<f64> for StaticConstant {
    fn from(value: f64) -> Self {
        let val = u64::from_ne_bytes(value.to_ne_bytes());
        Self::new(
            Rc::new(ryu::Buffer::new().format(value).to_string()),
            FpNumber::F64(val),
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

// Create unique identifiers (e.g., Labels in cast)
fn make_temp(counter: &'_ mut usize) -> impl FnMut(String) -> String + use<'_> {
    move |name| {
        let n = *counter;
        *counter += 1;
        format!("codegen.{name}.{n}")
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
        let mut counter = 0;
        let mut make_label = make_temp(&mut counter);
        for item in prog.top_level.into_iter() {
            match item {
                tacky::TopLevel::Fun(f) => {
                    let fun = Function::from_with_storage(
                        f,
                        &prog.symbols,
                        &mut constants,
                        &mut make_label,
                    );
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
        make_label: &mut impl FnMut(String) -> String,
    ) -> Self {
        let tacky::Function {
            name,
            signature,
            external_linkage: global,
            instructions: fun_instructions,
        } = node;

        // Prologue
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

        let mut gpr_args = vec![];
        let mut fpr_args = vec![];
        let mut stack_args = vec![];
        for (r#type, name) in signature.into_iter() {
            let param_type = AssemblyType::from_ast_type(r#type);
            match param_type {
                AssemblyType::Float | AssemblyType::Double
                    if fpr_args.len() < SYSTEM_V_FP_REGS.len() =>
                {
                    fpr_args.push((param_type, name));
                }
                // FIXME: Pointer types will also want to be pushed here (probably)
                // since they are essentially just word-sized unsigned ints
                AssemblyType::Byte
                | AssemblyType::Word
                | AssemblyType::Longword
                | AssemblyType::Quadword
                    if gpr_args.len() < SYSTEM_V_GP_REGS.len() =>
                {
                    gpr_args.push((param_type, name));
                }
                _ => {
                    stack_args.push((param_type, name));
                }
            }
        }

        let mut mappings = HashMap::new();
        // We always start with a stack bound of 8 for RBP
        // Include register args here since we move into them
        for (src_reg, (dst_type, dst)) in
            std::iter::zip(SYSTEM_V_GP_REGS.into_iter(), gpr_args.into_iter()).chain(
                std::iter::zip(SYSTEM_V_FP_REGS.into_iter(), fpr_args.into_iter()),
            )
        {
            instructions.push(Instruction::<Initial>::new(InstructionType::Mov {
                src: Operand::Reg(
                    src_reg
                        .as_section(RegSection::from_size(dst_type.size_bytes()).expect("FIXME")),
                ),
                dst: Operand::Pseudo {
                    name: dst.expect("FIXME: Is this always not null?"),
                    size: dst_type.size_bytes(),
                    r#type: dst_type,
                },
            }));
        }

        // Hardcoded 8 here due to pushing RBP
        let mut stack_bound = 8;
        for (arg_type, arg) in stack_args.into_iter() {
            stack_bound += 8;
            instructions.push(Instruction::<Initial>::new(InstructionType::Mov {
                src: Operand::Memory {
                    reg: Reg::RBP,
                    offset: stack_bound,
                    size: arg_type.size_bytes(),
                    r#type: arg_type.clone(),
                },
                dst: Operand::Pseudo {
                    name: arg.expect("FIXME: Is this always not null?"),
                    size: arg_type.size_bytes(),
                    r#type: arg_type,
                },
            }));
        }

        for instr in fun_instructions.into_iter() {
            instructions.extend(Instruction::<Initial>::from_tacky(
                instr, symbols, constants, make_label,
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
    DivDouble,
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
pub enum XmmReg {
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

impl From<&XmmReg> for usize {
    fn from(value: &XmmReg) -> Self {
        match value {
            XmmReg::XMM0 => 0,
            XmmReg::XMM1 => 1,
            XmmReg::XMM2 => 2,
            XmmReg::XMM3 => 3,
            XmmReg::XMM4 => 4,
            XmmReg::XMM5 => 5,
            XmmReg::XMM6 => 6,
            XmmReg::XMM7 => 7,
            XmmReg::XMM8 => 8,
            XmmReg::XMM9 => 9,
            XmmReg::XMM10 => 10,
            XmmReg::XMM11 => 11,
            XmmReg::XMM12 => 12,
            XmmReg::XMM13 => 13,
            XmmReg::XMM14 => 14,
            XmmReg::XMM15 => 15,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Reg {
    X86 { reg: X86Reg, section: RegSection },
    X64 { reg: X64Reg, section: RegSection },
    Xmm { reg: XmmReg, section: RegSection },
}

impl Reg {
    const RBP: Self = Self::X86 {
        reg: X86Reg::Bp,
        section: RegSection::Qword,
    };
    const RAX: Self = Self::X86 {
        reg: X86Reg::Ax,
        section: RegSection::Qword,
    };
    pub fn size(&self) -> usize {
        match self {
            Self::X86 { reg: _, section } => section.size(),
            Self::X64 { reg: _, section } => section.size(),
            Self::Xmm { reg: _, section } => section.size(),
        }
    }

    pub fn as_section(self, section: RegSection) -> Self {
        match self {
            Self::X86 { reg, .. } => Self::X86 { reg, section },
            Self::X64 { reg, .. } => Self::X64 { reg, section },
            Self::Xmm { reg, .. } => Self::Xmm { reg, section },
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
            Self::Xmm { reg, .. } => {
                write!(f, "xmm{}", usize::from(reg))
            }
        }
    }
}

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

impl CondCode {
    fn from_uses_cf_zf_op(value: tacky::BinaryOp, uses_cf_zf: bool) -> Self {
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
    CMovCC {
        src: Operand,
        dst: Operand,
        cond_code: CondCode,
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
    JmpCCRel {
        cond_code: CondCode,
        offset: i32,
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
            Operand::Pseudo {
                ref name,
                size,
                r#type,
            } => {
                match symbols.get(name) {
                    // 1. Check for static storage
                    Some(entry)
                        if matches!(entry.attribute, sema::tc::Attribute::Static { .. }) =>
                    {
                        Operand::Data {
                            name: Rc::clone(name),
                            size: entry.r#type.size_of(),
                            r#type,
                            is_const: false,
                        }
                    }
                    // 2. If it is not static, put it on the stack
                    _ => mappings
                        .entry(Rc::clone(name))
                        .or_insert_with(|| {
                            *stack_bound = align_up(*stack_bound, size);
                            *stack_bound += size;
                            Operand::Memory {
                                reg: Reg::RBP,
                                offset: -(*stack_bound as isize),
                                size,
                                r#type,
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
                InstructionType::CMovCC {
                    src,
                    dst,
                    cond_code,
                } => InstructionType::CMovCC {
                    src: convert_operand_offset(src),
                    dst: convert_operand_offset(dst),
                    cond_code,
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
                InstructionType::Cvtsi2sd { src, dst } => InstructionType::Cvtsi2sd {
                    src: convert_operand_offset(src),
                    dst: convert_operand_offset(dst),
                },
                InstructionType::Cvttsd2si { src, dst } => InstructionType::Cvttsd2si {
                    src: convert_operand_offset(src),
                    dst: convert_operand_offset(dst),
                },
                InstructionType::DivDouble { src, dst } => InstructionType::DivDouble {
                    src: convert_operand_offset(src),
                    dst: convert_operand_offset(dst),
                },
                instr @ InstructionType::Cdq(_) => instr,
                instr @ InstructionType::Jmp(_) => instr,
                instr @ InstructionType::JmpCC { .. } => instr,
                instr @ InstructionType::JmpCCRel { .. } => instr,
                instr @ InstructionType::Label(_) => instr,
                instr @ InstructionType::Pop(_) => instr,
                instr @ InstructionType::Call(_) => instr,
                instr @ InstructionType::Ret => instr,
            },
            phantom: PhantomData::<WithStorage>,
        }
    }
}

enum ImmRewrite {
    Ignore,
    Require,
    Error,
}

impl ImmRewrite {
    fn requires_rewrite(&self, arg: &Operand) -> bool {
        assert!(
            !(matches!(self, Self::Error) && arg.is_imm()),
            "Rewrite rule prohibits the use of an immediate for argument, but found {arg:?}."
        );
        matches!(self, Self::Require) && arg.is_imm()
    }
}

enum MemRewrite {
    Ignore,
    Default,
    UseAndStore,
    UseNoStore,
    StoreNoUse,
    #[allow(dead_code)]
    Error,
}

/// Struct encapsulating the rules for performing rewrites
/// * `imm_rule` - Rule for when the operand is an immedate.
/// * `mem_rule` - Rule for when the operand is a memory address.
/// * `use_other_op_size` - Flag for whether the rewrite register should use
///   its respective size or the other operand's size. (e.g., `Mov` will only
///   use the `dst` size, but `Movsx` each uses their own.
struct RewriteRule {
    imm_rule: ImmRewrite,
    mem_rule: MemRewrite,
    use_other_op_size: bool,
}

impl RewriteRule {
    fn new(imm_rule: ImmRewrite, mem_rule: MemRewrite, use_other_op_size: bool) -> Self {
        Self {
            imm_rule,
            mem_rule,
            use_other_op_size,
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

    /// General-purpose function designed to streamline all the invariants an
    /// instruction can have in a declarative way.
    /// NOTE: Does not have a register rewrite rule since, so far, there are
    /// no instructions which are not allowed to have register args but can
    /// have a memory or immediate.
    ///
    /// # Arguments
    ///
    /// * `src` -  Source operand.
    /// * `dst` -  Destination operand.
    /// * `src_rewrites` - Struct containing information on how to rewrite ops.
    /// * `dst_rewrites` - Struct containing information on how to rewrite ops.
    ///
    /// # Returns
    ///
    /// Vec<Self> - Vector of instructions which uphold desired semantics for
    /// a given function.
    fn rewrite_move(
        src: Operand,
        dst: Operand,
        src_rewrites: RewriteRule,
        dst_rewrites: RewriteRule,
        make_op: impl Fn(Operand, Operand) -> Self,
    ) -> Vec<Self> {
        // Rewrite registers
        let src_type = AssemblyType::from(&src);
        let dst_type = AssemblyType::from(&dst);
        let src_rewrite_size = if src_rewrites.use_other_op_size {
            dst_type.size_bytes()
        } else {
            src_type.size_bytes()
        };
        let dst_rewrite_size = if dst_rewrites.use_other_op_size {
            src_type.size_bytes()
        } else {
            dst_type.size_bytes()
        };

        // Step 1. Rewrite immediate values which are not allowed to be immediates

        // Rewrite the `src` and `dst` operands if they cannot be immediates
        // but are by using the designated rewrite registers
        let src_rewrite_reg = if src_type.uses_xmm_regs() {
            Operand::Reg(Reg::Xmm {
                reg: XmmReg::XMM14,
                section: RegSection::from_size(src_rewrite_size).expect("FIXME"),
            })
        } else {
            Operand::Reg(Reg::X64 {
                reg: X64Reg::R10,
                section: RegSection::from_size(src_rewrite_size).expect("FIXME"),
            })
        };

        let dst_rewrite_reg = if dst_type.uses_xmm_regs() {
            Operand::Reg(Reg::Xmm {
                reg: XmmReg::XMM15,
                section: RegSection::from_size(dst_rewrite_size).expect("FIXME"),
            })
        } else {
            Operand::Reg(Reg::X64 {
                reg: X64Reg::R11,
                section: RegSection::from_size(dst_rewrite_size).expect("FIXME"),
            })
        };

        let mut instrs = vec![];

        // Step 1. Rewrite immediate values which are not allowed to be immediates

        // Rewrite the `src` and `dst` operands if they cannot be immediates
        // but are by using the designated rewrite registers
        let src = if src_rewrites.imm_rule.requires_rewrite(&src) {
            instrs.push(Self::from_op(InstructionType::Mov {
                src,
                dst: src_rewrite_reg.clone(),
            }));
            src_rewrite_reg.clone()
        } else {
            src
        };

        let dst = if dst_rewrites.imm_rule.requires_rewrite(&dst) {
            instrs.push(Self::from_op(InstructionType::Mov {
                src: dst,
                dst: dst_rewrite_reg.clone(),
            }));
            dst_rewrite_reg.clone()
        } else {
            dst
        };

        // Step 2. Rewrite memory address values where they are not allowed

        // Happy path: Always good
        if dst.is_reg() {
            instrs.push(make_op(src, dst));
            return instrs;
        }
        // Ignore dispatches differently based on float vs. int
        //  - float: Behaves like `UseAndStore` (needs a register in the `dst`
        //    but will also use the value again.
        //  - int: Make sure there aren't two memory operands, perform `src`
        //    rewrite as needed.
        let dst_rewrite_rule =
            // `Default` for XMM regs is to require a register in the operand
            if dst_type.uses_xmm_regs() && matches!(dst_rewrites.mem_rule, MemRewrite::Default) {
                MemRewrite::UseAndStore
            } else {
                dst_rewrites.mem_rule
            };

        // Check for necessary rewrites
        match dst_rewrite_rule {
            MemRewrite::UseNoStore => {
                // E.g., When doing a `cmp` the `dst` won't be touched but it
                // needs to be put into a register
                instrs.push(Self::from_op(InstructionType::Mov {
                    src: dst.clone(),
                    dst: dst_rewrite_reg.clone(),
                }));
                instrs.push(make_op(src.clone(), dst_rewrite_reg.clone()));
            }
            MemRewrite::StoreNoUse => {
                // E.g., When converting int to double the `dst` operand is just
                // where result needs to end up
                instrs.push(make_op(src.clone(), dst_rewrite_reg.clone()));
                instrs.push(Self::from_op(InstructionType::Mov {
                    src: dst_rewrite_reg.clone(),
                    dst: dst.clone(),
                }));
            }
            MemRewrite::UseAndStore => {
                instrs.push(Self::from_op(InstructionType::Mov {
                    src: dst.clone(),
                    dst: dst_rewrite_reg.clone(),
                }));
                instrs.push(make_op(src.clone(), dst_rewrite_reg.clone()));
                instrs.push(Self::from_op(InstructionType::Mov {
                    src: dst_rewrite_reg.clone(),
                    dst: dst.clone(),
                }));
            }
            MemRewrite::Default | MemRewrite::Ignore => {
                // No instruction can use two memory addresses.
                // This case means a memory address is valid in the second
                // argument, but that we just can't have both.
                if src.is_mem() && dst.is_mem() {
                    instrs.push(Self::from_op(InstructionType::Mov {
                        src,
                        dst: src_rewrite_reg.clone(),
                    }));
                    instrs.push(make_op(src_rewrite_reg.clone(), dst));
                } else {
                    instrs.push(make_op(src, dst));
                }
            }
            MemRewrite::Error => unreachable!(),
        }

        instrs
    }

    fn fixup_stack_vars(self) -> Vec<Self> {
        match self.op {
            InstructionType::Mov { src, dst } => Self::rewrite_move(
                src,
                dst,
                RewriteRule::new(ImmRewrite::Ignore, MemRewrite::Default, true),
                RewriteRule::new(ImmRewrite::Error, MemRewrite::Ignore, false),
                |src, dst| Self::from_op(InstructionType::Mov { src, dst }),
            ),
            InstructionType::CMovCC {
                src,
                dst,
                cond_code,
            } => Self::rewrite_move(
                src,
                dst,
                RewriteRule::new(ImmRewrite::Require, MemRewrite::Default, true),
                RewriteRule::new(ImmRewrite::Error, MemRewrite::UseAndStore, false),
                |src, dst| {
                    Self::from_op(InstructionType::CMovCC {
                        src,
                        dst,
                        cond_code,
                    })
                },
            ),
            InstructionType::Movsx { src, dst } => Self::rewrite_move(
                src,
                dst,
                RewriteRule::new(ImmRewrite::Require, MemRewrite::Default, false),
                RewriteRule::new(ImmRewrite::Error, MemRewrite::StoreNoUse, false),
                |src, dst| Self::from_op(InstructionType::Movsx { src, dst }),
            ),
            InstructionType::MovZeroExtend {
                src,
                dst: reg @ Operand::Reg(_),
            } => {
                vec![Self::from_op(InstructionType::Mov { src, dst: reg })]
            }
            InstructionType::MovZeroExtend {
                src,
                dst: dst @ Operand::Memory { .. },
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
            InstructionType::Binary { op, src, dst } => Self::rewrite_move(
                src,
                dst,
                RewriteRule::new(ImmRewrite::Ignore, MemRewrite::Default, true),
                RewriteRule::new(ImmRewrite::Error, MemRewrite::Default, false),
                |src, dst| {
                    Self::from_op(InstructionType::Binary {
                        op: op.clone(),
                        src,
                        dst,
                    })
                },
            ),
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
            InstructionType::Cvtsi2sd { src, dst } => Self::rewrite_move(
                src,
                dst,
                RewriteRule::new(ImmRewrite::Require, MemRewrite::Default, false),
                RewriteRule::new(ImmRewrite::Error, MemRewrite::StoreNoUse, true),
                |src, dst| Self::from_op(InstructionType::Cvtsi2sd { src, dst }),
            ),
            InstructionType::Cvttsd2si { src, dst } => Self::rewrite_move(
                src,
                dst,
                RewriteRule::new(ImmRewrite::Require, MemRewrite::Default, false),
                RewriteRule::new(ImmRewrite::Error, MemRewrite::StoreNoUse, false),
                |src, dst| Self::from_op(InstructionType::Cvttsd2si { src, dst }),
            ),
            InstructionType::Cmp { src, dst } => Self::rewrite_move(
                src,
                dst,
                RewriteRule::new(ImmRewrite::Ignore, MemRewrite::Default, true),
                RewriteRule::new(ImmRewrite::Require, MemRewrite::UseNoStore, false),
                |src, dst| Self::from_op(InstructionType::Cmp { src, dst }),
            ),
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
                    !dst.is_imm(),
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
                    !dst.is_imm(),
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
                dst: dst @ Operand::Memory { .. } | dst @ Operand::Data { .. },
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

/// Classify a vector of tacky arguments into three lists:
///     1. Args passed via general purpose registers
///     2. Args passed via floating-point registers
///     3. Args passed via the stack
/// Returns: (GPR args, FPR args, Stack args)
fn classify_function_args(
    args: Vec<tacky::Val>,
    symbols: &tacky::SymbolTable,
) -> (Vec<tacky::Val>, Vec<tacky::Val>, Vec<tacky::Val>) {
    let mut gpr_args = vec![];
    let mut fpr_args = vec![];
    let mut stack_args = vec![];
    for arg in args.into_iter() {
        let arg_type = AssemblyType::from_tacky(&arg, symbols);
        match arg_type {
            AssemblyType::Float | AssemblyType::Double
                if fpr_args.len() < SYSTEM_V_FP_REGS.len() =>
            {
                fpr_args.push(arg);
            }
            // FIXME: Pointer types will also want to be pushed here (probably)
            // since they are essentially just word-sized unsigned ints
            AssemblyType::Byte
            | AssemblyType::Word
            | AssemblyType::Longword
            | AssemblyType::Quadword
                if gpr_args.len() < SYSTEM_V_GP_REGS.len() =>
            {
                gpr_args.push(arg);
            }
            _ => {
                stack_args.push(arg);
            }
        }
    }
    (gpr_args, fpr_args, stack_args)
}

impl Instruction<Initial> {
    fn from_tacky(
        instruction: tacky::Instruction,
        symbols: &tacky::SymbolTable,
        float_constants: &mut HashSet<StaticConstant>,
        make_label: &mut impl FnMut(String) -> String,
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
                let val_type = val.get_type(symbols);
                let src = Operand::from_tacky(val, symbols, float_constants);

                match val_type {
                    ast::Type {
                        base: ast::BaseType::Int { .. },
                        ..
                    } => {
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
                    ast::Type {
                        base: ast::BaseType::Float(_) | ast::BaseType::Double(_),
                        ..
                    } => {
                        vec![
                            new_instr(InstructionType::Mov {
                                src: src.clone(),
                                dst: Operand::Reg(Reg::Xmm {
                                    reg: XmmReg::XMM0,
                                    section: RegSection::from_size(src.size())
                                        .expect("NOT IMPLEMENTED YET :("),
                                }),
                            }),
                            new_instr(InstructionType::Ret),
                        ]
                    }
                    _ => unimplemented!(),
                }
            }
            tacky::Instruction::Unary { op, src, dst } => {
                if is_float(&src, symbols) && matches!(op, tacky::UnaryOp::Negate) {
                    let float_constant = StaticConstant::from(-0.0).with_alignment(16);
                    let neg_zero = float_constant.id();
                    // Super special 16-byte alignemnt needed here for SSE
                    float_constants.insert(float_constant);
                    let src = Operand::from_tacky(src, symbols, float_constants);
                    let dst = Operand::from_tacky(dst, symbols, float_constants);
                    return vec![
                        new_instr(InstructionType::Mov {
                            src: src.clone(),
                            dst: dst.clone(),
                        }),
                        new_instr(InstructionType::Binary {
                            op: BinaryOp::Xor,
                            src: Operand::Data {
                                name: neg_zero,
                                size: 16,
                                r#type: AssemblyType::Double,
                                is_const: true,
                            },
                            dst,
                        }),
                    ];
                }
                if is_float(&src, symbols) && matches!(op, tacky::UnaryOp::Not) {
                    let dst = Operand::from_tacky(dst, symbols, float_constants);
                    let xmm14 = Operand::Reg(Reg::Xmm {
                        reg: XmmReg::XMM14,
                        section: RegSection::Qword,
                    });
                    return vec![
                        new_instr(InstructionType::Binary {
                            op: BinaryOp::Xor,
                            src: xmm14.clone(),
                            dst: xmm14.clone(),
                        }),
                        new_instr(InstructionType::Cmp {
                            src: Operand::from_tacky(src, symbols, float_constants),
                            dst: xmm14,
                        }),
                        new_instr(InstructionType::Mov {
                            src: make_zero(dst.size(), false),
                            dst: dst.clone(),
                        }),
                        new_instr(InstructionType::SetCC {
                            cond_code: CondCode::E,
                            dst: dst.clone(),
                        }),
                        new_instr(InstructionType::CMovCC {
                            src: Operand::Imm(ast::Constant::I32(0)),
                            dst,
                            cond_code: CondCode::P,
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
                                    Operand::Pseudo { name, .. } => Operand::Pseudo {
                                        name,
                                        size: 1,
                                        r#type: AssemblyType::Byte,
                                    },
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
                    if is_float(&src1, symbols) {
                        let src1 = Operand::from_tacky(src1, symbols, float_constants);
                        let xmm14 = Operand::Reg(Reg::Xmm {
                            reg: XmmReg::XMM14,
                            section: RegSection::from_size(src1.size()).expect("FIXME"),
                        });
                        vec![
                            new_instr(InstructionType::Mov {
                                src: Operand::from_tacky(src2, symbols, float_constants),
                                dst: xmm14.clone(),
                            }),
                            new_instr(InstructionType::Binary {
                                op: BinaryOp::Multiply,
                                src: src1,
                                dst: xmm14.clone(),
                            }),
                            new_instr(InstructionType::Mov {
                                src: xmm14,
                                dst: Operand::from_tacky(dst, symbols, float_constants),
                            }),
                        ]
                    } else {
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
                            new_instr(InstructionType::Binary {
                                op: BinaryOp::DivDouble,
                                src: src2,
                                dst,
                            }),
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
                    assert!(
                        !(is_float(&src1, symbols) || is_float(&src2, symbols)),
                        "Remainder is not allowed on floats"
                    );
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
                op @ tacky::BinaryOp::Equal
                | op @ tacky::BinaryOp::NotEqual
                | op @ tacky::BinaryOp::LessThan
                | op @ tacky::BinaryOp::LessOrEqual
                | op @ tacky::BinaryOp::GreaterThan
                | op @ tacky::BinaryOp::GreaterOrEqual => {
                    // Unsigned integers and doubles both set the CF and ZF
                    // when doing comparisons
                    let float_cmp = is_float(&src1, symbols);
                    assert!(
                        float_cmp == is_float(&src2, symbols),
                        "Either both operators should be floats or neither should be floats."
                    );
                    let use_cf_zf_cmp = !is_signed(&src1, symbols) || is_float(&src2, symbols);
                    let mut instrs = vec![
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
                                let dst =
                                    Operand::from_tacky(dst.clone(), symbols, float_constants);
                                match dst {
                                    Operand::Pseudo { name, .. } => Operand::Pseudo {
                                        name,
                                        size: 1,
                                        r#type: AssemblyType::Byte,
                                    },
                                    _ => dst,
                                }
                            },
                        }),
                    ];
                    if float_cmp {
                        instrs.extend(vec![new_instr(InstructionType::CMovCC {
                            src: if op == tacky::BinaryOp::NotEqual {
                                Operand::Imm(ast::Constant::I32(1))
                            } else {
                                Operand::Imm(ast::Constant::I32(0))
                            },
                            dst: Operand::from_tacky(dst, symbols, float_constants),
                            cond_code: CondCode::P,
                        })]);
                    }
                    instrs
                }
                _ => unimplemented!(),
            },
            tacky::Instruction::JumpIfZero { condition, target } => {
                if is_float(&condition, symbols) {
                    let xmm0 = Reg::Xmm {
                        reg: XmmReg::XMM0,
                        section: RegSection::Qword,
                    };
                    vec![
                        new_instr(InstructionType::Binary {
                            op: BinaryOp::Xor,
                            src: Operand::Reg(xmm0),
                            dst: Operand::Reg(xmm0),
                        }),
                        new_instr(InstructionType::Cmp {
                            src: Operand::from_tacky(condition, symbols, float_constants),
                            dst: Operand::Reg(xmm0),
                        }),
                        new_instr(InstructionType::JmpCCRel {
                            cond_code: CondCode::P,
                            offset: 4,
                        }),
                        new_instr(InstructionType::JmpCC {
                            cond_code: CondCode::E,
                            identifier: target,
                        }),
                    ]
                } else {
                    vec![
                        new_instr(InstructionType::Cmp {
                            src: Operand::Imm(ast::Constant::I32(0)),
                            dst: Operand::from_tacky(condition, symbols, float_constants),
                        }),
                        new_instr(InstructionType::JmpCC {
                            cond_code: CondCode::E,
                            identifier: target,
                        }),
                    ]
                }
            }
            tacky::Instruction::JumpIfNotZero { condition, target } => {
                if is_float(&condition, symbols) {
                    let xmm0 = Reg::Xmm {
                        reg: XmmReg::XMM0,
                        section: RegSection::Qword,
                    };
                    vec![
                        new_instr(InstructionType::Binary {
                            op: BinaryOp::Xor,
                            src: Operand::Reg(xmm0),
                            dst: Operand::Reg(xmm0),
                        }),
                        new_instr(InstructionType::Cmp {
                            src: Operand::from_tacky(condition, symbols, float_constants),
                            dst: Operand::Reg(xmm0),
                        }),
                        new_instr(InstructionType::JmpCC {
                            cond_code: CondCode::P,
                            identifier: Rc::clone(&target),
                        }),
                        new_instr(InstructionType::JmpCC {
                            cond_code: CondCode::NE,
                            identifier: target,
                        }),
                    ]
                } else {
                    vec![
                        new_instr(InstructionType::Cmp {
                            src: Operand::Imm(ast::Constant::I32(0)),
                            dst: Operand::from_tacky(condition, symbols, float_constants),
                        }),
                        new_instr(InstructionType::JmpCC {
                            cond_code: CondCode::NE,
                            identifier: target,
                        }),
                    ]
                }
            }
            tacky::Instruction::Jump(label) => {
                vec![new_instr(InstructionType::Jmp(label))]
            }
            tacky::Instruction::Copy { src, dst } => vec![new_instr(InstructionType::Mov {
                src: Operand::from_tacky(src, symbols, float_constants),
                dst: Operand::from_tacky(dst, symbols, float_constants),
            })],
            tacky::Instruction::GetAddress { src, dst } => todo!(),
            tacky::Instruction::Load { src_ptr, dst } => {
                let dst = Operand::from_tacky(dst, symbols, float_constants);
                let src = Operand::from_tacky(src_ptr, symbols, float_constants);
                vec![
                    new_instr(InstructionType::Mov {
                        src,
                        dst: Operand::Reg(Reg::RAX),
                    }),
                    new_instr(InstructionType::Mov {
                        src: Operand::Memory {
                            reg: Reg::RAX,
                            offset: 0,
                            size: dst.size(),
                            r#type: AssemblyType::from(&dst),
                        },
                        dst,
                    }),
                ]
            }
            tacky::Instruction::Store { src, dst_ptr } => todo!(),
            tacky::Instruction::Label(label) => {
                vec![new_instr(InstructionType::Label(label))]
            }
            tacky::Instruction::FunCall { name, args, dst } => {
                let (gpr_args, fpr_args, stack_args) = classify_function_args(args, symbols);

                let num_stack_args = stack_args.len();
                let stack_padding = if num_stack_args % 2 == 1 { 8 } else { 0 };
                let mut v = vec![];

                if stack_padding != 0 {
                    v.push(new_instr(InstructionType::allocate_stack(stack_padding)));
                }

                // Setup all the GP and FP regs with arguments
                for (dst_reg, src_arg) in
                    std::iter::zip(SYSTEM_V_GP_REGS.iter(), gpr_args.into_iter()).chain(
                        std::iter::zip(SYSTEM_V_FP_REGS.iter(), fpr_args.into_iter()),
                    )
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
                    let arg = Operand::from_tacky(arg, symbols, float_constants);
                    match arg {
                        Operand::Imm(i) => {
                            v.push(new_instr(InstructionType::Push(Operand::Imm(i))))
                        }
                        // NOTE: If we go to push a non-64 bit register here,
                        // it will need to be rewritten in emission as pushing
                        // the full 64-bit register
                        Operand::Reg(r) => {
                            v.push(new_instr(InstructionType::Push(Operand::Reg(r))))
                        }
                        src @ Operand::Memory { .. }
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
                let dst_type = dst.get_type(symbols);
                let dst = Operand::from_tacky(dst, symbols, float_constants);

                // Determine how to get the return value into the destination
                match dst_type {
                    ast::Type {
                        base: ast::BaseType::Int { .. },
                        ..
                    } => {
                        let ax = Operand::Reg(Reg::X86 {
                            reg: X86Reg::Ax,
                            section: RegSection::from_size(dst.size())
                                .expect("NOT IMPLEMENTED YET :("),
                        });
                        v.push(new_instr(InstructionType::Mov { src: ax, dst }));
                    }
                    ast::Type {
                        base: ast::BaseType::Float(_) | ast::BaseType::Double(_),
                        ..
                    } => {
                        let xmm0 = Operand::Reg(Reg::Xmm {
                            reg: XmmReg::XMM0,
                            section: RegSection::from_size(dst.size())
                                .expect("NOT IMPLEMENTED YET :("),
                        });
                        v.push(new_instr(InstructionType::Mov { src: xmm0, dst }));
                    }
                    _ => unimplemented!(),
                }

                v
            }
            tacky::Instruction::ZeroExtend { src, dst } => {
                vec![new_instr(InstructionType::MovZeroExtend {
                    src: Operand::from_tacky(src, symbols, float_constants),
                    dst: Operand::from_tacky(dst, symbols, float_constants),
                })]
            }
            tacky::Instruction::DoubleToInt { src, dst } => {
                vec![new_instr(InstructionType::Cvttsd2si {
                    src: Operand::from_tacky(src, symbols, float_constants),
                    dst: Operand::from_tacky(dst, symbols, float_constants),
                })]
            }
            tacky::Instruction::IntToDouble { src, dst } => {
                vec![new_instr(InstructionType::Cvtsi2sd {
                    src: Operand::from_tacky(src, symbols, float_constants),
                    dst: Operand::from_tacky(dst, symbols, float_constants),
                })]
            }
            tacky::Instruction::DoubleToUInt { src, dst } => {
                // Check if the double is within the maximum range of a
                // signed long
                //  - True: Convert directly using the cvttsd2siq instruction
                //  - False:
                //      1. Subtract (LONG_MAX + 1) from value to get it in range
                //      2. Convert using cvttsd2siq instruction
                //      3. Add (LONG_MAX + 1) back to the value
                let float_constant = StaticConstant::from(StaticConstant::LONG_MAX_VAL);
                let long_max = float_constant.id();

                float_constants.insert(float_constant);
                let long_max = Operand::Data {
                    name: long_max,
                    size: core::mem::align_of::<f64>(),
                    r#type: AssemblyType::Double,
                    is_const: true,
                };

                let rax = Operand::Reg(Reg::X86 {
                    reg: X86Reg::Ax,
                    section: RegSection::Qword,
                });
                let eax = Operand::Reg(Reg::X86 {
                    reg: X86Reg::Ax,
                    section: RegSection::Dword,
                });

                let xmm14 = Operand::Reg(Reg::Xmm {
                    reg: XmmReg::XMM14,
                    section: RegSection::Qword,
                });

                let src = Operand::from_tacky(src, symbols, float_constants);
                let dst = Operand::from_tacky(dst, symbols, float_constants);

                // Fast path when we are just dealing with unsigned ints
                let dst_type = AssemblyType::from(&dst);
                if matches!(dst_type, AssemblyType::Longword) {
                    return vec![
                        new_instr(InstructionType::Cvttsd2si {
                            src: src.clone(),
                            dst: rax.clone(),
                        }),
                        new_instr(InstructionType::Mov { src: eax, dst }),
                    ];
                }

                let out_of_range_label = Rc::new(make_label("out_of_range".to_string()));
                let end_label = Rc::new(make_label("end".to_string()));

                vec![
                    // Let rewrites take care of this later and make sure
                    // the `dst` is in a register
                    new_instr(InstructionType::Cmp {
                        src: long_max.clone(),
                        dst: src.clone(),
                    }),
                    new_instr(InstructionType::JmpCC {
                        cond_code: CondCode::AE,
                        identifier: Rc::clone(&out_of_range_label),
                    }),
                    // Happy path: No truncation required
                    new_instr(InstructionType::Cvttsd2si {
                        src: src.clone(),
                        dst: dst.clone(),
                    }),
                    new_instr(InstructionType::Jmp(Rc::clone(&end_label))),
                    new_instr(InstructionType::Label(out_of_range_label)),
                    new_instr(InstructionType::Mov {
                        src,
                        dst: xmm14.clone(),
                    }),
                    new_instr(InstructionType::Binary {
                        op: BinaryOp::Subtract,
                        src: long_max,
                        dst: xmm14.clone(),
                    }),
                    new_instr(InstructionType::Cvttsd2si {
                        src: xmm14.clone(),
                        dst: dst.clone(),
                    }),
                    new_instr(InstructionType::Mov {
                        src: Operand::Imm(ast::Constant::U64(u64::MAX)),
                        dst: rax.clone(),
                    }),
                    new_instr(InstructionType::Binary {
                        op: BinaryOp::Add,
                        src: rax.clone(),
                        dst: dst.clone(),
                    }),
                    new_instr(InstructionType::Label(end_label)),
                ]
            }
            tacky::Instruction::UIntToDouble { src, dst } => {
                let rax = Operand::Reg(Reg::X86 {
                    reg: X86Reg::Ax,
                    section: RegSection::Qword,
                });
                let rdx = Operand::Reg(Reg::X86 {
                    reg: X86Reg::Dx,
                    section: RegSection::Qword,
                });

                let src = Operand::from_tacky(src, symbols, float_constants);
                let dst = Operand::from_tacky(dst, symbols, float_constants);

                // Fast path when we are just dealing with unsigned ints
                let src_type = AssemblyType::from(&src);
                if matches!(src_type, AssemblyType::Longword) {
                    return vec![
                        new_instr(InstructionType::MovZeroExtend {
                            src: src.clone(),
                            dst: Operand::Reg(Reg::X86 {
                                reg: X86Reg::Ax,
                                section: RegSection::Dword,
                            }),
                        }),
                        new_instr(InstructionType::Cvtsi2sd {
                            src: rax.clone(),
                            dst,
                        }),
                    ];
                }

                let out_of_range_label = Rc::new(make_label("out_of_range".to_string()));
                let end_label = Rc::new(make_label("end".to_string()));

                vec![
                    new_instr(InstructionType::Cmp {
                        src: make_zero(dst.size(), false),
                        dst: src.clone(),
                    }),
                    new_instr(InstructionType::JmpCC {
                        cond_code: CondCode::L,
                        identifier: Rc::clone(&out_of_range_label),
                    }),
                    // Explicitly zero out bytes here
                    new_instr(InstructionType::Cvtsi2sd {
                        src: src.clone(),
                        dst: dst.clone(),
                    }),
                    new_instr(InstructionType::Jmp(Rc::clone(&end_label))),
                    new_instr(InstructionType::Label(out_of_range_label)),
                    new_instr(InstructionType::Mov {
                        src: src.clone(),
                        dst: rax.clone(),
                    }),
                    new_instr(InstructionType::Mov {
                        src: rax.clone(),
                        dst: rdx.clone(),
                    }),
                    new_instr(InstructionType::Binary {
                        op: BinaryOp::Shr,
                        src: Operand::Imm(ast::Constant::U64(1)),
                        dst: rdx.clone(),
                    }),
                    new_instr(InstructionType::Binary {
                        op: BinaryOp::BitAnd,
                        src: Operand::Imm(ast::Constant::U64(1)),
                        dst: rax.clone(),
                    }),
                    new_instr(InstructionType::Binary {
                        op: BinaryOp::BitOr,
                        src: rax,
                        dst: rdx.clone(),
                    }),
                    new_instr(InstructionType::Cvtsi2sd {
                        src: rdx,
                        dst: dst.clone(),
                    }),
                    new_instr(InstructionType::Binary {
                        op: BinaryOp::Add,
                        src: dst.clone(),
                        dst: dst.clone(),
                    }),
                    new_instr(InstructionType::Label(end_label)),
                ]
            }
        }
    }
}

fn is_float(val: &tacky::Val, symbols: &tacky::SymbolTable) -> bool {
    matches!(
        val.get_type(symbols),
        ast::Type {
            base: ast::BaseType::Float(_) | ast::BaseType::Double(_),
            ..
        }
    )
}

fn is_signed(val: &tacky::Val, symbols: &tacky::SymbolTable) -> bool {
    match val.get_type(symbols) {
        ast::Type {
            base: ast::BaseType::Int { signed, .. },
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
    Pseudo {
        name: Rc<String>,
        size: usize,
        r#type: AssemblyType,
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
}

impl Operand {
    pub fn size(&self) -> usize {
        match self {
            Self::Imm(c) => c.get_type().size_of(),
            Self::Reg(r) => r.size(),
            Self::Pseudo { size, .. } => *size,
            Self::Memory { size, .. } => *size,
            Self::Data { size, .. } => *size,
        }
    }

    fn is_reg(&self) -> bool {
        matches!(self, Self::Reg(_))
    }

    fn is_imm(&self) -> bool {
        matches!(self, Self::Imm(_))
    }

    fn is_mem(&self) -> bool {
        !(self.is_reg() || self.is_imm())
    }

    fn from_tacky(
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
                Self::Pseudo {
                    name: Rc::clone(name),
                    size: val_type.size_bytes(),
                    r#type: val_type,
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
            Self::Pseudo { .. } => {
                unreachable!("Cannot create asm representation for a pseudoregister.")
            }
        }
    }
}
