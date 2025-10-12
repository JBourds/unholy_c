use super::{
    AssemblyType, Final, Initial, Instruction, InstructionType, Operand, RBP, Reg, RegSection,
    SYSTEM_V_FP_REGS, SYSTEM_V_GP_REGS, StaticConstant, WithStorage, X86Reg,
};
use crate::tacky;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Rc<String>,
    pub global: bool,
    pub instructions: Vec<Instruction<Final>>,
}

/// Classify a vector of tacky arguments into three lists:
///     1. Args passed via general purpose registers
///     2. Args passed via floating-point registers
///     3. Args passed via the stack
/// Returns: (GPR args, FPR args, Stack args)
pub(super) fn classify_function_args(
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

impl Function {
    pub(super) fn from_with_storage(
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
                    reg: RBP,
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
