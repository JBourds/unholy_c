use super::*;

/// Struct encapsulating the rules for performing rewrites
/// * `imm_rule` - Rule for when the operand is an immedate.
/// * `mem_rule` - Rule for when the operand is a memory address.
/// * `use_other_op_size` - Flag for whether the rewrite register should use
///   its respective size or the other operand's size. (e.g., `Mov` will only
///   use the `dst` size, but `Movsx` each uses their own.
pub(super) struct RewriteRule {
    pub(super) imm_rule: ImmRewrite,
    pub(super) mem_rule: MemRewrite,
    pub(super) use_other_op_size: bool,
}

pub(super) enum ImmRewrite {
    Ignore,
    Require,
    Error,
}

pub(super) enum MemRewrite {
    Ignore,
    Default,
    UseAndStore,
    UseNoStore,
    StoreNoUse,
    #[allow(dead_code)]
    Error,
}

impl ImmRewrite {
    pub(super) fn requires_rewrite(&self, arg: &Operand) -> bool {
        assert!(
            !(matches!(self, Self::Error) && arg.is_imm()),
            "Rewrite rule prohibits the use of an immediate for argument, but found {arg:?}."
        );
        matches!(self, Self::Require) && arg.is_imm()
    }
}

impl RewriteRule {
    pub(super) fn new(imm_rule: ImmRewrite, mem_rule: MemRewrite, use_other_op_size: bool) -> Self {
        Self {
            imm_rule,
            mem_rule,
            use_other_op_size,
        }
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
pub(super) fn rewrite_move(
    src: Operand,
    dst: Operand,
    src_rewrites: RewriteRule,
    dst_rewrites: RewriteRule,
    make_op: impl Fn(Operand, Operand) -> Instruction<WithStorage>,
) -> Vec<Instruction<WithStorage>> {
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

    // Step 1. Figure out what register should be used for the rewrite
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

    // Step 2. Rewrite immediate values which are not allowed to be
    // immediates for `src` and `dst` and make sure what remains in `src`
    // will have the correct value regardless of if it is rewritten.
    let src = if src_rewrites.imm_rule.requires_rewrite(&src) {
        instrs.push(Instruction::from_op(InstructionType::Mov {
            src,
            dst: src_rewrite_reg.clone(),
        }));
        src_rewrite_reg.clone()
    } else {
        src
    };

    let dst = if dst_rewrites.imm_rule.requires_rewrite(&dst) {
        instrs.push(Instruction::from_op(InstructionType::Mov {
            src: dst,
            dst: dst_rewrite_reg.clone(),
        }));
        dst_rewrite_reg.clone()
    } else {
        dst
    };

    // Step 3. Rewrite memory address values where they are not allowed

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
            instrs.push(Instruction::from_op(InstructionType::Mov {
                src: dst.clone(),
                dst: dst_rewrite_reg.clone(),
            }));
            instrs.push(make_op(src.clone(), dst_rewrite_reg.clone()));
        }
        MemRewrite::StoreNoUse => {
            // E.g., When converting int to double the `dst` operand is just
            // where result needs to end up
            instrs.push(make_op(src.clone(), dst_rewrite_reg.clone()));
            instrs.push(Instruction::from_op(InstructionType::Mov {
                src: dst_rewrite_reg.clone(),
                dst: dst.clone(),
            }));
        }
        MemRewrite::UseAndStore => {
            instrs.push(Instruction::from_op(InstructionType::Mov {
                src: dst.clone(),
                dst: dst_rewrite_reg.clone(),
            }));
            instrs.push(make_op(src.clone(), dst_rewrite_reg.clone()));
            instrs.push(Instruction::from_op(InstructionType::Mov {
                src: dst_rewrite_reg.clone(),
                dst: dst.clone(),
            }));
        }
        MemRewrite::Default | MemRewrite::Ignore => {
            // No instruction can use two memory addresses.
            // This case means a memory address is valid in the second
            // argument, but that we just can't have both.
            if src.is_mem() && dst.is_mem() {
                instrs.push(Instruction::from_op(InstructionType::Mov {
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
