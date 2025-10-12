use super::Operand;

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
