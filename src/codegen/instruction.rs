use super::*;
use crate::{ast, sema, tacky};
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub(super) enum Initial {}

#[derive(Debug, PartialEq)]
pub(super) enum WithStorage {}

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
    Lea {
        // Must be a memory operand!
        src: Operand,
        dst: Operand,
    },
}

#[derive(Debug, PartialEq)]
pub struct Instruction<T> {
    pub op: InstructionType,
    pub(super) phantom: PhantomData<T>,
}

impl InstructionType {
    pub(super) fn allocate_stack(bytes: usize) -> Self {
        Self::Binary {
            op: BinaryOp::Subtract,
            src: Operand::Imm(ast::Constant::I64(bytes.try_into().expect("i64 == isize"))),
            dst: Operand::Reg(Reg::X86 {
                reg: X86Reg::Sp,
                section: RegSection::Qword,
            }),
        }
    }

    pub(super) fn deallocate_stack(bytes: usize) -> Self {
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

impl Instruction<Initial> {
    pub(super) fn new(op: InstructionType) -> Self {
        Self {
            op,
            phantom: PhantomData::<Initial>,
        }
    }

    pub(super) fn from_tacky(
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
                    }
                    | ast::Type {
                        base: ast::BaseType::Ptr { .. } | ast::BaseType::Array { .. },
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
                    _ => unimplemented!("{val_type:#?}"),
                }
            }
            tacky::Instruction::Unary { op, src, dst } => {
                if is_float(&src, symbols) && matches!(op, tacky::UnaryOp::Negate) {
                    let float_constant = StaticConstant::from(-0.0).with_alignment(16);
                    let neg_zero = float_constant.id();
                    // Super special 16-byte alignment needed here for SSE
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
            tacky::Instruction::GetAddress { src, dst } => {
                let src = Operand::from_tacky(src, symbols, float_constants);
                let dst = Operand::from_tacky(dst, symbols, float_constants);
                assert!(
                    matches!(
                        src,
                        Operand::Memory { .. } | Operand::Pseudo { .. } | Operand::PseudoMem { .. }
                    ),
                    "Can only call `lea` on memory operands! Found: {src:#?}"
                );
                vec![new_instr(InstructionType::Lea { src, dst })]
            }
            tacky::Instruction::Load { src_ptr, dst } => {
                let dst = Operand::from_tacky(dst, symbols, float_constants);
                let src = Operand::from_tacky(src_ptr, symbols, float_constants);
                vec![
                    new_instr(InstructionType::Mov {
                        src,
                        dst: Operand::Reg(RAX),
                    }),
                    new_instr(InstructionType::Mov {
                        src: Operand::Memory {
                            reg: RAX,
                            offset: 0,
                            size: dst.size(),
                            r#type: AssemblyType::from(&dst),
                        },
                        dst,
                    }),
                ]
            }
            tacky::Instruction::Store { src, dst_ptr } => {
                let dst = Operand::from_tacky(dst_ptr, symbols, float_constants);
                let src = Operand::from_tacky(src, symbols, float_constants);
                let src_type = AssemblyType::from(&src);
                let src_size = src.size();
                vec![
                    new_instr(InstructionType::Mov {
                        src: dst,
                        dst: Operand::Reg(RAX),
                    }),
                    new_instr(InstructionType::Mov {
                        src,
                        dst: Operand::Memory {
                            reg: RAX,
                            offset: 0,
                            size: src_size,
                            r#type: src_type,
                        },
                    }),
                ]
            }
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
                    let arg_sz = arg.get_type(symbols).size_of();
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
                        | src @ Operand::Data { .. }
                        | src @ Operand::Indexed { .. }
                        | src @ Operand::PseudoMem { .. } => {
                            // FIXME: This should really stop being hardcoded
                            if arg_sz == 8 {
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
                let dst_sz = dst_type.size_of();
                let dst = Operand::from_tacky(dst, symbols, float_constants);

                // Determine how to get the return value into the destination
                match dst_type {
                    ast::Type {
                        base: ast::BaseType::Int { .. },
                        ..
                    }
                    | ast::Type {
                        base: ast::BaseType::Ptr { .. },
                        ..
                    } => {
                        let ax = Operand::Reg(Reg::X86 {
                            reg: X86Reg::Ax,
                            section: RegSection::from_size(dst_sz).expect("NOT IMPLEMENTED YET :("),
                        });
                        v.push(new_instr(InstructionType::Mov { src: ax, dst }));
                    }
                    ast::Type {
                        base: ast::BaseType::Float(_) | ast::BaseType::Double(_),
                        ..
                    } => {
                        let xmm0 = Operand::Reg(Reg::Xmm {
                            reg: XmmReg::XMM0,
                            section: RegSection::from_size(dst_sz).expect("NOT IMPLEMENTED YET :("),
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
            // TODO: Allow for const evaluation here with `Memory` operand
            tacky::Instruction::AddPtr {
                ptr,
                index,
                scale,
                dst,
            } => {
                let (multiply_index_and_scale, scale_to_use) = match scale {
                    1 => (false, 1),
                    2 => (false, 2),
                    4 => (false, 4),
                    8 => (false, 8),
                    _ => (true, 1),
                };
                let mut instructions = vec![new_instr(InstructionType::Mov {
                    src: Operand::from_tacky(ptr, symbols, float_constants),
                    dst: Operand::Reg(RAX),
                })];
                if multiply_index_and_scale {
                    instructions.push(new_instr(InstructionType::Binary {
                        op: BinaryOp::Multiply,
                        src: Operand::Imm(ast::Constant::U64(scale.try_into().unwrap())),
                        dst: Operand::Reg(RAX),
                    }));
                };
                instructions.push(new_instr(InstructionType::Mov {
                    src: Operand::from_tacky(index, symbols, float_constants),
                    dst: Operand::Reg(RDX),
                }));
                instructions.push(new_instr(InstructionType::Lea {
                    src: Operand::Indexed {
                        base: RAX,
                        index: RDX,
                        scale: scale_to_use,
                    },
                    dst: Operand::from_tacky(dst, symbols, float_constants),
                }));
                instructions
            }
            tacky::Instruction::CopyToOffset { src, dst, offset } => {
                vec![new_instr(InstructionType::Mov {
                    src: Operand::from_tacky(src, symbols, float_constants),
                    dst: Operand::PseudoMem {
                        name: dst,
                        offset: offset.try_into().unwrap(),
                    },
                })]
            }
        }
    }
}

impl Instruction<WithStorage> {
    pub(super) fn new(
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
                                reg: RBP,
                                offset: -(*stack_bound as isize),
                                size,
                                r#type,
                            }
                        })
                        .clone(),
                }
            }
            Operand::PseudoMem { ref name, offset } => {
                match symbols.get(name) {
                    // 1. Check for static storage
                    Some(entry)
                        if matches!(entry.attribute, sema::tc::Attribute::Static { .. }) =>
                    {
                        Operand::Data {
                            name: Rc::clone(name),
                            size: entry.r#type.size_of(),
                            r#type: AssemblyType::ByteArray {
                                size: entry.r#type.size_of(),
                                alignment: std::cmp::min(
                                    entry.r#type.base.size_of_base_type(),
                                    MAX_AGGREGATE_ALIGNMENT,
                                ),
                            },
                            is_const: false,
                        }
                    }
                    // 2. If it is not static, put it on the stack
                    Some(entry) => mappings
                        .entry(Rc::clone(name))
                        .or_insert_with(|| {
                            let ast::Type {
                                base: ast::BaseType::Array { element, size },
                                ..
                            } = &entry.r#type
                            else {
                                unimplemented!("Add structs once we get here.");
                            };
                            let byte_array = AssemblyType::from_ast_type(entry.r#type.clone());
                            let element_t = AssemblyType::from_ast_type(*element.clone());
                            *stack_bound = align_up(*stack_bound, byte_array.alignment());
                            *stack_bound += size;
                            Operand::Memory {
                                reg: RBP,
                                offset: -(*stack_bound as isize) + offset as isize,
                                size: element.size_of(),
                                r#type: element_t,
                            }
                        })
                        .clone(),
                    _ => unreachable!(),
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
                InstructionType::Lea { src, dst } => InstructionType::Lea {
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

    pub(super) fn from_op(op: InstructionType) -> Self {
        Self {
            op,
            phantom: PhantomData::<WithStorage>,
        }
    }

    pub(super) fn fixup_stack_vars(self) -> Vec<Self> {
        match self.op {
            InstructionType::Mov { src, dst } => rewrite_move(
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
            } => rewrite_move(
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
            InstructionType::Movsx { src, dst } => rewrite_move(
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
            InstructionType::Binary { op, src, dst } => rewrite_move(
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
            InstructionType::Lea { src, dst } => rewrite_move(
                src,
                dst,
                RewriteRule::new(ImmRewrite::Ignore, MemRewrite::Default, true),
                RewriteRule::new(ImmRewrite::Error, MemRewrite::UseAndStore, false),
                |src, dst| Self::from_op(InstructionType::Lea { src, dst }),
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
            InstructionType::Cvtsi2sd { src, dst } => rewrite_move(
                src,
                dst,
                RewriteRule::new(ImmRewrite::Require, MemRewrite::Default, false),
                RewriteRule::new(ImmRewrite::Error, MemRewrite::StoreNoUse, true),
                |src, dst| Self::from_op(InstructionType::Cvtsi2sd { src, dst }),
            ),
            InstructionType::Cvttsd2si { src, dst } => rewrite_move(
                src,
                dst,
                RewriteRule::new(ImmRewrite::Require, MemRewrite::Default, false),
                RewriteRule::new(ImmRewrite::Error, MemRewrite::StoreNoUse, false),
                |src, dst| Self::from_op(InstructionType::Cvttsd2si { src, dst }),
            ),
            InstructionType::Cmp { src, dst } => rewrite_move(
                src,
                dst,
                RewriteRule::new(ImmRewrite::Ignore, MemRewrite::Default, true),
                RewriteRule::new(ImmRewrite::Require, MemRewrite::UseNoStore, false),
                |src, dst| Self::from_op(InstructionType::Cmp { src, dst }),
            ),
            instr => vec![Self::from_op(instr)],
        }
    }

    pub(super) fn fixup_immediates(self) -> Vec<Self> {
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
