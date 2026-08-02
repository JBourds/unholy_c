use crate::tacky;

use super::*;

pub(crate) fn parse_assignment(node: ast::Expr, ctx: &mut Ctx) -> ExprResult {
    let ast::Expr::Assignment { lvalue, rvalue } = node else {
        unreachable!();
    };
    if rvalue.has_compound() {
        parse_compound_assignment(*lvalue, *rvalue, ctx)
    } else {
        parse_normal_assignment(*lvalue, *rvalue, ctx)
    }
}

fn parse_normal_assignment(lvalue: ast::Expr, rvalue: ast::Expr, ctx: &mut Ctx) -> ExprResult {
    let lval = Expr::parse_with(lvalue, ctx);
    let rval = Expr::parse_with_and_convert(rvalue.clone(), ctx);
    match lval {
        ExprResult::PlainOperand(Expr {
            mut instructions,
            val,
        }) => {
            let t = val.get_type(&ctx.symbols);
            match t.base {
                ast::BaseType::Array { element, size }
                    if matches!(rvalue, ast::Expr::String { .. }) && element.is_char() =>
                {
                    let ast::Expr::String { value } = rvalue else {
                        unreachable!()
                    };
                    let tacky::Val::Var(dst) = val.clone() else {
                        unreachable!()
                    };
                    let instructions: Vec<_> =
                        mov_chunker::MovChunker::new(value.as_bytes(), dst, size, 0).collect();
                    ExprResult::PlainOperand(Expr { instructions, val })
                }
                _ => {
                    instructions.extend(rval.instructions);
                    instructions.push(Instruction::Copy {
                        src: rval.val,
                        dst: val.clone(),
                    });
                    ExprResult::PlainOperand(Expr { instructions, val })
                }
            }
        }
        ExprResult::DereferencedPointer(Expr {
            mut instructions,
            val,
        }) => {
            instructions.extend(rval.instructions);
            instructions.push(Instruction::Store {
                src: rval.val.clone(),
                dst_ptr: val.clone(),
            });

            ExprResult::PlainOperand(Expr {
                instructions,
                val: rval.val,
            })
        }
        ExprResult::SubObject { base, offset } => {
            let Expr {
                mut instructions,
                val: rval,
            } = rval;
            instructions.push(Instruction::CopyToOffset {
                src: rval.clone(),
                dst: base,
                offset: offset.try_into().unwrap(),
            });
            ExprResult::PlainOperand(Expr {
                instructions,
                val: rval,
            })
        }
    }
}

// left shifts and right shifts don't require casts, everything else (minus
// pointers) does though
fn handle_upcast(op: ast::BinaryOp, val: &Val, arg_type: ast::Type, ctx: &mut Ctx) -> Expr {
    if matches!(op, ast::BinaryOp::LShift | ast::BinaryOp::RShift) {
        // Shifts don't convert the LHS to the RHS/common type, but the LHS
        // still undergoes integer promotion (char -> int).
        if val.get_type(&ctx.symbols).is_char() {
            Expr::cast(val.clone(), ast::Type::int(4, None), ctx)
        } else {
            Expr {
                instructions: vec![],
                val: val.clone(),
            }
        }
    } else {
        Expr::cast(val.clone(), arg_type, ctx)
    }
}

/// need to have a special function for handling compound assignment to avoid
/// evaluating the LHS more than once
fn parse_compound_assignment(lvalue: ast::Expr, rvalue: ast::Expr, ctx: &mut Ctx) -> ExprResult {
    let ast::Expr::Binary { op, left, right } = rvalue else {
        unreachable!();
    };
    let Some(op) = op.compound_op() else {
        unreachable!();
    };

    let lval = Expr::parse_with(lvalue, ctx);
    let rval = Expr::parse_with_and_convert(*right, ctx);
    match lval {
        ExprResult::PlainOperand(Expr {
            mut instructions,
            val,
        }) => {
            instructions.extend(rval.instructions);
            let lhs_type = val.get_type(&ctx.symbols);
            // binary operations on pointers need to be handled specially
            let res = if lhs_type.is_pointer() {
                let (new_instructions, dst) =
                    Expr::do_pointer_arithmetic(op, val.clone(), rval.val, ctx);
                instructions.extend(new_instructions);
                dst
            } else {
                let arg_type = rval.val.get_type(&ctx.symbols);
                let upcasted = handle_upcast(op, &val, arg_type, ctx);
                instructions.extend(upcasted.instructions);
                instructions.push(Instruction::Binary {
                    op: op.into(),
                    src1: upcasted.val.clone(),
                    src2: rval.val.clone(),
                    dst: upcasted.val.clone(),
                });
                let dst_type = val.get_type(&ctx.symbols);
                let downcasted = Expr::cast(upcasted.val.clone(), dst_type, ctx);
                instructions.extend(downcasted.instructions);
                downcasted.val
            };
            instructions.push(Instruction::Copy {
                src: res,
                dst: val.clone(),
            });
            ExprResult::PlainOperand(Expr { instructions, val })
        }
        ExprResult::DereferencedPointer(Expr {
            mut instructions,
            val,
        }) => {
            instructions.extend(rval.instructions);
            let dst_type = val.get_type(&ctx.symbols).deref();
            let intermediate = ctx.make_temp_var(dst_type.clone());
            instructions.push(Instruction::Load {
                src_ptr: val.clone(),
                dst: intermediate.clone(),
            });
            let binary_lhs = if let ast::Expr::Cast { target, exp: _ } = *left {
                handle_upcast(op, &intermediate, target, ctx)
            } else {
                Expr {
                    instructions: vec![],
                    val: intermediate.clone(),
                }
            };
            instructions.extend(binary_lhs.instructions);
            let arg_type = binary_lhs.val.get_type(&ctx.symbols);
            let dst = if arg_type.is_pointer() {
                let (new_instructions, dst) =
                    Expr::do_pointer_arithmetic(op, binary_lhs.val.clone(), rval.val, ctx);
                instructions.extend(new_instructions);
                dst
            } else {
                instructions.push(Instruction::Binary {
                    op: op.into(),
                    src1: binary_lhs.val.clone(),
                    src2: rval.val.clone(),
                    dst: binary_lhs.val.clone(),
                });
                binary_lhs.val
            };
            let downcasted = Expr::cast(dst.clone(), dst_type, ctx);
            instructions.extend(downcasted.instructions);
            instructions.push(Instruction::Store {
                src: downcasted.val.clone(),
                dst_ptr: val,
            });

            ExprResult::PlainOperand(Expr {
                instructions,
                val: downcasted.val,
            })
        }
    }
}
