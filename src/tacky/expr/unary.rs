use crate::ast::Type;

use super::*;

pub(crate) fn parse_unary(node: ast::Expr, ctx: &mut Ctx) -> ExprResult {
    let ast::Expr::Unary { op, expr } = node else {
        unreachable!();
    };
    match op {
        ast::UnaryOp::AddrOf => addr_of(*expr, ctx),
        ast::UnaryOp::PreInc => pre_inc(*expr, ctx),
        ast::UnaryOp::PostInc => post_inc(*expr, ctx),
        ast::UnaryOp::PreDec => pre_dec(*expr, ctx),
        ast::UnaryOp::PostDec => post_dec(*expr, ctx),
        ast::UnaryOp::Not => not(*expr, ctx),
        // Other operations have tacky unary op equivalents
        _ => {
            let Expr {
                mut instructions,
                val,
            } = Expr::parse_with_and_convert(*expr, ctx);
            let dst = ctx.make_temp_var(val.get_type(&ctx.symbols));
            instructions.push(Instruction::Unary {
                op: UnaryOp::from(op),
                src: val,
                dst: dst.clone(),
            });
            ExprResult::PlainOperand(Expr {
                instructions,
                val: dst,
            })
        }
    }
}

fn pre_inc(expr: ast::Expr, ctx: &mut Ctx) -> ExprResult {
    match Expr::parse_with(expr, ctx) {
        ExprResult::PlainOperand(Expr {
            mut instructions,
            val,
        }) => {
            instructions.push(Instruction::Binary {
                op: BinaryOp::Add,
                src1: val.clone(),
                src2: Val::Constant(Expr::unary_inc_dec_val(&val.get_type(&ctx.symbols))),
                dst: val.clone(),
            });
            ExprResult::PlainOperand(Expr { instructions, val })
        }
        ExprResult::DereferencedPointer(Expr {
            mut instructions,
            val,
        }) => {
            let t = val.get_type(&ctx.symbols).deref();
            let inc_val = Val::Constant(Expr::unary_inc_dec_val(&t));
            let intermediate = ctx.make_temp_var(t);
            instructions.extend([
                Instruction::Load {
                    src_ptr: val.clone(),
                    dst: intermediate.clone(),
                },
                Instruction::Binary {
                    op: BinaryOp::Add,
                    src1: intermediate.clone(),
                    src2: inc_val,
                    dst: intermediate.clone(),
                },
                Instruction::Store {
                    src: intermediate.clone(),
                    dst_ptr: val,
                },
            ]);
            ExprResult::PlainOperand(Expr {
                instructions,
                val: intermediate,
            })
        }
        _ => unreachable!("SubObject should have already been converted"),
    }
}

fn post_inc(expr: ast::Expr, ctx: &mut Ctx) -> ExprResult {
    match Expr::parse_with(expr, ctx) {
        ExprResult::PlainOperand(Expr {
            mut instructions,
            val,
        }) => {
            let t = val.get_type(&ctx.symbols);
            let inc_val = Val::Constant(Expr::unary_inc_dec_val(&t));
            let dst = ctx.make_temp_var(t);
            instructions.push(Instruction::Copy {
                src: val.clone(),
                dst: dst.clone(),
            });
            instructions.push(Instruction::Binary {
                op: BinaryOp::Add,
                src1: val.clone(),
                src2: inc_val,
                dst: val.clone(),
            });
            ExprResult::PlainOperand(Expr {
                instructions,
                val: dst,
            })
        }
        ExprResult::DereferencedPointer(Expr {
            mut instructions,
            val,
        }) => {
            let typ = val.get_type(&ctx.symbols).deref();
            let inc_val = Val::Constant(Expr::unary_inc_dec_val(&typ));
            let dst = ctx.make_temp_var(typ.clone());
            let intermediate = ctx.make_temp_var(typ);
            instructions.extend([
                Instruction::Load {
                    src_ptr: val.clone(),
                    dst: intermediate.clone(),
                },
                // Save this to return
                Instruction::Copy {
                    src: intermediate.clone(),
                    dst: dst.clone(),
                },
                Instruction::Binary {
                    op: BinaryOp::Add,
                    src1: intermediate.clone(),
                    src2: inc_val,
                    dst: intermediate.clone(),
                },
                Instruction::Store {
                    src: intermediate,
                    dst_ptr: val,
                },
            ]);
            ExprResult::PlainOperand(Expr {
                instructions,
                val: dst,
            })
        }
        _ => unreachable!("SubObject should have already been converted"),
    }
}

fn pre_dec(expr: ast::Expr, ctx: &mut Ctx) -> ExprResult {
    match Expr::parse_with(expr, ctx) {
        ExprResult::PlainOperand(Expr {
            mut instructions,
            val,
        }) => {
            instructions.push(Instruction::Binary {
                op: BinaryOp::Subtract,
                src1: val.clone(),
                src2: Val::Constant(Expr::unary_inc_dec_val(&val.get_type(&ctx.symbols))),
                dst: val.clone(),
            });
            ExprResult::PlainOperand(Expr { instructions, val })
        }
        ExprResult::DereferencedPointer(Expr {
            mut instructions,
            val,
        }) => {
            let t = val.get_type(&ctx.symbols).deref();
            let dec_val = Val::Constant(Expr::unary_inc_dec_val(&t));
            let intermediate = ctx.make_temp_var(t);
            instructions.extend([
                Instruction::Load {
                    src_ptr: val.clone(),
                    dst: intermediate.clone(),
                },
                Instruction::Binary {
                    op: BinaryOp::Subtract,
                    src1: intermediate.clone(),
                    src2: dec_val,
                    dst: intermediate.clone(),
                },
                Instruction::Store {
                    src: intermediate.clone(),
                    dst_ptr: val,
                },
            ]);
            ExprResult::PlainOperand(Expr {
                instructions,
                val: intermediate,
            })
        }
        _ => unreachable!("SubObject should have already been converted"),
    }
}

fn post_dec(expr: ast::Expr, ctx: &mut Ctx) -> ExprResult {
    match Expr::parse_with(expr, ctx) {
        ExprResult::PlainOperand(Expr {
            mut instructions,
            val,
        }) => {
            let t = val.get_type(&ctx.symbols);
            let dec_val = Val::Constant(Expr::unary_inc_dec_val(&t));
            let dst = ctx.make_temp_var(t);
            instructions.push(Instruction::Copy {
                src: val.clone(),
                dst: dst.clone(),
            });
            instructions.push(Instruction::Binary {
                op: BinaryOp::Subtract,
                src1: val.clone(),
                src2: dec_val,
                dst: val.clone(),
            });
            ExprResult::PlainOperand(Expr {
                instructions,
                val: dst,
            })
        }
        ExprResult::DereferencedPointer(Expr {
            mut instructions,
            val,
        }) => {
            let typ = val.get_type(&ctx.symbols).deref();
            let dec_val = Val::Constant(Expr::unary_inc_dec_val(&typ));
            let dst = ctx.make_temp_var(typ.clone());
            let intermediate = ctx.make_temp_var(typ);
            instructions.extend([
                Instruction::Load {
                    src_ptr: val.clone(),
                    dst: intermediate.clone(),
                },
                // Save this to return
                Instruction::Copy {
                    src: intermediate.clone(),
                    dst: dst.clone(),
                },
                Instruction::Binary {
                    op: BinaryOp::Subtract,
                    src1: intermediate.clone(),
                    src2: dec_val,
                    dst: intermediate.clone(),
                },
                Instruction::Store {
                    src: intermediate,
                    dst_ptr: val,
                },
            ]);
            ExprResult::PlainOperand(Expr {
                instructions,
                val: dst,
            })
        }
        _ => unreachable!("SubObject should have already been converted"),
    }
}

fn not(expr: ast::Expr, ctx: &mut Ctx) -> ExprResult {
    let Expr {
        mut instructions,
        val,
    } = Expr::parse_with_and_convert(expr, ctx);
    let dst = ctx.make_temp_var(ast::Type::int(4, None));
    instructions.push(Instruction::Unary {
        op: UnaryOp::Not,
        src: val,
        dst: dst.clone(),
    });
    ExprResult::PlainOperand(Expr {
        instructions,
        val: dst,
    })
}

fn addr_of(expr: ast::Expr, ctx: &mut Ctx) -> ExprResult {
    let result_expr = Expr::parse_with(expr, ctx);
    match result_expr {
        ExprResult::PlainOperand(expr) => {
            let Expr {
                mut instructions,
                val,
            } = expr;
            // array
            let val_t = val.get_type(&ctx.symbols);
            let t = if val_t.is_array() {
                val_t.maybe_decay()
            } else {
                ast::Type {
                    base: ast::BaseType::Ptr {
                        to: Box::new(val_t),
                        is_restrict: false,
                    },
                    alignment: ast::Type::PTR_ALIGNMENT,
                    is_const: false,
                }
            };
            let dst = ctx.make_temp_var(t);
            instructions.push(Instruction::GetAddress {
                src: val,
                dst: dst.clone(),
            });
            ExprResult::PlainOperand(Expr {
                instructions,
                val: dst,
            })
        }
        ExprResult::DereferencedPointer(Expr {
            mut instructions,
            val,
        }) => {
            if let Val::Var(ref name) = val
                && let Some(ptr) = ctx.symbols.get(name)
            {
                // Taking the address of a dereferenced pointer yields the
                // pointer itself. If the pointee is an array it decays to a
                // pointer-to-element; otherwise the result keeps the original
                // pointer type.
                let pointee = ptr.r#type.clone().deref();
                let dereferenced = if pointee.is_array() {
                    pointee.maybe_decay()
                } else {
                    ptr.r#type.clone()
                };
                let tmp = ctx.make_temp_var(dereferenced);
                instructions.push(Instruction::Copy {
                    src: val,
                    dst: tmp.clone(),
                });
                ExprResult::PlainOperand(Expr {
                    instructions,
                    val: tmp,
                })
            } else {
                unreachable!("cannot have constant expression for dereferenced pointer.");
            }
        }
        ExprResult::SubObject { base, offset } => {
            let base_type = ctx.get_var_type(&base);
            let struct_tag = base_type.assert_struct_get_tag();
            let member_type = ctx.get_struct_member_type_by_offset(&struct_tag, offset);
            let ptr_member_type = Type::pointer(Box::new(member_type));
            let dst = ctx.make_temp_var(ptr_member_type);
            let instructions = vec![
                Instruction::GetAddress {
                    src: Val::Var(base),
                    dst: dst.clone(),
                },
                Instruction::AddPtr {
                    ptr: dst.clone(),
                    index: Val::Constant(ast::Constant::I64(offset.try_into().unwrap())),
                    scale: 1,
                    dst: dst.clone(),
                },
            ];
            ExprResult::PlainOperand(Expr {
                instructions,
                val: dst,
            })
        }
    }
}
