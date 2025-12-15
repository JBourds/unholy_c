use super::*;

pub(crate) fn parse_assignment(
    node: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let ast::Expr::Assignment { lvalue, rvalue } = node else {
        unreachable!();
    };
    if rvalue.has_compound() {
        parse_compound_assignment(*lvalue, *rvalue, symbols, make_temp_var)
    } else {
        parse_normal_assignment(*lvalue, *rvalue, symbols, make_temp_var)
    }
}

fn parse_normal_assignment(
    lvalue: ast::Expr,
    rvalue: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let lval = Expr::parse_with(lvalue, symbols, make_temp_var);
    let rval = Expr::parse_with_and_convert(rvalue, symbols, make_temp_var);
    match lval {
        ExprResult::PlainOperand(Expr {
            mut instructions,
            val,
        }) => {
            instructions.extend(rval.instructions);

            instructions.push(Instruction::Copy {
                src: rval.val,
                dst: val.clone(),
            });
            ExprResult::PlainOperand(Expr { instructions, val })
        }
        ExprResult::DerefrencedPointer(Expr {
            mut instructions,
            val,
        }) => {
            instructions.extend(rval.instructions);

            instructions.push(Instruction::Store {
                src: rval.val.clone(),
                dst_ptr: val,
            });

            ExprResult::PlainOperand(Expr {
                instructions,
                val: rval.val,
            })
        }
    }
}

fn parse_compound_assignment(
    lvalue: ast::Expr,
    rvalue: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let ast::Expr::Binary { op, left, right } = rvalue else {
        unreachable!();
    };
    let Some(op) = op.compound_op() else {
        unreachable!();
    };
    let lval = Expr::parse_with(lvalue, symbols, make_temp_var);
    let rval = Expr::parse_with_and_convert(*right, symbols, make_temp_var);
    match lval {
        ExprResult::PlainOperand(Expr {
            mut instructions,
            val,
        }) => {
            instructions.extend(rval.instructions);
            let arg_type = rval.val.get_type(symbols);
            let upcasted = if matches!(op, ast::BinaryOp::LShift | ast::BinaryOp::RShift) {
                Expr {
                    instructions: vec![],
                    val: val.clone(),
                }
            } else {
                Expr::cast(val.clone(), arg_type, symbols, make_temp_var)
            };
            instructions.extend(upcasted.instructions);
            instructions.push(Instruction::Binary {
                op: op.into(),
                src1: upcasted.val.clone(),
                src2: rval.val.clone(),
                dst: upcasted.val.clone(),
            });
            let dst_type = val.get_type(symbols);
            let downcasted = Expr::cast(upcasted.val.clone(), dst_type, symbols, make_temp_var);
            instructions.extend(downcasted.instructions);
            instructions.push(Instruction::Copy {
                src: downcasted.val.clone(),
                dst: val,
            });
            ExprResult::PlainOperand(Expr {
                instructions,
                val: downcasted.val,
            })
        }
        ExprResult::DerefrencedPointer(Expr {
            mut instructions,
            val,
        }) => {
            instructions.extend(rval.instructions);
            let intermediate = Function::make_tacky_temp_var(
                val.get_type(symbols).deref(),
                symbols,
                make_temp_var,
            );
            let binary_lhs = if let ast::Expr::Cast { target, exp: _ } = *left {
                if matches!(op, ast::BinaryOp::LShift | ast::BinaryOp::RShift) {
                    Expr {
                        instructions: vec![],
                        val: intermediate.clone(),
                    }
                } else {
                    Expr::cast(intermediate.clone(), target, symbols, make_temp_var)
                }
            } else {
                Expr {
                    instructions: vec![],
                    val: intermediate.clone(),
                }
            };
            instructions.extend(binary_lhs.instructions);
            instructions.push(Instruction::Load {
                src_ptr: val.clone(),
                dst: binary_lhs.val.clone(),
            });
            instructions.push(Instruction::Binary {
                op: op.into(),
                src1: binary_lhs.val.clone(),
                src2: rval.val.clone(),
                dst: binary_lhs.val.clone(),
            });
            let dst_type = val.get_type(symbols).deref();
            let downcasted = Expr::cast(binary_lhs.val.clone(), dst_type, symbols, make_temp_var);
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
