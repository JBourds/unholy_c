use super::*;

pub(crate) fn parse_addr_of(
    node: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let ast::Expr::Unary {
        op: ast::UnaryOp::AddrOf,
        expr,
    } = node
    else {
        unreachable!();
    };
    let result_expr = Expr::parse_with(*expr, symbols, make_temp_var);
    match result_expr {
        ExprResult::PlainOperand(expr) => {
            let Expr {
                mut instructions,
                val,
            } = expr;
            let dst = Function::make_tacky_temp_var(
                ast::Type {
                    base: ast::BaseType::Ptr {
                        to: Box::new(val.get_type(symbols)),
                        is_restrict: false,
                    },
                    alignment: ast::Type::PTR_ALIGNMENT,
                    is_const: false,
                },
                symbols,
                make_temp_var,
            );
            instructions.push(Instruction::GetAddress {
                src: val,
                dst: dst.clone(),
            });
            ExprResult::PlainOperand(Expr {
                instructions,
                val: dst,
            })
        }
        ExprResult::DerefrencedPointer(expr) => ExprResult::PlainOperand(expr),
    }
}

pub(crate) fn parse_unary(
    node: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let ast::Expr::Unary { op, expr } = node else {
        unreachable!();
    };
    let (instructions, dst) = match op {
        ast::UnaryOp::PreInc => match Expr::parse_with(*expr, symbols, make_temp_var) {
            ExprResult::PlainOperand(Expr {
                mut instructions,
                val,
            }) => {
                instructions.push(Instruction::Binary {
                    op: BinaryOp::Add,
                    src1: val.clone(),
                    src2: Val::Constant(Expr::unary_inc_dec_val(&val.get_type(symbols))),
                    dst: val.clone(),
                });
                (instructions, val.clone())
            }
            ExprResult::DerefrencedPointer(Expr {
                mut instructions,
                val,
            }) => {
                let intermediate = Function::make_tacky_temp_var(
                    val.get_type(symbols).deref(),
                    symbols,
                    make_temp_var,
                );
                instructions.extend([
                    Instruction::Load {
                        src_ptr: val.clone(),
                        dst: intermediate.clone(),
                    },
                    Instruction::Binary {
                        op: BinaryOp::Add,
                        src1: intermediate.clone(),
                        src2: Val::Constant(
                            ast::Constant::const_from_type(&val.get_type(symbols).deref(), 1)
                                .expect("UnaryOp type has an ast::Constant equivalent"),
                        ),
                        dst: intermediate.clone(),
                    },
                    Instruction::Store {
                        src: intermediate.clone(),
                        dst_ptr: val,
                    },
                ]);
                (instructions, intermediate)
            }
        },
        ast::UnaryOp::PostInc => {
            match Expr::parse_with(*expr, symbols, make_temp_var) {
                ExprResult::PlainOperand(Expr {
                    mut instructions,
                    val,
                }) => {
                    let dst = Function::make_tacky_temp_var(
                        val.get_type(symbols),
                        symbols,
                        make_temp_var,
                    );
                    instructions.push(Instruction::Copy {
                        src: val.clone(),
                        dst: dst.clone(),
                    });
                    instructions.push(Instruction::Binary {
                        op: BinaryOp::Add,
                        src1: val.clone(),
                        src2: Val::Constant(Expr::unary_inc_dec_val(&val.get_type(symbols))),
                        dst: val.clone(),
                    });
                    (instructions, dst)
                }
                ExprResult::DerefrencedPointer(Expr {
                    mut instructions,
                    val,
                }) => {
                    let typ = val.get_type(symbols).deref();
                    let dst = Function::make_tacky_temp_var(typ.clone(), symbols, make_temp_var);
                    let intermediate = Function::make_tacky_temp_var(typ, symbols, make_temp_var);
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
                            src2: Val::Constant(
                                ast::Constant::const_from_type(&val.get_type(symbols).deref(), 1)
                                    .expect("UnaryOp type has an ast::Constant equivalent"),
                            ),
                            dst: intermediate.clone(),
                        },
                        Instruction::Store {
                            src: intermediate,
                            dst_ptr: val,
                        },
                    ]);
                    (instructions, dst)
                }
            }
        }
        ast::UnaryOp::PreDec => match Expr::parse_with(*expr, symbols, make_temp_var) {
            ExprResult::PlainOperand(Expr {
                mut instructions,
                val,
            }) => {
                instructions.push(Instruction::Binary {
                    op: BinaryOp::Subtract,
                    src1: val.clone(),
                    src2: Val::Constant(Expr::unary_inc_dec_val(&val.get_type(symbols))),
                    dst: val.clone(),
                });
                (instructions, val.clone())
            }
            ExprResult::DerefrencedPointer(Expr {
                mut instructions,
                val,
            }) => {
                let intermediate = Function::make_tacky_temp_var(
                    val.get_type(symbols).deref(),
                    symbols,
                    make_temp_var,
                );
                instructions.extend([
                    Instruction::Load {
                        src_ptr: val.clone(),
                        dst: intermediate.clone(),
                    },
                    Instruction::Binary {
                        op: BinaryOp::Subtract,
                        src1: intermediate.clone(),
                        src2: Val::Constant(
                            ast::Constant::const_from_type(&val.get_type(symbols).deref(), 1)
                                .expect("UnaryOp type has an ast::Constant equivalent"),
                        ),
                        dst: intermediate.clone(),
                    },
                    Instruction::Store {
                        src: intermediate.clone(),
                        dst_ptr: val,
                    },
                ]);
                (instructions, intermediate)
            }
        },
        ast::UnaryOp::PostDec => {
            match Expr::parse_with(*expr, symbols, make_temp_var) {
                ExprResult::PlainOperand(Expr {
                    mut instructions,
                    val,
                }) => {
                    let dst = Function::make_tacky_temp_var(
                        val.get_type(symbols),
                        symbols,
                        make_temp_var,
                    );
                    instructions.push(Instruction::Copy {
                        src: val.clone(),
                        dst: dst.clone(),
                    });
                    instructions.push(Instruction::Binary {
                        op: BinaryOp::Subtract,
                        src1: val.clone(),
                        src2: Val::Constant(Expr::unary_inc_dec_val(&val.get_type(symbols))),
                        dst: val.clone(),
                    });
                    (instructions, dst)
                }
                ExprResult::DerefrencedPointer(Expr {
                    mut instructions,
                    val,
                }) => {
                    let typ = val.get_type(symbols).deref();
                    let dst = Function::make_tacky_temp_var(typ.clone(), symbols, make_temp_var);
                    let intermediate = Function::make_tacky_temp_var(typ, symbols, make_temp_var);
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
                            src2: Val::Constant(
                                ast::Constant::const_from_type(&val.get_type(symbols).deref(), 1)
                                    .expect("UnaryOp type has an ast::Constant equivalent"),
                            ),
                            dst: intermediate.clone(),
                        },
                        Instruction::Store {
                            src: intermediate,
                            dst_ptr: val,
                        },
                    ]);
                    (instructions, dst)
                }
            }
        }
        ast::UnaryOp::Not => {
            let Expr {
                mut instructions,
                val,
            } = Expr::parse_with_and_convert(*expr, symbols, make_temp_var);
            let dst =
                Function::make_tacky_temp_var(ast::Type::int(4, None), symbols, make_temp_var);
            instructions.push(Instruction::Unary {
                op: UnaryOp::from(op),
                src: val,
                dst: dst.clone(),
            });
            (instructions, dst)
        }
        // Other operations have tacky unary op equivalents
        _ => {
            let Expr {
                mut instructions,
                val,
            } = Expr::parse_with_and_convert(*expr, symbols, make_temp_var);
            let dst = Function::make_tacky_temp_var(val.get_type(symbols), symbols, make_temp_var);
            instructions.push(Instruction::Unary {
                op: UnaryOp::from(op),
                src: val,
                dst: dst.clone(),
            });
            (instructions, dst)
        }
    };
    ExprResult::PlainOperand(Expr {
        instructions,
        val: dst,
    })
}
