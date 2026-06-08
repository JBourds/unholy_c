use super::*;

pub(crate) fn parse_unary(
    node: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let ast::Expr::Unary { op, expr } = node else {
        unreachable!();
    };
    match op {
        ast::UnaryOp::AddrOf => addr_of(*expr, symbols, make_temp_var),
        ast::UnaryOp::PreInc => pre_inc(*expr, symbols, make_temp_var),
        ast::UnaryOp::PostInc => post_inc(*expr, symbols, make_temp_var),
        ast::UnaryOp::PreDec => pre_dec(*expr, symbols, make_temp_var),
        ast::UnaryOp::PostDec => post_dec(*expr, symbols, make_temp_var),
        ast::UnaryOp::Not => not(*expr, symbols, make_temp_var),
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
            ExprResult::PlainOperand(Expr {
                instructions,
                val: dst,
            })
        }
    }
}

fn pre_inc(
    expr: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    match Expr::parse_with(expr, symbols, make_temp_var) {
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
            ExprResult::PlainOperand(Expr { instructions, val })
        }
        ExprResult::DereferencedPointer(Expr {
            mut instructions,
            val,
        }) => {
            let t = val.get_type(symbols).deref();
            let inc_val = Val::Constant(Expr::unary_inc_dec_val(&t));
            let intermediate = Function::make_tacky_temp_var(t, symbols, make_temp_var);
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
    }
}

fn post_inc(
    expr: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    match Expr::parse_with(expr, symbols, make_temp_var) {
        ExprResult::PlainOperand(Expr {
            mut instructions,
            val,
        }) => {
            let t = val.get_type(symbols);
            let inc_val = Val::Constant(Expr::unary_inc_dec_val(&t));
            let dst = Function::make_tacky_temp_var(t, symbols, make_temp_var);
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
            let typ = val.get_type(symbols).deref();
            let inc_val = Val::Constant(Expr::unary_inc_dec_val(&typ));
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
    }
}

fn pre_dec(
    expr: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    match Expr::parse_with(expr, symbols, make_temp_var) {
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
            ExprResult::PlainOperand(Expr { instructions, val })
        }
        ExprResult::DereferencedPointer(Expr {
            mut instructions,
            val,
        }) => {
            let t = val.get_type(symbols).deref();
            let dec_val = Val::Constant(Expr::unary_inc_dec_val(&t));
            let intermediate = Function::make_tacky_temp_var(t, symbols, make_temp_var);
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
    }
}

fn post_dec(
    expr: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    match Expr::parse_with(expr, symbols, make_temp_var) {
        ExprResult::PlainOperand(Expr {
            mut instructions,
            val,
        }) => {
            let t = val.get_type(symbols);
            let dec_val = Val::Constant(Expr::unary_inc_dec_val(&t));
            let dst = Function::make_tacky_temp_var(t, symbols, make_temp_var);
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
            let typ = val.get_type(symbols).deref();
            let dec_val = Val::Constant(Expr::unary_inc_dec_val(&typ));
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
    }
}

fn not(
    expr: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let Expr {
        mut instructions,
        val,
    } = Expr::parse_with_and_convert(expr, symbols, make_temp_var);
    let dst = Function::make_tacky_temp_var(ast::Type::int(4, None), symbols, make_temp_var);
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

fn addr_of(
    expr: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let result_expr = Expr::parse_with(expr, symbols, make_temp_var);
    match result_expr {
        ExprResult::PlainOperand(expr) => {
            let Expr {
                mut instructions,
                val,
            } = expr;
            // array
            let val_t = val.get_type(symbols);
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
            let dst = Function::make_tacky_temp_var(t, symbols, make_temp_var);
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
                && let Some(ptr) = symbols.get(name)
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
                let label = Rc::new(make_temp_var());
                symbols.new_entry(Rc::clone(&label), dereferenced);
                let tmp = Val::Var(label);
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
    }
}
