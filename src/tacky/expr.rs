use super::*;

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub instructions: Vec<Instruction>,
    pub val: Val,
}

impl Expr {
    fn do_pointer_arithmetic(
        op: ast::BinaryOp,
        left: Val,
        right: Val,
        make_temp_var: &mut impl FnMut() -> String,
        symbols: &mut SymbolTable,
    ) -> (Vec<Instruction>, Val) {
        let mut instructions = vec![];
        let left_t = left.get_type(symbols);
        let right_t = right.get_type(symbols);
        let (ptr, ptr_t, mut index) = if left_t.is_pointer() || left_t.is_array() {
            (left, left_t, right)
        } else {
            (right, right_t, left)
        };
        if op.is_sub() {
            let negated_tmp =
                Function::make_tacky_temp_var(index.get_type(symbols), symbols, make_temp_var);
            instructions.push(Instruction::Unary {
                op: UnaryOp::Negate,
                src: index,
                dst: negated_tmp.clone(),
            });
            index = negated_tmp;
        }
        let scale = ptr_t.base.size_of_base_type();
        let dst = Function::make_tacky_temp_var(ptr_t, symbols, make_temp_var);
        instructions.push(Instruction::AddPtr {
            ptr,
            index,
            scale,
            dst: dst.clone(),
        });
        (instructions, dst)
    }

    fn unary_inc_dec_val(t: &ast::Type) -> ast::Constant {
        // Typechecking will have caught any case where an array is invalid as
        // a pointer
        if t.is_pointer() || t.is_array() {
            ast::Constant::U64(t.base.size_of_base_type().try_into().unwrap())
        } else {
            ast::Constant::const_from_type(t, 1)
                .expect("UnaryOp type has an ast::Constant equivalent")
        }
    }

    fn parse_with(
        node: ast::Expr,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> ExprResult {
        match node {
            ast::Expr::Constant(v) => ExprResult::PlainOperand(Self {
                instructions: vec![],
                val: Val::from(v),
            }),
            ast::Expr::Unary {
                op: ast::UnaryOp::Deref,
                expr,
            } => ExprResult::DerefrencedPointer(Self::parse_with_and_convert(
                *expr,
                symbols,
                make_temp_var,
            )),
            ast::Expr::Unary {
                op: ast::UnaryOp::AddrOf,
                expr,
            } => {
                let result_expr = Self::parse_with(*expr, symbols, make_temp_var);
                match result_expr {
                    ExprResult::PlainOperand(expr) => {
                        let Self {
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
            ast::Expr::Unary { op, expr } => {
                let (instructions, dst) = match op {
                    ast::UnaryOp::PreInc => match Expr::parse_with(*expr, symbols, make_temp_var) {
                        ExprResult::PlainOperand(Expr {
                            mut instructions,
                            val,
                        }) => {
                            instructions.push(Instruction::Binary {
                                op: BinaryOp::Add,
                                src1: val.clone(),
                                src2: Val::Constant(Self::unary_inc_dec_val(
                                    &val.get_type(symbols),
                                )),
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
                                        ast::Constant::const_from_type(
                                            &val.get_type(symbols).deref(),
                                            1,
                                        )
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
                                    src2: Val::Constant(Self::unary_inc_dec_val(
                                        &val.get_type(symbols),
                                    )),
                                    dst: val.clone(),
                                });
                                (instructions, dst)
                            }
                            ExprResult::DerefrencedPointer(Expr {
                                mut instructions,
                                val,
                            }) => {
                                let typ = val.get_type(symbols).deref();
                                let dst = Function::make_tacky_temp_var(
                                    typ.clone(),
                                    symbols,
                                    make_temp_var,
                                );
                                let intermediate =
                                    Function::make_tacky_temp_var(typ, symbols, make_temp_var);
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
                                            ast::Constant::const_from_type(
                                                &val.get_type(symbols).deref(),
                                                1,
                                            )
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
                                src2: Val::Constant(Self::unary_inc_dec_val(
                                    &val.get_type(symbols),
                                )),
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
                                        ast::Constant::const_from_type(
                                            &val.get_type(symbols).deref(),
                                            1,
                                        )
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
                                    src2: Val::Constant(Self::unary_inc_dec_val(
                                        &val.get_type(symbols),
                                    )),
                                    dst: val.clone(),
                                });
                                (instructions, dst)
                            }
                            ExprResult::DerefrencedPointer(Expr {
                                mut instructions,
                                val,
                            }) => {
                                let typ = val.get_type(symbols).deref();
                                let dst = Function::make_tacky_temp_var(
                                    typ.clone(),
                                    symbols,
                                    make_temp_var,
                                );
                                let intermediate =
                                    Function::make_tacky_temp_var(typ, symbols, make_temp_var);
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
                                            ast::Constant::const_from_type(
                                                &val.get_type(symbols).deref(),
                                                1,
                                            )
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
                        let Self {
                            mut instructions,
                            val,
                        } = Expr::parse_with_and_convert(*expr, symbols, make_temp_var);
                        let dst = Function::make_tacky_temp_var(
                            ast::Type::int(4, None),
                            symbols,
                            make_temp_var,
                        );
                        instructions.push(Instruction::Unary {
                            op: UnaryOp::from(op),
                            src: val,
                            dst: dst.clone(),
                        });
                        (instructions, dst)
                    }
                    // Other operations have tacky unary op equivalents
                    _ => {
                        let Self {
                            mut instructions,
                            val,
                        } = Expr::parse_with_and_convert(*expr, symbols, make_temp_var);
                        let dst = Function::make_tacky_temp_var(
                            val.get_type(symbols),
                            symbols,
                            make_temp_var,
                        );
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
            ast::Expr::Binary { op, left, right } => {
                if op.is_logical() {
                    // <instructions for e1>
                    // v1 = <result of e1>
                    let Self {
                        mut instructions,
                        val: left_val,
                    } = Self::parse_with_and_convert(*left, symbols, make_temp_var);

                    let label = {
                        let mut label = make_temp_var();
                        // FIXME: make_temp_var() should support making label names
                        label.push_str(match op {
                            ast::BinaryOp::And => ".binary_false_label",
                            ast::BinaryOp::Or => ".binary_true_label",
                            _ => unreachable!(),
                        });
                        label.into()
                    };

                    let make_jmp_instruction = |val, target| match op {
                        ast::BinaryOp::And => Instruction::JumpIfZero {
                            condition: val,
                            target,
                        },
                        ast::BinaryOp::Or => Instruction::JumpIfNotZero {
                            condition: val,
                            target,
                        },
                        _ => unreachable!(),
                    };

                    // JumpIfZero(v1, label)
                    instructions.push(make_jmp_instruction(left_val.clone(), Rc::clone(&label)));

                    // <instructions for e2>
                    // v2 = <result of e2>
                    let Self {
                        instructions: right_instructions,
                        val: right_val,
                    } = Self::parse_with_and_convert(*right, symbols, make_temp_var);
                    instructions.extend(right_instructions);

                    let dst =
                        Function::make_tacky_temp_var(ast::Type::bool(), symbols, make_temp_var);
                    let end = {
                        // FIXME: Support label use case
                        let mut end = make_temp_var();
                        end.push_str(".binary_end_label");
                        end.into()
                    };

                    let (result_nojmp, result_jmp) = match op {
                        ast::BinaryOp::And => (1, 0),
                        ast::BinaryOp::Or => (0, 1),
                        _ => unreachable!(),
                    };

                    // JumpIfZero(v2, false_label)
                    instructions.push(make_jmp_instruction(right_val.clone(), Rc::clone(&label)));

                    // result = 1
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(ast::Constant::I32(result_nojmp)),
                        dst: dst.clone(),
                    });

                    // Jump(end)
                    instructions.push(Instruction::Jump(Rc::clone(&end)));

                    // Label(false_label)
                    instructions.push(Instruction::Label(Rc::clone(&label)));

                    // result = 0
                    instructions.push(Instruction::Copy {
                        src: Val::Constant(ast::Constant::I32(result_jmp)),
                        dst: dst.clone(),
                    });

                    // Label(end)
                    instructions.push(Instruction::Label(end));

                    ExprResult::PlainOperand(Self {
                        instructions,
                        val: dst,
                    })
                } else {
                    let Self {
                        mut instructions,
                        val: left_val,
                    } = Self::parse_with_and_convert(*left, symbols, make_temp_var);
                    let Self {
                        instructions: right_instructions,
                        val: right_val,
                    } = Self::parse_with_and_convert(*right, symbols, make_temp_var);
                    instructions.extend(right_instructions);

                    let left_t = left_val.get_type(symbols);
                    let right_t = right_val.get_type(symbols);

                    let dst_type = if op.is_relational() {
                        ast::Type::bool()
                    } else {
                        left_t.clone()
                    };

                    // pointer arithmetic uses special instruction
                    // make sure the pointer is always `src`
                    let dst = if op.is_add_sub() && left_t.is_pointer()
                        || right_t.is_pointer() && op.is_add_sub()
                    {
                        let (new_instructions, dst) = Self::do_pointer_arithmetic(
                            op,
                            left_val,
                            right_val,
                            make_temp_var,
                            symbols,
                        );
                        instructions.extend(new_instructions);
                        dst
                    } else {
                        let dst = Function::make_tacky_temp_var(dst_type, symbols, make_temp_var);
                        instructions.push(Instruction::Binary {
                            op: op.into(),
                            src1: left_val,
                            src2: right_val,
                            dst: dst.clone(),
                        });
                        dst
                    };
                    ExprResult::PlainOperand(Self {
                        instructions,
                        val: dst,
                    })
                }
            }
            ast::Expr::Var(name) => ExprResult::PlainOperand(Self {
                instructions: vec![],
                val: Val::Var(name),
            }),
            // Special case where we kept the compound op after the typechecking
            // rewrite so tacky can properly avoid evaluating a side-effecting
            // expression twice
            ast::Expr::Assignment { lvalue, rvalue } if rvalue.has_compound() => {
                let ast::Expr::Binary { op, left, right } = *rvalue else {
                    unreachable!();
                };
                let Some(op) = op.compound_op() else {
                    unreachable!();
                };
                let lval = Self::parse_with(*lvalue, symbols, make_temp_var);
                let rval = Self::parse_with_and_convert(*right, symbols, make_temp_var);
                match lval {
                    ExprResult::PlainOperand(Expr {
                        mut instructions,
                        val,
                    }) => {
                        instructions.extend(rval.instructions);
                        let arg_type = rval.val.get_type(symbols);
                        let upcasted =
                            if matches!(op, ast::BinaryOp::LShift | ast::BinaryOp::RShift) {
                                Self {
                                    instructions: vec![],
                                    val: val.clone(),
                                }
                            } else {
                                Self::cast(val.clone(), arg_type, symbols, make_temp_var)
                            };
                        instructions.extend(upcasted.instructions);
                        instructions.push(Instruction::Binary {
                            op: op.into(),
                            src1: upcasted.val.clone(),
                            src2: rval.val.clone(),
                            dst: upcasted.val.clone(),
                        });
                        let dst_type = val.get_type(symbols);
                        let downcasted =
                            Self::cast(upcasted.val.clone(), dst_type, symbols, make_temp_var);
                        instructions.extend(downcasted.instructions);
                        instructions.push(Instruction::Copy {
                            src: downcasted.val.clone(),
                            dst: val,
                        });
                        ExprResult::PlainOperand(Self {
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
                                Self {
                                    instructions: vec![],
                                    val: intermediate.clone(),
                                }
                            } else {
                                Self::cast(intermediate.clone(), target, symbols, make_temp_var)
                            }
                        } else {
                            Self {
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
                        let downcasted =
                            Self::cast(binary_lhs.val.clone(), dst_type, symbols, make_temp_var);
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
            ast::Expr::Assignment { lvalue, rvalue } => {
                let lval = Self::parse_with(*lvalue, symbols, make_temp_var);
                let rval = Self::parse_with_and_convert(*rvalue, symbols, make_temp_var);
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
                        ExprResult::PlainOperand(Self { instructions, val })
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
            ast::Expr::Conditional {
                condition,
                then,
                r#else,
            } => {
                let (e2_label, end_label) = {
                    let label = make_temp_var();
                    let e2_label = format!("{label}.cond_e2");
                    let end_label = format!("{label}.cond_end");
                    (Rc::new(e2_label), Rc::new(end_label))
                };
                let Expr {
                    mut instructions,
                    val,
                } = Expr::parse_with_and_convert(*condition, symbols, make_temp_var);

                instructions.push(Instruction::JumpIfZero {
                    condition: val,
                    target: Rc::clone(&e2_label),
                });

                let Expr {
                    instructions: e1_instructions,
                    val: e1_val,
                } = Expr::parse_with_and_convert(*then, symbols, make_temp_var);

                let result = Function::make_tacky_temp_var(
                    e1_val.get_type(symbols).clone(),
                    symbols,
                    make_temp_var,
                );

                instructions.extend(e1_instructions);
                instructions.push(Instruction::Copy {
                    src: e1_val,
                    dst: result.clone(),
                });

                instructions.push(Instruction::Jump(Rc::clone(&end_label)));
                instructions.push(Instruction::Label(Rc::clone(&e2_label)));

                let Expr {
                    instructions: e2_instructions,
                    val: e2_val,
                } = Expr::parse_with_and_convert(*r#else, symbols, make_temp_var);

                instructions.extend(e2_instructions);

                instructions.push(Instruction::Copy {
                    src: e2_val,
                    dst: result.clone(),
                });

                instructions.push(Instruction::Label(end_label));

                ExprResult::PlainOperand(Self {
                    instructions,
                    val: result,
                })
            }
            ast::Expr::FunCall { name, args } => {
                let SymbolEntry {
                    r#type:
                        ast::Type {
                            base: ast::BaseType::Fun { ret_t, .. },
                            ..
                        },
                    ..
                } = symbols
                    .get(&name)
                    .expect("Function '{name}' should already be in symbol table, but it was not!")
                else {
                    unreachable!(
                        "Function name '{name}' resulted in non-function type in symbol table"
                    );
                };
                let dst = Function::make_tacky_temp_var(*ret_t.clone(), symbols, make_temp_var);
                let (mut instructions, args) =
                    args.into_iter()
                        .fold((vec![], vec![]), |(mut instrs, mut args), arg| {
                            let Expr { instructions, val } =
                                Expr::parse_with_and_convert(arg, symbols, make_temp_var);
                            instrs.extend(instructions);
                            args.push(val);
                            (instrs, args)
                        });
                instructions.push(Instruction::FunCall {
                    name,
                    args,
                    dst: dst.clone(),
                });
                ExprResult::PlainOperand(Self {
                    instructions,
                    val: dst,
                })
            }
            ast::Expr::Cast { target, exp: expr } => {
                let Expr {
                    mut instructions,
                    val,
                } = Expr::parse_with_and_convert(*expr, symbols, make_temp_var);
                let Self {
                    instructions: cast_instrs,
                    val,
                } = Self::cast(val, target, symbols, make_temp_var);
                instructions.extend(cast_instrs);
                ExprResult::PlainOperand(Self { instructions, val })
            }
            ast::Expr::Subscript { expr, index } => {
                let Self {
                    mut instructions,
                    val: expr,
                } = Self::parse_with_and_convert(*expr, symbols, make_temp_var);
                let Self {
                    instructions: index_instructions,
                    val: index,
                } = Self::parse_with_and_convert(*index, symbols, make_temp_var);
                instructions.extend(index_instructions);
                let (new_instructions, dst) = Self::do_pointer_arithmetic(
                    ast::BinaryOp::Add,
                    expr,
                    index,
                    make_temp_var,
                    symbols,
                );
                instructions.extend(new_instructions);
                ExprResult::DerefrencedPointer(Self {
                    instructions,
                    val: dst,
                })
            }
        }
    }

    pub fn cast(
        val: Val,
        target: ast::Type,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Self {
        // I do this cause get_type currently clones on vars
        let val_type = val.get_type(symbols);
        let mut instructions = vec![];
        if target == val_type {
            return Self { instructions, val };
        }
        let dst = Function::make_tacky_temp_var(target.clone(), symbols, make_temp_var);

        let is_float = |t: &ast::Type| {
            matches!(
                t,
                ast::Type {
                    base: ast::BaseType::Float(_) | ast::BaseType::Double(_),
                    ..
                }
            )
        };

        // Double -> Integer
        if is_float(&val_type) {
            match target {
                ast::Type {
                    base:
                        ast::BaseType::Int {
                            nbytes: _,
                            signed: Some(false),
                        },
                    ..
                } => {
                    instructions.push(Instruction::DoubleToUInt {
                        src: val,
                        dst: dst.clone(),
                    });
                }
                ast::Type {
                    base:
                        ast::BaseType::Int {
                            nbytes: _,
                            signed: _,
                        },
                    ..
                } => {
                    instructions.push(Instruction::DoubleToInt {
                        src: val,
                        dst: dst.clone(),
                    });
                }
                // FIXME: Add chars here
                // We should not ever be trying to cast a double to
                // anything other than an int
                _ => unreachable!("Casting float type to {target:?}"),
            }
        } else if is_float(&target) {
            match val_type {
                ast::Type {
                    base:
                        ast::BaseType::Int {
                            nbytes: _,
                            signed: Some(false),
                        },
                    ..
                } => {
                    instructions.push(Instruction::UIntToDouble {
                        src: val,
                        dst: dst.clone(),
                    });
                }
                ast::Type {
                    base:
                        ast::BaseType::Int {
                            nbytes: _,
                            signed: _,
                        },
                    ..
                } => {
                    instructions.push(Instruction::IntToDouble {
                        src: val,
                        dst: dst.clone(),
                    });
                }
                // FIXME: Add chars here
                // We should not ever be trying to cast a double to
                // anything other than an int
                _ => unreachable!("Casting float type to {target:?}"),
            }
        } else {
            // Integer ops
            // FIXME: This needs to use PartialEq/Eq
            match target.base.nbytes().cmp(&val_type.base.nbytes()) {
                std::cmp::Ordering::Equal => {
                    instructions.push(Instruction::Copy {
                        src: val,
                        dst: dst.clone(),
                    });
                }
                std::cmp::Ordering::Less => {
                    instructions.push(Instruction::Truncate {
                        src: val,
                        dst: dst.clone(),
                    });
                }
                _ => match val_type {
                    ast::Type {
                        base: ast::BaseType::Int { signed, .. },
                        ..
                    } => {
                        if signed.is_none_or(|signed| signed) {
                            instructions.push(Instruction::SignExtend {
                                src: val,
                                dst: dst.clone(),
                            });
                        } else {
                            instructions.push(Instruction::ZeroExtend {
                                src: val,
                                dst: dst.clone(),
                            });
                        }
                    }
                    _ => unimplemented!(),
                },
            }
        }
        Self {
            instructions,
            val: dst,
        }
    }

    pub(crate) fn parse_with_and_convert(
        node: ast::Expr,
        symbols: &mut SymbolTable,
        make_temp_var: &mut impl FnMut() -> String,
    ) -> Self {
        match Self::parse_with(node, symbols, make_temp_var) {
            ExprResult::PlainOperand(expr) => expr,
            ExprResult::DerefrencedPointer(expr) => {
                let Self {
                    mut instructions,
                    val,
                } = expr;
                let dst = Function::make_tacky_temp_var(
                    val.get_type(symbols).deref(),
                    symbols,
                    make_temp_var,
                );
                instructions.push(Instruction::Load {
                    src_ptr: val,
                    dst: dst.clone(),
                });
                Self {
                    instructions,
                    val: dst,
                }
            }
        }
    }
}
