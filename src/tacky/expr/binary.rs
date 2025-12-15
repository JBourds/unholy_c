use super::*;

pub(crate) fn parse_binary(
    node: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let ast::Expr::Binary { op, left, right } = node else {
        unreachable!();
    };
    if op.is_logical() {
        // <instructions for e1>
        // v1 = <result of e1>
        let Expr {
            mut instructions,
            val: left_val,
        } = Expr::parse_with_and_convert(*left, symbols, make_temp_var);

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
        let Expr {
            instructions: right_instructions,
            val: right_val,
        } = Expr::parse_with_and_convert(*right, symbols, make_temp_var);
        instructions.extend(right_instructions);

        let dst = Function::make_tacky_temp_var(ast::Type::bool(), symbols, make_temp_var);
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

        ExprResult::PlainOperand(Expr {
            instructions,
            val: dst,
        })
    } else {
        let Expr {
            mut instructions,
            val: left_val,
        } = Expr::parse_with_and_convert(*left, symbols, make_temp_var);
        let Expr {
            instructions: right_instructions,
            val: right_val,
        } = Expr::parse_with_and_convert(*right, symbols, make_temp_var);
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
        let dst =
            if op.is_add_sub() && left_t.is_pointer() || right_t.is_pointer() && op.is_add_sub() {
                let (new_instructions, dst) =
                    Expr::do_pointer_arithmetic(op, left_val, right_val, make_temp_var, symbols);
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
        ExprResult::PlainOperand(Expr {
            instructions,
            val: dst,
        })
    }
}
