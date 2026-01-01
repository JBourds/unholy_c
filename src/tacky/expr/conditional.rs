use super::*;

pub(crate) fn parse_conditional(
    node: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let ast::Expr::Conditional {
        condition,
        then,
        r#else,
    } = node
    else {
        unreachable!();
    };
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

    let result =
        Function::make_tacky_temp_var(e1_val.get_type(symbols).clone(), symbols, make_temp_var);

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

    ExprResult::PlainOperand(Expr {
        instructions,
        val: result,
    })
}
