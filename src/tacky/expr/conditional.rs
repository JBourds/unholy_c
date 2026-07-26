use super::*;

pub(crate) fn parse_conditional(node: ast::Expr, ctx: &mut Ctx) -> ExprResult {
    let ast::Expr::Conditional {
        condition,
        then,
        r#else,
    } = node
    else {
        unreachable!();
    };
    let (e2_label, end_label) = {
        let label = ctx.make_temp_var_name();
        let e2_label = format!("{label}.cond_e2");
        let end_label = format!("{label}.cond_end");
        (Rc::new(e2_label), Rc::new(end_label))
    };
    let Expr {
        mut instructions,
        val,
    } = Expr::parse_with_and_convert(*condition, ctx);

    instructions.push(Instruction::JumpIfZero {
        condition: val,
        target: Rc::clone(&e2_label),
    });

    let Expr {
        instructions: e1_instructions,
        val: e1_val,
    } = Expr::parse_with_and_convert(*then, ctx);

    let e1_type = e1_val.get_type(&ctx.symbols).clone();
    let result = if e1_type.is_void() {
        Val::dummy()
    } else {
        ctx.make_temp_var(e1_type.clone())
    };

    instructions.extend(e1_instructions);
    if !e1_type.is_void() {
        instructions.push(Instruction::Copy {
            src: e1_val,
            dst: result.clone(),
        });
    }

    instructions.push(Instruction::Jump(Rc::clone(&end_label)));
    instructions.push(Instruction::Label(Rc::clone(&e2_label)));

    let Expr {
        instructions: e2_instructions,
        val: e2_val,
    } = Expr::parse_with_and_convert(*r#else, ctx);

    instructions.extend(e2_instructions);

    let e2_type = e2_val.get_type(&ctx.symbols).clone();
    if !e2_type.is_void() {
        instructions.push(Instruction::Copy {
            src: e2_val,
            dst: result.clone(),
        });
    }

    instructions.push(Instruction::Label(end_label));

    ExprResult::PlainOperand(Expr {
        instructions,
        val: result,
    })
}
