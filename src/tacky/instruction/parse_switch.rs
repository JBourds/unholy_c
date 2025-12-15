use super::*;

pub(crate) fn parse_switch(
    stmt: ast::Stmt,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> Vec<Instruction> {
    let ast::Stmt::Switch {
        condition,
        body,
        label,
        cases,
        default,
    } = stmt
    else {
        unreachable!();
    };
    let mut block_instructions = vec![];
    let label = label.expect("Switch statement must be labelled.");
    let cases = cases.expect("Cases must have been populated.");
    let break_label = Rc::new(format!("{label}.break"));
    // Cases:
    // 1. There are no cases: Still evaluate the condition and make
    //  instructions in case a goto jumps to a label in them.
    // 2. The switch value is a literal (or has been const-evaled):
    //  Perform a compile time comparison to all the cases and make
    //  a single jump to the right location.
    // 3. Perform a linear comparison for each case and jump if the
    //  value matches.
    if cases.is_empty() {
        let Expr { instructions, .. } =
            Expr::parse_with_and_convert(condition, symbols, make_temp_var);
        block_instructions.extend(instructions);
        block_instructions.push(Instruction::Jump(
            default.unwrap_or(Rc::clone(&break_label)),
        ));
        block_instructions.extend(Instruction::parse_stmt_with(*body, symbols, make_temp_var));
    } else if let ast::Expr::Constant(cond) = condition {
        let jump_label = cases
            .iter()
            .find(|&&(case, _)| case == cond)
            .map_or(default, |(_, label)| Some(Rc::clone(label)));
        let has_jump_label = jump_label.is_some();
        let jump_label = jump_label.unwrap_or(break_label.clone());
        block_instructions.push(Instruction::Jump(jump_label.clone()));
        block_instructions.extend(Instruction::parse_stmt_with(*body, symbols, make_temp_var));
        if !has_jump_label {
            block_instructions.push(Instruction::Label(jump_label));
        }
    } else {
        let Expr {
            instructions,
            val: switch_val,
        } = Expr::parse_with_and_convert(condition, symbols, make_temp_var);
        block_instructions.extend(instructions);
        for (case, label) in cases.iter() {
            let Expr {
                instructions,
                val: case_val,
            } = Expr::parse_with_and_convert(ast::Expr::Constant(*case), symbols, make_temp_var);
            block_instructions.extend(instructions);
            let dst = Function::make_tacky_temp_var(ast::Type::bool(), symbols, make_temp_var);
            block_instructions.push(Instruction::Binary {
                op: BinaryOp::Equal,
                src1: switch_val.clone(),
                src2: case_val,
                dst: dst.clone(),
            });
            block_instructions.push(Instruction::JumpIfNotZero {
                condition: dst,
                target: Rc::clone(label),
            });
        }
        block_instructions.push(Instruction::Jump(
            default.unwrap_or(Rc::clone(&break_label)),
        ));
        block_instructions.extend(Instruction::parse_stmt_with(*body, symbols, make_temp_var));
    }
    // Break label always goes after all instructions
    block_instructions.push(Instruction::Label(break_label));
    block_instructions
}
