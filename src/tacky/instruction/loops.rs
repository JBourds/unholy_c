use super::*;

pub(crate) fn parse_for(stmt: ast::Stmt, ctx: &mut Ctx) -> Vec<Instruction> {
    let ast::Stmt::For {
        init,
        condition,
        post,
        body,
        label,
    } = stmt
    else {
        unreachable!();
    };
    let mut block_instructions = vec![];
    let label = label.unwrap();
    let label_start = Rc::new(format!("{label}.start"));
    let label_continue = Rc::new(format!("{label}.continue"));
    let label_break = Rc::new(format!("{label}.break"));

    // <instructions for init>
    match *init {
        ast::ForInit::Decl(decl) => {
            block_instructions.extend(Instruction::parse_var_decl_with(decl, ctx));
        }
        ast::ForInit::Expr(Some(expr)) => {
            let Expr { instructions, .. } = Expr::parse_with_and_convert(expr, ctx);
            block_instructions.extend(instructions);
        }
        ast::ForInit::Expr(None) => {}
    }
    // Label(start)
    block_instructions.push(Instruction::Label(Rc::clone(&label_start)));
    // <instructions for condition>
    // v = <result of condition>
    if let Some(cond) = condition {
        let Expr {
            instructions: instructions_condition,
            val: val_condition,
        } = Expr::parse_with_and_convert(cond, ctx);
        block_instructions.extend(instructions_condition);
        // JumpIfZero(v, break_label)
        block_instructions.push(Instruction::JumpIfZero {
            condition: val_condition,
            target: Rc::clone(&label_break),
        });
    }
    // <instructions for body>
    block_instructions.extend(Instruction::parse_stmt_with(*body, ctx));
    // Label(continue_label)
    block_instructions.push(Instruction::Label(label_continue));
    // <instructions for post>
    if let Some(post) = post {
        let Expr {
            instructions: instructions_post,
            ..
        } = Expr::parse_with_and_convert(post, ctx);
        block_instructions.extend(instructions_post);
    }
    // Jump(Start)
    block_instructions.push(Instruction::Jump(label_start));
    // Label(break_label)
    block_instructions.push(Instruction::Label(label_break));
    block_instructions
}

pub(crate) fn parse_while(stmt: ast::Stmt, ctx: &mut Ctx) -> Vec<Instruction> {
    let ast::Stmt::While {
        condition,
        body,
        label,
    } = stmt
    else {
        unreachable!();
    };
    let mut block_instructions = vec![];
    let label = label.unwrap();
    let label_continue = Rc::new(format!("{label}.continue"));
    let label_break = Rc::new(format!("{label}.break"));

    // Label(continue_label)
    block_instructions.push(Instruction::Label(Rc::clone(&label_continue)));
    // <instructions for condition>
    // v = <result of condition>
    let Expr { instructions, val } = Expr::parse_with_and_convert(condition, ctx);
    block_instructions.extend(instructions);
    // JumpIfZero(v, break_label)
    block_instructions.push(Instruction::JumpIfZero {
        condition: val,
        target: Rc::clone(&label_break),
    });
    // <instructions for body>
    block_instructions.extend(Instruction::parse_stmt_with(*body, ctx));
    // Jump(continue_label)
    block_instructions.push(Instruction::Jump(label_continue));
    // Label(break_label)
    block_instructions.push(Instruction::Label(label_break));
    block_instructions
}

pub(crate) fn parse_do_while(stmt: ast::Stmt, ctx: &mut Ctx) -> Vec<Instruction> {
    let ast::Stmt::DoWhile {
        condition,
        body,
        label,
    } = stmt
    else {
        unreachable!();
    };
    let mut block_instructions = vec![];
    let label = label.unwrap();
    let label_start = Rc::new(format!("{label}.start"));
    let label_continue = Rc::new(format!("{label}.continue"));
    let label_break = Rc::new(format!("{label}.break"));

    // Label(start)
    block_instructions.push(Instruction::Label(Rc::clone(&label_start)));
    // <instructions for body>
    block_instructions.extend(Instruction::parse_stmt_with(*body, ctx));
    // Label(continue_label)
    block_instructions.push(Instruction::Label(label_continue));
    // <instructions for condition>
    // v = <result of condition>
    let Expr { instructions, val } = Expr::parse_with_and_convert(condition, ctx);
    block_instructions.extend(instructions);
    // JumpIfNotZero(v, start)
    block_instructions.push(Instruction::JumpIfNotZero {
        condition: val,
        target: label_start,
    });
    // Label(break_label)
    block_instructions.push(Instruction::Label(label_break));
    block_instructions
}
