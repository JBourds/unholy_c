use super::*;

pub(crate) fn parse_if(
    stmt: ast::Stmt,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> Vec<Instruction> {
    let ast::Stmt::If {
        condition,
        then,
        r#else,
    } = stmt
    else {
        unreachable!();
    };
    let mut block_instructions = vec![];
    let (else_label, end_label) = {
        let label = make_temp_var();
        // This isn't needed and can be simplified... To Bad!
        let Some((name, count)) = label.as_str().split_once('.') else {
            unreachable!("label should always be name.count");
        };
        let else_label = format!("{name}.{count}.if_else");
        let end_label = format!("{name}.{count}.if_end");
        (Rc::new(else_label), Rc::new(end_label))
    };
    let Expr {
        mut instructions,
        val,
    } = Expr::parse_with_and_convert(condition, symbols, make_temp_var);

    instructions.push(Instruction::JumpIfZero {
        condition: val,
        target: Rc::clone(match r#else {
            Some(_) => &else_label,
            None => &end_label,
        }),
    });

    instructions.extend(Instruction::parse_block_with(
        ast::Block(vec![ast::BlockItem::Stmt(*then)]),
        symbols,
        make_temp_var,
    ));

    if let Some(r#else) = r#else {
        instructions.push(Instruction::Jump(Rc::clone(&end_label)));
        instructions.push(Instruction::Label(Rc::clone(&else_label)));
        instructions.extend(Instruction::parse_block_with(
            ast::Block(vec![ast::BlockItem::Stmt(*r#else)]),
            symbols,
            make_temp_var,
        ));
    }

    instructions.push(Instruction::Label(Rc::clone(&end_label)));
    block_instructions.extend(instructions);
    block_instructions
}
