use super::*;

pub(crate) fn parse_fun_call(
    node: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let ast::Expr::FunCall { name, args } = node else {
        unreachable!()
    };
    let SymbolEntry {
        r#type:
            ast::Type {
                base: ast::BaseType::Fun { ret_t, .. },
                ..
            },
        ..
    } = symbols.get(&name).unwrap_or_else(|| {
        panic!("Function '{name}' should already be in symbol table, but it was not!")
    })
    else {
        unreachable!("Function name '{name}' resulted in non-function type in symbol table");
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
    ExprResult::PlainOperand(Expr {
        instructions,
        val: dst,
    })
}
