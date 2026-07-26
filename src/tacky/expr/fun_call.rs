use super::*;

pub(crate) fn parse_fun_call(node: ast::Expr, ctx: &mut Ctx) -> ExprResult {
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
    } = ctx.symbols.get(&name).unwrap_or_else(|| {
        panic!("Function '{name}' should already be in symbol table, but it was not!")
    })
    else {
        unreachable!("Function name '{name}' resulted in non-function type in symbol table");
    };
    let dst = if ret_t.is_void() {
        None
    } else {
        Some(ctx.make_temp_var(*ret_t.clone()))
    };
    let (mut instructions, args) =
        args.into_iter()
            .fold((vec![], vec![]), |(mut instrs, mut args), arg| {
                let Expr { instructions, val } = Expr::parse_with_and_convert(arg, ctx);
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
        val: dst.unwrap_or_else(Val::dummy),
    })
}
