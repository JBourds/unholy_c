use super::*;

pub(crate) fn parse_subscript(
    node: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let ast::Expr::Subscript { expr, index } = node else {
        unreachable!()
    };
    let Expr {
        mut instructions,
        val: expr,
    } = Expr::parse_with_and_convert(*expr, symbols, make_temp_var);
    let Expr {
        instructions: index_instructions,
        val: index,
    } = Expr::parse_with_and_convert(*index, symbols, make_temp_var);
    instructions.extend(index_instructions);
    let (new_instructions, dst) =
        Expr::do_pointer_arithmetic(ast::BinaryOp::Add, expr, index, make_temp_var, symbols);
    instructions.extend(new_instructions);
    decay_dereferenced_pointer(dst.clone(), symbols);
    ExprResult::DerefrencedPointer(Expr {
        instructions,
        val: dst,
    })
}
