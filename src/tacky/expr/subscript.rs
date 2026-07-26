use super::*;

pub(crate) fn parse_subscript(node: ast::Expr, ctx: &mut Ctx) -> ExprResult {
    let ast::Expr::Subscript { expr, index } = node else {
        unreachable!()
    };
    let Expr {
        mut instructions,
        val: ptr,
    } = Expr::parse_with_and_convert(*expr, ctx);
    let Expr {
        instructions: index_instructions,
        val: index,
    } = Expr::parse_with_and_convert(*index, ctx);
    instructions.extend(index_instructions);

    let (new_instructions, new_ptr) =
        Expr::do_pointer_arithmetic(ast::BinaryOp::Add, ptr, index, ctx);
    instructions.extend(new_instructions);

    ExprResult::DereferencedPointer(Expr {
        instructions,
        val: new_ptr,
    })
}
