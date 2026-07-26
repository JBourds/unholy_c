use super::*;

pub(crate) fn parse_cast(node: ast::Expr, ctx: &mut Ctx) -> ExprResult {
    let ast::Expr::Cast { target, exp: expr } = node else {
        unreachable!()
    };
    let Expr {
        mut instructions,
        val,
    } = Expr::parse_with_and_convert(*expr, ctx);
    let Expr {
        instructions: cast_instrs,
        val,
    } = Expr::cast(val, target, ctx);
    instructions.extend(cast_instrs);
    ExprResult::PlainOperand(Expr { instructions, val })
}
