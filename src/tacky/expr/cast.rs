use super::*;

pub(crate) fn parse_cast(
    node: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let ast::Expr::Cast { target, exp: expr } = node else {
        unreachable!()
    };
    let Expr {
        mut instructions,
        val,
    } = Expr::parse_with_and_convert(*expr, symbols, make_temp_var);
    let Expr {
        instructions: cast_instrs,
        val,
    } = Expr::cast(val, target, symbols, make_temp_var);
    instructions.extend(cast_instrs);
    ExprResult::PlainOperand(Expr { instructions, val })
}
