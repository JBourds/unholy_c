use super::*;

use crate::{
    ast,
    tacky::{self, ExprResult, SymbolTable, Val},
};

pub(crate) fn parse_sizeof(node: ast::Expr, symbols: &mut SymbolTable) -> ExprResult {
    let ast::Expr::SizeOf(expr) = node else {
        unreachable!()
    };
    let r#type = Expr::get_ast_expr_type(&*expr, symbols);

    ExprResult::PlainOperand(tacky::Expr {
        instructions: vec![],
        val: Val::Constant(ast::Constant::U64(
            r#type
                .size_of()
                .try_into()
                .expect("usize can coerce to u64"),
        )),
    })
}

pub(crate) fn parse_sizeof_type(node: ast::Expr, symbols: &mut SymbolTable) -> ExprResult {
    let ast::Expr::SizeOfT(r#type) = node else {
        unreachable!()
    };

    ExprResult::PlainOperand(tacky::Expr {
        instructions: vec![],
        val: Val::Constant(ast::Constant::U64(
            r#type
                .size_of()
                .try_into()
                .expect("usize can coerce to u64"),
        )),
    })
}
