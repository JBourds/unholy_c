use std::rc::Rc;

use crate::{
    ast,
    tacky::{self, ExprResult, SymbolTable, Val},
};

/// Reference the static string object directly.
///
/// A string literal used as a value is an lvalue of array type; the array ->
/// pointer decay the type checker inserts (an `AddrOf` wrapper) is what takes
/// its address.
pub(crate) fn parse_string(node: ast::Expr, symbols: &mut SymbolTable) -> ExprResult {
    let ast::Expr::String { value } = node else {
        unreachable!()
    };
    let string = symbols
        .get_string(&value)
        .map(Rc::clone)
        .expect("all string literals should be in symbol table");
    ExprResult::PlainOperand(tacky::Expr {
        instructions: vec![],
        val: Val::Var(string),
    })
}
