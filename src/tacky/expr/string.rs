use std::rc::Rc;

use crate::{
    ast,
    tacky::{self, ExprResult, Instruction, SymbolTable, Val},
};

/// Create the string object
pub(crate) fn parse_string(
    node: ast::Expr,
    symbols: &mut SymbolTable,
    make_temp_var: &mut impl FnMut() -> String,
) -> ExprResult {
    let ast::Expr::String { value } = node else {
        unreachable!()
    };
    let string = Val::Var(
        symbols
            .get_string(&value)
            .map(Rc::clone)
            .expect("all string literals should be in symbol table"),
    );
    let tmp = Val::Var(Rc::new(make_temp_var()));
    let instructions = vec![Instruction::GetAddress {
        src: string,
        dst: tmp.clone(),
    }];
    ExprResult::PlainOperand(tacky::Expr {
        instructions,
        val: tmp,
    })
}
