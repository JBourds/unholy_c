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
    let string = symbols
        .get_string(&value)
        .map(Rc::clone)
        .expect("all string literals should be in symbol table");
    let entry = symbols
        .get(&string)
        .expect("string must also be in symbol table");
    let label = Rc::new(make_temp_var());
    symbols.new_entry(Rc::clone(&label), entry.r#type.clone());
    let tmp = Val::Var(label);
    let instructions = vec![Instruction::GetAddress {
        src: Val::Var(string),
        dst: tmp.clone(),
    }];
    ExprResult::PlainOperand(tacky::Expr {
        instructions,
        val: tmp,
    })
}
