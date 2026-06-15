use crate::{
    ast,
    tacky::{self, ExprResult, Val},
};

pub(crate) fn parse_sizeof_type(node: ast::Expr) -> ExprResult {
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
