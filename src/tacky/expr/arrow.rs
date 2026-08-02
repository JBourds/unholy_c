use std::rc::Rc;

use crate::{
    ast,
    tacky::{Ctx, ExprResult},
};

pub(crate) fn parse_arrow_member(
    structure: ast::Expr,
    member: Rc<String>,
    ctx: &mut Ctx,
) -> ExprResult {
    todo!()
}
