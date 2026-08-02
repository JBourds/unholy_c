use std::rc::Rc;

use crate::{
    ast::{self, Constant},
    tacky::{Ctx, Expr, ExprResult, Instruction, Val, expr::arrow::member_of_deref_pointer},
};

pub(crate) fn parse_dot_member(
    structure: ast::Expr,
    member: Rc<String>,
    ctx: &mut Ctx,
) -> ExprResult {
    match Expr::parse_with(structure, ctx) {
        ExprResult::PlainOperand(expr) => {
            let var_name = expr.val.assert_var_get_name();
            let struct_type = expr.val.get_type(&ctx.symbols);
            let struct_tag = struct_type.assert_struct_get_tag();
            let member_entry = ctx.get_struct_member_by_name(&struct_tag, &member);
            ExprResult::SubObject {
                base: var_name,
                offset: member_entry.offset,
            }
        }
        ExprResult::SubObject { base, offset } => {
            let outer_type = ctx.get_var_type(&base);
            let outer_struct_tag = outer_type.assert_struct_get_tag();
            let outer_member_type = ctx.get_struct_member_type_by_offset(&outer_struct_tag, offset);
            let inner_struct_tag = outer_member_type.assert_struct_get_tag();
            let inner_member = ctx.get_struct_member_by_name(&inner_struct_tag, &member);
            ExprResult::SubObject {
                base,
                offset: offset + inner_member.offset,
            }
        }
        ExprResult::DereferencedPointer(expr) => member_of_deref_pointer(expr, member, ctx),
    }
}
