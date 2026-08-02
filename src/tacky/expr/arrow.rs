use std::rc::Rc;

use crate::{
    ast::{self, Constant},
    tacky::{Ctx, Expr, ExprResult, Instruction, Val},
};

pub(crate) fn member_of_deref_pointer(ptr: Expr, member: Rc<String>, ctx: &mut Ctx) -> ExprResult {
    let Expr {
        mut instructions,
        val,
    } = ptr;
    // TODO: Is the type of expr in here a *T or T?
    let ptr_expr_type = val.get_type(&ctx.symbols);
    let dst_ptr = ctx.make_temp_var(ptr_expr_type.clone());
    let deref_expr_type = ptr_expr_type.deref();
    let struct_tag = deref_expr_type.assert_struct_get_tag();
    let member_entry = ctx.get_struct_member_by_name(&struct_tag, &member);
    if member_entry.offset != 0 {
        instructions.push(Instruction::AddPtr {
            ptr: val,
            index: Val::Constant(Constant::I64(member_entry.offset.try_into().unwrap())),
            scale: 1,
            dst: dst_ptr.clone(),
        });
    }
    ExprResult::DereferencedPointer(Expr {
        instructions,
        val: dst_ptr,
    })
}

pub(crate) fn parse_arrow_member(ptr: ast::Expr, member: Rc<String>, ctx: &mut Ctx) -> ExprResult {
    let ptr = Expr::parse_with_and_convert(ptr, ctx);
    member_of_deref_pointer(ptr, member, ctx)
}
