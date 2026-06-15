pub mod attribute;
pub mod block;
pub mod decl;
pub mod expr;
pub mod fun_decl;
pub mod init;
pub mod initial_value;
pub mod program;
pub mod statement;
pub mod symbols;
pub mod var_init;

use crate::const_eval;

use anyhow::Context;

use crate::ast::{Expr, Type};
pub use attribute::Attribute;
pub use initial_value::InitialValue;
pub use symbols::{Scope, SymbolEntry, SymbolTable, SymbolTableGetType};

use super::*;
use block::{typecheck_block, typecheck_block_item};
use decl::typecheck_decl;
use expr::typecheck_expr_and_convert;
use fun_decl::typecheck_fun_decl;
use init::typecheck_init;
use program::typecheck_program;
use statement::typecheck_stmt;
use var_init::{typecheck_global_var_decl, typecheck_var_decl};

fn is_null_pointer_constant(e: &ast::Expr) -> bool {
    if let ast::Expr::Cast { target, exp } = e
        && target.is_pointer()
        && is_null_pointer_constant(exp)
    {
        return true;
    }
    let Ok(c) = const_eval::eval(e.clone()) else {
        return false;
    };
    matches!(
        c,
        ast::Constant::I8(0)
            | ast::Constant::I16(0)
            | ast::Constant::I32(0)
            | ast::Constant::I64(0)
            | ast::Constant::U8(0)
            | ast::Constant::U16(0)
            | ast::Constant::U32(0)
            | ast::Constant::U64(0)
    )
}

fn get_common_pointer_type(
    e1: &ast::Expr,
    e1_t: ast::Type,
    e2: &ast::Expr,
    e2_t: ast::Type,
) -> Result<ast::Type> {
    if e1_t == e2_t {
        Ok(e1_t)
    } else if is_null_pointer_constant(e1) {
        Ok(e2_t)
    } else if is_null_pointer_constant(e2) {
        Ok(e1_t)
    } else if e1_t.is_void_pointer() && e2_t.is_pointer()
        || e2_t.is_void_pointer() && e1_t.is_pointer()
    {
        Ok(Type::pointer(Box::new(Type::VOID)))
    } else {
        bail!(format!(
            "{e1:#?} and {e2:#?} are not compatable pointer types: {e1_t:#?} vs. {e2_t:#?}"
        ))
    }
}

fn convert_by_assignment(e: ast::Expr, e_t: &ast::Type, target: &ast::Type) -> Result<ast::Expr> {
    let null_pointer_convertable = || {
        e_t.is_arithmetic() && target.is_arithmetic()
            || is_null_pointer_constant(&e) && target.is_pointer()
    };
    let void_pointer_convertable = || {
        target.is_void_pointer() && e_t.is_pointer() || e_t.is_void_pointer() && target.is_pointer()
    };
    if e_t == target {
        Ok(e)
    } else if null_pointer_convertable() || void_pointer_convertable() {
        try_implicit_cast(target, e, e_t)
    } else {
        bail!("Cannot convert type for assignment.")
    }
}

#[derive(Debug)]
pub struct TypedExpr {
    expr: ast::Expr,
    r#type: ast::Type,
}

pub fn validate(stage: SemaStage<SwitchLabelling>) -> Result<SemaStage<TypeChecking>> {
    let mut symbols = SymbolTable::new_table();

    Ok(SemaStage {
        program: typecheck_program(stage.program, &mut symbols)
            .context("Failed to perform typechecking.")?,
        symbols: Some(symbols),
        stage: PhantomData::<TypeChecking>,
    })
}

fn try_implicit_cast(target: &ast::Type, from: ast::Expr, from_t: &ast::Type) -> Result<ast::Expr> {
    if from_t.is_pointer() && target.is_float() || target.is_pointer() && from_t.is_float() {
        bail!("Cannot convert between double and pointer.");
    }

    if target.is_array() {
        bail!("try_implicit_cast: Cannot cast to array type");
    }

    if from_t != target {
        Ok(ast::Expr::Cast {
            target: ast::Type {
                is_const: true,
                ..target.clone()
            },
            exp: Box::new(from),
        })
    } else {
        Ok(from)
    }
}

/// Try to implicitly cast into a boolean or, if the type is a floating point
/// value, convert into into a comparison against zero.
fn boolify(expr: Expr, r#type: &Type) -> Result<Expr> {
    if r#type.is_float() {
        let zero = ast::Expr::Constant(ast::Constant::const_from_type(r#type, 0)?);
        Ok(Expr::Binary {
            op: ast::BinaryOp::NotEqual,
            left: Box::new(expr),
            right: Box::new(zero),
        })
    } else {
        try_implicit_cast(&ast::Type::bool(), expr, r#type)
    }
}

/// If the type is an array, wrap it in an `AddrOf` and convert its type into a
/// pointer to the elements of the array.
fn maybe_decay_expr(texpr: TypedExpr) -> TypedExpr {
    let TypedExpr { expr, r#type } = texpr;

    if r#type.is_array() {
        let expr = ast::Expr::Unary {
            op: ast::UnaryOp::AddrOf,
            expr: Box::new(expr),
        };
        TypedExpr {
            expr,
            r#type: r#type.maybe_decay(),
        }
    } else {
        TypedExpr { expr, r#type }
    }
}

pub(super) fn validate_type_specifier(ty: &ast::Type) -> Result<()> {
    match ty {
        ast::Type {
            base: ast::BaseType::Array { element, .. },
            ..
        } => {
            ensure!(
                element.is_complete(),
                "Cannot have array of incomplete elements"
            );
            validate_type_specifier(element)?;
        }
        ast::Type {
            base: ast::BaseType::Ptr { to, .. },
            ..
        } => {
            validate_type_specifier(to)?;
        }
        ast::Type {
            base: ast::BaseType::Fun { ret_t, param_types },
            ..
        } => {
            validate_type_specifier(ret_t)?;
            for ty in param_types.iter() {
                validate_type_specifier(ty)?;
            }
        }
        _ => {}
    }
    Ok(())
}
