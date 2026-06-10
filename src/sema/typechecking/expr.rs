use super::{
    SymbolEntry, SymbolTable, TypedExpr, boolify, convert_by_assignment, get_common_pointer_type,
    is_null_pointer_constant, maybe_decay_expr, try_implicit_cast,
};

use anyhow::{Context, Result, bail, ensure};

use crate::ast;

use std::num::NonZeroUsize;
use std::rc::Rc;

pub fn typecheck_expr_and_convert(
    expr: &ast::Expr,
    symbols: &mut SymbolTable,
) -> Result<TypedExpr> {
    let texpr = typecheck_expr(expr, symbols)?;
    Ok(maybe_decay_expr(texpr))
}

fn typecheck_expr(expr: &ast::Expr, symbols: &mut SymbolTable) -> Result<TypedExpr> {
    match expr {
        ast::Expr::Var(var) => {
            if let Some(t) = symbols.get(var) {
                Ok(TypedExpr {
                    expr: expr.clone(),
                    r#type: t.r#type.clone(),
                })
            } else {
                bail!("Attempted to typecheck {var} but there was no type associated with it.");
            }
        }
        // This case has the semantics of a cast but rather than directly
        // converting to a cast expression frame it as an implicit promotion
        // so invalid assignments (e.g., Struct into an int) fail.
        ast::Expr::Assignment { lvalue, rvalue } => {
            let TypedExpr {
                expr: lexpr,
                r#type: left_t,
            } = typecheck_expr_and_convert(lvalue, symbols)
                .context("Failed to typecheck lvalue in assignment.")?;
            ensure!(
                lexpr.is_modifiable_lvalue(&left_t),
                "Expected valid lvalue target in assignment but found value {lexpr:?} with type {left_t:?}"
            );
            let TypedExpr {
                expr: rexpr,
                r#type: right_t,
            } = typecheck_expr_and_convert(rvalue, symbols)
                .context("Failed to typecheck rvalue in assignment.")?;

            // FIXME: Lazy clone :(
            Ok(TypedExpr {
                expr: ast::Expr::Assignment {
                    lvalue: Box::new(lexpr),
                    rvalue: Box::new(
                        convert_by_assignment(rexpr, &right_t, &left_t).context(
                            "Failed to implicitly cast righthand side during assignment.",
                        )?,
                    ),
                },
                r#type: left_t,
            })
        }
        ast::Expr::Unary { op, expr } => typecheck_unary(*op, expr, symbols),
        ast::Expr::Binary { op, left, right } => {
            let TypedExpr {
                expr: left,
                r#type: left_t,
            } = match op {
                // Don't allow lvalue conversion when it involves mutating the LHS
                // as this would change where an array var points to
                ast::BinaryOp::AddAssign | ast::BinaryOp::SubAssign => {
                    typecheck_expr(left, symbols)
                        .context("Failed to typecheck lefthand argument of binary operation.")?
                }
                _ => typecheck_expr_and_convert(left, symbols)
                    .context("Failed to typecheck lefthand argument of binary operation.")?,
            };
            let TypedExpr {
                expr: right,
                r#type: right_t,
            } = typecheck_expr_and_convert(right, symbols)
                .context("Failed to typecheck righthand argument of binary operation.")?;

            typecheck_binary(*op, left, left_t, right, right_t)
        }
        ast::Expr::Conditional {
            condition,
            then,
            r#else,
        } => {
            let TypedExpr {
                expr: condition_expr,
                r#type: condition_type,
            } = typecheck_expr_and_convert(condition, symbols)
                .context("Failed to typecheck ternary expression then branch.")?;

            let TypedExpr {
                expr: then_expr,
                r#type: then_type,
            } = typecheck_expr_and_convert(then, symbols)
                .context("Failed to typecheck ternary expression then branch.")?;

            let TypedExpr {
                expr: else_expr,
                r#type: else_type,
            } = typecheck_expr_and_convert(r#else, symbols)
                .context("Failed to typecheck ternary expression else branch.")?;

            let target = ast::Type::bool();
            let condition = Box::new(
                try_implicit_cast(&target, condition_expr, &condition_type).context(
                    "Unable to implicitly cast ternary expression condition into a boolean value.",
                )?,
            );

            let common_t = if then_type.is_pointer() || else_type.is_pointer() {
                get_common_pointer_type(
                    &then_expr,
                    then_type.clone(),
                    &else_expr,
                    else_type.clone(),
                )?
            } else {
                let then_base = match (&then_type.base, &else_type.base) {
                    (ast::BaseType::Void, ast::BaseType::Void) => ast::BaseType::Void,
                    (then_base, else_base) => {
                        ast::BaseType::lift(then_base.clone(), else_base.clone())
                            .context("Ternary expression branches evaluate to different types.")?
                            .1
                    }
                };
                ast::Type {
                    base: then_base,
                    alignment: std::cmp::max(then_type.alignment, else_type.alignment),
                    ..then_type.clone()
                }
            };

            let then = convert_by_assignment(then_expr, &then_type, &common_t)
                .context("Unable to implicitly cast \"then\" branch of ternary expression to its common type {common_type:?}")?;
            let r#else = convert_by_assignment(else_expr, &else_type, &common_t)
                .context("Unable to implicitly cast \"else\" branch of ternary expression to its common type {common_type:?}")?;

            Ok(TypedExpr {
                expr: ast::Expr::Conditional {
                    condition,
                    then: Box::new(then),
                    r#else: Box::new(r#else),
                },
                r#type: common_t,
            })
        }
        ast::Expr::FunCall { name, args } => match symbols.get(name) {
            Some(SymbolEntry {
                r#type:
                    ast::Type {
                        base: ast::BaseType::Fun { param_types, ret_t },
                        ..
                    },
                ..
            }) => {
                // FIXME: Lazy clones
                let param_types = param_types.clone();
                let ret_t = ret_t.clone();
                if args.len() != param_types.len() {
                    bail!(
                        "Expected {} args but received {} when calling \"{name}\".",
                        param_types.len(),
                        args.len()
                    );
                }
                let ret_t = *ret_t.clone();
                let args = args
                    .iter()
                    .zip(param_types.iter())
                    .map(|(arg, exp_t)| {
                        typecheck_expr_and_convert(arg, symbols).and_then(
                            |TypedExpr { expr, r#type }| {
                                convert_by_assignment(expr, &r#type, exp_t)
                                    .context("failed to typecheck and convert expression")
                            },
                        )
                    })
                    .collect::<Result<Vec<_>>>()
                    .context("failed to convert args for function call")?;
                Ok(TypedExpr {
                    expr: ast::Expr::FunCall {
                        name: Rc::clone(name),
                        args,
                    },
                    r#type: ret_t,
                })
            }
            Some(SymbolEntry { r#type: t, .. }) => {
                bail!("Expected function type, but found type {t}.")
            }
            _ => bail!("Could not find symbol with name {name}."),
        },
        ast::Expr::Cast { target, exp } => {
            let TypedExpr { expr, r#type } = typecheck_expr_and_convert(exp, symbols)
                .context("Failed to typecheck casted expression.")?;

            if target.is_pointer() && r#type.is_float() {
                bail!("Cannot cast floating point number to pointer");
            }

            if target.is_float() && r#type.is_pointer() {
                bail!("Cannot cast pointer to floating point number");
            }
            ensure!(
                target.is_scalar() || target.is_void(),
                "Can only cast to scalar and void types"
            );
            ensure!(
                r#type.is_scalar() || target.is_void(),
                "Can only cast a non-scalar type to void"
            );

            let expr = if *target != r#type {
                expr.cast_to(target.clone())
            } else {
                expr
            };

            Ok(TypedExpr {
                expr,
                r#type: target.clone(),
            })
        }
        expr @ ast::Expr::Constant(constant) => Ok(TypedExpr {
            expr: expr.clone(),
            r#type: ast::Type {
                base: ast::BaseType::from(constant),
                alignment: ast::BaseType::from(constant).default_alignment(),
                is_const: true,
            },
        }),
        ast::Expr::Subscript { expr, index } => {
            // Figure out which one is the integer
            let TypedExpr {
                expr,
                r#type: expr_t,
            } = typecheck_expr_and_convert(expr, symbols)?;
            let TypedExpr {
                expr: index,
                r#type: index_t,
            } = typecheck_expr_and_convert(index, symbols)?;
            let (ptr, ptr_t, index) = match (expr_t, index_t) {
                (expr_t, index_t) if expr_t.is_pointer() && index_t.is_integer() => {
                    (expr, expr_t, index)
                }
                (expr_t, index_t) if expr_t.is_integer() && index_t.is_pointer() => {
                    (index, index_t, expr)
                }
                (expr_t, index_t) => bail!(
                    "Subscript takes one pointer type and one integer type, got: {expr_t:#?}, {index_t:#?}"
                ),
            };
            ensure!(
                ptr_t.is_pointer_to_complete(),
                "Cannot subscript pointer to incomplete (void) type"
            );
            Ok(TypedExpr {
                expr: ast::Expr::Subscript {
                    expr: Box::new(ptr),
                    index: Box::new(index.cast_to(ast::Type::PTRDIFF_T)),
                },
                r#type: ptr_t.deref(),
            })
        }
        ast::Expr::String { value } => {
            let base = ast::BaseType::Array {
                element: Box::new(ast::Type::char(None)),
                size: value.len() + 1,
            };
            let alignment = base.default_alignment();
            let _ = symbols.get_or_make_string(Rc::clone(value));
            Ok(TypedExpr {
                expr: ast::Expr::String {
                    value: Rc::clone(value),
                },
                r#type: ast::Type {
                    base,
                    alignment,
                    is_const: true,
                },
            })
        }
        ast::Expr::SizeOf(expr) => {
            let TypedExpr { expr: _, r#type } = typecheck_expr(expr, symbols)?;
            Ok(TypedExpr {
                expr: ast::Expr::SizeOfT(r#type),
                r#type: ast::Type::USIZE,
            })
        }
        expr @ ast::Expr::SizeOfT(_) => Ok(TypedExpr {
            expr: expr.clone(),
            r#type: ast::Type::USIZE,
        }),
    }
}

fn typecheck_unary(
    op: ast::UnaryOp,
    expr: &ast::Expr,
    symbols: &mut SymbolTable,
) -> Result<TypedExpr> {
    let TypedExpr { expr, r#type } = match op {
        // Don't lvalue convert in these cases
        ast::UnaryOp::AddrOf
        | ast::UnaryOp::PreInc
        | ast::UnaryOp::PostInc
        | ast::UnaryOp::PreDec
        | ast::UnaryOp::PostDec => typecheck_expr(expr, symbols),
        _ => typecheck_expr_and_convert(expr, symbols),
    }
    .context("Failed to typecheck nested unary expression.")?;
    ensure!(
        !r#type.is_void(),
        "Cannot perform unary operation on void type."
    );
    ensure!(
        !(op.is_bitwise()
            && matches!(
                r#type,
                ast::Type {
                    base: ast::BaseType::Float(_)
                        | ast::BaseType::Double(_)
                        | ast::BaseType::Ptr { .. },
                    ..
                }
            )),
        "Cannot perform a bitwise unary operation on a floating point value."
    );
    if matches!(op, ast::UnaryOp::Not) {
        ensure!(
            r#type.is_scalar(),
            "Cannot have a non-scalar controlling value."
        );
    }
    let operand_is_char = r#type.is_char();
    let r#type = match op {
        ast::UnaryOp::AddrOf if expr.is_lvalue() => ast::Type {
            base: ast::BaseType::Ptr {
                to: Box::new(r#type),
                is_restrict: false,
            },
            alignment: NonZeroUsize::new(core::mem::size_of::<usize>()).unwrap(),
            is_const: false,
        },
        ast::UnaryOp::AddrOf => bail!("Cannot take the address of a non-lvalue"),
        ast::UnaryOp::Deref => {
            ensure!(
                r#type.is_pointer_to_complete(),
                "Cannot dereference pointer to incomplete (void) type"
            );
            r#type.deref()
        }
        ast::UnaryOp::Negate => {
            if r#type.is_pointer() {
                bail!("Cannot apply unary negate operation to pointer.")
            } else if r#type.is_char() {
                ast::Type::int(4, None)
            } else {
                r#type
            }
        }
        ast::UnaryOp::Complement => {
            if r#type.is_pointer() {
                bail!("Cannot apply unary complement operation to pointer.")
            } else if r#type.is_char() {
                ast::Type::int(4, None)
            } else {
                r#type
            }
        }
        op @ ast::UnaryOp::PostInc
        | op @ ast::UnaryOp::PostDec
        | op @ ast::UnaryOp::PreInc
        | op @ ast::UnaryOp::PreDec => {
            if !expr.is_modifiable_lvalue(&r#type) {
                bail!("Cannot apply unary {op:?} to non-lvalues");
            }
            r#type
        }
        ast::UnaryOp::Not => ast::Type::bool(),
    };
    // Integer-promote a char operand for negate/complement so the
    // operation actually runs at int width (the result type was already
    // widened above); otherwise it computes in char width and is only
    // correct when something later re-promotes it.
    let expr = if operand_is_char && matches!(op, ast::UnaryOp::Negate | ast::UnaryOp::Complement) {
        expr.cast_to(ast::Type::int(4, None))
    } else {
        expr
    };
    Ok(TypedExpr {
        expr: ast::Expr::Unary {
            op,
            expr: Box::new(expr),
        },
        r#type,
    })
}

/// Typecheck a binary expression whose operands have already been checked.
/// Dispatches to the rule that applies: logical, pointer arithmetic/comparison,
/// shift, or ordinary arithmetic.
fn typecheck_binary(
    op: ast::BinaryOp,
    left: ast::Expr,
    left_t: ast::Type,
    right: ast::Expr,
    right_t: ast::Type,
) -> Result<TypedExpr> {
    validate_pointer_comparison(op, &left, &left_t, &right, &right_t)?;

    // Logical operands are evaluated in a boolean context.
    if op.is_logical() {
        ensure!(
            left_t.is_scalar() && right_t.is_scalar(),
            "Cannot have a non-scalar controlling value."
        );
        return Ok(TypedExpr {
            expr: ast::Expr::Binary {
                op,
                left: Box::new(boolify(left, &left_t)?),
                right: Box::new(boolify(right, &right_t)?),
            },
            r#type: ast::Type::bool(),
        });
    }

    if let Some(result) = try_pointer_binary(op, &left, &left_t, &right, &right_t)? {
        return Ok(result);
    }

    ensure!(
        left_t.is_complete() && right_t.is_complete(),
        "LHS and RHS arguments must either be pointers or arithmetic types. Cannot be incomplete (void) types."
    );

    let common_t = compute_common_type(op, &left, &left_t, &right, &right_t)?;

    ensure!(
        !((op.is_bitwise() || matches!(op, ast::BinaryOp::Remainder | ast::BinaryOp::ModAssign))
            && common_t.is_float()),
        "Cannot perform a bitwise or remainder binary operation on a floating point value."
    );

    if op.is_shift() {
        typecheck_shift(op, left, left_t, right)
    } else {
        typecheck_arithmetic(op, left, left_t, right, right_t, common_t)
    }
}

/// Pointer relational operands must be compatible (or compared against a null
/// pointer constant, which is only allowed with `==`/`!=`).
fn validate_pointer_comparison(
    op: ast::BinaryOp,
    left: &ast::Expr,
    left_t: &ast::Type,
    right: &ast::Expr,
    right_t: &ast::Type,
) -> Result<()> {
    if left_t.is_pointer() && op.is_relational() {
        if (is_null_pointer_constant(left) || is_null_pointer_constant(right))
            || (left_t.is_void_pointer() ^ right_t.is_void_pointer())
        {
            ensure!(
                matches!(op, ast::BinaryOp::Equal | ast::BinaryOp::NotEqual),
                format!(
                    "Error in \"{op:#?}\" comparison: lefthand side with type {left_t:#?} and righthand side with type {right_t:#?}. Expressions: \nLeft: {left:#?}\nRight: {right:#?}"
                )
            );
        } else {
            ensure!(
                left_t == right_t,
                format!(
                    "Error in \"{op:#?}\" comparison: lefthand side with type {left_t:#?} and righthand side with type {right_t:#?}. Expressions: \nLeft: {left:#?}\nRight: {right:#?}"
                )
            );
        }
    }
    Ok(())
}

/// Handle the pointer-arithmetic and pointer-comparison forms. Returns `Some`
/// when `op` is one of those forms, `None` when it is ordinary scalar work.
fn try_pointer_binary(
    op: ast::BinaryOp,
    left: &ast::Expr,
    left_t: &ast::Type,
    right: &ast::Expr,
    right_t: &ast::Type,
) -> Result<Option<TypedExpr>> {
    let ptrdiff_cast = |e: &ast::Expr| e.clone().cast_to(ast::Type::PTRDIFF_T);

    // ptr +/- int
    if matches!(op, ast::BinaryOp::Add | ast::BinaryOp::Subtract)
        && left_t.is_pointer_to_complete()
        && right_t.is_integer()
    {
        return Ok(Some(TypedExpr {
            expr: ast::Expr::Binary {
                op,
                left: Box::new(left.clone()),
                right: Box::new(ptrdiff_cast(right)),
            },
            r#type: left_t.clone(),
        }));
    }
    // ptr (+/-)= int
    if matches!(op, ast::BinaryOp::AddAssign | ast::BinaryOp::SubAssign)
        && left_t.is_pointer_to_complete()
        && right_t.is_integer()
        && left.is_lvalue()
    {
        return Ok(Some(TypedExpr {
            expr: ast::Expr::Assignment {
                lvalue: Box::new(left.clone()),
                rvalue: Box::new(ast::Expr::Binary {
                    op,
                    left: Box::new(left.clone()),
                    right: Box::new(ptrdiff_cast(right)),
                }),
            },
            r#type: left_t.clone(),
        }));
    }
    // int + ptr
    if matches!(op, ast::BinaryOp::Add) && left_t.is_integer() && right_t.is_pointer_to_complete() {
        return Ok(Some(TypedExpr {
            expr: ast::Expr::Binary {
                op: ast::BinaryOp::Add,
                left: Box::new(ptrdiff_cast(left)),
                right: Box::new(right.clone()),
            },
            r#type: right_t.clone(),
        }));
    }
    // ptr1 - ptr2
    if matches!(op, ast::BinaryOp::Subtract)
        && left_t.is_pointer_to_complete()
        && right_t.is_pointer_to_complete()
        && left_t == right_t
    {
        return Ok(Some(TypedExpr {
            expr: ast::Expr::Binary {
                op: ast::BinaryOp::Subtract,
                left: Box::new(left.clone()),
                right: Box::new(right.clone()),
            },
            r#type: ast::Type::PTRDIFF_T,
        }));
    }
    // ptr1 </<=/>/>= ptr2
    if op.is_ordering() && left_t.is_pointer_to_complete() && right_t.is_pointer_to_complete() {
        return Ok(Some(TypedExpr {
            expr: ast::Expr::Binary {
                op,
                left: Box::new(left.clone()),
                right: Box::new(right.clone()),
            },
            r#type: ast::Type::bool(),
        }));
    }
    if matches!(op, ast::BinaryOp::Subtract | ast::BinaryOp::SubAssign)
        && !left_t.is_pointer_to_complete()
        && right_t.is_pointer_to_complete()
    {
        bail!("Cannot subtract pointer from non pointer type")
    }
    if matches!(op, ast::BinaryOp::Add | ast::BinaryOp::AddAssign)
        && left_t.is_pointer_to_complete()
        && right_t.is_pointer_to_complete()
    {
        bail!("cannot add two pointers together")
    }

    Ok(None)
}

/// The common type the two operands are converted to: a common pointer type if
/// either side is a pointer, otherwise the lifted (promoted) scalar type.
fn compute_common_type(
    op: ast::BinaryOp,
    left: &ast::Expr,
    left_t: &ast::Type,
    right: &ast::Expr,
    right_t: &ast::Type,
) -> Result<ast::Type> {
    if left_t.is_pointer() || right_t.is_pointer() {
        ensure!(
            !op.is_mult_div_mod() && !op.is_bitwise(),
            "Attempted to perform binary operation other than addition or subtraction on pointer type."
        );
        get_common_pointer_type(left, left_t.clone(), right, right_t.clone())
    } else {
        let (lifted_left_t, _) = ast::BaseType::lift(left_t.base.clone(), right_t.base.clone())
            .context("Unable to promote {left_t:#?} and {right_t:#?} to a common type.")?;
        Ok(ast::Type {
            base: lifted_left_t,
            is_const: true,
            alignment: std::cmp::max(left_t.alignment, right_t.alignment),
        })
    }
}

/// Shifts don't find a common type; the left operand is integer-promoted
/// (char -> int) and the result takes that promoted type.
fn typecheck_shift(
    op: ast::BinaryOp,
    left: ast::Expr,
    left_t: ast::Type,
    right: ast::Expr,
) -> Result<TypedExpr> {
    let (left, left_t) = if left_t.is_char() {
        (left.cast_to(ast::Type::I32), ast::Type::I32)
    } else {
        (left, left_t)
    };
    Ok(TypedExpr {
        expr: ast::Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        },
        r#type: left_t,
    })
}

/// Ordinary arithmetic/bitwise/comparison: convert both operands to the common
/// type, emitting an assignment back to the LHS for compound forms.
fn typecheck_arithmetic(
    op: ast::BinaryOp,
    left: ast::Expr,
    left_t: ast::Type,
    right: ast::Expr,
    right_t: ast::Type,
    common_t: ast::Type,
) -> Result<TypedExpr> {
    let casted_left = (common_t != left_t).then(|| left.clone().cast_to(common_t.clone()));
    let casted_right = (common_t != right_t).then(|| right.clone().cast_to(common_t.clone()));
    let result_type = if op.is_relational() {
        ast::Type::I32
    } else {
        common_t
    };

    if op.compound_op().is_some() {
        ensure!(
            left.is_modifiable_lvalue(&left_t),
            "Compound operations are only valid on modifiable lvalues."
        );
        Ok(TypedExpr {
            expr: ast::Expr::Assignment {
                lvalue: Box::new(left.clone()),
                rvalue: Box::new(ast::Expr::Binary {
                    op,
                    left: Box::new(casted_left.unwrap_or(left)),
                    right: Box::new(casted_right.unwrap_or(right)),
                }),
            },
            r#type: result_type,
        })
    } else {
        Ok(TypedExpr {
            expr: ast::Expr::Binary {
                op,
                left: Box::new(casted_left.unwrap_or(left)),
                right: Box::new(casted_right.unwrap_or(right)),
            },
            r#type: result_type,
        })
    }
}
