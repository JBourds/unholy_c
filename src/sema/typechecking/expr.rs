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
        ast::Expr::Unary { op, expr } => {
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
                ast::UnaryOp::Deref => r#type.deref(),
                ast::UnaryOp::Negate => {
                    if r#type.is_pointer() {
                        bail!("Cannot apply unary negate operation to pointer.")
                    } else if r#type.is_char() {
                        ast::Type::int(4, None)
                    } else {
                        r#type
                    }
                }
                ast::UnaryOp::Complement if r#type.is_pointer() => {
                    bail!("Cannot apply unary complement operation to pointer.")
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
                _ => r#type,
            };
            Ok(TypedExpr {
                expr: ast::Expr::Unary {
                    op: *op,
                    expr: Box::new(expr),
                },
                r#type,
            })
        }
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

            // Only allow null pointer constant comparisons with == or !=
            if left_t.is_pointer() && op.is_relational() {
                if is_null_pointer_constant(&left) || is_null_pointer_constant(&right) {
                    // can have null pointer comparison but only with == and !=
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

            // Evaluate all operands in a boolean context.
            if op.is_logical() {
                return Ok(TypedExpr {
                    expr: ast::Expr::Binary {
                        op: *op,
                        left: Box::new(boolify(left, &left_t)?),
                        right: Box::new(boolify(right, &right_t)?),
                    },
                    r#type: ast::Type::bool(),
                });
            }
            match (*op, left_t.clone(), right_t.clone()) {
                // ptr +/- int
                (op @ ast::BinaryOp::Add | op @ ast::BinaryOp::Subtract, left_t, right_t)
                    if left_t.is_pointer() && right_t.is_integer() =>
                {
                    return Ok(TypedExpr {
                        expr: ast::Expr::Binary {
                            op,
                            left: Box::new(left),
                            right: Box::new(ast::Expr::Cast {
                                target: ast::Type::PTRDIFF_T,
                                exp: Box::new(right.clone()),
                            }),
                        },
                        r#type: left_t,
                    });
                }
                // ptr (+/-)= int
                (
                    op @ ast::BinaryOp::AddAssign | op @ ast::BinaryOp::SubAssign,
                    left_t,
                    right_t,
                ) if left_t.is_pointer() && right_t.is_integer() && left.is_lvalue() => {
                    return Ok(TypedExpr {
                        expr: ast::Expr::Assignment {
                            lvalue: Box::new(left.clone()),
                            rvalue: Box::new(ast::Expr::Binary {
                                op,
                                left: Box::new(left),
                                right: Box::new(ast::Expr::Cast {
                                    target: ast::Type::PTRDIFF_T,
                                    exp: Box::new(right.clone()),
                                }),
                            }),
                        },
                        r#type: left_t,
                    });
                }
                // int + ptr
                (ast::BinaryOp::Add, left_t, right_t)
                    if left_t.is_integer() && right_t.is_pointer() =>
                {
                    return Ok(TypedExpr {
                        expr: ast::Expr::Binary {
                            op: ast::BinaryOp::Add,
                            left: Box::new(ast::Expr::Cast {
                                target: ast::Type::PTRDIFF_T,
                                exp: Box::new(left.clone()),
                            }),
                            right: Box::new(right),
                        },
                        r#type: right_t,
                    });
                }
                // ptr1 - ptr2
                (ast::BinaryOp::Subtract, left_t, right_t)
                    if left_t.is_pointer() && right_t.is_pointer() && left_t == right_t =>
                {
                    return Ok(TypedExpr {
                        expr: ast::Expr::Binary {
                            op: ast::BinaryOp::Subtract,
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        r#type: ast::Type::PTRDIFF_T,
                    });
                }
                // ptr1 </<=/>/>= ptr2
                (
                    op @ ast::BinaryOp::LessThan
                    | op @ ast::BinaryOp::LessOrEqual
                    | op @ ast::BinaryOp::GreaterThan
                    | op @ ast::BinaryOp::GreaterOrEqual,
                    left_t,
                    right_t,
                ) if left_t.is_pointer() && right_t.is_pointer() => {
                    return Ok(TypedExpr {
                        expr: ast::Expr::Binary {
                            op,
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        r#type: ast::Type::bool(),
                    });
                }
                (ast::BinaryOp::Subtract | ast::BinaryOp::SubAssign, left_t, right_t)
                    if !left_t.is_pointer() && right_t.is_pointer() =>
                {
                    bail!("Cannot subtract pointer from non pointer type")
                }
                (ast::BinaryOp::Add | ast::BinaryOp::AddAssign, left_t, right_t)
                    if left_t.is_pointer() && right_t.is_pointer() =>
                {
                    bail!("cannot add two pointers together")
                }

                _ => {} // Not a 'valid' pointer arithmitic case
            }

            let common_t = if left_t.is_pointer() || right_t.is_pointer() {
                ensure!(
                    !matches!(
                        op,
                        ast::BinaryOp::Multiply
                            | ast::BinaryOp::Divide
                            | ast::BinaryOp::Remainder
                            | ast::BinaryOp::MultAssign
                            | ast::BinaryOp::DivAssign
                            | ast::BinaryOp::ModAssign
                    ) && !op.is_bitwise(),
                    format!(
                        "Attempted to perform binary operation other than addition or subtraction on pointer type."
                    )
                );
                get_common_pointer_type(&left, left_t.clone(), &right, right_t.clone())?
            } else {
                let (lifted_left_t, _) =
                    ast::BaseType::lift(left_t.base.clone(), right_t.base.clone()).context(
                        "Unable to promote {left_t:#?} and {right_t:#?} to a common type.",
                    )?;
                ast::Type {
                    base: lifted_left_t.clone(),
                    is_const: true,
                    alignment: std::cmp::max(left_t.alignment, right_t.alignment),
                }
            };

            ensure!(
                !(op.is_bitwise()
                    | matches!(op, ast::BinaryOp::Remainder | ast::BinaryOp::ModAssign)
                    && matches!(
                        common_t,
                        ast::Type {
                            base: ast::BaseType::Float(_) | ast::BaseType::Double(_),
                            ..
                        }
                    )),
                "Cannot perform a bitwise or remainder binary operation on a floating point value."
            );

            // Bitshifts do not upcast, and are just the type of the LHS
            // assuming that it is a valid shift (not a float)
            if matches!(op, ast::BinaryOp::LShift | ast::BinaryOp::RShift) {
                Ok(TypedExpr {
                    expr: ast::Expr::Binary {
                        op: *op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    r#type: left_t,
                })
            } else {
                let casted_left = if common_t != left_t {
                    Some(ast::Expr::Cast {
                        target: common_t.clone(),
                        exp: Box::new(left.clone()),
                    })
                } else {
                    None
                };
                let casted_right = if common_t != right_t {
                    Some(ast::Expr::Cast {
                        target: common_t.clone(),
                        exp: Box::new(right.clone()),
                    })
                } else {
                    None
                };
                match op.compound_op() {
                    Some(_) => {
                        ensure!(
                            left.is_modifiable_lvalue(&left_t),
                            "Compound operations are only valid on modifiable lvalues."
                        );
                        Ok(TypedExpr {
                            expr: ast::Expr::Assignment {
                                lvalue: Box::new(left.clone()),
                                rvalue: Box::new(ast::Expr::Binary {
                                    op: *op,
                                    left: Box::new(casted_left.unwrap_or(left)),
                                    right: Box::new(casted_right.unwrap_or(right)),
                                }),
                            },
                            r#type: if op.is_relational() {
                                ast::Type::int(4, None)
                            } else {
                                common_t
                            },
                        })
                    }
                    _ => Ok(TypedExpr {
                        expr: ast::Expr::Binary {
                            op: *op,
                            left: Box::new(casted_left.unwrap_or(left)),
                            right: Box::new(casted_right.unwrap_or(right)),
                        },
                        r#type: if op.is_relational() {
                            ast::Type::int(4, None)
                        } else {
                            common_t
                        },
                    }),
                }
            }
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
                let (then_base, _) =
                    ast::BaseType::lift(then_type.base.clone(), else_type.base.clone())
                        .context("Ternary expression branches evaluate to different types.")?;
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

            if target.is_array() {
                bail!("Cannot cast to array");
            }

            let expr = if *target != r#type {
                ast::Expr::Cast {
                    target: target.clone(),
                    exp: Box::new(expr),
                }
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
            match (expr_t, index_t) {
                (expr_t, index_t) if expr_t.is_pointer() && index_t.is_integer() => Ok(TypedExpr {
                    expr: ast::Expr::Subscript {
                        expr: Box::new(expr),
                        index: Box::new(ast::Expr::Cast {
                            target: ast::Type::PTRDIFF_T,
                            exp: Box::new(index),
                        }),
                    },
                    r#type: expr_t.deref(),
                }),
                (expr_t, index_t) if expr_t.is_integer() && index_t.is_pointer() => Ok(TypedExpr {
                    expr: ast::Expr::Subscript {
                        expr: Box::new(ast::Expr::Cast {
                            target: ast::Type::PTRDIFF_T,
                            exp: Box::new(expr),
                        }),
                        index: Box::new(index),
                    },
                    r#type: index_t.deref(),
                }),
                (expr_t, index_t) => bail!(
                    "Subscript takes one pointer type and one integer type, got: {expr_t:#?}, {index_t:#?}"
                ),
            }
        }
        ast::Expr::String { .. } => todo!(),
    }
}
