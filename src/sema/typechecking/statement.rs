use crate::ast;

use super::{
    SymbolEntry, SymbolTable, TypeTable, TypedExpr, const_eval, convert_by_assignment,
    typecheck_block, typecheck_expr_and_convert, typecheck_var_decl,
};
use anyhow::{Context, Error, Result, bail, ensure};

use std::collections::HashSet;
use std::rc::Rc;

pub fn typecheck_stmt(
    stmt: ast::Stmt,
    symbols: &mut SymbolTable,
    structs: &mut TypeTable,
    function: Option<Rc<String>>,
) -> Result<ast::Stmt> {
    match stmt {
        ast::Stmt::Compound(block) => Ok::<ast::Stmt, Error>(ast::Stmt::Compound(
            typecheck_block(block, symbols, structs, function)
                .context("Unable to typecheck block withint statement.")?,
        )),
        ast::Stmt::Return(expr) => {
            if let Some(function) = function.as_ref()
                && let Some(SymbolEntry {
                    r#type:
                        ast::Type {
                            base:
                                ast::BaseType::Fun {
                                    ret_t: return_type, ..
                                },
                            ..
                        },
                    ..
                }) = symbols.get(function)
            {
                let return_type = return_type.clone();
                if let Some(expr) = expr {
                    let TypedExpr { expr, r#type } =
                        typecheck_expr_and_convert(&expr, symbols, structs)
                            .context("failed to typecheck expression and convert")?;
                    Ok(ast::Stmt::Return(Some(
                    convert_by_assignment(expr, &r#type, &return_type.clone()).context(format!(
                        "Unable to implicitly cast return value to expected return type in \"{}\"",
                        function
                    ))?,
                )))
                } else {
                    ensure!(
                        return_type.is_void(),
                        "Cannot have void return in function expecting non-void return type"
                    );
                    Ok(ast::Stmt::Return(None))
                }
            } else {
                bail!("Invalid return statement out of function body.");
            }
        }
        ast::Stmt::Expr(expr) => Ok(ast::Stmt::Expr(
            typecheck_expr_and_convert(&expr, symbols, structs)
                .context("Failed to typecheck expression statement.")?
                .expr,
        )),
        ast::Stmt::If {
            condition,
            then,
            r#else,
        } => {
            let condition = typecheck_expr_and_convert(&condition, symbols, structs)
                .context("Failed to typecheck if block condition.")?;
            ensure!(
                condition.r#type.is_scalar(),
                "If condition expression must be scalar"
            );
            let then = typecheck_stmt(*then, symbols, structs, function.clone())
                .context("Failed to typecheck if branch of conditional.")?;
            let r#else = if let Some(r#else) = r#else {
                Some(
                    typecheck_stmt(*r#else, symbols, structs, function)
                        .context("Failed to typecheck else branch of conditional.")?,
                )
            } else {
                None
            };
            Ok(ast::Stmt::If {
                condition: condition.expr,
                then: Box::new(then),
                r#else: r#else.map(Box::new),
            })
        }
        ast::Stmt::While {
            condition,
            body,
            label,
        } => {
            let condition = typecheck_expr_and_convert(&condition, symbols, structs)
                .context("Failed to typecheck if block condition.")?;
            ensure!(
                condition.r#type.is_scalar(),
                "While condition expression must be scalar"
            );
            let body = typecheck_stmt(*body, symbols, structs, function)
                .context("Failed to typecheck for loop body.")?;
            Ok(ast::Stmt::While {
                body: Box::new(body),
                condition: condition.expr,
                label,
            })
        }
        ast::Stmt::DoWhile {
            body,
            condition,
            label,
        } => {
            let condition = typecheck_expr_and_convert(&condition, symbols, structs)
                .context("Failed to typecheck for loop condition.")?;
            ensure!(
                condition.r#type.is_scalar(),
                "Do-While condition expression must be scalar"
            );
            let body = typecheck_stmt(*body, symbols, structs, function)
                .context("Failed to typecheck for loop body.")?;
            Ok(ast::Stmt::DoWhile {
                body: Box::new(body),
                condition: condition.expr,
                label,
            })
        }
        ast::Stmt::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let init = match *init {
                ast::ForInit::Decl(decl) => {
                    if decl.storage_class.is_some() {
                        bail!(
                            "For-loop counter var \"{}\" cannot have storage class specifier",
                            decl.name
                        );
                    }
                    ast::ForInit::Decl(
                        typecheck_var_decl(decl, symbols, structs)
                            .context("Failed to typecheck for loop initializations.")?,
                    )
                }
                ast::ForInit::Expr(Some(ref expr)) => {
                    let TypedExpr { expr, r#type } =
                        typecheck_expr_and_convert(expr, symbols, structs)
                            .context("Failed to typecheck for loop initialization expression.")?;
                    if r#type.is_struct() || r#type.is_union() {
                        ensure!(
                            r#type.is_complete(),
                            "cannot use incomplete struct/union as ForInit"
                        );
                    }
                    ast::ForInit::Expr(Some(expr))
                }
                _ => ast::ForInit::Expr(None),
            };
            let post = if let Some(post) = post {
                Some(
                    typecheck_expr_and_convert(&post, symbols, structs)
                        .context("Failed to typecheck for loop post condition.")?
                        .expr,
                )
            } else {
                None
            };
            let condition = if let Some(condition) = condition {
                let condition = typecheck_expr_and_convert(&condition, symbols, structs)
                    .context("Failed to typecheck for loop condition.")?;
                ensure!(
                    condition.r#type.is_scalar(),
                    "For-Loop condition expression must be scalar"
                );
                Some(condition.expr)
            } else {
                None
            };
            let body = typecheck_stmt(*body, symbols, structs, function)
                .context("Failed to typecheck for loop body.")?;
            Ok(ast::Stmt::For {
                init: Box::new(init),
                condition,
                post,
                body: Box::new(body),
                label,
            })
        }
        ast::Stmt::Case { value, stmt, label } => {
            let value = typecheck_expr_and_convert(&value, symbols, structs)
                .context("Failed to typecheck case value.")?
                .expr;
            let stmt = typecheck_stmt(*stmt, symbols, structs, function)
                .context("Failed to typecheck case statement.")?;
            Ok(ast::Stmt::Case {
                value,
                stmt: Box::new(stmt),
                label,
            })
        }
        ast::Stmt::Switch {
            condition,
            body,
            cases,
            label,
            default,
        } => {
            let TypedExpr {
                expr: condition,
                r#type: condition_type,
            } = typecheck_expr_and_convert(&condition, symbols, structs)
                .context("Failed to typecheck switch expression.")?;
            ensure!(
                condition_type.is_scalar(),
                "Switch statement condition must be scalar"
            );

            if condition_type.is_function() || condition_type.is_pointer() {
                bail!("Cannot switch on {condition:#?} as it has type {condition_type:#?}");
            }
            // The integer promotions are performed on the controlling
            // expression, so case labels are compared as int (e.g. case 356
            // stays distinct from case 100 when switching on a char).
            let (condition, condition_type) = if condition_type.is_char() {
                let promoted = ast::Type::int(4, None);
                (condition.cast_to(promoted.clone()), promoted)
            } else {
                (condition, condition_type)
            };
            let body = typecheck_stmt(*body, symbols, structs, function)
                .context("Failed to typecheck switch body.")?;
            let mut casted_cases = vec![];
            let cases = cases.as_ref().expect(
                "At this point there should be cases or an empty vector, but never a None variant.",
            );
            let mut case_values = HashSet::new();
            for (val, s) in cases.iter() {
                let expr = ast::Expr::Constant(*val);
                let TypedExpr { expr, r#type } =
                    typecheck_expr_and_convert(&expr, symbols, structs)
                        .context("failed to typecheck and convert expression")?;
                let expr = convert_by_assignment(expr, &r#type, &condition_type).context(
                    format!("Unable to implicitly case constant to type {condition_type:#?}"),
                )?;
                let constant = const_eval::eval(expr)
                    .context("Unable to convert case expression into constant value.")?;
                ensure!(
                    case_values.insert(constant),
                    format!("Duplicate case values in switch: {constant:?}")
                );
                casted_cases.push((constant, Rc::clone(s)));
            }
            Ok(ast::Stmt::Switch {
                condition,
                body: Box::new(body),
                cases: Some(casted_cases),
                label,
                default,
            })
        }
        ast::Stmt::Null => Ok(stmt),
        ast::Stmt::Break(_) => Ok(stmt),
        ast::Stmt::Continue(_) => Ok(stmt),
        ast::Stmt::Label { name, stmt } => Ok(ast::Stmt::Label {
            name,
            stmt: Box::new(
                typecheck_stmt(*stmt, symbols, structs, function)
                    .context("Unable to typecheck statement within label.")?,
            ),
        }),
        ast::Stmt::Default { label, stmt } => Ok(ast::Stmt::Default {
            label,
            stmt: Box::new(
                typecheck_stmt(*stmt, symbols, structs, function)
                    .context("Unable to typecheck statement within default label.")?,
            ),
        }),
        ast::Stmt::Goto(_) => Ok(stmt),
    }
    .context("Failed to typecheck statement.")
}
