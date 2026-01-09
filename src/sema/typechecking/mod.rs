pub mod attribute;
pub mod initial_value;
pub mod symbols;

use crate::const_eval;
use std::collections::HashSet;
use std::num::NonZeroUsize;

use anyhow::{Context, Error};

use crate::ast::{Expr, StorageClass, Type};
pub use attribute::Attribute;
pub use initial_value::InitialValue;
pub use symbols::{Scope, SymbolEntry, SymbolTable};

use super::*;

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
    } else {
        bail!(format!(
            "{e1:#?} and {e2:#?} are not compatable pointer types: {e1_t:#?} vs. {e2_t:#?}"
        ))
    }
}

fn convert_by_assignment(e: ast::Expr, e_t: &ast::Type, target: &ast::Type) -> Result<ast::Expr> {
    if e_t == target {
        Ok(e)
    } else if e_t.is_arithmetic() && target.is_arithmetic()
        || is_null_pointer_constant(&e) && target.is_pointer()
    {
        try_implicit_cast(target, e, e_t)
    } else {
        bail!("Cannot convert type for assignment.")
    }
}

#[derive(Debug)]
struct TypedExpr {
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

fn typecheck_program(program: ast::Program, symbols: &mut SymbolTable) -> Result<ast::Program> {
    let mut declarations = vec![];
    for decl in program.declarations.into_iter() {
        match decl {
            ast::Declaration::FunDecl(f) => {
                let name = Rc::clone(&f.name);
                declarations.push(ast::Declaration::FunDecl(
                    typecheck_fun_decl(f, symbols).context(format!(
                        "Unable to typecheck function declaration for {name}"
                    ))?,
                ));
            }
            ast::Declaration::VarDecl(v) => {
                let name = Rc::clone(&v.name);
                declarations.push(ast::Declaration::VarDecl(
                    typecheck_global_var_decl(v, symbols).context(format!(
                        "Unable to typecheck variable declaration for {name}"
                    ))?,
                ));
            }
        }
    }
    Ok(ast::Program { declarations })
}

fn typecheck_block(
    block: ast::Block,
    symbols: &mut SymbolTable,
    function: Option<Rc<String>>,
) -> Result<ast::Block> {
    symbols.push_scope();
    let items = block
        .into_items()
        .into_iter()
        .map(|item| typecheck_block_item(item, symbols, function.clone()))
        .collect::<Result<Vec<_>>>()
        .context("Failed to typecheck all block items")?;
    symbols
        .pop_scope()
        .expect("We just popped the scope so this should not fail.");
    Ok(ast::Block(items))
}

fn typecheck_block_item(
    item: ast::BlockItem,
    symbols: &mut SymbolTable,
    function: Option<Rc<String>>,
) -> Result<ast::BlockItem> {
    match item {
        ast::BlockItem::Stmt(stmt) => Ok(ast::BlockItem::Stmt(
            typecheck_stmt(stmt, symbols, function).context("Failed to typecheck block item")?,
        )),
        ast::BlockItem::Decl(decl) => Ok(ast::BlockItem::Decl(
            typecheck_decl(decl, symbols).context("Failed to typecheck block item")?,
        )),
    }
}

fn typecheck_stmt(
    stmt: ast::Stmt,
    symbols: &mut SymbolTable,
    function: Option<Rc<String>>,
) -> Result<ast::Stmt> {
    match stmt {
        ast::Stmt::Compound(block) => Ok::<ast::Stmt, Error>(
                ast::Stmt::Compound(
                    typecheck_block(block, symbols, function).context("Unable to typecheck block withint statement.")?
                )
            ),
        ast::Stmt::Return(Some(expr)) => {
                if let Some(function) = function {
                    let TypedExpr { expr, r#type } = typecheck_expr_and_convert(&expr, symbols)
                        .context("failed to typecheck expression and convert")?;
                    if let Some(SymbolEntry {
                        r#type:
                            ast::Type {
                                base: ast::BaseType::Fun { ret_t: expected, .. },
                                ..
                            },
                        ..
                    }) = symbols.get(&function)
                    {
                        Ok(ast::Stmt::Return(Some(convert_by_assignment(expr, &r#type, &expected.clone())
                                    .context(format!("Unable to implicitly cast return value to expected return type in \"{}\"", function))?
                                    )))
                    } else {
                        bail!("Could not find function {function} in symbol table.")
                    }
                } else {
                    bail!("Invalid return statement out of function body.");
                }
            }
        ast::Stmt::Expr(expr) => Ok(ast::Stmt::Expr(typecheck_expr_and_convert(&expr, symbols)
                .context("Failed to typecheck expression statement.")?.expr)),
        ast::Stmt::If {
                condition,
                then,
                r#else,
            } => {
                let condition = typecheck_expr_and_convert(&condition, symbols)
                    .context("Failed to typecheck if block condition.")?.expr;
                let then = typecheck_stmt(*then, symbols, function.clone())
                    .context("Failed to typecheck if branch of conditional.")?;
                let r#else = if let Some(r#else) = r#else { Some(typecheck_stmt(*r#else, symbols, function)
                    .context("Failed to typecheck else branch of conditional.")?)
                } else { None };
                Ok(ast::Stmt::If { condition, then: Box::new(then), r#else: r#else.map(Box::new) })

            }
        ast::Stmt::While {
                condition, body, label,
            } => {
                let condition = typecheck_expr_and_convert(&condition, symbols)
                        .context("Failed to typecheck for loop condition.")?.expr;
                let body = typecheck_stmt(*body, symbols, function)
                    .context("Failed to typecheck for loop body.")?;
                Ok(ast::Stmt::While { body: Box::new(body), condition, label })
            }
        ast::Stmt::DoWhile {
                body, condition, label,
            } => {
                let condition = typecheck_expr_and_convert(&condition, symbols)
                        .context("Failed to typecheck for loop condition.")?.expr;
                let body = typecheck_stmt(*body, symbols, function)
                    .context("Failed to typecheck for loop body.")?;
                Ok(ast::Stmt::DoWhile { body: Box::new(body), condition, label })
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
                            typecheck_var_decl(decl, symbols)
                            .context("Failed to typecheck for loop initializations.")?)
                    }
                    ast::ForInit::Expr(Some(ref expr)) => {
                        ast::ForInit::Expr(
                            Some(typecheck_expr_and_convert(expr, symbols)
                            .map(|t_expr| t_expr.expr)
                            .context("Failed to typecheck for loop initialization expression.")?)
                        )
                    }
                    _ => ast::ForInit::Expr(None)
                };
                let post = if let Some(post) = post {
                    Some(typecheck_expr_and_convert(&post, symbols)
                        .context("Failed to typecheck for loop post condition.")?.expr)
                } else { None };
                let condition = if let Some(condition) = condition {
                    Some(typecheck_expr_and_convert(&condition, symbols)
                        .context("Failed to typecheck for loop condition.")?.expr)
                } else { None };
                let body = typecheck_stmt(*body, symbols, function)
                    .context("Failed to typecheck for loop body.")?;
                Ok(ast::Stmt::For {
                    init: Box::new(init), condition, post, body: Box::new(body), label
                })
            }
        ast::Stmt::Case { value, stmt, label } => {
                let value = typecheck_expr_and_convert(&value, symbols)
                    .context("Failed to typecheck case value.")?.expr;
                let stmt = typecheck_stmt(*stmt, symbols, function)
                    .context("Failed to typecheck case statement.")?;
                Ok(ast::Stmt::Case { value, stmt: Box::new(stmt), label })
            }
        ast::Stmt::Switch {
                condition,
                body,
                cases,
                label,
                default,
            } => {
                let TypedExpr { expr: condition, r#type: condition_type } = typecheck_expr_and_convert(&condition, symbols)
                    .context("Failed to typecheck switch expression.")?;

                if condition_type.is_function() || condition_type.is_pointer() {
                    bail!("Cannot switch on {condition:#?} as it has type {condition_type:#?}");
                }
                let body = typecheck_stmt(*body, symbols, function)
                    .context("Failed to typecheck switch body.")?;
                let mut casted_cases = vec![];
                let cases = cases.as_ref().expect("At this point there should be cases or an empty vector, but never a None variant.");
                let mut case_values = HashSet::new();
                for (val, s) in cases.iter() {
                    let expr = ast::Expr::Constant(*val);
                    let TypedExpr { expr, r#type } = typecheck_expr_and_convert(&expr, symbols)
                        .context("failed to typecheck and convert expression")?;
                    let expr = convert_by_assignment(expr, &r#type, &condition_type)
                        .context(format!("Unable to implicitly case constant to type {condition_type:#?}"))?;
                    let constant = const_eval::eval(expr)
                        .context("Unable to convert case expression into constant value.")?;
                    ensure!(case_values.insert(constant), format!("Duplicate case values in switch: {constant:?}"));
                    casted_cases.push((constant, Rc::clone(s)));
                }
                Ok(ast::Stmt::Switch { condition, body: Box::new(body), cases: Some(casted_cases), label, default})
            }
        ast::Stmt::Null => Ok(stmt),
        ast::Stmt::Break(_) => Ok(stmt),
        ast::Stmt::Continue(_) => Ok(stmt),
        ast::Stmt::Label { name, stmt } => Ok(ast::Stmt::Label {
            name,
            stmt: Box::new(typecheck_stmt(*stmt, symbols, function).context("Unable to typecheck statement within label.")?)
        }
        ),
        ast::Stmt::Default { label, stmt } => Ok(ast::Stmt::Default {
            label,
            stmt: Box::new(typecheck_stmt(*stmt, symbols, function).context("Unable to typecheck statement within default label.")?)
        }
        ),
        ast::Stmt::Goto(_) => Ok(stmt),
        ast::Stmt::Return(None) => Ok(ast::Stmt::Return(None)),
    }
    .context("Failed to typecheck statement.")
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

fn typecheck_expr_and_convert(expr: &ast::Expr, symbols: &mut SymbolTable) -> Result<TypedExpr> {
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
                ast::UnaryOp::Negate if r#type.is_pointer() => {
                    bail!("Cannot apply unary negate operation to pointer.")
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

fn typecheck_decl(decl: ast::Declaration, symbols: &mut SymbolTable) -> Result<ast::Declaration> {
    Ok(match decl {
        ast::Declaration::FunDecl(decl) => {
            let name = Rc::clone(&decl.name);
            ast::Declaration::FunDecl(
                typecheck_fun_decl(decl, symbols)
                    .context(format!("Unable to typecheck \"{name}\" declaration"))?,
            )
        }
        ast::Declaration::VarDecl(decl) => {
            let name = Rc::clone(&decl.name);
            ast::Declaration::VarDecl(
                typecheck_var_decl(decl, symbols)
                    .context(format!("Unable to typecheck \"{name}\" declaration"))?,
            )
        }
    })
}

fn typecheck_fun_decl(decl: ast::FunDecl, symbols: &mut SymbolTable) -> Result<ast::FunDecl> {
    // Special case: Push scope and iterate over block items here so the
    // function parameters get put into the same scope as the block items
    symbols.push_scope();

    ensure!(
        !matches!(decl.r#type.base.clone(), ast::BaseType::Fun { ret_t, .. } if ret_t.is_array()),
        "Cannot have functions return array types",
    );

    symbols.declare_fun(&decl)?;
    // Treat parameters as declarations without values
    let block = if let Some(block) = decl.block {
        let items = block
            .into_items()
            .into_iter()
            .map(|item| typecheck_block_item(item, symbols, Some(decl.name.clone())))
            .collect::<Result<Vec<_>>>()
            .context(format!(
                "Failed to typecheck function declaration for \"{}\"",
                decl.name
            ))?;
        Some(ast::Block(items))
    } else {
        None
    };
    symbols
        .pop_scope()
        .expect("We just popped the scope so this should not fail.");
    Ok(ast::FunDecl { block, ..decl })
}

fn typecheck_global_var_decl(
    decl: ast::VarDecl,
    symbols: &mut SymbolTable,
) -> Result<ast::VarDecl> {
    ensure!(
        symbols.scope() == Scope::Global,
        "Global vars must be declared in global scope"
    );
    typecheck_var_decl(decl, symbols)
}

fn typecheck_var_decl(decl: ast::VarDecl, symbols: &mut SymbolTable) -> Result<ast::VarDecl> {
    let target = &decl.r#type;
    let entry = symbols.declare_var(&decl).context(format!(
        "Failed to typecheck local variable declaration: for {}",
        decl.name
    ))?;
    if decl
        .storage_class
        .is_some_and(|cls| cls == StorageClass::Extern)
    {
        ensure!(
            decl.init.is_none(),
            "Cannot provide a definition for a variable with extern storage class."
        );
    }
    let decl = match decl.init {
        Some(init) => {
            let init = typecheck_init(target, init, symbols, &decl.name)?;
            if let Attribute::Static {
                initial_value: _,
                external_linkage,
            } = entry.attribute
            {
                let attribute = Attribute::Static {
                    initial_value: InitialValue::from_initializer(&decl.r#type, &init, symbols)
                        .context("unable to create initial value from initializer")?,
                    external_linkage,
                };
                if let Some(entry) = symbols.get_mut(&decl.name) {
                    entry.attribute = attribute;
                }
            }
            ast::VarDecl {
                init: Some(init),
                ..decl
            }
        }
        None => ast::VarDecl { init: None, ..decl },
    };
    Ok(decl)
}

fn typecheck_init(
    target: &Type,
    init: ast::Initializer,
    symbols: &mut SymbolTable,
    name: &Rc<String>,
) -> Result<ast::Initializer> {
    match (target, init) {
        (target, ast::Initializer::SingleInit(..)) if target.is_array() => {
            bail!("Arrays cannot be initialized with a `SingleInit`")
        }
        (_, ast::Initializer::SingleInit(expr)) => {
            let TypedExpr { expr, r#type } = typecheck_expr_and_convert(&expr, symbols)
                .context("failed to typecheck expression and convert")?;
            Ok(ast::Initializer::SingleInit(
                convert_by_assignment(expr, &r#type, target)
                    .context(format!(
                        "Failed to typecheck initialization for variable \"{}\"",
                        name,
                    ))?
                    .into(),
            ))
        }

        (
            ast::Type {
                base: ast::BaseType::Array { element, size },
                ..
            },
            ast::Initializer::CompundInit(inits),
        ) => {
            if inits.len() > *size {
                bail!("Initializer {inits:#?} has to many elements for array of len {size}");
            }
            let mut inits = inits
                .into_iter()
                .map(|i| typecheck_init(element, i, symbols, name))
                .collect::<Result<Vec<ast::Initializer>>>()?;
            while inits.len() < *size {
                inits.push(ast::Initializer::zero_initializer(element)?);
            }

            Ok(ast::Initializer::CompundInit(inits))
        }
        _ => bail!("Cannot assign compound initializer to non array var decl"),
    }
}
