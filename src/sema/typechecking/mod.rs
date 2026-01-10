pub mod attribute;
pub mod expr;
pub mod initial_value;
pub mod symbols;
pub mod var_init;

use crate::const_eval;
use std::collections::HashSet;

use anyhow::{Context, Error};

use crate::ast::{Expr, Type};
pub use attribute::Attribute;
pub use initial_value::InitialValue;
pub use symbols::{Scope, SymbolEntry, SymbolTable};

use super::*;
use expr::typecheck_expr_and_convert;
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
