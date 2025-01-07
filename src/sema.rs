use std::{collections::HashMap, rc::Rc};

use crate::ast;
use anyhow::{bail, Result};

pub fn validate<'a>(program: &ast::Program<'a>) -> Result<ast::Program<'a>> {
    let valid_function = validate_function(&program.function)?;
    Ok(ast::Program {
        function: valid_function,
    })
}

fn validate_function<'a>(function: &ast::Function<'a>) -> Result<ast::Function<'a>> {
    let mut variable_map = HashMap::new();
    let mut count = 0;
    let mut unique_name_generator = move |name: &str| -> String {
        let new_name = format!("{name}.{count}");
        count += 1;
        new_name
    };
    let valid_items = function
        .items
        .iter()
        .try_fold(Vec::new(), |mut items, block_item| {
            items.push(validate_blockitem(
                block_item,
                &mut variable_map,
                &mut unique_name_generator,
            )?);
            Ok::<Vec<ast::BlockItem>, anyhow::Error>(items)
        })?;

    Ok(ast::Function {
        ret_t: function.ret_t,
        name: &function.name,
        signature: function.signature.clone(),
        items: valid_items,
    })
}

fn validate_blockitem(
    instruction: &ast::BlockItem,
    variable_map: &mut HashMap<Rc<String>, Rc<String>>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::BlockItem> {
    match instruction {
        ast::BlockItem::Stmt(stmt) => Ok(ast::BlockItem::Stmt(resolve_stmt(stmt, variable_map)?)),
        ast::BlockItem::Decl(ast::Declaration { typ, name, expr }) => {
            if variable_map.contains_key(name) {
                bail!("Duplicate variable declaration '{name}'");
            }
            let unique_name = Rc::new(make_temporary(name));
            variable_map.insert(Rc::clone(name), Rc::clone(&unique_name));

            let expr = match expr {
                Some(expr) => Some(resolve_expr(expr, variable_map)?),
                None => None,
            };

            Ok(ast::BlockItem::Decl(ast::Declaration {
                typ: *typ,
                name: unique_name,
                expr,
            }))
        }
    }
}

fn resolve_stmt(
    stmt: &ast::Stmt,
    variable_map: &HashMap<Rc<String>, Rc<String>>,
) -> Result<ast::Stmt> {
    match stmt {
        ast::Stmt::Return(Some(expr)) => {
            Ok(ast::Stmt::Return(Some(resolve_expr(expr, variable_map)?)))
        }
        ast::Stmt::Return(None) => Ok(ast::Stmt::Return(None)),
        ast::Stmt::Expr(expr) => Ok(ast::Stmt::Expr(resolve_expr(expr, variable_map)?)),
        ast::Stmt::Null => Ok(ast::Stmt::Null),
    }
}

fn resolve_expr(
    expr: &ast::Expr,
    variable_map: &HashMap<Rc<String>, Rc<String>>,
) -> Result<ast::Expr> {
    match expr {
        ast::Expr::Assignment { lvalue, rvalue } => {
            match **dbg!(lvalue) {
                ast::Expr::Var(_) => (),
                _ => bail!("Invalid lvalue '{:?}'", expr),
            };
            Ok(ast::Expr::Assignment {
                lvalue: Box::new(resolve_expr(lvalue, variable_map)?),
                rvalue: Box::new(resolve_expr(rvalue, variable_map)?),
            })
        }
        ast::Expr::Var(var) => {
            if let Some(name) = variable_map.get(var) {
                Ok(ast::Expr::Var(Rc::clone(name)))
            } else {
                bail!("Undeclared variable '{var}'")
            }
        }
        ast::Expr::Literal(lit) => Ok(ast::Expr::Literal(*lit)),
        ast::Expr::Unary { op, expr } => Ok(ast::Expr::Unary {
            op: *op,
            expr: Box::new(resolve_expr(expr, variable_map)?),
        }),
        ast::Expr::Binary { op, left, right } => Ok(ast::Expr::Binary {
            op: *op,
            left: Box::new(resolve_expr(left, variable_map)?),
            right: Box::new(resolve_expr(right, variable_map)?),
        }),
    }
}
