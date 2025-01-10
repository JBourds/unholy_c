use std::{collections::HashMap, rc::Rc};

use crate::ast;
use anyhow::{bail, Result};

pub fn validate<'a>(program: &ast::Program<'a>) -> Result<ast::Program<'a>> {
    let valid_function = validate_function(&program.function)?;
    validate_gotos(&ast::Program {
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
        name: function.name,
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
        ast::BlockItem::Decl(ast::Declaration { typ, name, init }) => {
            if variable_map.contains_key(name) {
                bail!("Duplicate variable declaration '{name}'");
            }
            let unique_name = Rc::new(make_temporary(name));
            variable_map.insert(Rc::clone(name), Rc::clone(&unique_name));

            let init = match init {
                Some(expr) => Some(resolve_expr(expr, variable_map)?),
                None => None,
            };

            Ok(ast::BlockItem::Decl(ast::Declaration {
                typ: *typ,
                name: unique_name,
                init,
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
        ast::Stmt::If {
            condition,
            then,
            r#else,
        } => Ok(ast::Stmt::If {
            condition: resolve_expr(condition, variable_map)?,
            then: Box::new(resolve_stmt(then, variable_map)?),
            r#else: r#else
                .as_ref()
                .map(Ok)
                .map(
                    |result_box_stmt: std::result::Result<&Box<ast::Stmt>, anyhow::Error>| {
                        assert!(result_box_stmt.is_ok());
                        resolve_stmt(result_box_stmt.unwrap(), variable_map)
                    },
                )
                .transpose()?
                .map(Box::new),
        }),
        ast::Stmt::Null => Ok(ast::Stmt::Null),
        ast::Stmt::Goto(label) => Ok(ast::Stmt::Goto(Rc::clone(label))),
        ast::Stmt::Label(label) => Ok(ast::Stmt::Label(Rc::clone(label))),
    }
}

fn resolve_expr(
    expr: &ast::Expr,
    variable_map: &HashMap<Rc<String>, Rc<String>>,
) -> Result<ast::Expr> {
    match expr {
        ast::Expr::Assignment { lvalue, rvalue } => {
            match **lvalue {
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
        ast::Expr::Unary { op, expr } => {
            if op.is_valid_for(expr) {
                Ok(ast::Expr::Unary {
                    op: *op,
                    expr: Box::new(resolve_expr(expr, variable_map)?),
                })
            } else {
                bail!("Op {:?} is invalid for expression {:?}", op, expr)
            }
        }
        ast::Expr::Binary { op, left, right } => Ok(ast::Expr::Binary {
            op: *op,
            left: Box::new(resolve_expr(left, variable_map)?),
            right: Box::new(resolve_expr(right, variable_map)?),
        }),
        ast::Expr::Conditional {
            condition,
            then,
            r#else,
        } => Ok(ast::Expr::Conditional {
            condition: Box::new(resolve_expr(condition, variable_map)?),
            then: Box::new(resolve_expr(then, variable_map)?),
            r#else: Box::new(resolve_expr(r#else, variable_map)?),
        }),
    }
}

pub fn validate_gotos<'a>(program: &ast::Program<'a>) -> Result<ast::Program<'a>> {
    let mut label_map = HashMap::new();

    let mut new_blocks = Vec::with_capacity(program.function.items.len());

    let mut last_thing_was_a_label = (false, false);
    for block_item in program.function.items.iter() {
        last_thing_was_a_label = match block_item {
            ast::BlockItem::Stmt(ast::Stmt::Label(_)) => (last_thing_was_a_label.1, true),
            _ => (last_thing_was_a_label.1, false),
        };

        let fixed = match block_item {
            ast::BlockItem::Stmt(stmt) => ast::BlockItem::Stmt(resolve_gotos_stmt(
                program.function.name,
                stmt,
                &mut label_map,
            )?),
            _ if last_thing_was_a_label.0 => bail!("A label most be followed by a statement, not expression. To get around this, add a null statement i.e: (label: ;)"),
            _ => block_item.clone(),
        };
        new_blocks.push(fixed);
    }
    if last_thing_was_a_label.1 {
        bail!("Label must be followed by a statement. To get around this, add a null statement i.e: (label: ;)")
    }

    let mut new_blocks1 = Vec::with_capacity(new_blocks.len());
    for block_item in new_blocks.into_iter() {
        let fixed = match block_item {
            ast::BlockItem::Stmt(stmt) => {
                ast::BlockItem::Stmt(validate_goto_stmt(&stmt, &label_map)?)
            }
            _ => block_item.clone(),
        };
        new_blocks1.push(fixed);
    }

    Ok(ast::Program {
        function: ast::Function {
            ret_t: program.function.ret_t,
            name: program.function.name,
            signature: program.function.signature.clone(),
            items: new_blocks1,
        },
    })
}

pub fn resolve_gotos_stmt(
    func_name: &str,
    stmt: &ast::Stmt,
    label_map: &mut HashMap<Rc<String>, Rc<String>>,
) -> Result<ast::Stmt> {
    match stmt {
        ast::Stmt::If {
            condition,
            then,
            r#else,
        } => Ok(ast::Stmt::If {
            condition: condition.clone(),
            then: Box::new(resolve_gotos_stmt(func_name, then, label_map)?),
            r#else: match r#else {
                Some(r#else) => Some(Box::new(resolve_gotos_stmt(func_name, r#else, label_map)?)),
                None => None,
            },
        }),
        ast::Stmt::Label(name) => {
            if label_map.contains_key(name) {
                bail!("Duplicate labels {name}");
            } else {
                let new_name = Rc::new(format!("{func_name}.{name}"));
                label_map.insert(Rc::clone(name), Rc::clone(&new_name));
                Ok(ast::Stmt::Label(new_name))
            }
        }
        _ => Ok(stmt.clone()),
    }
}

pub fn validate_goto_stmt(
    stmt: &ast::Stmt,
    label_map: &HashMap<Rc<String>, Rc<String>>,
) -> Result<ast::Stmt> {
    match stmt {
        ast::Stmt::If {
            condition,
            then,
            r#else,
        } => Ok(ast::Stmt::If {
            condition: condition.clone(),
            then: Box::new(validate_goto_stmt(then, label_map)?),
            r#else: match r#else {
                Some(r#else) => Some(Box::new(validate_goto_stmt(r#else, label_map)?)),
                None => None,
            },
        }),
        ast::Stmt::Goto(label) => {
            if let Some(new_label) = label_map.get(label) {
                Ok(ast::Stmt::Goto(Rc::clone(new_label)))
            } else {
                bail!("Goto label '{label}' does not exist");
            }
        }
        _ => Ok(stmt.clone()),
    }
}
