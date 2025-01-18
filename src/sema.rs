use std::{collections::HashMap, marker::PhantomData, rc::Rc};

use crate::ast;
use anyhow::{bail, Result};

// Frame entry consists of a bool for whether the variable value is from the
// current scope as well as the string variable name
type FrameEntry = (bool, Rc<String>);

pub enum Initial {}
pub enum VariableResolution {}
pub enum GotoValidation {}
pub enum LoopLabelling {}
pub enum SwitchLabelling {}
pub enum Final {}

pub struct SemaStage<T> {
    pub program: ast::Program,
    stage: PhantomData<T>,
}

pub fn validate(program: ast::Program) -> Result<SemaStage<Final>> {
    let stage = SemaStage {
        program,
        stage: PhantomData::<Initial>,
    };
    let stage = variables::validate(stage)?;
    let stage = gotos::validate(stage)?;
    let stage = loops::validate(stage)?;

    Ok(SemaStage {
        program: stage.program,
        stage: PhantomData::<Final>,
    })
}

mod variables {
    use super::*;

    pub fn validate(stage: SemaStage<Initial>) -> Result<SemaStage<VariableResolution>> {
        let valid_function = validate_function(stage.program.function)?;

        Ok(SemaStage {
            program: ast::Program {
                function: valid_function,
            },
            stage: PhantomData::<VariableResolution>,
        })
    }

    fn validate_function(function: ast::Function) -> Result<ast::Function> {
        let mut variable_map = HashMap::new();
        let mut count = 0;
        let mut unique_name_generator = move |name: &str| -> String {
            let new_name = format!("{name}.{count}");
            count += 1;
            new_name
        };
        let ast::Function {
            ret_t,
            name,
            signature,
            block,
        } = function;

        let block = if let Some(block) = block {
            Some(validate_block(
                block,
                &mut variable_map,
                &mut unique_name_generator,
            )?)
        } else {
            None
        };

        Ok(ast::Function {
            ret_t,
            name,
            signature,
            block,
        })
    }

    fn validate_block(
        block: ast::Block,
        variable_map: &mut HashMap<Rc<String>, FrameEntry>,
        make_temporary: &mut impl FnMut(&str) -> String,
    ) -> Result<ast::Block> {
        let valid_items =
            block
                .into_items()
                .into_iter()
                .try_fold(Vec::new(), |mut items, block_item| {
                    items.push(validate_blockitem(
                        block_item,
                        variable_map,
                        make_temporary,
                    )?);
                    Ok::<Vec<ast::BlockItem>, anyhow::Error>(items)
                })?;
        Ok(ast::Block(valid_items))
    }

    fn validate_blockitem(
        instruction: ast::BlockItem,
        variable_map: &mut HashMap<Rc<String>, FrameEntry>,
        make_temporary: &mut impl FnMut(&str) -> String,
    ) -> Result<ast::BlockItem> {
        match instruction {
            ast::BlockItem::Stmt(stmt) => Ok(ast::BlockItem::Stmt(resolve_stmt(
                stmt,
                variable_map,
                make_temporary,
            )?)),
            ast::BlockItem::Decl(ast::Declaration { typ, name, init }) => {
                if variable_map
                    .get(&name)
                    .is_some_and(|(from_this_frame, _)| *from_this_frame)
                {
                    bail!("Duplicate variable declaration '{name}'");
                }
                let unique_name = Rc::new(make_temporary(&name));
                variable_map.insert(Rc::clone(&name), (true, Rc::clone(&unique_name)));

                let init = match init {
                    Some(expr) => Some(resolve_expr(expr, variable_map)?),
                    None => None,
                };

                Ok(ast::BlockItem::Decl(ast::Declaration {
                    typ,
                    name: unique_name,
                    init,
                }))
            }
        }
    }

    fn resolve_stmt(
        stmt: ast::Stmt,
        variable_map: &HashMap<Rc<String>, FrameEntry>,
        make_temporary: &mut impl FnMut(&str) -> String,
    ) -> Result<ast::Stmt> {
        let make_new_scope = |variable_map: &HashMap<Rc<String>, FrameEntry>| {
            variable_map
                .iter()
                .fold(HashMap::new(), |mut map, (key, (_, var))| {
                    map.insert(Rc::clone(key), (false, Rc::clone(var)));
                    map
                })
        };
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
                then: Box::new(resolve_stmt(*then, variable_map, make_temporary)?),
                r#else: match r#else {
                    Some(r#else) => Some(Box::new(resolve_stmt(
                        *r#else,
                        variable_map,
                        make_temporary,
                    )?)),
                    None => None,
                },
            }),
            ast::Stmt::Break(label) => todo!(),
            ast::Stmt::Continue(label) => todo!(),
            ast::Stmt::While {
                condition,
                body,
                label,
            } => todo!(),
            ast::Stmt::DoWhile {
                body,
                condition,
                label,
            } => todo!(),
            ast::Stmt::For {
                init,
                condition,
                post,
                body,
                label,
            } => todo!(),
            ast::Stmt::Null => Ok(ast::Stmt::Null),
            ast::Stmt::Compound(block) => {
                let mut new_map = make_new_scope(variable_map);
                let block = validate_block(block, &mut new_map, make_temporary)?;
                Ok(ast::Stmt::Compound(block))
            }
            ast::Stmt::Goto(label) => Ok(ast::Stmt::Goto(label)),
            ast::Stmt::Label(label) => Ok(ast::Stmt::Label(label)),
        }
    }

    fn resolve_expr(
        expr: ast::Expr,
        variable_map: &HashMap<Rc<String>, FrameEntry>,
    ) -> Result<ast::Expr> {
        match expr {
            ast::Expr::Assignment { lvalue, rvalue } => {
                let lvalue = match *lvalue {
                    ast::Expr::Var(v) => ast::Expr::Var(v),
                    _ => bail!(
                        "Invalid lvalue '{:?}'",
                        ast::Expr::Assignment { lvalue, rvalue }
                    ),
                };
                Ok(ast::Expr::Assignment {
                    lvalue: Box::new(resolve_expr(lvalue, variable_map)?),
                    rvalue: Box::new(resolve_expr(*rvalue, variable_map)?),
                })
            }
            ast::Expr::Var(var) => {
                if let Some((_, name)) = variable_map.get(&var) {
                    Ok(ast::Expr::Var(Rc::clone(name)))
                } else {
                    bail!("Undeclared variable '{var}'")
                }
            }
            ast::Expr::Literal(lit) => Ok(ast::Expr::Literal(lit)),
            ast::Expr::Unary { op, expr } => {
                if op.is_valid_for(&expr) {
                    Ok(ast::Expr::Unary {
                        op,
                        expr: Box::new(resolve_expr(*expr, variable_map)?),
                    })
                } else {
                    bail!("Op {:?} is invalid for expression {:?}", op, expr)
                }
            }
            ast::Expr::Binary { op, left, right } => Ok(ast::Expr::Binary {
                op,
                left: Box::new(resolve_expr(*left, variable_map)?),
                right: Box::new(resolve_expr(*right, variable_map)?),
            }),
            ast::Expr::Conditional {
                condition,
                then,
                r#else,
            } => Ok(ast::Expr::Conditional {
                condition: Box::new(resolve_expr(*condition, variable_map)?),
                then: Box::new(resolve_expr(*then, variable_map)?),
                r#else: Box::new(resolve_expr(*r#else, variable_map)?),
            }),
        }
    }
}

mod gotos {
    use super::*;
    pub fn validate(stage: SemaStage<VariableResolution>) -> Result<SemaStage<GotoValidation>> {
        let ast::Function {
            ret_t,
            name,
            signature,
            block,
        } = stage.program.function;

        let mut label_map = HashMap::new();
        let block = if let Some(block) = block {
            let block = resolve_block(block, &name, &mut label_map)?;
            let block = validate_block(block, &label_map)?;
            Some(block)
        } else {
            None
        };

        Ok(SemaStage {
            program: ast::Program {
                function: ast::Function {
                    ret_t,
                    name,
                    signature,
                    block,
                },
            },
            stage: PhantomData::<GotoValidation>,
        })
    }

    fn resolve_block(
        block: ast::Block,
        func_name: &Rc<String>,
        label_map: &mut HashMap<Rc<String>, Rc<String>>,
    ) -> Result<ast::Block> {
        let mut block_items = Vec::with_capacity(block.items().len());

        let mut last_thing_was_a_label = (false, false);
        for block_item in block.into_items().into_iter() {
            last_thing_was_a_label = match block_item {
                ast::BlockItem::Stmt(ast::Stmt::Label(_)) => (last_thing_was_a_label.1, true),
                _ => (last_thing_was_a_label.1, false),
            };

            let fixed = match block_item {
                ast::BlockItem::Stmt(stmt) => ast::BlockItem::Stmt(resolve_stmt(
                    stmt,
                    func_name,
                    label_map,
                )?),
                _ if last_thing_was_a_label.0 => bail!("A label most be followed by a statement, not expression. To get around this, add a null statement i.e: (label: ;)"),
                _ => block_item,
            };
            block_items.push(fixed);
        }
        if last_thing_was_a_label.1 {
            bail!("Label must be followed by a statement. To get around this, add a null statement i.e: (label: ;)")
        }

        Ok(ast::Block(block_items))
    }

    fn resolve_stmt(
        stmt: ast::Stmt,
        func_name: &Rc<String>,
        label_map: &mut HashMap<Rc<String>, Rc<String>>,
    ) -> Result<ast::Stmt> {
        match stmt {
            ast::Stmt::Compound(block) => Ok(ast::Stmt::Compound(resolve_block(
                block, func_name, label_map,
            )?)),
            ast::Stmt::If {
                condition,
                then,
                r#else,
            } => Ok(ast::Stmt::If {
                condition: condition.clone(),
                then: Box::new(resolve_stmt(*then, func_name, label_map)?),
                r#else: match r#else {
                    Some(r#else) => Some(Box::new(resolve_stmt(*r#else, func_name, label_map)?)),
                    None => None,
                },
            }),
            ast::Stmt::Label(name) => {
                if label_map.contains_key(&name) {
                    bail!("Duplicate labels {name}");
                } else {
                    let new_name = Rc::new(format!("{func_name}.{name}"));
                    label_map.insert(Rc::clone(&name), Rc::clone(&new_name));
                    Ok(ast::Stmt::Label(new_name))
                }
            }
            _ => Ok(stmt.clone()),
        }
    }

    fn validate_block(
        block: ast::Block,
        label_map: &HashMap<Rc<String>, Rc<String>>,
    ) -> Result<ast::Block> {
        let mut block_items = Vec::with_capacity(block.items().len());

        for block_item in block.into_items().into_iter() {
            let fixed = match block_item {
                ast::BlockItem::Stmt(stmt) => ast::BlockItem::Stmt(validate_stmt(stmt, label_map)?),
                _ => block_item,
            };
            block_items.push(fixed);
        }

        Ok(ast::Block(block_items))
    }

    fn validate_stmt(
        stmt: ast::Stmt,
        label_map: &HashMap<Rc<String>, Rc<String>>,
    ) -> Result<ast::Stmt> {
        match stmt {
            ast::Stmt::Compound(block) => {
                Ok(ast::Stmt::Compound(validate_block(block, label_map)?))
            }
            ast::Stmt::If {
                condition,
                then,
                r#else,
            } => Ok(ast::Stmt::If {
                condition: condition.clone(),
                then: Box::new(validate_stmt(*then, label_map)?),
                r#else: match r#else {
                    Some(r#else) => Some(Box::new(validate_stmt(*r#else, label_map)?)),
                    None => None,
                },
            }),
            ast::Stmt::Goto(label) => {
                if let Some(new_label) = label_map.get(&label) {
                    Ok(ast::Stmt::Goto(Rc::clone(new_label)))
                } else {
                    bail!("Goto label '{label}' does not exist");
                }
            }
            _ => Ok(stmt.clone()),
        }
    }
}

mod loops {
    use super::*;

    pub fn validate(stage: SemaStage<GotoValidation>) -> Result<SemaStage<LoopLabelling>> {
        let SemaStage { program, .. } = stage;
        if let Some(block) = program.function.block {
            let mut count = 0;
            let mut unique_name_generator = move |name: &str| -> String {
                let new_name = format!("{name}.{count}");
                count += 1;
                new_name
            };
            let block = resolve_block(block, None, &mut unique_name_generator)?;
            Ok(SemaStage {
                program: ast::Program {
                    function: ast::Function {
                        block: Some(block),
                        ..program.function
                    },
                },
                stage: PhantomData::<LoopLabelling>,
            })
        } else {
            Ok(SemaStage {
                program,
                stage: PhantomData::<LoopLabelling>,
            })
        }
    }

    fn resolve_block(
        block: ast::Block,
        loop_label: Option<Rc<String>>,
        make_label: &mut impl FnMut(&str) -> String,
    ) -> Result<ast::Block> {
        let mut resolved_block_items = Vec::with_capacity(block.items().len());
        for item in block.into_items().into_iter() {
            match item {
                ast::BlockItem::Stmt(stmt) => {
                    resolved_block_items.push(ast::BlockItem::Stmt(resolve_stmt(
                        stmt,
                        loop_label.clone(),
                        make_label,
                    )?));
                }
                item => {
                    resolved_block_items.push(item);
                }
            }
        }
        Ok(ast::Block(resolved_block_items))
    }

    fn resolve_stmt(
        stmt: ast::Stmt,
        loop_label: Option<Rc<String>>,
        make_label: &mut impl FnMut(&str) -> String,
    ) -> Result<ast::Stmt> {
        Ok(stmt)
    }
}
