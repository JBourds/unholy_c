use std::{collections::HashMap, marker::PhantomData, rc::Rc};

use crate::ast;
use anyhow::{bail, Result};

// Frame entry consists of a bool for whether the variable value is from the
// current scope as well as the string variable name
type FrameEntry = (bool, Rc<String>);

enum Initial {}
enum VariableResolution {}
enum GotoValidation {}
enum LoopLabelling {}
enum SwitchLabelling {}
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
    let stage = switch::validate(stage)?;
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

    fn resolve_decl(
        decl: ast::Declaration,
        variable_map: &mut HashMap<Rc<String>, FrameEntry>,
        make_temporary: &mut impl FnMut(&str) -> String,
    ) -> Result<ast::Declaration> {
        if variable_map
            .get(&decl.name)
            .is_some_and(|(from_this_frame, _)| *from_this_frame)
        {
            bail!("Duplicate variable declaration '{}'", decl.name);
        }
        let unique_name = Rc::new(make_temporary(&decl.name));
        variable_map.insert(Rc::clone(&decl.name), (true, Rc::clone(&unique_name)));

        let init = match decl.init {
            Some(expr) => Some(resolve_expr(expr, variable_map)?),
            None => None,
        };

        Ok(ast::Declaration {
            name: unique_name,
            init,
            ..decl
        })
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
            ast::BlockItem::Decl(decl) => Ok(ast::BlockItem::Decl(resolve_decl(
                decl,
                variable_map,
                make_temporary,
            )?)),
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
            ast::Stmt::Break(label) => Ok(ast::Stmt::Break(label)),
            ast::Stmt::Continue(label) => Ok(ast::Stmt::Continue(label)),
            ast::Stmt::While {
                condition,
                body,
                label,
            } => Ok(ast::Stmt::While {
                condition: resolve_expr(condition, variable_map)?,
                body: Box::new(resolve_stmt(*body, variable_map, make_temporary)?),
                label,
            }),
            ast::Stmt::DoWhile {
                body,
                condition,
                label,
            } => Ok(ast::Stmt::DoWhile {
                condition: resolve_expr(condition, variable_map)?,
                body: Box::new(resolve_stmt(*body, variable_map, make_temporary)?),
                label,
            }),
            ast::Stmt::For {
                init,
                condition,
                post,
                body,
                label,
            } => {
                let mut new_map = make_new_scope(variable_map);
                let init = match init {
                    ast::ForInit::Decl(decl) => {
                        ast::ForInit::Decl(resolve_decl(decl, &mut new_map, make_temporary)?)
                    }
                    ast::ForInit::Expr(Some(expr)) => {
                        ast::ForInit::Expr(Some(resolve_expr(expr, &new_map)?))
                    }
                    init => init,
                };
                let condition = if let Some(expr) = condition {
                    Some(resolve_expr(expr, &new_map)?)
                } else {
                    None
                };
                let post = if let Some(expr) = post {
                    Some(resolve_expr(expr, &new_map)?)
                } else {
                    None
                };
                Ok(ast::Stmt::For {
                    init,
                    condition,
                    post,
                    body: Box::new(resolve_stmt(*body, &new_map, make_temporary)?),
                    label,
                })
            }
            ast::Stmt::Null => Ok(ast::Stmt::Null),
            ast::Stmt::Compound(block) => {
                let mut new_map = make_new_scope(variable_map);
                let block = validate_block(block, &mut new_map, make_temporary)?;
                Ok(ast::Stmt::Compound(block))
            }
            ast::Stmt::Goto(label) => Ok(ast::Stmt::Goto(label)),
            label @ ast::Stmt::Label { .. } => Ok(label),
            ast::Stmt::Default(label) => Ok(ast::Stmt::Default(label)),
            ast::Stmt::Switch {
                condition,
                body,
                label,
                cases,
                default,
            } => Ok(ast::Stmt::Switch {
                condition: resolve_expr(condition, variable_map)?,
                body: Box::new(resolve_stmt(*body, variable_map, make_temporary)?),
                label,
                cases,
                default,
            }),
            ast::Stmt::Case { value, body, label } => Ok(ast::Stmt::Case {
                value: resolve_expr(value, variable_map)?,
                body: Box::new(resolve_stmt(*body, variable_map, make_temporary)?),
                label,
            }),
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
        for block_item in block.into_items().into_iter() {
            let fixed = match block_item {
                ast::BlockItem::Stmt(stmt) => {
                    ast::BlockItem::Stmt(resolve_stmt(stmt, func_name, label_map)?)
                }
                _ => block_item,
            };
            block_items.push(fixed);
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
            ast::Stmt::Label { name, stmt } => {
                if label_map.contains_key(&name) {
                    bail!("Duplicate labels {name}");
                } else {
                    let new_name = Rc::new(format!("{func_name}.{name}"));
                    label_map.insert(Rc::clone(&name), Rc::clone(&new_name));
                    Ok(ast::Stmt::Label {
                        name: new_name,
                        stmt,
                    })
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

mod switch {
    use std::ops::{BitAnd, BitOr, BitXor};

    use anyhow::{ensure, Context};
    use ast::Literal;

    use super::*;

    struct SwitchContext {
        name: Option<Rc<String>>,
        label_map: HashMap<ast::Literal, Rc<String>>,
        active: bool,
        default: Option<Rc<String>>,
    }

    impl SwitchContext {
        pub fn new() -> Self {
            Self {
                name: None,
                label_map: HashMap::new(),
                active: false,
                default: None,
            }
        }

        pub fn with_name(name: Rc<String>) -> Self {
            Self {
                name: Some(name),
                label_map: HashMap::new(),
                active: true,
                default: None,
            }
        }

        pub fn bound(&self) -> bool {
            self.name.is_some()
        }
    }

    pub fn validate(stage: SemaStage<GotoValidation>) -> Result<SemaStage<SwitchLabelling>> {
        let SemaStage { program, .. } = stage;
        Ok(SemaStage {
            program: ast::Program {
                function: resolve_function(program.function)?,
            },
            stage: PhantomData::<SwitchLabelling>,
        })
    }

    fn resolve_function(function: ast::Function) -> Result<ast::Function> {
        if let Some(block) = function.block {
            let mut count = 0;
            let mut unique_name_generator = |name: &str| -> String {
                let new_name = format!("{}.{name}.{count}", function.name);
                count += 1;
                new_name
            };
            Ok(ast::Function {
                block: Some(resolve_block(
                    block,
                    &mut SwitchContext::new(),
                    &mut unique_name_generator,
                )?),
                ..function
            })
        } else {
            Ok(function)
        }
    }

    fn resolve_block(
        block: ast::Block,
        switch_context: &mut SwitchContext,
        make_label: &mut impl FnMut(&str) -> String,
    ) -> Result<ast::Block> {
        let mut resolved_block_items = Vec::with_capacity(block.items().len());
        for item in block.into_items().into_iter() {
            match item {
                ast::BlockItem::Stmt(stmt) => {
                    resolved_block_items.push(ast::BlockItem::Stmt(resolve_stmt(
                        stmt,
                        switch_context,
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
        switch_context: &mut SwitchContext,
        make_label: &mut impl FnMut(&str) -> String,
    ) -> Result<ast::Stmt> {
        match (stmt, switch_context.active) {
            (ast::Stmt::Break(label), false) => Ok(ast::Stmt::Break(label)),
            (ast::Stmt::Break(None), true) => {
                Ok(ast::Stmt::Break(Some(switch_context.name.clone().unwrap())))
            }
            (ast::Stmt::Break(Some(_)), _) => unreachable!(),
            (ast::Stmt::Default(label), _) => {
                ensure!(label.is_none());
                let name = match switch_context.name.clone() {
                    Some(name) => name,
                    None => bail!("Encountered default statement outside of switch statement"),
                };
                let label = Rc::new(format!("{name}.case-default"));

                if switch_context.default.is_some() {
                    bail!("Duplicate default statement");
                }
                switch_context.default = Some(Rc::clone(&label));
                Ok(ast::Stmt::Default(Some(label)))
            }
            (ast::Stmt::Case { value, body, label }, _) => {
                ensure!(label.is_none());
                let const_value = const_eval(value.clone())?;
                let name = match switch_context.name.clone() {
                    Some(name) => name,
                    None => bail!("Encountered case statement outside of switch statement"),
                };
                let label = Rc::new(format!("{name}.case{const_value}"));
                if switch_context.label_map.contains_key(&const_value) {
                    bail!("Duplicate case statement: {const_value}");
                }
                ensure!(switch_context
                    .label_map
                    .insert(const_value, Rc::clone(&label))
                    .is_none());
                let body = resolve_stmt(*body, switch_context, make_label)?;
                Ok(ast::Stmt::Case {
                    value: ast::Expr::Literal(const_value),
                    body: Box::new(body),
                    label: Some(label),
                })
            }
            (
                ast::Stmt::Switch {
                    condition,
                    body,
                    label: None,
                    cases: None,
                    default: None,
                },
                _,
            ) => {
                let label = Rc::new(make_label("switch"));
                let mut switch_context = SwitchContext::with_name(Rc::clone(&label));
                let body = resolve_stmt(*body, &mut switch_context, make_label)?;

                let cases = switch_context
                    .label_map
                    .drain()
                    .collect::<Vec<(Literal, Rc<String>)>>();

                Ok(ast::Stmt::Switch {
                    condition,
                    body: Box::new(body),
                    label: Some(label),
                    cases: Some(cases),
                    default: switch_context.default,
                })
            }
            (ast::Stmt::Switch { .. }, _) => unreachable!(),
            (ast::Stmt::Compound(block), _) => Ok(ast::Stmt::Compound(resolve_block(
                block,
                switch_context,
                make_label,
            )?)),
            (
                ast::Stmt::If {
                    condition,
                    then,
                    r#else,
                },
                _,
            ) => {
                let then = Box::new(resolve_stmt(*then, switch_context, make_label)?);
                let r#else = match r#else {
                    Some(r#else) => {
                        Some(Box::new(resolve_stmt(*r#else, switch_context, make_label)?))
                    }
                    None => None,
                };
                Ok(ast::Stmt::If {
                    condition,
                    then,
                    r#else,
                })
            }
            (
                ast::Stmt::While {
                    condition,
                    body,
                    label,
                },
                active,
            ) => {
                switch_context.active = false;
                let body = resolve_stmt(*body, switch_context, make_label)?;
                switch_context.active = active;
                Ok(ast::Stmt::While {
                    condition,
                    body: Box::new(body),
                    label,
                })
            }
            (
                ast::Stmt::DoWhile {
                    body,
                    condition,
                    label,
                },
                active,
            ) => {
                switch_context.active = false;
                let body = resolve_stmt(*body, switch_context, make_label)?;
                switch_context.active = active;
                Ok(ast::Stmt::DoWhile {
                    body: Box::new(body),
                    condition,
                    label,
                })
            }
            (
                ast::Stmt::For {
                    init,
                    condition,
                    post,
                    body,
                    label,
                },
                active,
            ) => {
                switch_context.active = false;
                let body = resolve_stmt(*body, switch_context, make_label)?;
                switch_context.active = active;
                Ok(ast::Stmt::For {
                    init,
                    condition,
                    post,
                    body: Box::new(body),
                    label,
                })
            }
            (stmt, _) => Ok(stmt),
        }
    }

    fn const_eval(expr: ast::Expr) -> Result<ast::Literal> {
        match expr {
            ast::Expr::Var(_) => bail!("Variables cannot be constant evaluated"),
            ast::Expr::Assignment { .. } => {
                bail!("Assignment cannot be constant evaluated")
            }
            ast::Expr::Literal(literal) => Ok(literal),
            ast::Expr::Unary { op, expr } => {
                const_eval_unary(op, *expr).context("UnaryOp const eval failed")
            }
            ast::Expr::Binary { op, left, right } => {
                const_eval_binary(op, *left, *right).context("BinaryOp const eval failed")
            }
            ast::Expr::Conditional {
                condition,
                then,
                r#else,
            } => {
                let condition = const_eval(*condition)?;
                let val = match condition {
                    ast::Literal::Int(v) => v,
                };
                if val > 0 {
                    const_eval(*then)
                } else {
                    const_eval(*r#else)
                }
            }
        }
    }

    fn const_eval_unary(op: ast::UnaryOp, expr: ast::Expr) -> Result<ast::Literal> {
        let literal = const_eval(expr)?;
        let val = match literal {
            ast::Literal::Int(v) => v,
        };
        let new_val = match op {
            ast::UnaryOp::Complement => !val,
            ast::UnaryOp::Negate => -val,
            ast::UnaryOp::Not => !val,
            ast::UnaryOp::PreInc
            | ast::UnaryOp::PreDec
            | ast::UnaryOp::PostInc
            | ast::UnaryOp::PostDec => bail!("{op:#?} is not allowed in const expresion"),
        };
        Ok(ast::Literal::Int(new_val))
    }

    fn const_eval_binary(
        op: ast::BinaryOp,
        left: ast::Expr,
        right: ast::Expr,
    ) -> Result<ast::Literal> {
        match op {
            ast::BinaryOp::And => {
                let left_val = get_value(const_eval(left)?);
                if left_val == 0 {
                    return Ok(ast::Literal::Int(0));
                }
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(if right_val > 0 { 1 } else { 0 }))
            }
            ast::BinaryOp::Or => {
                let left_val = get_value(const_eval(left)?);
                if left_val > 0 {
                    return Ok(ast::Literal::Int(1));
                }
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(if right_val > 0 { 1 } else { 0 }))
            }
            ast::BinaryOp::Add => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(left_val.wrapping_add(right_val)))
            }
            ast::BinaryOp::Subtract => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(left_val.wrapping_sub(right_val)))
            }
            ast::BinaryOp::Multiply => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(left_val.wrapping_mul(right_val)))
            }
            ast::BinaryOp::Divide => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(left_val.wrapping_div(right_val)))
            }
            ast::BinaryOp::Remainder => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(left_val.wrapping_rem(right_val)))
            }
            ast::BinaryOp::BitAnd => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(left_val.bitand(right_val)))
            }
            ast::BinaryOp::BitOr => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(left_val.bitor(right_val)))
            }
            ast::BinaryOp::Xor => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(left_val.bitxor(right_val)))
            }
            ast::BinaryOp::LShift => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(left_val << right_val))
            }
            ast::BinaryOp::RShift => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(left_val << right_val))
            }
            ast::BinaryOp::Equal => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(if left_val == right_val { 1 } else { 0 }))
            }
            ast::BinaryOp::NotEqual => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int(if left_val != right_val { 1 } else { 0 }))
            }
            ast::BinaryOp::LessThan => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int((left_val < right_val) as i32))
            }
            ast::BinaryOp::LessOrEqual => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int((left_val <= right_val) as i32))
            }
            ast::BinaryOp::GreaterThan => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int((left_val > right_val) as i32))
            }
            ast::BinaryOp::GreaterOrEqual => {
                let left_val = get_value(const_eval(left)?);
                let right_val = get_value(const_eval(right)?);
                Ok(ast::Literal::Int((left_val >= right_val) as i32))
            }
            ast::BinaryOp::Assign
            | ast::BinaryOp::AddAssign
            | ast::BinaryOp::SubAssign
            | ast::BinaryOp::MultAssign
            | ast::BinaryOp::DivAssign
            | ast::BinaryOp::ModAssign
            | ast::BinaryOp::AndAssign
            | ast::BinaryOp::OrAssign
            | ast::BinaryOp::XorAssign
            | ast::BinaryOp::LShiftAssign
            | ast::BinaryOp::RShiftAssign => bail!("Assignment cannot happen in const expression"),
            ast::BinaryOp::Ternary => {
                unreachable!("Ternary expressions are not true binary operands.")
            }
        }
    }

    fn get_value(lit: ast::Literal) -> i32 {
        match lit {
            ast::Literal::Int(v) => v,
        }
    }
}

mod loops {
    use super::*;

    pub fn validate(stage: SemaStage<SwitchLabelling>) -> Result<SemaStage<LoopLabelling>> {
        let SemaStage { program, .. } = stage;
        Ok(SemaStage {
            program: ast::Program {
                function: resolve_function(program.function)?,
            },
            stage: PhantomData::<LoopLabelling>,
        })
    }

    fn resolve_function(function: ast::Function) -> Result<ast::Function> {
        if let Some(block) = function.block {
            let mut count = 0;
            let mut unique_name_generator = |name: &str| -> String {
                let new_name = format!("{}.{name}.{count}", Rc::clone(&function.name));
                count += 1;
                new_name
            };
            Ok(ast::Function {
                block: Some(resolve_block(block, None, &mut unique_name_generator)?),
                ..function
            })
        } else {
            Ok(function)
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
        match (stmt, loop_label) {
            (ast::Stmt::Break(_), None) => {
                bail!("Cannot use 'break' outside of a loop or switch statement.")
            }
            // Break has not been associated with a loop label yet
            (ast::Stmt::Break(None), Some(label)) => Ok(ast::Stmt::Break(Some(label))),
            // Break is already associated with a switch case, don't change anything
            (ast::Stmt::Break(Some(label)), _) => Ok(ast::Stmt::Break(Some(label))),
            (ast::Stmt::Continue(_), None) => bail!("Cannot use 'continue' outside of a loop."),
            (ast::Stmt::Continue(None), Some(label)) => Ok(ast::Stmt::Continue(Some(label))),
            (
                ast::Stmt::While {
                    condition,
                    body,
                    label: None,
                },
                _,
            ) => {
                let label = Rc::new(make_label("while"));
                Ok(ast::Stmt::While {
                    condition,
                    body: Box::new(resolve_stmt(*body, Some(Rc::clone(&label)), make_label)?),
                    label: Some(label),
                })
            }
            (
                ast::Stmt::DoWhile {
                    body,
                    condition,
                    label: None,
                },
                _,
            ) => {
                let label = Rc::new(make_label("do_while"));
                Ok(ast::Stmt::DoWhile {
                    condition,
                    body: Box::new(resolve_stmt(*body, Some(Rc::clone(&label)), make_label)?),
                    label: Some(label),
                })
            }
            (
                ast::Stmt::For {
                    init,
                    condition,
                    post,
                    body,
                    label: None,
                },
                _,
            ) => {
                let label = Rc::new(make_label("for"));
                Ok(ast::Stmt::For {
                    init,
                    condition,
                    post,
                    body: Box::new(resolve_stmt(*body, Some(Rc::clone(&label)), make_label)?),
                    label: Some(label),
                })
            }
            (ast::Stmt::While { label: Some(_), .. }, _) => unreachable!(),
            (ast::Stmt::DoWhile { label: Some(_), .. }, _) => unreachable!(),
            (ast::Stmt::For { label: Some(_), .. }, _) => unreachable!(),
            (ast::Stmt::Compound(block), label) => Ok(ast::Stmt::Compound(resolve_block(
                block, label, make_label,
            )?)),
            (
                ast::Stmt::If {
                    condition,
                    then,
                    r#else,
                },
                label,
            ) => {
                let then = Box::new(resolve_stmt(*then, label.clone(), make_label)?);
                let r#else = if let Some(r#else) = r#else {
                    Some(Box::new(resolve_stmt(*r#else, label, make_label)?))
                } else {
                    None
                };
                Ok(ast::Stmt::If {
                    condition,
                    then,
                    r#else,
                })
            }
            (stmt, _) => Ok(stmt),
        }
    }
}
