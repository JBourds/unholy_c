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
}

pub fn validate(stage: SemaStage<GotoValidation>) -> Result<SemaStage<SwitchLabelling>> {
    let valid_declarations = stage
        .program
        .declarations
        .into_iter()
        .map(|d| match d {
            ast::Declaration::FunDecl(f) => Ok(ast::Declaration::FunDecl(resolve_function(f)?)),
            ast::Declaration::VarDecl(..) => unimplemented!(),
        })
        .collect::<Result<Vec<ast::Declaration>, Error>>()?;
    Ok(SemaStage {
        program: ast::Program {
            declarations: valid_declarations,
        },
        stage: PhantomData::<SwitchLabelling>,
    })
}

fn resolve_function(function: ast::FunDecl) -> Result<ast::FunDecl> {
    if let Some(block) = function.block {
        let mut count = 0;
        let mut unique_name_generator = |name: &str| -> String {
            let new_name = format!("{}.{name}.{count}", function.name);
            count += 1;
            new_name
        };
        Ok(ast::FunDecl {
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
        (ast::Stmt::Default { label, stmt }, _) => {
            ensure!(label.is_none());
            let name = match switch_context.name.clone() {
                Some(name) => name,
                None => bail!("Encountered default statement outside of switch statement"),
            };
            let label = Rc::new(format!("{name}.case.default"));

            if switch_context.default.is_some() {
                bail!("Duplicate default statement");
            }
            switch_context.default = Some(Rc::clone(&label));
            let stmt = Box::new(resolve_stmt(*stmt, switch_context, make_label)?);
            Ok(ast::Stmt::Default {
                label: Some(label),
                stmt,
            })
        }
        (ast::Stmt::Case { value, stmt, label }, _) => {
            ensure!(label.is_none());
            let const_value = const_eval(value)?;
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
            let stmt = resolve_stmt(*stmt, switch_context, make_label)?;
            Ok(ast::Stmt::Case {
                value: ast::Expr::Literal(const_value),
                stmt: Box::new(stmt),
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
            let condition = if let Ok(cond) = const_eval(condition.clone()) {
                ast::Expr::Literal(cond)
            } else {
                condition
            };

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
                Some(r#else) => Some(Box::new(resolve_stmt(*r#else, switch_context, make_label)?)),
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
            let ast::Literal::Int(val) = condition;
            if val > 0 {
                const_eval(*then)
            } else {
                const_eval(*r#else)
            }
        }
        _ => unimplemented!(),
    }
}

fn const_eval_unary(op: ast::UnaryOp, expr: ast::Expr) -> Result<ast::Literal> {
    let literal = const_eval(expr)?;
    let ast::Literal::Int(val) = literal;
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

fn const_eval_binary(op: ast::BinaryOp, left: ast::Expr, right: ast::Expr) -> Result<ast::Literal> {
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
