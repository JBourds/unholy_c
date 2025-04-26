use anyhow::ensure;
use ast::Constant;

use super::*;

struct SwitchContext {
    name: Option<Rc<String>>,
    label_map: HashMap<ast::Constant, Rc<String>>,
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
            ast::Declaration::VarDecl(v) => Ok(ast::Declaration::VarDecl(v)),
        })
        .collect::<Result<Vec<ast::Declaration>, Error>>()?;
    Ok(SemaStage {
        program: ast::Program {
            declarations: valid_declarations,
        },
        symbols: stage.symbols,
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
            let const_value = const_eval::eval(value)?;
            let name = match switch_context.name.clone() {
                Some(name) => name,
                None => bail!("Encountered case statement outside of switch statement"),
            };
            let label = Rc::new(format!("{name}.case{const_value}"));
            if switch_context.label_map.contains_key(&const_value) {
                bail!("Duplicate case statement: {const_value}");
            }
            ensure!(
                switch_context
                    .label_map
                    .insert(const_value, Rc::clone(&label))
                    .is_none()
            );
            let stmt = resolve_stmt(*stmt, switch_context, make_label)?;
            Ok(ast::Stmt::Case {
                value: ast::Expr::Constant(const_value),
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
                .collect::<Vec<(Constant, Rc<String>)>>();
            let condition = if let Ok(cond) = const_eval::eval(condition.clone()) {
                ast::Expr::Constant(cond)
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
