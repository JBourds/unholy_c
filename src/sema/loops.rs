use super::*;

pub fn validate(stage: SemaStage<SwitchLabelling>) -> Result<SemaStage<LoopLabelling>> {
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
        stage: PhantomData::<LoopLabelling>,
    })
}

fn resolve_function(function: ast::FunDecl) -> Result<ast::FunDecl> {
    if let Some(block) = function.block {
        let mut count = 0;
        let mut unique_name_generator = |name: &str| -> String {
            let new_name = format!("{}.{name}.{count}", Rc::clone(&function.name));
            count += 1;
            new_name
        };
        Ok(ast::FunDecl {
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

/// Function which resolves any unlabeled continue or break statements with
/// their associated function, erroring out if there is not one.
/// Assumes that the previous stage of switch labelling has worked correctly
/// and has not incorrectly labelled any break statements as belonging to
/// a switch statement if they are closer to a loop context.
fn resolve_stmt(
    stmt: ast::Stmt,
    loop_label: Option<Rc<String>>,
    make_label: &mut impl FnMut(&str) -> String,
) -> Result<ast::Stmt> {
    match (stmt, loop_label) {
        (ast::Stmt::Break(None), None) => {
            bail!("Cannot use 'break' outside of a loop or switch statement.")
        }
        (ast::Stmt::Break(None), Some(loop_label)) => Ok(ast::Stmt::Break(Some(loop_label))),
        (ast::Stmt::Break(Some(switch_label)), _) => Ok(ast::Stmt::Break(Some(switch_label))),
        (ast::Stmt::Continue(_), None) => bail!("Cannot use 'continue' outside of a loop."),
        (ast::Stmt::Continue(None), Some(loop_label)) => Ok(ast::Stmt::Continue(Some(loop_label))),
        (
            ast::Stmt::While {
                condition,
                body,
                label: None,
            },
            _,
        ) => {
            let loop_label = Rc::new(make_label("while"));
            Ok(ast::Stmt::While {
                condition,
                body: Box::new(resolve_stmt(
                    *body,
                    Some(Rc::clone(&loop_label)),
                    make_label,
                )?),
                label: Some(loop_label),
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
            let loop_label = Rc::new(make_label("do_while"));
            Ok(ast::Stmt::DoWhile {
                condition,
                body: Box::new(resolve_stmt(
                    *body,
                    Some(Rc::clone(&loop_label)),
                    make_label,
                )?),
                label: Some(loop_label),
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
            let loop_label = Rc::new(make_label("for"));
            Ok(ast::Stmt::For {
                init,
                condition,
                post,
                body: Box::new(resolve_stmt(
                    *body,
                    Some(Rc::clone(&loop_label)),
                    make_label,
                )?),
                label: Some(loop_label),
            })
        }
        (ast::Stmt::While { label: Some(_), .. }, _) => unreachable!(),
        (ast::Stmt::DoWhile { label: Some(_), .. }, _) => unreachable!(),
        (ast::Stmt::For { label: Some(_), .. }, _) => unreachable!(),
        (ast::Stmt::Compound(block), loop_label) => Ok(ast::Stmt::Compound(resolve_block(
            block, loop_label, make_label,
        )?)),
        (
            ast::Stmt::If {
                condition,
                then,
                r#else,
            },
            loop_label,
        ) => {
            let then = Box::new(resolve_stmt(*then, loop_label.clone(), make_label)?);
            let r#else = if let Some(r#else) = r#else {
                Some(Box::new(resolve_stmt(*r#else, loop_label, make_label)?))
            } else {
                None
            };
            Ok(ast::Stmt::If {
                condition,
                then,
                r#else,
            })
        }
        (ast::Stmt::Label { name, stmt }, loop_label) => Ok(ast::Stmt::Label {
            name,
            stmt: Box::new(resolve_stmt(*stmt, loop_label, make_label)?),
        }),
        (ast::Stmt::Default { label, stmt }, loop_label) => Ok(ast::Stmt::Default {
            label,
            stmt: Box::new(resolve_stmt(*stmt, loop_label, make_label)?),
        }),
        (ast::Stmt::Case { value, label, stmt }, loop_label) => Ok(ast::Stmt::Case {
            value,
            label,
            stmt: Box::new(resolve_stmt(*stmt, loop_label, make_label)?),
        }),
        (
            ast::Stmt::Switch {
                condition,
                body,
                label,
                cases,
                default,
            },
            loop_label,
        ) => Ok(ast::Stmt::Switch {
            condition,
            body: Box::new(resolve_stmt(*body, loop_label, make_label)?),
            label,
            cases,
            default,
        }),
        (stmt, _) => Ok(stmt),
    }
}
