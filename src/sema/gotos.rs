use super::*;
pub fn validate(stage: SemaStage<TypeChecking>) -> Result<SemaStage<GotoValidation>> {
    let valid_declarations = stage
        .program
        .declarations
        .into_iter()
        .map(|d| match d {
            ast::Declaration::FunDecl(f) => {
                let mut label_map = HashMap::new();
                let block = if let Some(b) = f.block {
                    let b = resolve_block(b, &f.name, &mut label_map)?;
                    let b = validate_block(b, &mut label_map)?;
                    Some(b)
                } else {
                    None
                };
                Ok(ast::Declaration::FunDecl(ast::FunDecl { block, ..f }))
            }
            ast::Declaration::VarDecl(v) => Ok(ast::Declaration::VarDecl(v)),
        })
        .collect::<Result<Vec<ast::Declaration>, Error>>()?;

    Ok(SemaStage {
        program: ast::Program {
            declarations: valid_declarations,
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
            let new_name = Rc::new(format!("{func_name}.{name}"));
            ensure!(
                label_map
                    .insert(Rc::clone(&name), Rc::clone(&new_name))
                    .is_none(),
                "Duplicate label \"{name}\" in function \"{func_name}\"."
            );
            Ok(ast::Stmt::Label {
                name: new_name,
                stmt: Box::new(resolve_stmt(*stmt, func_name, label_map)?),
            })
        }
        ast::Stmt::Case { value, stmt, label } => Ok(ast::Stmt::Case {
            value,
            stmt: Box::new(resolve_stmt(*stmt, func_name, label_map)?),
            label,
        }),
        ast::Stmt::Default { label, stmt } => Ok(ast::Stmt::Default {
            label,
            stmt: Box::new(resolve_stmt(*stmt, func_name, label_map)?),
        }),
        ast::Stmt::DoWhile {
            body,
            condition,
            label,
        } => Ok(ast::Stmt::DoWhile {
            body: Box::new(resolve_stmt(*body, func_name, label_map)?),
            condition,
            label,
        }),
        ast::Stmt::While {
            body,
            condition,
            label,
        } => Ok(ast::Stmt::While {
            body: Box::new(resolve_stmt(*body, func_name, label_map)?),
            condition,
            label,
        }),
        ast::Stmt::For {
            init,
            condition,
            post,
            body,
            label,
        } => Ok(ast::Stmt::For {
            init,
            condition,
            post,
            body: Box::new(resolve_stmt(*body, func_name, label_map)?),
            label,
        }),
        ast::Stmt::Switch {
            condition,
            body,
            label,
            cases,
            default,
        } => Ok(ast::Stmt::Switch {
            condition,
            body: Box::new(resolve_stmt(*body, func_name, label_map)?),
            label,
            cases,
            default,
        }),
        _ => Ok(stmt.clone()),
    }
}

fn validate_block(
    block: ast::Block,
    label_map: &mut HashMap<Rc<String>, Rc<String>>,
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
    label_map: &mut HashMap<Rc<String>, Rc<String>>,
) -> Result<ast::Stmt> {
    match stmt {
        ast::Stmt::Goto(label) => {
            if let Some(new_label) = label_map.get(&label) {
                Ok(ast::Stmt::Goto(Rc::clone(new_label)))
            } else {
                bail!("Goto label '{label}' does not exist");
            }
        }
        ast::Stmt::Compound(block) => Ok(ast::Stmt::Compound(validate_block(block, label_map)?)),
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
        ast::Stmt::Label { name, stmt } => Ok(ast::Stmt::Label {
            name,
            stmt: Box::new(validate_stmt(*stmt, label_map)?),
        }),
        ast::Stmt::Case { value, stmt, label } => Ok(ast::Stmt::Case {
            value,
            stmt: Box::new(validate_stmt(*stmt, label_map)?),
            label,
        }),
        ast::Stmt::Default { label, stmt } => Ok(ast::Stmt::Default {
            label,
            stmt: Box::new(validate_stmt(*stmt, label_map)?),
        }),
        ast::Stmt::DoWhile {
            body,
            condition,
            label,
        } => Ok(ast::Stmt::DoWhile {
            body: Box::new(validate_stmt(*body, label_map)?),
            condition,
            label,
        }),
        ast::Stmt::While {
            body,
            condition,
            label,
        } => Ok(ast::Stmt::While {
            body: Box::new(validate_stmt(*body, label_map)?),
            condition,
            label,
        }),
        ast::Stmt::For {
            init,
            condition,
            post,
            body,
            label,
        } => Ok(ast::Stmt::For {
            init,
            condition,
            post,
            body: Box::new(validate_stmt(*body, label_map)?),
            label,
        }),
        ast::Stmt::Switch {
            condition,
            body,
            label,
            cases,
            default,
        } => Ok(ast::Stmt::Switch {
            condition,
            body: Box::new(validate_stmt(*body, label_map)?),
            label,
            cases,
            default,
        }),
        _ => Ok(stmt.clone()),
    }
}
