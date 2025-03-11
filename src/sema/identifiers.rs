use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct IdentEntry {
    pub from_current_scope: bool,
    pub name: Rc<String>,
    pub has_external_linkage: bool,
}

impl IdentEntry {
    fn new_local(name: Rc<String>) -> Self {
        Self {
            from_current_scope: true,
            name,
            has_external_linkage: false,
        }
    }
    fn new_external(name: Rc<String>) -> Self {
        Self {
            from_current_scope: true,
            name,
            has_external_linkage: true,
        }
    }
    fn from_parent_scope(entry: &Self) -> Self {
        Self {
            from_current_scope: false,
            ..entry.clone()
        }
    }
}

fn make_new_scope(ident_map: &HashMap<Rc<String>, IdentEntry>) -> HashMap<Rc<String>, IdentEntry> {
    ident_map
        .iter()
        .fold(HashMap::new(), |mut map, (key, entry)| {
            map.insert(Rc::clone(key), IdentEntry::from_parent_scope(entry));
            map
        })
}

pub fn validate(stage: SemaStage<Initial>) -> Result<SemaStage<IdentResolution>> {
    let mut ident_map = HashMap::new();
    let mut count = 0;
    let mut unique_name_generator = move |name: &str| -> String {
        let new_name = format!("{name}.{count}");
        count += 1;
        new_name
    };
    let valid_declarations = stage
        .program
        .declarations
        .into_iter()
        .map(|d| match d {
            ast::Declaration::FunDecl(f) => Ok(ast::Declaration::FunDecl(resolve_fun_decl(
                f,
                &mut ident_map,
                &mut unique_name_generator,
            )?)),
            ast::Declaration::VarDecl(v) => Ok(ast::Declaration::VarDecl(
                resolve_file_scope_var_decl(v, &mut ident_map),
            )),
        })
        .collect::<Result<Vec<ast::Declaration>, Error>>()?;

    Ok(SemaStage {
        program: ast::Program {
            declarations: valid_declarations,
        },
        symbols: stage.symbols,
        stage: PhantomData::<IdentResolution>,
    })
}

fn validate_block(
    block: ast::Block,
    ident_map: &mut HashMap<Rc<String>, IdentEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::Block> {
    let valid_items =
        block
            .into_items()
            .into_iter()
            .try_fold(Vec::new(), |mut items, block_item| {
                items.push(validate_blockitem(block_item, ident_map, make_temporary)?);
                Ok::<Vec<ast::BlockItem>, anyhow::Error>(items)
            })?;
    Ok(ast::Block(valid_items))
}

fn resolve_local_var_decl(
    decl: ast::VarDecl,
    ident_map: &mut HashMap<Rc<String>, IdentEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::VarDecl> {
    if let Some(prev_entry) = ident_map.get(&decl.name) {
        if prev_entry.from_current_scope && !(prev_entry.has_external_linkage
                && decl.storage_class == Some(ast::StorageClass::Extern)) {
            bail!("Conflicting local declaration '{}' ", decl.name);
        }
    }
    if let Some(ast::StorageClass::Extern) = decl.storage_class {
        _ = ident_map.insert(
            Rc::clone(&decl.name),
            IdentEntry::new_external(Rc::clone(&decl.name)),
        );
        Ok(decl)
    } else {
        let unique_name = resolve_automatic(decl.name, ident_map, make_temporary)?;
        let init = match decl.init {
            Some(expr) => Some(resolve_expr(expr, ident_map)?),
            None => None,
        };

        Ok(ast::VarDecl {
            name: unique_name,
            init,
            ..decl
        })
    }
}

fn resolve_file_scope_var_decl(
    decl: ast::VarDecl,
    ident_map: &mut HashMap<Rc<String>, IdentEntry>,
) -> ast::VarDecl {
    _ = ident_map.insert(
        Rc::clone(&decl.name),
        IdentEntry::new_external(Rc::clone(&decl.name)),
    );
    decl
}

fn resolve_automatic(
    name: Rc<String>,
    ident_map: &mut HashMap<Rc<String>, IdentEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<Rc<String>> {
    if ident_map
        .get(&name)
        .is_some_and(|entry| entry.from_current_scope)
    {
        bail!("Duplicate local declaration '{}'", name);
    }
    let unique_name = Rc::new(make_temporary(&name));
    ident_map.insert(
        Rc::clone(&name),
        IdentEntry::new_local(Rc::clone(&unique_name)),
    );
    Ok(unique_name)
}

fn resolve_fun_decl(
    decl: ast::FunDecl,
    ident_map: &mut HashMap<Rc<String>, IdentEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::FunDecl> {
    // Reject a duplicate declaration if it is from the current scope but
    // doesn't have external linkage, since it is a local variable
    if ident_map
        .get(&decl.name)
        .is_some_and(|entry| entry.from_current_scope && !entry.has_external_linkage)
    {
        bail!(
            "Duplicate declaration for variable \"{}\" and function \"{}\"",
            decl.name,
            decl.name
        );
    }
    ident_map.insert(
        Rc::clone(&decl.name),
        IdentEntry::new_external(Rc::clone(&decl.name)),
    );
    let mut inner_map = make_new_scope(ident_map);
    let new_params = decl
        .signature
        .into_iter()
        .map(|(typ, name)| {
            // Resolve automatic variables for parameter names
            if let Some(name) = name {
                resolve_automatic(Rc::clone(&name), &mut inner_map, make_temporary)
                    .map(|name| (typ, Some(name)))
            } else {
                Ok((typ, None))
            }
        })
        .collect::<Result<Vec<(ast::Type, Option<Rc<String>>)>, Error>>()?;
    let body = if let Some(body) = decl.block {
        let items = body
            .into_items()
            .into_iter()
            .map(|item| validate_blockitem(item, &mut inner_map, make_temporary))
            .collect::<Result<Vec<ast::BlockItem>, Error>>()?;
        Some(ast::Block(items))
    } else {
        None
    };
    Ok(ast::FunDecl {
        signature: new_params,
        block: body,
        ..decl
    })
}

fn resolve_decl(
    decl: ast::Declaration,
    ident_map: &mut HashMap<Rc<String>, IdentEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::Declaration> {
    match decl {
        ast::Declaration::VarDecl(decl) => {
            resolve_local_var_decl(decl, ident_map, make_temporary).map(ast::Declaration::VarDecl)
        }
        ast::Declaration::FunDecl(decl) => {
            resolve_fun_decl(decl, ident_map, make_temporary).map(ast::Declaration::FunDecl)
        }
    }
}

fn validate_blockitem(
    item: ast::BlockItem,
    ident_map: &mut HashMap<Rc<String>, IdentEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::BlockItem> {
    match item {
        ast::BlockItem::Stmt(stmt) => Ok(ast::BlockItem::Stmt(resolve_stmt(
            stmt,
            ident_map,
            make_temporary,
        )?)),
        ast::BlockItem::Decl(decl) => Ok(ast::BlockItem::Decl(resolve_decl(
            decl,
            ident_map,
            make_temporary,
        )?)),
    }
}

fn resolve_stmt(
    stmt: ast::Stmt,
    ident_map: &HashMap<Rc<String>, IdentEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::Stmt> {
    match stmt {
        ast::Stmt::Return(Some(expr)) => {
            Ok(ast::Stmt::Return(Some(resolve_expr(expr, ident_map)?)))
        }
        ast::Stmt::Return(None) => Ok(ast::Stmt::Return(None)),
        ast::Stmt::Expr(expr) => Ok(ast::Stmt::Expr(resolve_expr(expr, ident_map)?)),
        ast::Stmt::If {
            condition,
            then,
            r#else,
        } => Ok(ast::Stmt::If {
            condition: resolve_expr(condition, ident_map)?,
            then: Box::new(resolve_stmt(*then, ident_map, make_temporary)?),
            r#else: match r#else {
                Some(r#else) => Some(Box::new(resolve_stmt(*r#else, ident_map, make_temporary)?)),
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
            condition: resolve_expr(condition, ident_map)?,
            body: Box::new(resolve_stmt(*body, ident_map, make_temporary)?),
            label,
        }),
        ast::Stmt::DoWhile {
            body,
            condition,
            label,
        } => Ok(ast::Stmt::DoWhile {
            condition: resolve_expr(condition, ident_map)?,
            body: Box::new(resolve_stmt(*body, ident_map, make_temporary)?),
            label,
        }),
        ast::Stmt::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let mut new_map = make_new_scope(ident_map);
            let init = match init {
                ast::ForInit::Decl(decl) => {
                    ast::ForInit::Decl(resolve_local_var_decl(decl, &mut new_map, make_temporary)?)
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
            let mut new_map = make_new_scope(ident_map);
            let block = validate_block(block, &mut new_map, make_temporary)?;
            Ok(ast::Stmt::Compound(block))
        }
        ast::Stmt::Goto(label) => Ok(ast::Stmt::Goto(label)),
        ast::Stmt::Label { name, stmt } => Ok(ast::Stmt::Label {
            name,
            stmt: Box::new(resolve_stmt(*stmt, ident_map, make_temporary)?),
        }),
        ast::Stmt::Default { stmt, label } => Ok(ast::Stmt::Default {
            stmt: Box::new(resolve_stmt(*stmt, ident_map, make_temporary)?),
            label,
        }),
        ast::Stmt::Switch {
            condition,
            body,
            label,
            cases,
            default,
        } => Ok(ast::Stmt::Switch {
            condition: resolve_expr(condition, ident_map)?,
            body: Box::new(resolve_stmt(*body, ident_map, make_temporary)?),
            label,
            cases,
            default,
        }),
        ast::Stmt::Case { value, stmt, label } => Ok(ast::Stmt::Case {
            value: resolve_expr(value, ident_map)?,
            stmt: Box::new(resolve_stmt(*stmt, ident_map, make_temporary)?),
            label,
        }),
    }
}

fn resolve_expr(expr: ast::Expr, ident_map: &HashMap<Rc<String>, IdentEntry>) -> Result<ast::Expr> {
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
                lvalue: Box::new(resolve_expr(lvalue, ident_map)?),
                rvalue: Box::new(resolve_expr(*rvalue, ident_map)?),
            })
        }
        ast::Expr::Var(var) => {
            if let Some(IdentEntry { name, .. }) = ident_map.get(&var) {
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
                    expr: Box::new(resolve_expr(*expr, ident_map)?),
                })
            } else {
                bail!("Op {:?} is invalid for expression {:?}", op, expr)
            }
        }
        ast::Expr::Binary { op, left, right } => Ok(ast::Expr::Binary {
            op,
            left: Box::new(resolve_expr(*left, ident_map)?),
            right: Box::new(resolve_expr(*right, ident_map)?),
        }),
        ast::Expr::Conditional {
            condition,
            then,
            r#else,
        } => Ok(ast::Expr::Conditional {
            condition: Box::new(resolve_expr(*condition, ident_map)?),
            then: Box::new(resolve_expr(*then, ident_map)?),
            r#else: Box::new(resolve_expr(*r#else, ident_map)?),
        }),
        ast::Expr::FunCall { name, args } => {
            // Replace the name of the function with whatever is there in
            // the ident map. If a local variable is defined shadowing the
            // function, then this will return its unique name.
            let name = if let Some(IdentEntry { name, .. }) = ident_map.get(&name) {
                Rc::clone(name)
            } else {
                bail!("Cannot call unknown identifier {}.", name);
            };
            let valid_args = args
                .into_iter()
                .map(|a| resolve_expr(a, ident_map))
                .collect::<Result<Vec<ast::Expr>, Error>>()?;
            Ok(ast::Expr::FunCall {
                name,
                args: valid_args,
            })
        }
    }
}
