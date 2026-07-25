use crate::sema::typechecking::TagType;

use super::*;

use anyhow::Context;

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

#[derive(Clone, Debug, PartialEq)]
pub struct TagEntry {
    pub from_current_scope: bool,
    pub name: Rc<String>,
    pub struct_or_union: TagType,
}

impl TagEntry {
    fn new_local(name: Rc<String>, struct_or_union: TagType) -> Self {
        Self {
            from_current_scope: true,
            name,
            struct_or_union,
        }
    }

    fn from_parent_scope(entry: &Self) -> Self {
        Self {
            from_current_scope: false,
            ..entry.clone()
        }
    }

    fn make_new_scope(tag_map: &HashMap<Rc<String>, Self>) -> HashMap<Rc<String>, Self> {
        tag_map
            .iter()
            .fold(HashMap::new(), |mut map, (key, entry)| {
                map.insert(Rc::clone(key), Self::from_parent_scope(entry));
                map
            })
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
    let mut tag_map = HashMap::new();
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
            ast::Declaration::Fun(f) => Ok(ast::Declaration::Fun(resolve_fun_decl(
                f,
                &mut ident_map,
                &mut tag_map,
                &mut unique_name_generator,
            )?)),
            ast::Declaration::Var(v) => Ok(ast::Declaration::Var(resolve_file_scope_var_decl(
                v,
                &mut ident_map,
                &tag_map,
            )?)),
            ast::Declaration::Struct(decl) => {
                resolve_struct_decl(decl, &mut tag_map, &mut unique_name_generator)
                    .map(ast::Declaration::Struct)
                    .context("failed to resolve_struct_decl")
            }
            ast::Declaration::Union(decl) => {
                resolve_union_decl(decl, &mut tag_map, &mut unique_name_generator)
                    .map(ast::Declaration::Union)
                    .context("failed to resolve_union_decl")
            }
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
    tag_map: &mut HashMap<Rc<String>, TagEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::Block> {
    let valid_items =
        block
            .into_items()
            .into_iter()
            .try_fold(Vec::new(), |mut items, block_item| {
                items.push(validate_blockitem(
                    block_item,
                    ident_map,
                    tag_map,
                    make_temporary,
                )?);
                Ok::<Vec<ast::BlockItem>, anyhow::Error>(items)
            })?;
    Ok(ast::Block(valid_items))
}

fn resolve_local_var_decl(
    decl: ast::VarDecl,
    ident_map: &mut HashMap<Rc<String>, IdentEntry>,
    tag_map: &HashMap<Rc<String>, TagEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::VarDecl> {
    if let Some(prev_entry) = ident_map.get(&decl.name)
        && prev_entry.from_current_scope
        && !(prev_entry.has_external_linkage
            && decl.storage_class == Some(ast::StorageClass::Extern))
    {
        bail!("Conflicting local declaration '{}' ", decl.name);
    }
    let decl = {
        let r#type = resolve_type(decl.r#type, tag_map)
            .context("failed to resolve type for local declaration")?;
        ast::VarDecl { r#type, ..decl }
    };
    if let Some(ast::StorageClass::Extern) = decl.storage_class {
        _ = ident_map.insert(
            Rc::clone(&decl.name),
            IdentEntry::new_external(Rc::clone(&decl.name)),
        );
        Ok(decl)
    } else {
        let unique_name = resolve_automatic(decl.name, ident_map, make_temporary)?;
        let init = match decl.init {
            Some(init) => Some(resolve_init(init, ident_map, tag_map)?),
            None => None,
        };

        Ok(ast::VarDecl {
            name: unique_name,
            init,
            ..decl
        })
    }
}

fn resolve_init(
    init: ast::Initializer,
    ident_map: &mut HashMap<Rc<String>, IdentEntry>,
    tag_map: &HashMap<Rc<String>, TagEntry>,
) -> Result<ast::Initializer> {
    match init {
        ast::Initializer::SingleInit(expr) => Ok(ast::Initializer::SingleInit(
            resolve_expr(*expr, ident_map, tag_map)?.into(),
        )),
        ast::Initializer::CompoundInit(inits) => Ok(ast::Initializer::CompoundInit(
            inits
                .into_iter()
                .map(|i| resolve_init(i, ident_map, tag_map))
                .collect::<Result<Vec<ast::Initializer>>>()?,
        )),
    }
}

fn resolve_file_scope_var_decl(
    decl: ast::VarDecl,
    ident_map: &mut HashMap<Rc<String>, IdentEntry>,
    tag_map: &HashMap<Rc<String>, TagEntry>,
) -> Result<ast::VarDecl> {
    _ = ident_map.insert(
        Rc::clone(&decl.name),
        IdentEntry::new_external(Rc::clone(&decl.name)),
    );
    let r#type = resolve_type(decl.r#type.clone(), tag_map).context(format!(
        "use of type before declaration for vardecl: {decl:?}"
    ))?;

    Ok(ast::VarDecl { r#type, ..decl })
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
    tag_map: &mut HashMap<Rc<String>, TagEntry>,
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
    let decl = ast::FunDecl {
        r#type: resolve_type(decl.r#type, tag_map)?,
        ..decl
    };

    let mut inner_map = make_new_scope(ident_map);
    let mut inner_tag_map = TagEntry::make_new_scope(tag_map);
    let new_params = decl
        .params
        .into_iter()
        .map(|name| {
            // Resolve automatic variables for parameter names
            if let Some(name) = name {
                resolve_automatic(Rc::clone(&name), &mut inner_map, make_temporary)
                    .map(Option::Some)
            } else {
                Ok(None)
            }
        })
        .collect::<Result<Vec<Option<Rc<String>>>, Error>>()?;
    let body = if let Some(body) = decl.block {
        let items = body
            .into_items()
            .into_iter()
            .map(|item| {
                validate_blockitem(item, &mut inner_map, &mut inner_tag_map, make_temporary)
            })
            .collect::<Result<Vec<ast::BlockItem>, Error>>()?;
        Some(ast::Block(items))
    } else {
        None
    };

    Ok(ast::FunDecl {
        params: new_params,
        block: body,
        ..decl
    })
}

fn resolve_struct_decl(
    decl: ast::StructDecl,
    tag_map: &mut HashMap<Rc<String>, TagEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::StructDecl> {
    let unique_tag = match tag_map.get(&decl.tag) {
        Some(entry) if entry.from_current_scope => {
            ensure!(
                entry.struct_or_union == TagType::Struct,
                "cannot use tag {} in current scope since its taken by a union",
                decl.tag
            );
            Rc::clone(&entry.name)
        }
        _ => {
            let tag = Rc::new(make_temporary(&decl.tag));
            let entry = TagEntry::new_local(Rc::clone(&tag), TagType::Struct);
            tag_map.insert(Rc::clone(&decl.tag), entry);
            tag
        }
    };

    let members = decl
        .members
        .into_iter()
        .map(|member| {
            Ok(ast::MemberDecl {
                r#type: resolve_type(member.r#type, tag_map)
                    .context(format!("failed to process {}.{}", decl.tag, member.name,))?,
                ..member
            })
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(ast::StructDecl {
        tag: unique_tag,
        members,
    })
}

fn resolve_union_decl(
    decl: ast::UnionDecl,
    tag_map: &mut HashMap<Rc<String>, TagEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::UnionDecl> {
    let unique_tag = match tag_map.get(&decl.tag) {
        Some(entry) if entry.from_current_scope => {
            ensure!(
                entry.struct_or_union == TagType::Union,
                "cannot use tag {} in current scope since its taken by a struct",
                decl.tag
            );
            Rc::clone(&entry.name)
        }
        _ => {
            let tag = Rc::new(make_temporary(&decl.tag));
            let entry = TagEntry::new_local(Rc::clone(&tag), TagType::Union);
            tag_map.insert(Rc::clone(&decl.tag), entry);
            tag
        }
    };

    let members = decl
        .members
        .into_iter()
        .map(|member| {
            Ok(ast::MemberDecl {
                r#type: resolve_type(member.r#type, tag_map)
                    .context(format!("failed to process {}.{}", decl.tag, member.name,))?,
                ..member
            })
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(ast::UnionDecl {
        tag: unique_tag,
        members,
    })
}

fn resolve_decl(
    decl: ast::Declaration,
    ident_map: &mut HashMap<Rc<String>, IdentEntry>,
    tag_map: &mut HashMap<Rc<String>, TagEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::Declaration> {
    match decl {
        ast::Declaration::Var(decl) => {
            resolve_local_var_decl(decl, ident_map, tag_map, make_temporary)
                .map(ast::Declaration::Var)
        }
        ast::Declaration::Fun(decl) => {
            resolve_fun_decl(decl, ident_map, tag_map, make_temporary).map(ast::Declaration::Fun)
        }
        ast::Declaration::Struct(decl) => {
            resolve_struct_decl(decl, tag_map, make_temporary).map(ast::Declaration::Struct)
        }
        ast::Declaration::Union(decl) => {
            resolve_union_decl(decl, tag_map, make_temporary).map(ast::Declaration::Union)
        }
    }
}

fn validate_blockitem(
    item: ast::BlockItem,
    ident_map: &mut HashMap<Rc<String>, IdentEntry>,
    tag_map: &mut HashMap<Rc<String>, TagEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::BlockItem> {
    match item {
        ast::BlockItem::Stmt(stmt) => Ok(ast::BlockItem::Stmt(resolve_stmt(
            stmt,
            ident_map,
            tag_map,
            make_temporary,
        )?)),
        ast::BlockItem::Decl(decl) => Ok(ast::BlockItem::Decl(resolve_decl(
            decl,
            ident_map,
            tag_map,
            make_temporary,
        )?)),
    }
}

fn resolve_stmt(
    stmt: ast::Stmt,
    ident_map: &HashMap<Rc<String>, IdentEntry>,
    tag_map: &HashMap<Rc<String>, TagEntry>,
    make_temporary: &mut impl FnMut(&str) -> String,
) -> Result<ast::Stmt> {
    match stmt {
        ast::Stmt::Return(Some(expr)) => Ok(ast::Stmt::Return(Some(resolve_expr(
            expr, ident_map, tag_map,
        )?))),
        ast::Stmt::Return(None) => Ok(ast::Stmt::Return(None)),
        ast::Stmt::Expr(expr) => Ok(ast::Stmt::Expr(resolve_expr(expr, ident_map, tag_map)?)),
        ast::Stmt::If {
            condition,
            then,
            r#else,
        } => Ok(ast::Stmt::If {
            condition: resolve_expr(condition, ident_map, tag_map)?,
            then: Box::new(resolve_stmt(*then, ident_map, tag_map, make_temporary)?),
            r#else: match r#else {
                Some(r#else) => Some(Box::new(resolve_stmt(
                    *r#else,
                    ident_map,
                    tag_map,
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
            condition: resolve_expr(condition, ident_map, tag_map)?,
            body: Box::new(resolve_stmt(*body, ident_map, tag_map, make_temporary)?),
            label,
        }),
        ast::Stmt::DoWhile {
            body,
            condition,
            label,
        } => Ok(ast::Stmt::DoWhile {
            condition: resolve_expr(condition, ident_map, tag_map)?,
            body: Box::new(resolve_stmt(*body, ident_map, tag_map, make_temporary)?),
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
            let new_tag_map = TagEntry::make_new_scope(tag_map);
            let init = match *init {
                ast::ForInit::Decl(ref decl) => ast::ForInit::Decl(resolve_local_var_decl(
                    decl.clone(),
                    &mut new_map,
                    &new_tag_map,
                    make_temporary,
                )?),
                ast::ForInit::Expr(Some(expr)) => {
                    ast::ForInit::Expr(Some(resolve_expr(expr, &new_map, &new_tag_map)?))
                }
                init => init,
            };
            let condition = if let Some(expr) = condition {
                Some(resolve_expr(expr, &new_map, &new_tag_map)?)
            } else {
                None
            };
            let post = if let Some(expr) = post {
                Some(resolve_expr(expr, &new_map, &new_tag_map)?)
            } else {
                None
            };
            Ok(ast::Stmt::For {
                init: Box::new(init),
                condition,
                post,
                body: Box::new(resolve_stmt(*body, &new_map, &new_tag_map, make_temporary)?),
                label,
            })
        }
        ast::Stmt::Null => Ok(ast::Stmt::Null),
        ast::Stmt::Compound(block) => {
            let mut new_map = make_new_scope(ident_map);
            let mut new_tag_map = TagEntry::make_new_scope(tag_map);
            let block = validate_block(block, &mut new_map, &mut new_tag_map, make_temporary)?;
            Ok(ast::Stmt::Compound(block))
        }
        ast::Stmt::Goto(label) => Ok(ast::Stmt::Goto(label)),
        ast::Stmt::Label { name, stmt } => Ok(ast::Stmt::Label {
            name,
            stmt: Box::new(resolve_stmt(*stmt, ident_map, tag_map, make_temporary)?),
        }),
        ast::Stmt::Default { stmt, label } => Ok(ast::Stmt::Default {
            stmt: Box::new(resolve_stmt(*stmt, ident_map, tag_map, make_temporary)?),
            label,
        }),
        ast::Stmt::Switch {
            condition,
            body,
            label,
            cases,
            default,
        } => Ok(ast::Stmt::Switch {
            condition: resolve_expr(condition, ident_map, tag_map)?,
            body: Box::new(resolve_stmt(*body, ident_map, tag_map, make_temporary)?),
            label,
            cases,
            default,
        }),
        ast::Stmt::Case { value, stmt, label } => Ok(ast::Stmt::Case {
            value: resolve_expr(value, ident_map, tag_map)?,
            stmt: Box::new(resolve_stmt(*stmt, ident_map, tag_map, make_temporary)?),
            label,
        }),
    }
}

fn resolve_expr(
    expr: ast::Expr,
    ident_map: &HashMap<Rc<String>, IdentEntry>,
    tag_map: &HashMap<Rc<String>, TagEntry>,
) -> Result<ast::Expr> {
    match expr {
        ast::Expr::Assignment { lvalue, rvalue } => Ok(ast::Expr::Assignment {
            lvalue: Box::new(resolve_expr(*lvalue, ident_map, tag_map)?),
            rvalue: Box::new(resolve_expr(*rvalue, ident_map, tag_map)?),
        }),
        ast::Expr::Var(var) => {
            if let Some(IdentEntry { name, .. }) = ident_map.get(&var) {
                Ok(ast::Expr::Var(Rc::clone(name)))
            } else {
                bail!("Undeclared variable '{var}'")
            }
        }
        node @ ast::Expr::Constant(_) => Ok(node),
        ast::Expr::Unary { op, expr } => {
            if op.is_valid_for(&expr) {
                Ok(ast::Expr::Unary {
                    op,
                    expr: Box::new(resolve_expr(*expr, ident_map, tag_map)?),
                })
            } else {
                bail!("Op {:?} is invalid for expression {:?}", op, expr)
            }
        }
        ast::Expr::Binary { op, left, right } => Ok(ast::Expr::Binary {
            op,
            left: Box::new(resolve_expr(*left, ident_map, tag_map)?),
            right: Box::new(resolve_expr(*right, ident_map, tag_map)?),
        }),
        ast::Expr::Conditional {
            condition,
            then,
            r#else,
        } => Ok(ast::Expr::Conditional {
            condition: Box::new(resolve_expr(*condition, ident_map, tag_map)?),
            then: Box::new(resolve_expr(*then, ident_map, tag_map)?),
            r#else: Box::new(resolve_expr(*r#else, ident_map, tag_map)?),
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
                .map(|a| resolve_expr(a, ident_map, tag_map))
                .collect::<Result<Vec<ast::Expr>, Error>>()?;
            Ok(ast::Expr::FunCall {
                name,
                args: valid_args,
            })
        }
        ast::Expr::Cast { exp, target } => Ok(ast::Expr::Cast {
            target: resolve_type(target, tag_map)?,
            exp: Box::new(resolve_expr(*exp, ident_map, tag_map)?),
        }),
        ast::Expr::Subscript { expr, index } => Ok(ast::Expr::Subscript {
            expr: resolve_expr(*expr, ident_map, tag_map)?.into(),
            index: resolve_expr(*index, ident_map, tag_map)?.into(),
        }),
        expr @ ast::Expr::String { .. } => Ok(expr),
        ast::Expr::SizeOf(expr) => Ok(ast::Expr::SizeOf(
            resolve_expr(*expr, ident_map, tag_map)?.into(),
        )),
        ast::Expr::SizeOfT(ty) => Ok(ast::Expr::SizeOfT(resolve_type(ty, tag_map)?)),
        ast::Expr::Dot { structure, member } => Ok(ast::Expr::Dot {
            structure: resolve_expr(*structure, ident_map, tag_map)?.into(),
            member,
        }),
        ast::Expr::Arrow { pointer, member } => Ok(ast::Expr::Arrow {
            pointer: resolve_expr(*pointer, ident_map, tag_map)?.into(),
            member,
        }),
    }
}

fn resolve_type(r#type: ast::Type, tag_map: &HashMap<Rc<String>, TagEntry>) -> Result<ast::Type> {
    let base = match r#type.base {
        ast::BaseType::Struct { tag, size } => {
            let Some(new_tag) = tag_map.get(&tag) else {
                bail!("attempting to use structure {tag} before its defined");
            };
            ensure!(
                new_tag.struct_or_union == TagType::Struct,
                "cannot use union tag for struct"
            );
            ast::BaseType::Struct {
                tag: Rc::clone(&new_tag.name),
                size,
            }
        }
        ast::BaseType::Union { tag, size } => {
            let Some(new_tag) = tag_map.get(&tag) else {
                bail!("attempting to use union {tag} before its defined");
            };
            ensure!(
                new_tag.struct_or_union == TagType::Union,
                "cannot use struct tag for struct"
            );
            ast::BaseType::Union {
                tag: Rc::clone(&new_tag.name),
                size,
            }
        }
        ast::BaseType::Ptr { to, is_restrict } => ast::BaseType::Ptr {
            to: resolve_type(*to, tag_map)?.into(),
            is_restrict,
        },
        ast::BaseType::Array { element, size } => ast::BaseType::Array {
            element: resolve_type(*element, tag_map)?.into(),
            size,
        },
        ast::BaseType::Fun { ret_t, param_types } => ast::BaseType::Fun {
            ret_t: resolve_type(*ret_t, tag_map)?.into(),
            param_types: param_types
                .into_iter()
                .map(|param| resolve_type(param, tag_map))
                .collect::<Result<Vec<_>>>()?,
        },
        base => base,
    };

    Ok(ast::Type { base, ..r#type })
}
