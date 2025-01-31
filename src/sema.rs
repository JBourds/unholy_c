use std::{collections::HashMap, marker::PhantomData, rc::Rc};

use crate::ast;
use anyhow::{bail, ensure, Error, Result};

enum Initial {}
enum IdentResolution {}
enum TypeChecking {}
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
    let stage = identifiers::validate(stage)?;
    let stage = typechecking::validate(stage)?;
    let stage = gotos::validate(stage)?;
    let stage = switch::validate(stage)?;
    let stage = loops::validate(stage)?;

    Ok(SemaStage {
        program: stage.program,
        stage: PhantomData::<Final>,
    })
}

mod identifiers {
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

    fn make_new_scope(
        ident_map: &HashMap<Rc<String>, IdentEntry>,
    ) -> HashMap<Rc<String>, IdentEntry> {
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
        let valid_functions = stage
            .program
            .functions
            .into_iter()
            .map(|f| resolve_fun_decl(f, &mut ident_map, &mut unique_name_generator))
            .collect::<Result<Vec<ast::FunDecl>, Error>>()?;

        Ok(SemaStage {
            program: ast::Program {
                functions: valid_functions,
            },
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

    fn resolve_var_decl(
        decl: ast::VarDecl,
        ident_map: &mut HashMap<Rc<String>, IdentEntry>,
        make_temporary: &mut impl FnMut(&str) -> String,
    ) -> Result<ast::VarDecl> {
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
                resolve_var_decl(decl, ident_map, make_temporary).map(ast::Declaration::VarDecl)
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
                    Some(r#else) => {
                        Some(Box::new(resolve_stmt(*r#else, ident_map, make_temporary)?))
                    }
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
                        ast::ForInit::Decl(resolve_var_decl(decl, &mut new_map, make_temporary)?)
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

    fn resolve_expr(
        expr: ast::Expr,
        ident_map: &HashMap<Rc<String>, IdentEntry>,
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
}

mod typechecking {
    use super::*;

    #[derive(Debug)]
    pub struct SymbolEntry {
        r#type: ast::Type,
        defined: bool,
        scope: Scope,
    }

    #[derive(Debug)]
    pub struct SymbolTable {
        global: HashMap<Rc<String>, SymbolEntry>,
        scopes: Vec<HashMap<Rc<String>, SymbolEntry>>,
    }

    #[derive(Debug)]
    pub enum Scope {
        Global,
        Local(usize),
    }

    impl SymbolTable {
        fn new_table() -> Self {
            Self {
                global: HashMap::new(),
                scopes: vec![],
            }
        }

        fn new_entry(&self, decl: &ast::Declaration, global: bool) -> Result<SymbolEntry> {
            Ok(SymbolEntry {
                r#type: decl.into(),
                defined: decl.defining(),
                scope: if global { Scope::Global } else { self.scope() },
            })
        }

        fn get(&self, key: &Rc<String>) -> Option<&SymbolEntry> {
            let local = self.get_local(key);
            if local.is_none() {
                self.get_global(key)
            } else {
                local
            }
        }

        fn insert_scope(&mut self, key: Rc<String>, entry: SymbolEntry) -> Option<SymbolEntry> {
            match entry.scope {
                Scope::Global => self.global.insert(key, entry),
                Scope::Local(frame) => self.scopes[frame].insert(key, entry),
            }
        }

        #[inline]
        fn scope(&self) -> Scope {
            match self.scopes.len() {
                0 => Scope::Global,
                n => Scope::Local(n - 1),
            }
        }

        fn get_local(&self, key: &Rc<String>) -> Option<&SymbolEntry> {
            for scope in self.scopes.iter().rev() {
                if let Some(entry) = scope.get(key) {
                    return Some(entry);
                }
            }
            None
        }

        fn get_global(&self, key: &Rc<String>) -> Option<&SymbolEntry> {
            self.global.get(key)
        }

        fn get_decl_info(decl: &ast::Declaration) -> (Rc<String>, ast::Type, bool) {
            match decl {
                ast::Declaration::FunDecl(fun) => (
                    Rc::clone(&fun.name),
                    ast::Type::from(fun),
                    fun.block.is_some(),
                ),
                ast::Declaration::VarDecl(ast::VarDecl { typ, name, init }) => {
                    (Rc::clone(name), typ.clone(), init.is_some())
                }
            }
        }

        fn declare_in_scope(&mut self, decl: &ast::Declaration, global: bool) -> Result<()> {
            let (name, r#type, defining_ident) = Self::get_decl_info(decl);
            match self.insert_scope(Rc::clone(&name), self.new_entry(decl, global)?) {
                Some(SymbolEntry { defined: true, .. }) if defining_ident => {
                    bail!("Redefining {name} when it is already defined.")
                }
                Some(SymbolEntry { r#type: prev_t, .. }) if r#type != prev_t => {
                    bail!(
                        "Redeclaring {name} as {type} when it was previously declared as {prev_t}"
                    )
                }
                // Defining or redeclaring global symbol with correct type: Ok!
                _ => Ok(()),
            }
        }

        fn declare_global(&mut self, decl: &ast::Declaration) -> Result<()> {
            self.declare_in_scope(decl, true)
        }

        fn declare_local(&mut self, decl: &ast::Declaration) -> Result<()> {
            self.declare_in_scope(decl, false)
        }

        // Lazy clones :(
        fn declare_fun(&mut self, decl: &ast::FunDecl) -> Result<()> {
            // If a function is defined at global scope, it still needs to create
            // a local stack frame (index 0) from which to typecheck
            if decl.block.is_some() && matches!(self.scope(), Scope::Local(n) if n > 0) {
                bail!(
                    "Attempted to define function {} outside of global scope.",
                    decl.name
                );
            }
            let wrapped_decl = ast::Declaration::FunDecl(decl.clone());
            self.declare_global(&wrapped_decl)?;
            if matches!(self.scope(), Scope::Local(_)) {
                // Declare function and all its params into local scope
                self.declare_local(&wrapped_decl)?;
                for (typ, name) in decl.signature.iter() {
                    if let Some(name) = name {
                        let param_decl = ast::Declaration::VarDecl(ast::VarDecl {
                            name: Rc::clone(name),
                            init: None,
                            typ: typ.clone(),
                        });
                        self.declare_local(&param_decl)?;
                    }
                }
            }
            Ok(())
        }
        fn declare_var(&mut self, decl: &ast::VarDecl) -> Result<()> {
            let decl = ast::Declaration::VarDecl(decl.clone());
            self.declare_local(&decl)
        }

        fn has_type(&self, key: &Rc<String>, expected: ast::Type) -> Result<()> {
            match self.get(key) {
                Some(SymbolEntry { r#type, .. }) if *r#type == expected => Ok(()),

                Some(SymbolEntry { r#type, .. }) => {
                    bail!("Expected type {expected} for \"{key}\" but found type {type} instead.")
                }
                None => bail!("Found no type information for symbol \"{key}\"."),
            }
        }

        fn push_scope(&mut self) {
            self.scopes.push(HashMap::new());
        }

        fn pop_scope(&mut self) -> Result<()> {
            if self.scopes.is_empty() {
                bail!("Already in global scope, cannot pop symbol table.")
            } else {
                self.scopes.pop();
                Ok(())
            }
        }
    }

    pub fn validate(stage: SemaStage<IdentResolution>) -> Result<SemaStage<TypeChecking>> {
        let mut symbols = SymbolTable::new_table();
        typecheck_program(&stage.program, &mut symbols)?;
        Ok(SemaStage {
            program: stage.program,
            stage: PhantomData::<TypeChecking>,
        })
    }

    fn typecheck_program(program: &ast::Program, symbols: &mut SymbolTable) -> Result<()> {
        for f in program.functions.iter() {
            typecheck_fun_decl(f, symbols)?;
        }
        Ok(())
    }

    fn typecheck_block(block: &ast::Block, symbols: &mut SymbolTable) -> Result<()> {
        symbols.push_scope();
        for item in block.items().iter() {
            typecheck_block_item(item, symbols)?;
        }
        symbols.pop_scope()
    }

    fn typecheck_block_item(item: &ast::BlockItem, symbols: &mut SymbolTable) -> Result<()> {
        match item {
            ast::BlockItem::Stmt(stmt) => typecheck_stmt(stmt, symbols).map(|_| ()),
            ast::BlockItem::Decl(decl) => typecheck_decl(decl, symbols).map(|_| ()),
        }
    }

    fn typecheck_stmt(stmt: &ast::Stmt, symbols: &mut SymbolTable) -> Result<()> {
        match stmt {
            ast::Stmt::Compound(block) => typecheck_block(block, symbols),
            ast::Stmt::Return(Some(expr)) => typecheck_expr(expr, symbols),
            ast::Stmt::Expr(expr) => typecheck_expr(expr, symbols),
            ast::Stmt::If {
                condition,
                then,
                r#else,
            } => {
                typecheck_expr(condition, symbols)?;
                typecheck_stmt(then, symbols)?;
                r#else
                    .as_ref()
                    .map_or(Ok(()), |stmt| typecheck_stmt(stmt, symbols))
            }
            ast::Stmt::While {
                condition, body, ..
            } => {
                typecheck_expr(condition, symbols)?;
                typecheck_stmt(body, symbols)
            }
            ast::Stmt::DoWhile {
                body, condition, ..
            } => {
                typecheck_expr(condition, symbols)?;
                typecheck_stmt(body, symbols)
            }
            ast::Stmt::For {
                init,
                condition,
                post,
                body,
                ..
            } => {
                match init {
                    ast::ForInit::Decl(decl) => {
                        typecheck_var_decl(decl, symbols)?;
                    }
                    ast::ForInit::Expr(Some(expr)) => {
                        typecheck_expr(expr, symbols)?;
                    }
                    _ => {}
                }
                if let Some(condition) = condition {
                    typecheck_expr(condition, symbols)?;
                }
                if let Some(post) = post {
                    typecheck_expr(post, symbols)?;
                }
                typecheck_stmt(body, symbols)
            }
            ast::Stmt::Case { value, stmt, .. } => {
                typecheck_expr(value, symbols)?;
                typecheck_stmt(stmt, symbols)
            }
            ast::Stmt::Switch {
                condition,
                body,
                cases,
                ..
            } => {
                typecheck_expr(condition, symbols)?;
                typecheck_stmt(body, symbols)?;
                if let Some(cases) = cases {
                    for (literal, name) in cases.iter() {
                        if !literal.is_int() {
                            bail!("Non-integer type in case {name}.");
                        }
                    }
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn typecheck_expr(expr: &ast::Expr, symbols: &mut SymbolTable) -> Result<()> {
        match expr {
            ast::Expr::Var(name) => {
                // TODO: Fix this once we have more types
                symbols.has_type(name, ast::Type::Int)
            }
            ast::Expr::Assignment { lvalue, rvalue } => {
                typecheck_expr(lvalue, symbols)?;
                typecheck_expr(rvalue, symbols)
            }
            ast::Expr::Unary { expr, .. } => typecheck_expr(expr, symbols),
            ast::Expr::Binary { left, right, .. } => {
                typecheck_expr(left, symbols)?;
                typecheck_expr(right, symbols)
            }
            ast::Expr::Conditional {
                condition,
                then,
                r#else,
            } => {
                typecheck_expr(condition, symbols)?;
                typecheck_expr(then, symbols)?;
                typecheck_expr(r#else, symbols)
            }
            ast::Expr::FunCall { name, args } => {
                match symbols.get(name) {
                    Some(SymbolEntry {
                        r#type: ast::Type::Fun { param_types, .. },
                        ..
                    }) => {
                        if args.len() != param_types.len() {
                            bail!(
                                "Expected {} args but received {} when calling \"{name}\".",
                                param_types.len(),
                                args.len()
                            );
                        }
                        // Will need to be uprooted once more sophisticated types
                        // are added in order to verify arg types match expected
                        for arg in args.iter() {
                            typecheck_expr(arg, symbols)?;
                        }
                        Ok(())
                    }
                    Some(SymbolEntry { r#type: t, .. }) => {
                        bail!("Expected function type, but found type {t}.")
                    }
                    _ => bail!("Could not find symbol with name {name}."),
                }
            }
            _ => Ok(()),
        }
    }

    fn typecheck_decl(decl: &ast::Declaration, symbols: &mut SymbolTable) -> Result<()> {
        match decl {
            ast::Declaration::FunDecl(decl) => typecheck_fun_decl(decl, symbols),
            ast::Declaration::VarDecl(decl) => typecheck_var_decl(decl, symbols),
        }
    }

    fn typecheck_fun_decl(decl: &ast::FunDecl, symbols: &mut SymbolTable) -> Result<()> {
        // Special case: Push scope and iterate over block items here so the
        // function parameters get put into the same scope as the block items
        symbols.push_scope();
        symbols.declare_fun(decl)?;
        // Treat parameters as declarations without values
        if let Some(ref block) = decl.block {
            for item in block.items() {
                typecheck_block_item(item, symbols)?;
            }
        }
        symbols.pop_scope()
    }

    fn typecheck_var_decl(decl: &ast::VarDecl, symbols: &mut SymbolTable) -> Result<()> {
        symbols.declare_var(decl)
    }
}

mod gotos {
    use super::*;
    pub fn validate(stage: SemaStage<TypeChecking>) -> Result<SemaStage<GotoValidation>> {
        let mut label_map = HashMap::new();
        let valid_functions = stage
            .program
            .functions
            .into_iter()
            .map(|f| {
                let block = if let Some(b) = f.block {
                    let b = resolve_block(b, &f.name, &mut label_map)?;
                    let b = validate_block(b, &mut label_map)?;
                    Some(b)
                } else {
                    None
                };
                Ok(ast::FunDecl { block, ..f })
            })
            .collect::<Result<Vec<ast::FunDecl>, Error>>()?;

        Ok(SemaStage {
            program: ast::Program {
                functions: valid_functions,
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
                    "Duplicate labels {name}."
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
    }

    pub fn validate(stage: SemaStage<GotoValidation>) -> Result<SemaStage<SwitchLabelling>> {
        let valid_functions = stage
            .program
            .functions
            .into_iter()
            .map(resolve_function)
            .collect::<Result<Vec<ast::FunDecl>, Error>>()?;
        Ok(SemaStage {
            program: ast::Program {
                functions: valid_functions,
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
        let valid_functions = stage
            .program
            .functions
            .into_iter()
            .map(resolve_function)
            .collect::<Result<Vec<ast::FunDecl>, Error>>()?;
        Ok(SemaStage {
            program: ast::Program {
                functions: valid_functions,
            },
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
            (ast::Stmt::Continue(None), Some(loop_label)) => {
                Ok(ast::Stmt::Continue(Some(loop_label)))
            }
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
}
