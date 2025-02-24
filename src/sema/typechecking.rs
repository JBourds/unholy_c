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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Scope {
    Global,
    Local(usize),
}

impl Scope {
    fn shadows(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Global, Self::Global) => false,
            (Self::Global, Self::Local(_)) => false,
            (Self::Local(_), Self::Global) => true,
            (Self::Local(n1), Self::Local(n2)) => n1 > n2,
        }
    }
}

impl SymbolTable {
    fn new_table() -> Self {
        Self {
            global: HashMap::new(),
            scopes: vec![],
        }
    }

    fn new_entry(&self, decl: &ast::Declaration, scope: Scope) -> Result<SymbolEntry> {
        Ok(SymbolEntry {
            r#type: decl.into(),
            defined: decl.defining(),
            scope,
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
            ast::Declaration::VarDecl(ast::VarDecl {
                typ,
                name,
                init,
                storage_class: _,
            }) => (Rc::clone(name), typ.clone(), init.is_some()),
        }
    }

    fn declare_in_scope(&mut self, decl: &ast::Declaration, scope: Scope) -> Result<()> {
        let (name, new_type, defining_ident) = Self::get_decl_info(decl);
        if let Some(SymbolEntry {
            r#type: old_type,
            defined: already_defined,
            scope: old_scope,
        }) = self.get(&name)
        {
            // There is already a declaration for this name, cases include:
            //  1. It is a function:
            //      I)   New declaration matches existing type (OK)
            //      II)  New declaration has no args specified (Potentially
            //           any number of args- still OK)
            //      III) New declaration type doesn't match existing (ERROR)
            //      IV)  New declaration redefines existing definition
            //  2. It is a variable:
            //      I)  New declaration shadows existing one (OK)
            //      II) New declaration doesn't shadow (ERROR)
            if !scope.shadows(old_scope) {
                if *old_type != new_type {
                    match new_type {
                            ast::Type::Fun { param_types, .. } if param_types.is_empty() => {},
                            _ => bail!(
                                "Redeclaring {name} as {new_type} when it was previously declared as {old_type}"
                            )
                        }
                }
                if *already_defined && defining_ident {
                    bail!("Redefining {name} when it is already defined.")
                }
            } else {
                // Local variables can shadow, but functions cannot
                self.insert_scope(name, self.new_entry(decl, scope)?);
            }
        } else {
            self.insert_scope(name, self.new_entry(decl, scope)?);
        }
        Ok(())
    }

    // Lazy clones :(
    fn declare_fun(&mut self, decl: &ast::FunDecl) -> Result<()> {
        if decl.block.is_some() && matches!(self.scope(), Scope::Local(n) if n > 0) {
            bail!(
                "Attempted to define function {} outside of global scope.",
                decl.name
            );
        }
        let wrapped_decl = ast::Declaration::FunDecl(decl.clone());
        self.declare_in_scope(&wrapped_decl, Scope::Global)?;
        if let scope @ Scope::Local(_) = self.scope() {
            // Declare function and all its params into local scope
            self.declare_in_scope(&wrapped_decl, scope)?;
            for (typ, name) in decl.signature.iter() {
                if let Some(name) = name {
                    let param_decl = ast::Declaration::VarDecl(ast::VarDecl {
                        name: Rc::clone(name),
                        init: None,
                        typ: typ.clone(),
                        storage_class: todo!(),
                    });
                    self.declare_in_scope(&param_decl, scope)?;
                }
            }
        }
        Ok(())
    }
    fn declare_var(&mut self, decl: &ast::VarDecl) -> Result<()> {
        let decl = ast::Declaration::VarDecl(decl.clone());
        self.declare_in_scope(&decl, self.scope())
    }

    fn has_type(&self, key: &Rc<String>, expected: ast::Type) -> Result<()> {
        match self.get(key) {
            Some(SymbolEntry { r#type, .. }) if *r#type == expected => Ok(()),

            Some(SymbolEntry { r#type, .. }) => {
                bail!(
                    "Expected type \"{expected}\" for \"{key}\" but found type \"{type}\" instead."
                )
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
    for d in program.declarations.iter() {
        typecheck_decl(d, symbols)?;
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
    symbols.declare_var(decl)?;
    if let Some(init) = &decl.init {
        typecheck_expr(init, symbols)
    } else {
        Ok(())
    }
}
