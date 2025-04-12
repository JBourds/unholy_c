use std::cmp;

use anyhow::Context;

use super::*;

#[derive(Clone, Debug)]
pub struct SymbolEntry {
    pub r#type: ast::Type,
    pub defined: bool,
    pub scope: Scope,
    pub attribute: Attribute,
}

#[derive(Clone, Copy, Debug)]
pub enum Attribute {
    Fun {
        external_linkage: bool,
    },
    Static {
        initial_value: InitialValue,
        external_linkage: bool,
    },
    Local,
}

impl Attribute {
    fn from_var_with_scope(var: &ast::VarDecl, scope: Scope) -> Result<Self> {
        if matches!(scope, Scope::Local(..)) && var.storage_class == Some(ast::StorageClass::Extern)
        {
            ensure!(
                var.init.is_none(),
                "Local var '{}' is extern but has an initial value",
                var.name
            );
        }
        let initial_value = if let Some(init_val) = InitialValue::from_var_with_scope(var, scope)? {
            init_val
        } else {
            match scope {
                Scope::Global => match var.storage_class {
                    Some(ast::StorageClass::Static) | None => InitialValue::Tentative, // Global non-externals with no initilizer are marked as tentative
                    Some(ast::StorageClass::Extern) => InitialValue::None,
                },
                Scope::Local(..) => match var.storage_class {
                    Some(ast::StorageClass::Static) => InitialValue::Initial(0), // Local Statics with no initilizer get defaulted to zero
                    Some(ast::StorageClass::Extern) | None => InitialValue::None,
                },
            }
        };

        if initial_value == InitialValue::None
            && var.storage_class.is_none()
            && matches!(scope, Scope::Local(..))
        {
            return Ok(Attribute::Local);
        }

        let external_linkage = match scope {
            Scope::Global => match var.storage_class {
                Some(ast::StorageClass::Static) => false,
                Some(ast::StorageClass::Extern) | None => true,
            },
            Scope::Local(..) => match var.storage_class {
                Some(ast::StorageClass::Static) | None => false,
                Some(ast::StorageClass::Extern) => true,
            },
        };

        Ok(Attribute::Static {
            initial_value,
            external_linkage,
        })
    }

    fn from_fun(fun: &ast::FunDecl) -> Self {
        Attribute::Fun {
            external_linkage: fun.storage_class != Some(ast::StorageClass::Static),
        }
    }

    fn from_decl_with_scope(decl: &ast::Declaration, scope: Scope) -> Result<Self> {
        match decl {
            ast::Declaration::FunDecl(f) => Ok(Self::from_fun(f)),
            ast::Declaration::VarDecl(v) => Self::from_var_with_scope(v, scope).context(format!(
                "Failed to process attributes for variable '{}'",
                v.name
            )),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InitialValue {
    Initial(i32),
    Tentative,
    None,
}

impl Ord for InitialValue {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (Self::Initial(_), Self::Initial(_)) => cmp::Ordering::Equal,
            (Self::None, Self::None) => cmp::Ordering::Equal,
            (Self::Tentative, Self::Tentative) => cmp::Ordering::Equal,
            (_, Self::None) => cmp::Ordering::Greater,
            (Self::Initial(_), Self::Tentative) => cmp::Ordering::Greater,
            (Self::None, _) => cmp::Ordering::Less,
            (Self::Tentative, Self::Initial(_)) => cmp::Ordering::Less,
        }
    }
}

impl PartialOrd for InitialValue {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl InitialValue {
    fn from_expr(expr: &ast::Expr) -> Result<Self> {
        let val = const_eval::eval(expr.clone()).context("Failed to const eval expression")?;
        match val {
            ast::Constant::Int(i) => Ok(InitialValue::Initial(i)),
            ast::Constant::Long(..) => todo!(),
        }
    }

    fn from_var_with_scope(var: &ast::VarDecl, scope: Scope) -> Result<Option<Self>> {
        match (scope, var.init.as_ref()) {
            (Scope::Global, Some(expr)) => {
                let init = Self::from_expr(expr)
                    .context(format!("Evaluating expression for '{}' failed", var.name))?;
                Ok(Some(init))
            }
            (Scope::Local(..), Some(expr)) => match var.storage_class {
                Some(ast::StorageClass::Static) => Ok(Some(
                    Self::from_expr(expr)
                        .context(format!("Evaluating expression for '{}' failed", var.name))?,
                )),
                None => Ok(None), // Locals technically dont have initial values
                Some(ast::StorageClass::Extern) => unreachable!(),
            },
            (Scope::Global, None) => match var.storage_class {
                Some(ast::StorageClass::Static) | None => Ok(Some(InitialValue::Tentative)), // Global non-externals with no initilizer are marked as tentative
                Some(ast::StorageClass::Extern) => Ok(Some(InitialValue::None)),
            },
            (Scope::Local(..), None) => match var.storage_class {
                Some(ast::StorageClass::Static) => Ok(Some(InitialValue::Initial(0))), // Local Statics with no initilizer get defaulted to zero
                Some(ast::StorageClass::Extern) | None => Ok(Some(InitialValue::None)),
            },
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    pub global: HashMap<Rc<String>, SymbolEntry>,
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
            attribute: Attribute::from_decl_with_scope(decl, scope)?,
        })
    }

    pub fn get(&self, key: &Rc<String>) -> Option<&SymbolEntry> {
        let local = self.get_local(key);
        if local.is_none() {
            self.get_global(key)
        } else {
            local
        }
    }

    fn insert_scope(&mut self, key: Rc<String>, entry: SymbolEntry) -> Option<SymbolEntry> {
        // Declare local static vars in global scope as well so it is
        // easy to iterate over (unique names make this legal)
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

    fn get_decl_info(
        decl: &ast::Declaration,
    ) -> (Rc<String>, ast::Type, bool, Option<ast::StorageClass>) {
        match decl {
            ast::Declaration::FunDecl(fun) => (
                Rc::clone(&fun.name),
                ast::Type::from(fun),
                fun.block.is_some(),
                fun.storage_class,
            ),
            ast::Declaration::VarDecl(ast::VarDecl {
                typ,
                name,
                init,
                storage_class,
            }) => (Rc::clone(name), typ.clone(), init.is_some(), *storage_class),
        }
    }

    fn check_attribute(
        old_attrib: &Attribute,
        name: &str,
        storage_class: Option<ast::StorageClass>,
        scope: Scope,
    ) -> Result<Attribute> {
        match old_attrib {
            Attribute::Fun { external_linkage } => {
                if !(*external_linkage) {
                    // If we are `static void foo(void)`, then
                    // ```
                    // void foo(void);
                    // static void foo(void);
                    // extern void foo(void);
                    // ```
                    // are all okay
                } else if storage_class == Some(ast::StorageClass::Static) {
                    bail!("Redeclaring function '{name}' as static when it was previously defined with external linkage");
                }
            }
            Attribute::Static {
                external_linkage, ..
            } => {
                match scope {
                    Scope::Global => {
                        if !(*external_linkage) {
                            // If we (foo) are declared as static, then following declarations are okay
                            // ```
                            // static int foo;
                            // extern int foo;
                            // ```
                            // However just `int foo;` is not.

                            if storage_class.is_none() {
                                bail!("Global variable '{name}' was previously declared as static");
                            }
                        } else {
                            // If we (foo) are declared extern,
                            // then we cannot be redeclared as static
                            if storage_class == Some(ast::StorageClass::Static) {
                                bail!("Redeclaring variable '{name}' as static when it was previously defined with external linkage");
                            }
                        }
                    }
                    Scope::Local(..) => match storage_class {
                        Some(ast::StorageClass::Extern) => {} // Vars with linkage can be declared multiple times
                        Some(ast::StorageClass::Static) | None => {
                            bail!("Variable '{name}' declared multiple times in scope")
                        }
                    },
                }
            }
            Attribute::Local => {}
        };
        Ok(*old_attrib)
    }

    fn declare_in_scope(&mut self, decl: &ast::Declaration, scope: Scope) -> Result<()> {
        let (name, mut new_type, defining_ident, storage_class) = Self::get_decl_info(decl);
        if let Some(SymbolEntry {
            r#type: old_type,
            defined: already_defined,
            scope: old_scope,
            attribute: old_attrib,
        }) = self.get(&name)
        {
            // There is already a declaration for this name, cases include:
            //  1. It is a function:
            //      I)   New declaration matches existing type (OK)
            //      II)  New declaration has no args specified (Potentially
            //           any number of args- still OK)
            //      III) New declaration type doesn't match existing (ERROR)
            //      IV)  New declaration redefines existing definition (ERROR)
            //      V)   New declaration conflicts with previous declarations
            //           linkage (ERROR)
            //  2. It is a variable:
            //      I)   New declaration shadows existing one (OK)
            //      II)  New declaration doesn't shadow (ERROR)
            //      III) New declaration storage class conflicts with previous
            //           one (ERROR)
            if !scope.shadows(old_scope) {
                if *old_type != new_type {
                    match (old_type, &new_type) {
                        (ast::Type::Fun {..}, ast::Type::Fun { param_types, .. }) if param_types.is_empty() => new_type = old_type.clone(),
                        _ => bail!(
                            "Redeclaring '{name}' as {new_type} when it was previously declared as {old_type}"
                        )
                    }
                }
                if *already_defined && defining_ident {
                    bail!("Redefining '{name}' when it is already defined.")
                }
                let mut attribute = Self::check_attribute(old_attrib, &name, storage_class, scope)?;
                match decl {
                    ast::Declaration::FunDecl(..) => {}
                    ast::Declaration::VarDecl(var) => {
                        let new_attribute = Attribute::from_var_with_scope(var, scope)?;
                        if let (
                            Attribute::Static {
                                initial_value: old_val,
                                external_linkage,
                            },
                            Attribute::Static {
                                initial_value: new_val,
                                ..
                            },
                        ) = (attribute, new_attribute)
                        {
                            // If it takes precedence
                            if new_val > old_val {
                                attribute = Attribute::Static {
                                    initial_value: new_val,
                                    external_linkage,
                                };
                            }
                        }
                    }
                }
                self.insert_scope(
                    name,
                    SymbolEntry {
                        r#type: new_type,
                        defined: *already_defined || defining_ident,
                        scope,
                        attribute,
                    },
                );
            } else {
                // Local variables can shadow (only if not extern), but functions cannot
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
        if decl.storage_class == Some(ast::StorageClass::Static)
            && matches!(self.scope(), Scope::Local(n) if n > 0)
        {
            bail!("Attempted to define local function {} as static", decl.name);
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
                        storage_class: None,
                    });
                    self.declare_in_scope(&param_decl, scope)?;
                }
            }
        }
        Ok(())
    }
    fn declare_var(&mut self, decl: &ast::VarDecl) -> Result<()> {
        let key = decl.name.clone();
        let storage_class = decl.storage_class;
        let decl = ast::Declaration::VarDecl(decl.clone());
        match storage_class {
            Some(ast::StorageClass::Extern) => self.declare_in_scope(&decl, Scope::Global)?,
            // If we declare a static in
            Some(ast::StorageClass::Static)
                if self.get_global(&key).is_none() && self.scope() != Scope::Global =>
            {
                self.declare_in_scope(&decl, Scope::Global)?;
            }
            _ => {}
        }
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
        symbols: Some(symbols),
        stage: PhantomData::<TypeChecking>,
    })
}

fn typecheck_program(program: &ast::Program, symbols: &mut SymbolTable) -> Result<()> {
    for decl in program.declarations.iter() {
        match decl {
            ast::Declaration::FunDecl(f) => typecheck_fun_decl(f, symbols)?,
            ast::Declaration::VarDecl(v) => typecheck_global_var_decl(v, symbols)?,
        }
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
                    if decl.storage_class.is_some() {
                        bail!(
                            "For-loop counter var '{}' cannot have storage class specifier",
                            decl.name
                        );
                    }
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

fn typecheck_global_var_decl(decl: &ast::VarDecl, symbols: &mut SymbolTable) -> Result<()> {
    ensure!(
        symbols.scope() == Scope::Global,
        "Global vars must be declared in global scope"
    );
    symbols.declare_var(decl)?;
    Ok(())
}

fn typecheck_var_decl(decl: &ast::VarDecl, symbols: &mut SymbolTable) -> Result<()> {
    symbols.declare_var(decl)?;
    if let Some(init) = &decl.init {
        typecheck_expr(init, symbols)
    } else {
        Ok(())
    }
}
