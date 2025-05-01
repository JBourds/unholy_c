use std::cmp;

use anyhow::{Context, Error};

use super::*;

#[derive(Clone, Debug)]
pub struct SymbolEntry {
    pub r#type: ast::Type,
    pub defined: bool,
    pub scope: Scope,
    pub attribute: Attribute,
}

#[derive(Clone, Debug)]
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
    fn from_var_with_scope(
        var: &ast::VarDecl,
        scope: Scope,
        symbols: &mut SymbolTable,
    ) -> Result<Self> {
        if matches!(scope, Scope::Local(..)) && var.typ.storage == Some(ast::StorageClass::Extern) {
            ensure!(
                var.init.is_none(),
                "Local var '{}' is extern but has an initial value",
                var.name
            );
        }
        let initial_value = if let Some(init_val) =
            InitialValue::from_var_with_scope(var, scope, symbols)?
        {
            init_val
        } else {
            match scope {
                Scope::Global => match var.typ.storage {
                    Some(ast::StorageClass::Static) | None => InitialValue::Tentative, // Global non-externals with no initilizer are marked as tentative
                    Some(ast::StorageClass::Extern) => InitialValue::None,
                    _ => unreachable!(
                        "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                    ),
                },
                Scope::Local(..) => match var.typ.storage {
                    Some(ast::StorageClass::Static) => {
                        InitialValue::Initial(vec![0; var.typ.base.nbytes()].into())
                    } // Local Statics with no initilizer get defaulted to zero
                    Some(ast::StorageClass::Extern) | None => InitialValue::None,
                    _ => unreachable!(
                        "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                    ),
                },
            }
        };

        if initial_value == InitialValue::None
            && var.typ.storage.is_none()
            && matches!(scope, Scope::Local(..))
        {
            return Ok(Attribute::Local);
        }

        let external_linkage = match scope {
            Scope::Global => match var.typ.storage {
                Some(ast::StorageClass::Static) => false,
                Some(ast::StorageClass::Extern) | None => true,
                _ => unreachable!(
                    "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                ),
            },
            Scope::Local(..) => match var.typ.storage {
                Some(ast::StorageClass::Static) | None => false,
                Some(ast::StorageClass::Extern) => true,
                _ => unreachable!(
                    "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                ),
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

    fn from_decl_with_scope(
        decl: &ast::Declaration,
        scope: Scope,
        symbols: &mut SymbolTable,
    ) -> Result<Self> {
        match decl {
            ast::Declaration::FunDecl(f) => Ok(Self::from_fun(f)),
            ast::Declaration::VarDecl(v) => Self::from_var_with_scope(v, scope, symbols).context(
                format!("Failed to process attributes for variable '{}'", v.name),
            ),
        }
    }

    pub fn has_external_linkage(&self) -> bool {
        match self {
            Self::Fun { external_linkage } => *external_linkage,
            Self::Static {
                external_linkage, ..
            } => *external_linkage,
            Self::Local => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InitialValue {
    Initial(Rc<[u8]>),
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
    // TODO: Make this not dependent on host computer byte ordering
    fn from_expr(r#type: &ast::Type, expr: &ast::Expr, symbols: &mut SymbolTable) -> Result<Self> {
        let expr = try_implicit_cast(r#type, expr, symbols).context(
            "Failed to perform implicit casting when constructing initial value for declaration",
        )?;
        let val = const_eval::eval(expr.clone()).context("Failed to const eval expression")?;
        match val {
            ast::Constant::I8(val) => Ok(InitialValue::Initial(val.to_ne_bytes().to_vec().into())),
            ast::Constant::I16(val) => Ok(InitialValue::Initial(val.to_ne_bytes().to_vec().into())),
            ast::Constant::I32(val) => Ok(InitialValue::Initial(val.to_ne_bytes().to_vec().into())),
            ast::Constant::I64(val) => Ok(InitialValue::Initial(val.to_ne_bytes().to_vec().into())),
            ast::Constant::U8(val) => Ok(InitialValue::Initial(val.to_ne_bytes().to_vec().into())),
            ast::Constant::U16(val) => Ok(InitialValue::Initial(val.to_ne_bytes().to_vec().into())),
            ast::Constant::U32(val) => Ok(InitialValue::Initial(val.to_ne_bytes().to_vec().into())),
            ast::Constant::U64(val) => Ok(InitialValue::Initial(val.to_ne_bytes().to_vec().into())),
        }
    }

    fn from_var_with_scope(
        var: &ast::VarDecl,
        scope: Scope,
        symbols: &mut SymbolTable,
    ) -> Result<Option<Self>> {
        match (scope, var.init.as_ref()) {
            (Scope::Global, Some(expr)) => {
                let init = Self::from_expr(&var.typ, expr, symbols)
                    .context(format!("Evaluating expression for '{}' failed", var.name))?;
                Ok(Some(init))
            }
            (Scope::Local(..), Some(expr)) => match var.typ.storage {
                Some(ast::StorageClass::Static) => Ok(Some(
                    Self::from_expr(&var.typ, expr, symbols)
                        .context(format!("Evaluating expression for '{}' failed", var.name))?,
                )),
                None => Ok(None), // Locals technically dont have initial values
                Some(ast::StorageClass::Extern) => unreachable!(),
                _ => unreachable!(
                    "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                ),
            },
            (Scope::Global, None) => match var.typ.storage {
                Some(ast::StorageClass::Static) | None => Ok(Some(InitialValue::Tentative)), // Global non-externals with no initilizer are marked as tentative
                Some(ast::StorageClass::Extern) => Ok(Some(InitialValue::None)),
                _ => unreachable!(
                    "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                ),
            },
            (Scope::Local(..), None) => match var.typ.storage {
                Some(ast::StorageClass::Static) => Ok(Some(InitialValue::Initial(
                    vec![0; var.typ.base.nbytes()].into(),
                ))), // Local Statics with no initilizer get defaulted to zero
                Some(ast::StorageClass::Extern) | None => Ok(Some(InitialValue::None)),
                _ => unreachable!(
                    "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                ),
            },
        }
    }
}

#[derive(Debug, Default)]
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

    fn new_entry(&mut self, decl: &ast::Declaration, scope: Scope) -> Result<SymbolEntry> {
        Ok(SymbolEntry {
            r#type: decl.into(),
            defined: decl.defining(),
            scope,
            attribute: Attribute::from_decl_with_scope(decl, scope, self)?,
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
                    bail!(
                        "Redeclaring function '{name}' as static when it was previously defined with external linkage"
                    );
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
                                bail!(
                                    "Redeclaring variable '{name}' as static when it was previously defined with external linkage"
                                );
                            }
                        }
                    }
                    Scope::Local(..) => match storage_class {
                        Some(ast::StorageClass::Extern) => {} // Vars with linkage can be declared multiple times
                        Some(ast::StorageClass::Static) | None => {
                            bail!("Variable '{name}' declared multiple times in scope")
                        }
                        _ => unreachable!(
                            "Earlier passes of the compiler should have reduced \"auto\" and \"register\" storage classes to be None"
                        ),
                    },
                }
            }
            Attribute::Local => {}
        };
        Ok(old_attrib.clone())
    }

    fn declare_in_scope(&mut self, decl: &ast::Declaration, scope: Scope) -> Result<SymbolEntry> {
        let (name, mut new_type, defining_ident) = Self::get_decl_info(decl);

        let entry = if let Some(entry) = self.get(&name) {
            // FIXME: Lazy way to make rust shutup about the immutable borrow
            // overlapping with the mutable one
            let SymbolEntry {
                r#type: old_type,
                defined: already_defined,
                scope: old_scope,
                attribute: old_attrib,
            } = entry.clone();
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
            if !scope.shadows(&old_scope) {
                // Cases 1.1 and 2.1
                if old_type != new_type {
                    match (&old_type.base, &new_type.base) {
                        // Case 1.2
                        (ast::BaseType::Fun { .. }, ast::BaseType::Fun { param_types, .. })
                            if param_types.is_empty() =>
                        {
                            new_type = old_type.clone()
                        }
                        _ => bail!(
                            "Redeclaring '{name}' as {new_type} when it was previously declared as {old_type}"
                        ),
                    }
                }
                if already_defined && defining_ident {
                    bail!("Redefining '{name}' when it is already defined.")
                }
                let mut attribute =
                    Self::check_attribute(&old_attrib, &name, new_type.storage, scope)?;
                match decl {
                    ast::Declaration::FunDecl(..) => {}
                    ast::Declaration::VarDecl(var) => {
                        let new_attribute = Attribute::from_var_with_scope(var, scope, self)?;
                        if let (
                            Attribute::Static {
                                initial_value: old_val,
                                external_linkage,
                            },
                            Attribute::Static {
                                initial_value: new_val,
                                ..
                            },
                        ) = (attribute.clone(), new_attribute)
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
                SymbolEntry {
                    r#type: new_type,
                    defined: already_defined || defining_ident,
                    scope,
                    attribute,
                }
            } else {
                // Local variables can shadow (only if not extern), but functions cannot
                self.new_entry(decl, scope)?
            }
        } else {
            self.new_entry(decl, scope)?
        };
        self.insert_scope(name, entry.clone());
        Ok(entry)
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
                    });
                    self.declare_in_scope(&param_decl, scope)?;
                }
            }
        }
        Ok(())
    }
    fn declare_var(&mut self, decl: &ast::VarDecl) -> Result<SymbolEntry> {
        let key = decl.name.clone();
        let storage_class = decl.typ.storage;
        let decl = ast::Declaration::VarDecl(decl.clone());

        match storage_class {
            Some(ast::StorageClass::Extern) => {
                self.declare_in_scope(&decl, Scope::Global)?;
            }
            Some(ast::StorageClass::Static)
                if self.get_global(&key).is_none() && self.scope() != Scope::Global =>
            {
                self.declare_in_scope(&decl, Scope::Global)?;
            }
            _ => {}
        }
        self.declare_in_scope(&decl, self.scope())
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

struct TypedExpr {
    expr: ast::Expr,
    r#type: ast::Type,
}

pub fn validate(stage: SemaStage<SwitchLabelling>) -> Result<SemaStage<TypeChecking>> {
    let mut symbols = SymbolTable::new_table();

    Ok(SemaStage {
        program: typecheck_program(stage.program, &mut symbols)
            .context("Failed to perform typechecking.")?,
        symbols: Some(symbols),
        stage: PhantomData::<TypeChecking>,
    })
}

fn typecheck_program(program: ast::Program, symbols: &mut SymbolTable) -> Result<ast::Program> {
    let mut declarations = vec![];
    for decl in program.declarations.into_iter() {
        match decl {
            ast::Declaration::FunDecl(f) => {
                let name = Rc::clone(&f.name);
                declarations.push(ast::Declaration::FunDecl(
                    typecheck_fun_decl(f, symbols).context(format!(
                        "Unable to typecheck function declaration for {name}"
                    ))?,
                ));
            }
            ast::Declaration::VarDecl(v) => {
                let name = Rc::clone(&v.name);
                declarations.push(ast::Declaration::VarDecl(
                    typecheck_global_var_decl(v, symbols).context(format!(
                        "Unable to typecheck variable declaration for {name}"
                    ))?,
                ));
            }
        }
    }
    Ok(ast::Program { declarations })
}

fn typecheck_block(
    block: ast::Block,
    symbols: &mut SymbolTable,
    function: Option<Rc<String>>,
) -> Result<ast::Block> {
    symbols.push_scope();
    let items = block
        .into_items()
        .into_iter()
        .map(|item| typecheck_block_item(item, symbols, function.clone()))
        .collect::<Result<Vec<_>>>()
        .context("Failed to typecheck all block items")?;
    symbols
        .pop_scope()
        .expect("We just popped the scope so this should not fail.");
    Ok(ast::Block(items))
}

fn typecheck_block_item(
    item: ast::BlockItem,
    symbols: &mut SymbolTable,
    function: Option<Rc<String>>,
) -> Result<ast::BlockItem> {
    match item {
        ast::BlockItem::Stmt(stmt) => Ok(ast::BlockItem::Stmt(
            typecheck_stmt(stmt, symbols, function)
                .context("Failed to typecheck block item: {item:#?}")?,
        )),
        ast::BlockItem::Decl(decl) => Ok(ast::BlockItem::Decl(
            typecheck_decl(decl, symbols).context("Failed to typecheck block item: {item:#?}")?,
        )),
    }
}

fn typecheck_stmt(
    stmt: ast::Stmt,
    symbols: &mut SymbolTable,
    function: Option<Rc<String>>,
) -> Result<ast::Stmt> {
    match stmt {
        ast::Stmt::Compound(block) => Ok::<ast::Stmt, Error>(
                ast::Stmt::Compound(
                    typecheck_block(block, symbols, function).context("Unable to typecheck block withint statement.")?
                )
            ),
        ast::Stmt::Return(Some(expr)) => {
                if let Some(function) = function {
                    let TypedExpr { r#type: found, expr } = typecheck_expr(&expr, symbols)?;
                    if let Some(SymbolEntry {
                        r#type:
                            ast::Type {
                                base: ast::BaseType::Fun { ret_t: expected, .. },
                                ..
                            },
                        ..
                    }) = symbols.get(&function)
                    {
                        // TODO: Fix this once we add pointers
                        if !found.base.can_assign_to(&expected.base) {
                            bail!("Found return type: \"{found}\" in function \"{function}\" but expected return type: \"{expected}\"");
                        } else {
                            Ok(ast::Stmt::Return(Some(try_implicit_cast(&expected.clone(), &expr, symbols)
                                        .context(format!("Unable to implicitly cast return value to expected return type in \"{}\"", function))?
                                        )))
                        }
                    } else {
                        bail!("Could not find function {function} in symbol table.")
                    }
                } else {
                    bail!("Invalid return statement out of function body.");
                }
            }
        ast::Stmt::Expr(expr) => Ok(ast::Stmt::Expr(typecheck_expr(&expr, symbols)
                .context("Failed to typecheck expression statement.")?.expr)),
        ast::Stmt::If {
                condition,
                then,
                r#else,
            } => {
                let condition = typecheck_expr(&condition, symbols)
                    .context("Failed to typecheck if block condition.")?.expr;
                let then = typecheck_stmt(*then, symbols, function.clone())
                    .context("Failed to typecheck if branch of conditional.")?;
                let r#else = if let Some(r#else) = r#else { Some(typecheck_stmt(*r#else, symbols, function)
                    .context("Failed to typecheck else branch of conditional.")?)
                } else { None };
                Ok(ast::Stmt::If { condition, then: Box::new(then), r#else: r#else.map(Box::new) })

            }
        ast::Stmt::While {
                condition, body, label,
            } => {
                let condition = typecheck_expr(&condition, symbols)
                        .context("Failed to typecheck for loop condition.")?.expr;
                let body = typecheck_stmt(*body, symbols, function)
                    .context("Failed to typecheck for loop body.")?;
                Ok(ast::Stmt::While { body: Box::new(body), condition, label })
            }
        ast::Stmt::DoWhile {
                body, condition, label,
            } => {
                let condition = typecheck_expr(&condition, symbols)
                        .context("Failed to typecheck for loop condition.")?.expr;
                let body = typecheck_stmt(*body, symbols, function)
                    .context("Failed to typecheck for loop body.")?;
                Ok(ast::Stmt::DoWhile { body: Box::new(body), condition, label })
            }
        ast::Stmt::For {
                init,
                condition,
                post,
                body,
                label,
            } => {
                let init = match *init {
                    ast::ForInit::Decl(decl) => {
                        if decl.typ.storage.is_some() {
                            bail!(
                                "For-loop counter var '{}' cannot have storage class specifier",
                                decl.name
                            );
                        }
                        ast::ForInit::Decl(
                            typecheck_var_decl(decl, symbols)
                            .context("Failed to typecheck for loop initializations.")?)
                    }
                    ast::ForInit::Expr(Some(ref expr)) => {
                        ast::ForInit::Expr(
                            Some(typecheck_expr(expr, symbols)
                            .map(|t_expr| t_expr.expr)
                            .context("Failed to typecheck for loop initialization expression.")?)
                        )
                    }
                    _ => ast::ForInit::Expr(None)
                };
                let post = if let Some(post) = post {
                    Some(typecheck_expr(&post, symbols)
                        .context("Failed to typecheck for loop post condition.")?.expr)
                } else { None };
                let condition = if let Some(condition) = condition {
                    Some(typecheck_expr(&condition, symbols)
                        .context("Failed to typecheck for loop condition.")?.expr)
                } else { None };
                let body = typecheck_stmt(*body, symbols, function)
                    .context("Failed to typecheck for loop body.")?;
                Ok(ast::Stmt::For {
                    init: Box::new(init), condition, post, body: Box::new(body), label
                })
            }
        ast::Stmt::Case { value, stmt, label } => {
                let value = typecheck_expr(&value, symbols)
                    .context("Failed to typecheck case value.")?.expr;
                let stmt = typecheck_stmt(*stmt, symbols, function)
                    .context("Failed to typecheck case statement.")?;
                Ok(ast::Stmt::Case { value, stmt: Box::new(stmt), label })
            }
        ast::Stmt::Switch {
                condition,
                body,
                cases,
                label,
                default,
            } => {
                let TypedExpr { expr: condition, r#type: condition_type } = typecheck_expr(&condition, symbols)
                    .context("Failed to typecheck switch expression.")?;

                if matches!(condition_type.base, ast::BaseType::Fun { .. }) {
                    bail!("Cannot switch on {condition:#?} as it has type {condition_type:#?}");
                }
                
                let body = typecheck_stmt(*body, symbols, function)
                    .context("Failed to typecheck switch body.")?;
                if let Some(ref cases) = cases {
                    for (literal, name) in cases.iter() {
                        if !literal.is_int() {
                            bail!("Non-integer type in case {name}.");
                        }
                    }
                }
                Ok(ast::Stmt::Switch { condition, body: Box::new(body), cases, label, default})
            }
        ast::Stmt::Null => Ok(stmt),
        ast::Stmt::Break(_) => Ok(stmt),
        ast::Stmt::Continue(_) => Ok(stmt),
        ast::Stmt::Label { name, stmt } => Ok(ast::Stmt::Label {
            name,
            stmt: Box::new(typecheck_stmt(*stmt, symbols, function).context("Unable to typecheck statement within label.")?)
        }
        ),
        ast::Stmt::Default { label, stmt } => Ok(ast::Stmt::Default {
            label,
            stmt: Box::new(typecheck_stmt(*stmt, symbols, function).context("Unable to typecheck statement within default label.")?)
        }
        ),
        ast::Stmt::Goto(_) => Ok(stmt),
        ast::Stmt::Return(None) => Ok(ast::Stmt::Return(None)),
    }
    .context("Failed to typecheck statement.")
}

fn try_implicit_cast(
    target: &ast::Type,
    from: &ast::Expr,
    symbols: &mut SymbolTable,
) -> Result<ast::Expr> {
    let TypedExpr {
        expr: right,
        r#type: right_t,
    } = typecheck_expr(from, symbols)
        .context("Failed to typecheck from argument in implicit cast.")?;
    ensure!(
        right_t.base.can_assign_to(&target.base),
        "Incompatible types. Cannot assign value of type {right_t:#?} to value of type {target:#?}"
    );
    if right_t != *target {
        Ok(ast::Expr::Cast {
            target: ast::Type {
                storage: None,
                is_const: true,
                ..target.clone()
            },
            exp: Box::new(right),
        })
    } else {
        Ok(right)
    }
}

fn typecheck_expr(expr: &ast::Expr, symbols: &mut SymbolTable) -> Result<TypedExpr> {
    match expr {
        ast::Expr::Var(var) => {
            if let Some(t) = symbols.get(var) {
                Ok(TypedExpr {
                    expr: expr.clone(),
                    r#type: t.r#type.clone(),
                })
            } else {
                bail!("Attempted to typecheck {var} but there was no type associated with it.");
            }
        }
        // TODO: Fix once we get to pointers
        // This case has the semantics of a cast but rather than directly
        // converting to a cast expression frame it as an implicit promotion
        // so invalid assignments (e.g., Struct into an int) fail.
        ast::Expr::Assignment { lvalue, rvalue } => {
            ensure!(
                lvalue.is_lvalue(),
                "Expected target in assignment but found {lvalue:?}"
            );
            let TypedExpr {
                expr: _,
                r#type: left_t,
            } = typecheck_expr(lvalue, symbols)
                .context("Failed to typecheck lvalue in assignment.")?;
            // FIXME: Lazy clone :(
            Ok(TypedExpr {
                expr: ast::Expr::Assignment {
                    lvalue: lvalue.clone(),
                    rvalue: Box::new(
                        try_implicit_cast(&left_t, rvalue, symbols).context(
                            "Failed to implicitly cast righthand side during assignment.",
                        )?,
                    ),
                },
                r#type: left_t,
            })
        }
        ast::Expr::Unary { op, expr } => {
            let TypedExpr { expr, r#type } = typecheck_expr(expr, symbols)
                .context("Failed to typecheck nested unary expression.")?;
            Ok(TypedExpr {
                expr: ast::Expr::Unary {
                    op: *op,
                    expr: Box::new(expr),
                },
                r#type,
            })
        }
        // TODO: Fix once we get to pointers
        ast::Expr::Binary { op, left, right } => {
            let TypedExpr {
                expr: left,
                r#type: left_t,
            } = typecheck_expr(left, symbols)
                .context("Failed to typecheck lefthand argument of binary operation.")?;
            let TypedExpr {
                expr: right,
                r#type: right_t,
            } = typecheck_expr(right, symbols)
                .context("Failed to typecheck righthand argument of binary operation.")?;

            let (lifted_left_t, lifted_right_t) =
                ast::BaseType::lift(left_t.base.clone(), right_t.base.clone())
                    .context("Unable to promote {left_t:#?} and {right_t:#?} to a common type.")?;
            // With a binary expression, we force both operands to be of
            // the same type so the decision between left or right is
            // arbitrary
            let common_t = ast::Type {
                base: lifted_left_t.clone(),
                storage: None,
                is_const: true,
                ..left_t
            };
            let left = if lifted_left_t != left_t.base {
                ast::Expr::Cast {
                    target: common_t.clone(),
                    exp: Box::new(left),
                }
            } else {
                left
            };
            let right = if lifted_right_t != right_t.base {
                ast::Expr::Cast {
                    target: common_t.clone(),
                    exp: Box::new(right),
                }
            } else {
                right
            };
            let exp = ast::Expr::Binary {
                op: *op,
                left: Box::new(left),
                right: Box::new(right),
            };
            if op.is_logical() {
                // Booleans are always represented as integers
                let target = ast::Type::bool();
                Ok(TypedExpr {
                    expr: ast::Expr::Cast {
                        target: target.clone(),
                        exp: Box::new(exp),
                    },
                    r#type: target,
                })
            } else {
                Ok(TypedExpr {
                    expr: exp,
                    r#type: common_t,
                })
            }
        }
        ast::Expr::Conditional {
            condition,
            then,
            r#else,
        } => {
            // TODO: Make two branches have same output type
            let target = ast::Type::bool();
            let condition = Box::new(try_implicit_cast(&target, condition, symbols).context(
                "Unable to implicitly cast ternary expression condition into a boolean value.",
            )?);
            let TypedExpr {
                expr: then_expr,
                r#type: then_type,
            } = typecheck_expr(then, symbols)
                .context("Failed to typecheck ternay expression then branch.")?;
            let TypedExpr {
                expr: else_expr,
                r#type: else_type,
            } = typecheck_expr(r#else, symbols)
                .context("Failed to typecheck ternay expression else branch.")?;
            let (then_base, _) = ast::BaseType::lift(then_type.base.clone(), else_type.base)
                .context("Ternary expression branches evaluate to different types.")?;
            let common_type = ast::Type {
                base: then_base,
                ..then_type.clone()
            };
            let then = Box::new(ast::Expr::Cast {
                target: common_type.clone(),
                exp: Box::new(then_expr),
            });
            let r#else = Box::new(ast::Expr::Cast {
                target: common_type,
                exp: Box::new(else_expr),
            });
            // Find common type between then and else
            Ok(TypedExpr {
                expr: ast::Expr::Conditional {
                    condition,
                    then,
                    r#else,
                },
                r#type: then_type,
            })
        }
        ast::Expr::FunCall { name, args } => match symbols.get(name) {
            Some(SymbolEntry {
                r#type:
                    ast::Type {
                        base: ast::BaseType::Fun { param_types, ret_t },
                        ..
                    },
                ..
            }) => {
                // FIXME: Lazy clones
                let param_types = param_types.clone();
                let ret_t = ret_t.clone();
                if args.len() != param_types.len() {
                    bail!(
                        "Expected {} args but received {} when calling \"{name}\".",
                        param_types.len(),
                        args.len()
                    );
                }
                let ret_t = *ret_t.clone();
                let args = args
                    .iter()
                    .zip(param_types.iter())
                    .map(|(arg, exp_t)| try_implicit_cast(exp_t, arg, symbols))
                    .collect::<Result<Vec<_>>>()?;
                Ok(TypedExpr {
                    expr: ast::Expr::FunCall {
                        name: Rc::clone(name),
                        args,
                    },
                    r#type: ret_t,
                })
            }
            Some(SymbolEntry { r#type: t, .. }) => {
                bail!("Expected function type, but found type {t}.")
            }
            _ => bail!("Could not find symbol with name {name}."),
        },
        expr @ ast::Expr::Cast { target, .. } => Ok(TypedExpr {
            expr: expr.clone(),
            r#type: target.clone(),
        }),
        expr @ ast::Expr::Constant(constant) => Ok(TypedExpr {
            expr: expr.clone(),
            r#type: ast::Type {
                base: ast::BaseType::from(constant),
                alignment: ast::BaseType::from(constant).default_alignment(),
                ptr: None,
                storage: None,
                is_const: true,
            },
        }),
    }
}

fn typecheck_decl(decl: ast::Declaration, symbols: &mut SymbolTable) -> Result<ast::Declaration> {
    Ok(match decl {
        ast::Declaration::FunDecl(decl) => {
            let name = Rc::clone(&decl.name);
            ast::Declaration::FunDecl(
                typecheck_fun_decl(decl, symbols)
                    .context(format!("Unable to typecheck \"{name}\" declaration"))?,
            )
        }
        ast::Declaration::VarDecl(decl) => {
            let name = Rc::clone(&decl.name);
            ast::Declaration::VarDecl(
                typecheck_var_decl(decl, symbols)
                    .context(format!("Unable to typecheck \"{name}\" declaration"))?,
            )
        }
    })
}

fn typecheck_fun_decl(decl: ast::FunDecl, symbols: &mut SymbolTable) -> Result<ast::FunDecl> {
    // Special case: Push scope and iterate over block items here so the
    // function parameters get put into the same scope as the block items
    symbols.push_scope();
    symbols.declare_fun(&decl)?;
    // Treat parameters as declarations without values
    let block = if let Some(block) = decl.block {
        let items = block
            .into_items()
            .into_iter()
            .map(|item| typecheck_block_item(item, symbols, Some(decl.name.clone())))
            .collect::<Result<Vec<_>>>()
            .context(format!(
                "Failed to typecheck function declaration for \"{}\"",
                decl.name
            ))?;
        Some(ast::Block(items))
    } else {
        None
    };
    symbols
        .pop_scope()
        .expect("We just popped the scope so this should not fail.");
    Ok(ast::FunDecl { block, ..decl })
}

fn typecheck_global_var_decl(
    decl: ast::VarDecl,
    symbols: &mut SymbolTable,
) -> Result<ast::VarDecl> {
    ensure!(
        symbols.scope() == Scope::Global,
        "Global vars must be declared in global scope"
    );
    typecheck_var_decl(decl, symbols)
}

fn typecheck_var_decl(decl: ast::VarDecl, symbols: &mut SymbolTable) -> Result<ast::VarDecl> {
    let entry = symbols.declare_var(&decl).context(format!(
        "Failed to typecheck local variable declaration: for {}",
        decl.name
    ))?;
    let target = entry.r#type;
    let init = if let Some(init) = decl.init {
        Some(try_implicit_cast(&target, &init, symbols).context(format!(
            "Failed to typecheck initialization for variable \"{}\": {init:#?}",
            decl.name
        ))?)
    } else {
        None
    };
    Ok(ast::VarDecl { init, ..decl })
}
