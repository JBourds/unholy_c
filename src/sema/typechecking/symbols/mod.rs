use crate::ast;

use super::Attribute;

use anyhow::{Context, Result, bail};

use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct SymbolEntry {
    pub r#type: ast::Type,
    pub defined: bool,
    pub scope: Scope,
    pub attribute: Attribute,
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
    pub fn new_table() -> Self {
        Self {
            global: HashMap::new(),
            scopes: vec![],
        }
    }

    fn new_entry(&mut self, decl: &ast::Declaration, scope: Scope) -> Result<SymbolEntry> {
        let mut r#type: ast::Type = decl.into();

        if matches!(decl, &ast::Declaration::FunDecl(..)) {
            r#type = r#type.maybe_decay();
        }
        Ok(SymbolEntry {
            r#type,
            defined: decl.defining(),
            scope,
            attribute: Attribute::from_decl_with_scope(decl, scope, self)?,
        })
    }

    pub fn get(&self, key: &Rc<String>) -> Option<&SymbolEntry> {
        Self::get_local(&self.scopes, key).or(Self::get_global(&self.global, key))
    }

    pub fn get_mut(&mut self, key: &Rc<String>) -> Option<&mut SymbolEntry> {
        Self::get_local_mut(&mut self.scopes, key).or(Self::get_global_mut(&mut self.global, key))
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
    pub fn scope(&self) -> Scope {
        match self.scopes.len() {
            0 => Scope::Global,
            n => Scope::Local(n - 1),
        }
    }

    fn get_local<'a>(
        scopes: &'a Vec<HashMap<Rc<String>, SymbolEntry>>,
        key: &Rc<String>,
    ) -> Option<&'a SymbolEntry> {
        for scope in scopes.iter().rev() {
            if let Some(entry) = scope.get(key) {
                return Some(entry);
            }
        }
        None
    }

    fn get_local_mut<'a>(
        scopes: &'a mut Vec<HashMap<Rc<String>, SymbolEntry>>,
        key: &Rc<String>,
    ) -> Option<&'a mut SymbolEntry> {
        for scope in scopes.iter_mut().rev() {
            if let Some(entry) = scope.get_mut(key) {
                return Some(entry);
            }
        }
        None
    }

    fn get_global<'a>(
        globals: &'a HashMap<Rc<String>, SymbolEntry>,
        key: &Rc<String>,
    ) -> Option<&'a SymbolEntry> {
        globals.get(key)
    }

    fn get_global_mut<'a>(
        globals: &'a mut HashMap<Rc<String>, SymbolEntry>,
        key: &Rc<String>,
    ) -> Option<&'a mut SymbolEntry> {
        globals.get_mut(key)
    }

    fn get_decl_info(
        decl: &ast::Declaration,
    ) -> (Rc<String>, ast::Type, Option<ast::StorageClass>, bool) {
        match decl {
            ast::Declaration::FunDecl(fun) => (
                Rc::clone(&fun.name),
                ast::Type::from(fun).maybe_decay(),
                fun.storage_class.as_ref().copied(),
                fun.block.is_some(),
            ),
            ast::Declaration::VarDecl(ast::VarDecl {
                r#type,
                name,
                init,
                storage_class,
            }) => (
                Rc::clone(name),
                r#type.clone(),
                storage_class.as_ref().copied(),
                init.is_some(),
            ),
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
                        "Redeclaring function \"{name}\" as static when it was previously defined with external linkage"
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
                                bail!(
                                    "Global variable \"{name}\" was previously declared as static"
                                );
                            }
                        } else {
                            // If we (foo) are declared extern,
                            // then we cannot be redeclared as static
                            if storage_class == Some(ast::StorageClass::Static) {
                                bail!(
                                    "Redeclaring variable \"{name}\" as static when it was previously defined with external linkage"
                                );
                            }
                        }
                    }
                    Scope::Local(..) => match storage_class {
                        Some(ast::StorageClass::Extern) => {} // Vars with linkage can be declared multiple times
                        Some(ast::StorageClass::Static) | None => {
                            bail!("Variable \"{name}\" declared multiple times in scope")
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
        let (name, new_type, storage_class, defining_ident) = Self::get_decl_info(decl);

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
                    bail!(
                        "Redeclaring \"{name}\" as \"{new_type}\" when it was previously declared as \"{old_type}\""
                    );
                }
                if already_defined && defining_ident {
                    bail!("Redefining \"{name}\" when it is already defined.")
                }
                let mut attribute =
                    Self::check_attribute(&old_attrib, &name, storage_class, scope)?;
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
    pub fn declare_fun(&mut self, decl: &ast::FunDecl) -> Result<()> {
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
            for (r#type, name) in decl.signature()
                .context("sema.typechecking.declare_fun(): Error getting function declaration in signature.")?
                .into_iter() {
                if let Some(name) = name {
                    let param_decl = ast::Declaration::VarDecl(ast::VarDecl {
                        name: Rc::clone(name),
                        init: None,
                        r#type: r#type.clone(),
                        storage_class: None,
                    });
                    self.declare_in_scope(&param_decl, scope)?;
                }
            }
        }
        Ok(())
    }

    pub fn declare_var(&mut self, decl: &ast::VarDecl) -> Result<SymbolEntry> {
        let key = decl.name.clone();
        let storage_class = decl.storage_class;
        let decl = ast::Declaration::VarDecl(decl.clone());
        match storage_class {
            Some(ast::StorageClass::Extern) => self.declare_in_scope(&decl, Scope::Global),
            Some(ast::StorageClass::Static)
                if Self::get_global(&self.global, &key).is_none()
                    && self.scope() != Scope::Global =>
            {
                self.declare_in_scope(&decl, Scope::Global)
            }
            _ => self.declare_in_scope(&decl, self.scope()),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) -> Result<()> {
        if self.scopes.is_empty() {
            bail!("Already in global scope, cannot pop symbol table.")
        } else {
            self.scopes.pop();
            Ok(())
        }
    }
}
