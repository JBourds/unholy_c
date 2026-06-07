use super::{Attribute, InitialValue, Scope, SymbolTable, typecheck_init};
use crate::ast::StorageClass;
use crate::{ast, sema::typechecking::init::pad_compound_init};

use anyhow::{Context, Result, ensure};

pub fn typecheck_global_var_decl(
    decl: ast::VarDecl,
    symbols: &mut SymbolTable,
) -> Result<ast::VarDecl> {
    ensure!(
        symbols.scope() == Scope::Global,
        "Global vars must be declared in global scope"
    );
    typecheck_var_decl(decl, symbols)
}

pub fn typecheck_var_decl(decl: ast::VarDecl, symbols: &mut SymbolTable) -> Result<ast::VarDecl> {
    let target = &decl.r#type;
    let entry = symbols.declare_var(&decl).context(format!(
        "Failed to typecheck local variable declaration: for {}",
        decl.name
    ))?;
    if decl
        .storage_class
        .is_some_and(|cls| cls == StorageClass::Extern)
    {
        ensure!(
            decl.init.is_none(),
            "Cannot provide a definition for a variable with extern storage class."
        );
    }
    let decl = match decl.init {
        Some(init) => {
            // Make sure the init is fixed up
            let init = pad_compound_init(target, init, symbols, &decl.name)?;
            if let Attribute::Static {
                initial_value: _,
                external_linkage,
            } = entry.attribute
            {
                let attribute = Attribute::Static {
                    initial_value: InitialValue::from_initializer(target, &init, symbols)
                        .context("unable to create initial value from initializer")?,
                    external_linkage,
                };
                if let Some(entry) = symbols.get_mut(&decl.name) {
                    entry.attribute = attribute;
                }
            }
            // AST rewrite happens here, make sure we do this after creating
            // the InitialValue for globals/statics otherwise it gets rewritten
            // twice causing typechecking to fail.
            let init = typecheck_init(target, init, symbols, &decl.name)?;
            ast::VarDecl {
                init: Some(init),
                ..decl
            }
        }
        None => ast::VarDecl { init: None, ..decl },
    };
    Ok(decl)
}
