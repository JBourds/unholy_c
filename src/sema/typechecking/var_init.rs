use super::{Attribute, InitialValue, Scope, SymbolTable, TypeTable, fixup_type, typecheck_init};
use crate::ast::StorageClass;
use crate::sema::typechecking::validate_type_specifier;
use crate::{ast, sema::typechecking::init::pad_compound_init};

use anyhow::{Context, Result, ensure};

pub fn typecheck_global_var_decl(
    decl: ast::VarDecl,
    symbols: &mut SymbolTable,
    structs: &TypeTable,
) -> Result<ast::VarDecl> {
    ensure!(
        symbols.scope() == Scope::Global,
        "Global vars must be declared in global scope"
    );
    typecheck_var_decl(decl, symbols, structs)
}

pub fn typecheck_var_decl(
    decl: ast::VarDecl,
    symbols: &mut SymbolTable,
    structs: &TypeTable,
) -> Result<ast::VarDecl> {
    let target = fixup_type(decl.r#type.clone(), structs);
    validate_type_specifier(&target).context("Invalid type in variable declaration")?;
    ensure!(!target.is_void(), "Unholy C does not allow void variables!");
    let entry = symbols.declare_var(&decl, structs).context(format!(
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
            if let Attribute::Static {
                initial_value: _,
                external_linkage,
            } = entry.attribute
            {
                // Make sure the init is fixed up
                let init = pad_compound_init(&target, init, symbols, structs, &decl.name)?;
                let attribute = Attribute::Static {
                    initial_value: InitialValue::from_initializer(&target, &init, symbols, structs)
                        .context("unable to create initial value from initializer")?,
                    external_linkage,
                };
                if let Some(entry) = symbols.get_mut(&decl.name) {
                    entry.attribute = attribute;
                }
                ast::VarDecl {
                    init: Some(init),
                    r#type: target,
                    ..decl
                }
            } else {
                ast::VarDecl {
                    init: Some(typecheck_init(&target, init, symbols, structs, &decl.name)?),
                    r#type: target,
                    ..decl
                }
            }
        }
        None => ast::VarDecl {
            init: None,
            r#type: target,
            ..decl
        },
    };
    Ok(decl)
}
