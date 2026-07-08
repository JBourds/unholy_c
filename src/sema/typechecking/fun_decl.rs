use crate::{ast, sema::typechecking::validate_type_specifier};

use super::{SymbolTable, TypeTable, fixup_type, typecheck_block_item};

use anyhow::{Context, Result, ensure};

pub fn typecheck_fun_decl(
    decl: ast::FunDecl,
    symbols: &mut SymbolTable,
    structs: &mut TypeTable,
) -> Result<ast::FunDecl> {
    // Special case: Push scope and iterate over block items here so the
    // function parameters get put into the same scope as the block items
    symbols.push_scope();
    structs.push_scope();

    // Make sure that the function does not take or return incomplete types
    validate_type_specifier(&decl.r#type).context("Invalid type in variable declaration")?;
    let ast::BaseType::Fun {
        ref ret_t,
        ref param_types,
    } = decl.r#type.base
    else {
        panic!("Function declaration type isn't a function type?");
    };
    let ret_t = fixup_type(*ret_t.clone(), structs);
    let param_types: Vec<_> = param_types
        .iter()
        .map(|r#type| fixup_type(r#type.clone(), structs))
        .collect();
    ensure!(
        !param_types.iter().any(|ty| ty.is_void()),
        "Cannot have void function parameter arguments"
    );
    ensure!(
        !ret_t.is_array(),
        "Cannot have functions return array types"
    );

    let decl = ast::FunDecl {
        r#type: ast::Type {
            base: ast::BaseType::Fun {
                ret_t: ret_t.clone().into(),
                param_types: param_types.to_vec(),
            },
            ..decl.r#type
        },
        ..decl
    };

    symbols.declare_fun(&decl, structs)?;
    // Treat parameters as declarations without values
    let block = if let Some(block) = decl.block {
        if ret_t.is_struct() || ret_t.is_union() {
            ensure!(
                ret_t.is_complete(),
                "Cannot have functions return incomplete struct/union types"
            );
        }
        for param in param_types {
            if param.is_struct() || param.is_union() {
                ensure!(
                    param.is_complete(),
                    "param types cannot be incomplete struct/union types"
                );
            }
        }
        let items = block
            .into_items()
            .into_iter()
            .map(|item| typecheck_block_item(item, symbols, structs, Some(decl.name.clone())))
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
    structs
        .pop_scope()
        .expect("We just popped the scope so this should not fail.");
    Ok(ast::FunDecl { block, ..decl })
}
