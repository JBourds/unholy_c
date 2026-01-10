use crate::ast;

use super::{SymbolTable, typecheck_block_item};

use anyhow::{Context, Result, ensure};

pub fn typecheck_fun_decl(decl: ast::FunDecl, symbols: &mut SymbolTable) -> Result<ast::FunDecl> {
    // Special case: Push scope and iterate over block items here so the
    // function parameters get put into the same scope as the block items
    symbols.push_scope();

    ensure!(
        !matches!(decl.r#type.base.clone(), ast::BaseType::Fun { ret_t, .. } if ret_t.is_array()),
        "Cannot have functions return array types",
    );

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
