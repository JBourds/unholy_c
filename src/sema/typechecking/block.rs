use super::{SymbolTable, TypeTable, typecheck_decl, typecheck_stmt};
use crate::ast;

use anyhow::{Context, Result};

use std::rc::Rc;

pub fn typecheck_block(
    block: ast::Block,
    symbols: &mut SymbolTable,
    structs: &mut TypeTable,
    function: Option<Rc<String>>,
) -> Result<ast::Block> {
    symbols.push_scope();
    structs.push_scope();
    let items = block
        .into_items()
        .into_iter()
        .map(|item| typecheck_block_item(item, symbols, structs, function.clone()))
        .collect::<Result<Vec<_>>>()
        .context("Failed to typecheck all block items")?;
    symbols
        .pop_scope()
        .expect("We just popped the scope so this should not fail.");
    structs
        .pop_scope()
        .expect("we just popped the scope so this should not fail");
    Ok(ast::Block(items))
}

pub fn typecheck_block_item(
    item: ast::BlockItem,
    symbols: &mut SymbolTable,
    structs: &mut TypeTable,
    function: Option<Rc<String>>,
) -> Result<ast::BlockItem> {
    match item {
        ast::BlockItem::Stmt(stmt) => Ok(ast::BlockItem::Stmt(
            typecheck_stmt(stmt, symbols, structs, function)
                .context("Failed to typecheck block item")?,
        )),
        ast::BlockItem::Decl(decl) => Ok(ast::BlockItem::Decl(
            typecheck_decl(decl, symbols, structs).context("Failed to typecheck block item")?,
        )),
    }
}
