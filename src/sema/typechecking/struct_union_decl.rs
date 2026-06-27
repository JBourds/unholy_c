use crate::{ast, sema::typechecking::validate_type_specifier};

use super::{SymbolTable, TypeTable, typecheck_block_item};

use anyhow::{Context, Result, ensure};

pub fn typecheck_struct_decl(
    decl: ast::StructDecl,
    symbols: &mut SymbolTable,
    structs: &mut TypeTable,
) -> Result<ast::StructDecl> {
    todo!()
}

pub fn typecheck_union_decl(
    decl: ast::StructDecl,
    symbols: &mut SymbolTable,
    structs: &mut TypeTable,
) -> Result<ast::StructDecl> {
    todo!()
}
