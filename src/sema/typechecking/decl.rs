use crate::ast;

use super::{
    SymbolTable, TypeTable, typecheck_fun_decl, typecheck_struct_decl, typecheck_union_decl,
    typecheck_var_decl,
};

use anyhow::{Context, Result};

use std::rc::Rc;

pub fn typecheck_decl(
    decl: ast::Declaration,
    symbols: &mut SymbolTable,
    structs: &mut TypeTable,
) -> Result<ast::Declaration> {
    Ok(match decl {
        ast::Declaration::FunDecl(decl) => {
            let name = Rc::clone(&decl.name);
            ast::Declaration::FunDecl(
                typecheck_fun_decl(decl, symbols, structs)
                    .context(format!("Unable to typecheck \"{name}\" declaration"))?,
            )
        }
        ast::Declaration::VarDecl(decl) => {
            let name = Rc::clone(&decl.name);
            ast::Declaration::VarDecl(
                typecheck_var_decl(decl, symbols, structs)
                    .context(format!("Unable to typecheck \"{name}\" declaration"))?,
            )
        }
        ast::Declaration::StructDecl(decl) => {
            let name = Rc::clone(&decl.tag);
            ast::Declaration::StructDecl(
                typecheck_struct_decl(decl, structs)
                    .context(format!("Unable to typecheck \"{name}\" declaration"))?,
            )
        }
        ast::Declaration::UnionDecl(..) => todo!(),
    })
}
